/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Env.Store
import MaithicBridge.Server.Guard

/-!
# Elaboration and reports

Runs Lean's own frontend on request text and turns the outcome into wire types. The kernel is
part of that frontend: a declaration appears in the resulting environment only after the kernel
has accepted it, and the axioms it rests on are collected from the environment itself.
-/

namespace Maithic.Bridge

open Lean Elab

/-- Replaces the options of the outermost scope of a command state. -/
def modifyOptions (s : Command.State) (f : Options → Options) : Command.State :=
  match s.scopes with
  | [] => s
  | sc :: rest => { s with scopes := { sc with opts := f sc.opts } :: rest }

/-- Converts one message to a diagnostic. -/
def toDiagnostic (m : Message) : IO Diagnostic := do
  let severity := match m.severity with
    | .information => "info"
    | .warning => "warning"
    | .error => "error"
  let text ← m.data.toString
  return {
    severity, line := m.pos.line, column := m.pos.column,
    endLine := m.endPos.map (·.line), endColumn := m.endPos.map (·.column), text }

/-- The textual kind of a constant. -/
def constKind : ConstantInfo → String
  | .axiomInfo _ => "axiom"
  | .defnInfo _ => "def"
  | .thmInfo _ => "theorem"
  | .opaqueInfo _ => "opaque"
  | .quotInfo _ => "quot"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "constructor"
  | .recInfo _ => "recursor"

/-- Runs a `CoreM` action against an environment. -/
def runCore (env : Environment) (x : CoreM α) : IO α := do
  let ctx : Core.Context := { fileName := virtualFile, fileMap := default }
  let st : Core.State := { env }
  let (a, _) ← x.toIO ctx st
  return a

/-- Pretty-prints an expression in the given environment, falling back to the raw form. -/
def ppIn (env : Environment) (e : Expr) : IO String := do
  try
    let fmt ← runCore env (Meta.ppExpr e).run'
    return fmt.pretty 100
  catch _ =>
    return toString e

/-- The axioms a constant depends on, sorted by name. -/
def axiomsIn (env : Environment) (n : Name) : IO (Array Name) := do
  let axs ← runCore env (collectAxioms n)
  return axs.qsort Name.lt

/-- Reports the facts about one declaration. -/
def declReport (env : Environment) (name : String) : IO DeclReport := do
  let n := name.toName
  match env.find? n with
  | none => return { name, found := false }
  | some ci =>
    let type ← ppIn env ci.type
    let axs ← axiomsIn env n
    return {
      name, found := true, kind := constKind ci, type,
      axioms := axs.map toString,
      usesSorry := axs.contains ``sorryAx }

/-- Names declared in `env` that are absent from `base` and that somebody wrote: a declaration
has a place in the source, and what Lean derives from it, such as the equations of a recursive
definition, has none. -/
def newDeclNames (base env : Environment) : Array String :=
  let names := env.constants.map₂.foldl (init := #[]) fun acc n _ =>
    if base.contains n || n.isInternalDetail || (declRangeExt.find? env n).isNone then acc else acc.push n
  (names.qsort Name.lt).map toString

/-- Builds the report for a finished elaboration. When `names` is empty the report covers every
declaration added since `since`. -/
def mkReport (since : Environment) (state : Command.State) (names : Array String)
    (startMs : Nat) : IO CheckReport := do
  let diagnostics ← state.messages.toList.toArray.mapM toDiagnostic
  let errors := (diagnostics.filter (·.severity == "error")).size
  let warnings := (diagnostics.filter (·.severity == "warning")).size
  let names := if names.isEmpty then newDeclNames since state.env else names
  let decls ← names.mapM (declReport state.env)
  let now ← IO.monoMsNow
  return { ok := errors == 0, errors, warnings, diagnostics, decls, elapsedMs := now - startMs }

/-- Elaborates commands from a parser position on top of a command state. -/
def elabFrom (inputCtx : Parser.InputContext) (parserState : Parser.ModuleParserState)
    (state : Command.State) (limits : Limits) : SessionM Command.State := do
  modify fun s => { s with elaborations := s.elaborations + 1 }
  let state := modifyOptions { state with messages := {} } limits.apply
  let st ← IO.processCommands inputCtx parserState state
  return st.commandState

/-- Elaborates source text without a header on top of a command state. -/
def elabText (state : Command.State) (source : String) (limits : Limits) :
    SessionM Command.State :=
  elabFrom (Parser.mkInputContext source virtualFile) {} state limits

/-- The result of running commands on a stored environment. -/
structure RunResult where
  report : CheckReport
  /-- Handle of the derived environment, when it was kept. -/
  env : Option Nat

/-- Runs commands on a stored environment. The derived environment is stored only when `keep` is
set and elaboration produced no error. The parent is never modified. -/
def runCommand (envId : Nat) (source : String) (names : Array String) (keep : Bool)
    (limits : Limits) : HandlerM RunResult := do
  let startMs ← IO.monoMsNow
  let parent ← getEnv envId
  let after ← elabText parent.state source limits
  let report ← mkReport parent.state.env after names startMs
  if keep && report.ok then
    let id ← putEnv { parent with state := after, parent := some envId, depth := parent.depth + 1 }
    return { report, env := some id }
  else
    return { report, env := none }

/-- Elaborates a whole file: the header is resolved through the header cache and the commands run
on top of it. Diagnostics carry positions in the original text. -/
def checkFile (source : String) (names : Array String) (keep : Bool) (limits : Limits) :
    HandlerM RunResult := do
  let startMs ← IO.monoMsNow
  let header ← sourceHeader source
  let (headerId, _) ← loadHeader header.imports
  let parent ← getEnv headerId
  let after ← elabFrom header.inputCtx header.parserState parent.state limits
  let report ← mkReport parent.state.env after names startMs
  if keep && report.ok then
    let id ← putEnv { parent with state := after, parent := some headerId, depth := 1 }
    return { report, env := some id }
  else
    return { report, env := none }

/-- Tries candidates on a stored environment, in order. The untrusted part of each candidate must
pass the guard before the candidate is elaborated. With `stopOnFirst`, candidates after the first
one that elaborates without error are skipped. -/
def tryCandidates (envId : Nat) (candidates : Array Candidate) (names : Array String)
    (stopOnFirst : Bool) (limits : Limits) : HandlerM (Array CandidateReport) := do
  let parent ← getEnv envId
  let mut out : Array CandidateReport := #[]
  for h : i in [0:candidates.size] do
    let c := candidates[i]
    let untrusted := if c.untrusted.isEmpty then c.source else c.untrusted
    match Guard.violation untrusted with
    | some reason =>
      out := out.push { index := i, admitted := false, rejection := reason }
    | none =>
      let startMs ← IO.monoMsNow
      let after ← elabText parent.state c.source limits
      let report ← mkReport parent.state.env after names startMs
      out := out.push { index := i, admitted := true, report := some report }
      if stopOnFirst && report.ok then
        break
  return out

end Maithic.Bridge
