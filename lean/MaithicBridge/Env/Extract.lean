/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Extract
import Maithic.Trace
import Maithic.Lint
import MaithicBridge.Env.Elab
import MaithicBridge.Proof.Session

/-!
# Extraction

Elaborates a whole file with information trees switched on and reads out what the libraries
`Maithic.Extract`, `Maithic.Trace` and `Maithic.Lint` can tell about it: the declarations with
their dependencies and documentation, the imports, the tactic steps with the goals before and
after, the open `sorry` positions with their goals, and the findings of the lint rules.

The file is read and nothing of it is kept, but the environment of its imports stays in the
store like any other header. An import list costs memory in proportion to what it imports, so a
client that reads many files with different imports should use a process for each, as the
`maithic` command does. A file with errors is described as far as it elaborated.
-/

namespace Maithic.Bridge

open Lean Elab

/-- What `extract` is asked to read out besides the declarations. -/
structure ExtractOptions where
  module : String := ""
  /-- The tactic steps of every proof. -/
  trace : Bool := false
  /-- Theorems elsewhere in the environment that state the same thing. -/
  duplicates : Bool := false
  /-- Run lint rules; all that ship when `rules` is empty. -/
  lint : Bool := false
  rules : Array String := #[]
  convention : Maithic.Lint.Convention := {}

/-- A `sorry` in tactic position: where it stands and what is to be shown there. -/
structure Hole where
  decl : String
  line : Nat
  column : Nat
  goals : Array String
  deriving ToJson

/-- Elaborates a file and describes it. -/
def extractFile (source : String) (options : ExtractOptions) (limits : Limits) : HandlerM Json := do
  let startMs ← IO.monoMsNow
  let rules ← match Maithic.Lint.select Maithic.Lint.builtin options.rules with
    | .ok rules => pure (if options.rules.isEmpty then Maithic.Lint.builtin else rules)
    | .error reason => throw (.invalidParams reason)
  let header ← sourceHeader source
  let (headerId, _) ← loadHeader header.imports
  let base := (← getEnv headerId).state
  modify fun s => { s with elaborations := s.elaborations + 1 }
  let state := modifyOptions { base with messages := {} } fun opts =>
    (limits.apply opts).set `Elab.async false
  let state := { state with infoState := { enabled := true } }
  let done ← IO.processCommands header.inputCtx header.parserState state
  let after := done.commandState
  let report ← mkReport base.env after #["_"] startMs
  let report := { report with decls := #[] }
  let fileMap := header.inputCtx.fileMap
  let trees := after.infoState.trees
  let decls ← Maithic.Extract.runMeta after.env
    (Maithic.Extract.declarationsSince base.env options.module options.duplicates)
  let decls := Maithic.Extract.attachTexts decls (Maithic.Extract.declarationTexts fileMap trees)
  let steps ← if options.trace then Maithic.Trace.steps fileMap trees else pure #[]
  let mut holes : Array Hole := #[]
  for (ctx, info) in sorryHoles trees do
    let position := fileMap.toPosition (info.stx.getPos?.getD 0)
    let name := match ctx.parentDecl? with
      | some n => toString ((privateToUserName? n).getD n)
      | none => ""
    holes := holes.push {
      decl := name, line := position.line, column := position.column,
      goals := ← Maithic.Trace.goalTexts ctx info.mctxBefore info.goalsBefore }
  let findings := if options.lint then
      Maithic.Lint.run rules { declarations := decls, convention := options.convention }
    else #[]
  let now ← IO.monoMsNow
  return Json.mkObj [
    ("report", toJson { report with elapsedMs := now - startMs }),
    ("module", Json.str options.module),
    ("imports", toJson header.imports.toList.eraseDups),
    ("moduleDocs", toJson (Maithic.Extract.moduleDocs after.env)),
    ("declarations", toJson decls),
    ("steps", toJson steps),
    ("holes", toJson holes),
    ("findings", toJson findings)]

end Maithic.Bridge
