/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Env.Elab
import MaithicBridge.Proof.Session
import MaithicBridge.Env.Extract

/-!
# Request handlers

One handler per method. A handler receives the `params` object of the request and returns the
`result` object of the response, or fails with a typed error.
-/

namespace Maithic.Bridge

open Lean

/-- The methods the server understands, as reported by `handshake`. -/
def methods : Array String := #[
  "handshake", "loadHeader", "runCommand", "checkFile", "extract", "tryCandidates", "releaseEnv", "guard",
  "startProof", "runTactic", "tryTactics", "getState", "releaseState", "stats", "selfTest",
  "shutdown"]

/-- `handshake`: versions and the list of methods. -/
def handleHandshake (_ : Json) : HandlerM Json :=
  return Json.mkObj [
    ("bridge", Json.str bridgeVersion),
    ("protocol", toJson protocolVersion),
    ("lean", Json.str Lean.versionString),
    ("methods", toJson methods)]

/-- `loadHeader`: import a list of modules and return a handle to the base environment. -/
def handleLoadHeader (params : Json) : HandlerM Json := do
  let imports : Array String := optField params "imports" #[]
  let (env, cached) ← loadHeader imports
  return Json.mkObj [("env", toJson env), ("cached", Json.bool cached)]

/-- Encodes a run result. -/
def runResultJson (r : RunResult) : Json :=
  match r.env with
  | some id => Json.mkObj [("report", toJson r.report), ("env", toJson id)]
  | none => Json.mkObj [("report", toJson r.report)]

/-- `runCommand`: elaborate commands on a stored environment. -/
def handleRunCommand (params : Json) : HandlerM Json := do
  let env : Nat ← param params "env"
  let source : String ← param params "source"
  let names : Array String := optField params "names" #[]
  let keep : Bool := optField params "keep" false
  let r ← runCommand env source names keep (Limits.ofParams params)
  return runResultJson r

/-- `checkFile`: elaborate a whole file, header included. -/
def handleCheckFile (params : Json) : HandlerM Json := do
  let source : String ← param params "source"
  let names : Array String := optField params "names" #[]
  let keep : Bool := optField params "keep" false
  let r ← checkFile source names keep (Limits.ofParams params)
  return runResultJson r

/-- The naming convention of a request. Each of `theorems`, `definitions` and `types` may be given;
what is not given keeps its default, and a convention the rule for names does not know is refused,
since it would switch the rule off without a word. -/
def conventionOf (params : Json) : Except BridgeError Maithic.Lint.Convention := do
  let standard : Maithic.Lint.Convention := {}
  let given := (params.getObjVal? "convention").toOption.getD (Json.mkObj [])
  let read (key standard : String) : Except BridgeError String :=
    match given.getObjValAs? String key with
    | .error _ => pure standard
    | .ok name =>
      if Maithic.Lint.conventions.contains name then pure name
      else throw (.invalidParams
        s!"'{name}' is not a naming convention for {key}; the conventions are {", ".intercalate Maithic.Lint.conventions}")
  return {
    theorems := ← read "theorems" standard.theorems,
    definitions := ← read "definitions" standard.definitions,
    types := ← read "types" standard.types }

/-- `extract`: elaborate a whole file and describe it: declarations, imports, and on request the
tactic steps, the theorems that state the same thing, and the findings of the lint rules. -/
def handleExtract (params : Json) : HandlerM Json := do
  let source : String ← param params "source"
  let convention ← conventionOf params
  let options : ExtractOptions := {
    module := optField params "module" "",
    trace := optField params "trace" false,
    duplicates := optField params "duplicates" false,
    lint := optField params "lint" false,
    rules := optField params "rules" #[],
    convention }
  extractFile source options (Limits.ofParams params)

/-- `tryCandidates`: elaborate guarded candidates on a stored environment. -/
def handleTryCandidates (params : Json) : HandlerM Json := do
  let env : Nat ← param params "env"
  let candidates : Array Candidate ← param params "candidates"
  let names : Array String := optField params "names" #[]
  let stopOnFirst : Bool := optField params "stopOnFirst" false
  let results ← tryCandidates env candidates names stopOnFirst (Limits.ofParams params)
  return Json.mkObj [("results", toJson results)]

/-- `releaseEnv`: free a stored environment. -/
def handleReleaseEnv (params : Json) : HandlerM Json := do
  let env : Nat ← param params "env"
  let released ← releaseEnv env
  return Json.mkObj [("released", Json.bool released)]

/-- `guard`: run the candidate guard alone. -/
def handleGuard (params : Json) : HandlerM Json := do
  let candidate : String ← param params "candidate"
  match Guard.violation candidate with
  | none => return Json.mkObj [("admitted", Json.bool true), ("rejection", Json.str "")]
  | some reason => return Json.mkObj [("admitted", Json.bool false), ("rejection", Json.str reason)]

/-- The optional `goal` parameter: the position of the goal a tactic is focused on. -/
def goalParam (params : Json) : Option Nat :=
  (params.getObjValAs? Nat "goal").toOption

/-- `startProof`: open proof states at the `sorry` positions of declarations. The text is given as
`source`, or as a `statement` (a declaration up to its `:=`), which is completed with `by sorry`. -/
def handleStartProof (params : Json) : HandlerM Json := do
  let env : Nat ← param params "env"
  let source ← match params.getObjValAs? String "statement" with
    | .ok statement => pure (statement ++ " := by\n  sorry")
    | .error _ => (param params "source" : HandlerM String)
  let r ← startProof env source (Limits.ofParams params)
  return Json.mkObj [("report", toJson r.report), ("states", toJson r.states)]

/-- `runTactic`: run a tactic on a proof state. -/
def handleRunTactic (params : Json) : HandlerM Json := do
  let state : Nat ← param params "state"
  let tactic : String ← param params "tactic"
  let outcome ← runTactic state tactic (goalParam params) (Limits.ofParams params)
  return toJson outcome

/-- `tryTactics`: run several tactics on the same proof state. -/
def handleTryTactics (params : Json) : HandlerM Json := do
  let state : Nat ← param params "state"
  let tactics : Array String ← param params "tactics"
  let stopOnSolved : Bool := optField params "stopOnSolved" false
  let outcomes ← tryTactics state tactics (goalParam params) stopOnSolved (Limits.ofParams params)
  return Json.mkObj [("results", toJson outcomes)]

/-- `getState`: the goals of a stored proof state and where it comes from. -/
def handleGetState (params : Json) : HandlerM Json := do
  let state : Nat ← param params "state"
  let entry ← getProof state
  let goals ← entry.snapshot.goalViews
  let fields : List (String × Json) := [
    ("state", toJson state), ("env", toJson entry.env), ("depth", toJson entry.depth),
    ("goals", toJson goals), ("solved", Json.bool goals.isEmpty)]
  match entry.parent with
  | some parent => return Json.mkObj (fields ++ [("parent", toJson parent)])
  | none => return Json.mkObj fields

/-- `releaseState`: free a stored proof state. -/
def handleReleaseState (params : Json) : HandlerM Json := do
  let state : Nat ← param params "state"
  let released ← releaseProof state
  return Json.mkObj [("released", Json.bool released)]

/-- `stats`: counters of the session. -/
def handleStats (_ : Json) : HandlerM Json := do
  let s ← get
  return Json.mkObj [
    ("requests", toJson s.requests),
    ("environments", toJson s.envs.size),
    ("proofStates", toJson s.proofs.size),
    ("headers", toJson s.headers.size),
    ("headerHits", toJson s.headerHits),
    ("headerMisses", toJson s.headerMisses),
    ("elaborations", toJson s.elaborations),
    ("tacticRuns", toJson s.tacticRuns)]

/-- One step of the self-test: a name and whether it passed. -/
structure SelfCheck where
  name : String
  passed : Bool
  deriving ToJson, FromJson, Repr

/-- `selfTest`: exercise the core functions end to end and report health. The server proves a
small theorem, confirms that a false one is rejected, that an injected axiom is visible in the
audit, that the guard blocks an evaluation command, and that a proof state can be opened, refuses
a wrong tactic and is closed by a right one. The states it opens are released again. -/
def handleSelfTest (_ : Json) : HandlerM Json := do
  let (env, _) ← loadHeader #[]
  let good ← runCommand env "theorem maithic_selftest_ok : 2 + 2 = 4 := rfl"
    #["maithic_selftest_ok"] false {}
  let goodOk := good.report.ok && good.report.decls.all fun d =>
    d.found && d.kind == "theorem" && d.axioms.isEmpty
  let bad ← runCommand env "theorem maithic_selftest_bad : 2 + 2 = 5 := rfl"
    #["maithic_selftest_bad"] false {}
  let badOk := !bad.report.ok
  let ax ← runCommand env
    "axiom maithic_selftest_ax : False\ntheorem maithic_selftest_cheat : 1 = 2 := maithic_selftest_ax.elim"
    #["maithic_selftest_cheat"] false {}
  let axOk := ax.report.decls.all fun d => d.axioms.contains "maithic_selftest_ax"
  let guardOk := !Guard.admits "by #eval IO.println 1" && Guard.admits "by omega"
  let opened ← startProof env "theorem maithic_selftest_step (n : Nat) : n + 0 = n := by\n  sorry" {}
  let stepOk ← match opened.states[0]? with
    | none => pure false
    | some first => do
      let wrong ← runTactic first.state "exact trivial" none {}
      let right ← runTactic first.state "rfl" none {}
      let _ ← releaseProof first.state
      if let some id := right.state then
        let _ ← releaseProof id
      pure (first.goals.size == 1 && !wrong.ok && right.ok && right.solved)
  let checks : Array SelfCheck := #[
    { name := "kernel accepts a true statement", passed := goodOk },
    { name := "kernel rejects a false statement", passed := badOk },
    { name := "axiom audit sees injected axioms", passed := axOk },
    { name := "guard blocks evaluation commands", passed := guardOk },
    { name := "tactics step a proof state to its end", passed := stepOk }]
  return Json.mkObj [
    ("healthy", Json.bool (checks.all (·.passed))),
    ("checks", toJson checks)]

/-- Dispatches a request to its handler. `shutdown` is handled by the loop. -/
def handle (method : String) (params : Json) : HandlerM Json :=
  match method with
  | "handshake" => handleHandshake params
  | "loadHeader" => handleLoadHeader params
  | "runCommand" => handleRunCommand params
  | "checkFile" => handleCheckFile params
  | "extract" => handleExtract params
  | "tryCandidates" => handleTryCandidates params
  | "releaseEnv" => handleReleaseEnv params
  | "guard" => handleGuard params
  | "startProof" => handleStartProof params
  | "runTactic" => handleRunTactic params
  | "tryTactics" => handleTryTactics params
  | "getState" => handleGetState params
  | "releaseState" => handleReleaseState params
  | "stats" => handleStats params
  | "selfTest" => handleSelfTest params
  | "shutdown" => return Json.mkObj [("bye", Json.bool true)]
  | other => throw (.methodNotFound other)

end Maithic.Bridge
