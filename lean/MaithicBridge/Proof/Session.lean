/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Proof.Snapshot

/-!
# Opening and stepping proofs

The requests that work on proof states. `startProof` elaborates declarations whose proofs say
`sorry` and stores a state for every such position; `runTactic` runs a tactic on a stored state
and stores the state after it; `tryTactics` does that for many tactics from the same state, which
is the shape of one expansion step of a proof search.

Tactic text is untrusted in the same way candidate proofs are: it passes the guard before it is
parsed. A tactic that fails leaves no state behind and is reported as an outcome, not as a
protocol error: for a search, failure is the common case.
-/

namespace Maithic.Bridge

open Lean Elab

/-- A proof state opened by `startProof`: its handle, where its `sorry` stands, and its goals. -/
structure OpenedProof where
  state : Nat
  line : Nat
  column : Nat
  goals : Array Goal
  deriving ToJson, FromJson, Repr, BEq

/-- The result of `startProof`. -/
structure StartResult where
  report : CheckReport
  states : Array OpenedProof

/-- Elaborates source text with information trees switched on and declarations elaborated in
order, so that the state at every tactic position is on record when elaboration returns. -/
def elabWithInfo (state : Command.State) (source : String) (limits : Limits) :
    SessionM (Command.State × FileMap) := do
  modify fun s => { s with elaborations := s.elaborations + 1 }
  let inputCtx := Parser.mkInputContext source virtualFile
  let state := modifyOptions { state with messages := {} } fun opts =>
    (limits.apply opts).set `Elab.async false
  let state := { state with infoState := { enabled := true } }
  let st ← IO.processCommands inputCtx {} state
  return (st.commandState, inputCtx.fileMap)

/-- Opens proofs on a stored environment. `source` holds one or more declarations whose proofs
say `sorry` in tactic position; a state is stored for each such position, in source order. When
the text has errors, no state is opened and the report says why. The environment is not changed
and the declarations are not added to it. -/
def startProof (envId : Nat) (source : String) (limits : Limits) : HandlerM StartResult := do
  let startMs ← IO.monoMsNow
  let parent ← getEnv envId
  let (after, fileMap) ← elabWithInfo parent.state source limits
  let report ← mkReport parent.state.env after #[] startMs
  if !report.ok then
    return { report, states := #[] }
  let mut states : Array OpenedProof := #[]
  for (ctx, info) in sorryHoles after.infoState.trees do
    let snapshot ← ProofSnapshot.ofTacticInfo ctx info
    let goals ← snapshot.goalViews
    let id ← putProof { snapshot, env := envId, parent := none, depth := 0 }
    let position := fileMap.toPosition (info.stx.getPos?.getD 0)
    states := states.push { state := id, line := position.line, column := position.column, goals }
  return { report, states }

/-- Focuses a tactic on one goal: the goal at `index` becomes the only goal while the tactic runs,
and the goals it leaves take its place among the others. -/
def onGoal (index : Nat) (x : Tactic.TacticM Unit) : Tactic.TacticM Unit := do
  let goals ← Tactic.getGoals
  match goals[index]? with
  | none => throwError "there is no goal {index}; the state has {goals.length} goal(s)"
  | some goal =>
    Tactic.setGoals [goal]
    x
    let rest ← Tactic.getGoals
    Tactic.setGoals (goals.take index ++ rest ++ goals.drop (index + 1))

/-- Runs tactic text on a stored proof state. With `goal`, the tactic sees only the goal at that
position. The parent state is never modified; on success the state after the tactic is stored
and its handle returned. -/
def runTactic (stateId : Nat) (tactic : String) (goal : Option Nat) (limits : Limits) :
    HandlerM TacticOutcome := do
  let entry ← getProof stateId
  modify fun s => { s with tacticRuns := s.tacticRuns + 1 }
  if let some reason := Guard.violation tactic then
    return { ok := false, kind := "guard", error := reason }
  match parseTactics entry.snapshot.coreState.env tactic with
  | .error message => return { ok := false, kind := "parse", error := message }
  | .ok syntaxTree =>
    let run : Tactic.TacticM Unit := match goal with
      | some index => onGoal index (Tactic.evalTactic syntaxTree)
      | none => Tactic.evalTactic syntaxTree
    let result ← (entry.snapshot.step limits run).toBaseIO
    match result with
    | .error e => return { ok := false, kind := "tactic", error := toString e }
    | .ok (.error message) => return { ok := false, kind := "tactic", error := message }
    | .ok (.ok ((), after)) =>
      let diagnostics ← after.coreState.messages.toList.toArray.mapM fun m => (toDiagnostic m : IO Diagnostic)
      match diagnostics.find? (·.severity == "error") with
      | some d => return { ok := false, kind := "tactic", error := d.text, diagnostics }
      | none =>
        let goals ← after.goalViews
        let id ← putProof { snapshot := after, env := entry.env, parent := some stateId,
                            depth := entry.depth + 1 }
        return { ok := true, state := some id, goals, solved := goals.isEmpty, diagnostics }

/-- Runs each tactic on the same proof state, independently of the others, and returns the
outcomes in order. With `stopOnSolved`, tactics after the first one that closes every goal are
not run. -/
def tryTactics (stateId : Nat) (tactics : Array String) (goal : Option Nat) (stopOnSolved : Bool)
    (limits : Limits) : HandlerM (Array TacticOutcome) := do
  let mut out : Array TacticOutcome := #[]
  for tactic in tactics do
    let outcome ← runTactic stateId tactic goal limits
    out := out.push outcome
    if stopOnSolved && outcome.ok && outcome.solved then
      break
  return out

end Maithic.Bridge
