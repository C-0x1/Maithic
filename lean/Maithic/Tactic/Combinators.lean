/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Tactic combinators

Small building blocks used by `maithic_auto` and available on their own:

* `maithic_progress tac` fails when `tac` leaves the goals unchanged;
* `maithic_close tac` succeeds only when `tac` closes the main goal, and restores the state
  otherwise;
* `maithic_first [tac₁, tac₂, …]` commits to the first tactic that makes progress;
* `maithic_unfold_head` unfolds the definition at the head of the goal, which turns a named
  proposition such as a benchmark problem into its statement.
-/

namespace Maithic.Tactic

open Lean Elab Tactic Meta

/-- The goals together with their instantiated types: what "no progress" compares. -/
private def goalFingerprint : TacticM (List MVarId × List Expr) := do
  let goals ← getGoals
  let types ← goals.mapM fun g => do instantiateMVars (← g.getType)
  return (goals, types)

/--
`maithic_progress tac` runs `tac` and fails if the list of goals and their types is unchanged
afterwards. Use it to reject steps such as a `simp` call that does nothing.
-/
elab "maithic_progress " t:tacticSeq : tactic => do
  let before ← goalFingerprint
  evalTactic t
  let after ← goalFingerprint
  if before == after then
    throwError "maithic_progress: the tactic made no progress"

/--
`maithic_close tac` runs `tac` on the main goal and succeeds only if that goal is closed. On
failure the proof state is exactly what it was before.
-/
elab "maithic_close " t:tacticSeq : tactic => do
  let saved ← saveState
  try
    focusAndDone (evalTactic t)
  catch e =>
    saved.restore
    throwError "maithic_close: the tactic did not close the goal\n{e.toMessageData}"

/--
`maithic_first [tac₁, tac₂, …]` tries the tactics in order and commits to the first one that
succeeds and makes progress. It fails if none does.
-/
syntax (name := maithicFirst) "maithic_first " "[" tactic,+ "]" : tactic

@[tactic maithicFirst] def evalMaithicFirst : Tactic := fun stx => do
  match stx with
  | `(tactic| maithic_first [$ts,*]) =>
    for t in ts.getElems do
      let saved ← saveState
      let progressed ← tryCatchRuntimeEx
        (do
          let before ← goalFingerprint
          evalTactic t
          return before != (← goalFingerprint))
        (fun _ => pure false)
      if progressed then
        return
      saved.restore
    throwError "maithic_first: none of the {ts.getElems.size} tactics made progress"
  | _ => throwUnsupportedSyntax

/--
`maithic_unfold_head` replaces the goal by the body of the definition at its head. It fails when
the goal does not start with an unfoldable definition.
-/
elab "maithic_unfold_head" : tactic => do
  let goal ← getMainGoal
  let target ← instantiateMVars (← goal.getType)
  match ← delta? target with
  | some unfolded => replaceMainGoal [← goal.replaceTargetDefEq unfolded]
  | none => throwError "maithic_unfold_head: the goal does not start with a definition"

end Maithic.Tactic
