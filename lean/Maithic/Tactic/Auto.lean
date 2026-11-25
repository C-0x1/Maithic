/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Tactic.Combinators

/-!
# `maithic_auto`

A portfolio tactic: it tries the automation that ships with Lean in a fixed order and closes the
goal with the first step that works. Every step runs under its own heartbeat budget, so one slow
step cannot starve the others, and a step that does not close the goal leaves no trace.

The order is cheapest first: `rfl`, `trivial`, `simp`, `omega`, `decide`, `simp_all`, `grind`.
The same order is used by the portfolio proposer of the prover, so that what the prover finds
without a model is exactly what `maithic_auto` finds inside the editor.
-/

namespace Maithic.Tactic

open Lean Elab Tactic Meta

/-- The default heartbeat budget of one step of `maithic_auto`, in thousands. -/
def autoStepHeartbeats : Nat := 40000

/-- The names of the steps of `maithic_auto`, in the order they are tried. -/
def autoSteps : List String := ["rfl", "trivial", "simp", "omega", "decide", "simp_all", "grind"]

/-- Runs a tactic under its own heartbeat budget, given in thousands of heartbeats and counted
from the moment the tactic starts. A budget of zero means no limit, as for `maxHeartbeats`. -/
def withStepBudget (thousands : Nat) (x : TacticM α) : TacticM α := do
  let start ← IO.getNumHeartbeats
  withTheReader Core.Context
    (fun ctx => { ctx with maxHeartbeats := thousands * 1000, initHeartbeats := start }) x

/--
`maithic_auto` closes the main goal with core automation, or fails without changing the state.

* `maithic_auto` uses the default budget for each step.
* `maithic_auto (heartbeats := 5000)` gives each step 5,000 thousand heartbeats; zero removes
  the limit.

Before the portfolio runs, the tactic introduces hypotheses. If no step closes the goal and the
goal starts with a definition, the definition is unfolded once and the portfolio runs again.
-/
syntax (name := maithicAuto) "maithic_auto" (" (" &"heartbeats" " := " num ")")? : tactic

/-- The tactic syntax of each step. -/
private def stepSyntax : TacticM (List (String × Syntax)) := do
  return [
    ("rfl", ← `(tactic| rfl)),
    ("trivial", ← `(tactic| trivial)),
    ("simp", ← `(tactic| simp)),
    ("omega", ← `(tactic| omega)),
    ("decide", ← `(tactic| decide)),
    ("simp_all", ← `(tactic| simp_all)),
    ("grind", ← `(tactic| grind))]

/-- Tries every step on the main goal. Returns the name of the step that closed it. A step that
fails, leaves goals open or runs out of budget is undone before the next one starts. -/
private def runPortfolio (budget : Nat) : TacticM (Option String) := do
  for (name, stx) in ← stepSyntax do
    let saved ← saveState
    let closed ← tryCatchRuntimeEx
      (do withStepBudget budget <| focusAndDone (evalTactic stx); pure true)
      (fun _ => do saved.restore; pure false)
    if closed then
      return some name
  return none

@[tactic maithicAuto] def evalMaithicAuto : Tactic := fun stx => do
  let budget := match stx with
    | `(tactic| maithic_auto (heartbeats := $n)) => n.getNat
    | _ => autoStepHeartbeats
  let saved ← saveState
  let attempt : TacticM Bool := focus do
    evalTactic (← `(tactic| intros))
    if (← getGoals).isEmpty then return true
    if (← runPortfolio budget).isSome then return true
    let goal ← getMainGoal
    let target ← instantiateMVars (← goal.getType)
    match ← delta? target with
    | none => return false
    | some unfolded =>
      replaceMainGoal [← goal.replaceTargetDefEq unfolded]
      evalTactic (← `(tactic| intros))
      if (← getGoals).isEmpty then return true
      return (← runPortfolio budget).isSome
  let closed ← tryCatchRuntimeEx attempt (fun _ => pure false)
  unless closed do
    saved.restore
    throwError "maithic_auto: no automation step closed the goal (tried {autoSteps})"

end Maithic.Tactic
