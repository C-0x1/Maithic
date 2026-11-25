/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Oracle.Logic.LinearInt

/-!
# `maithic_linear_int`

Closes a goal from a certificate of linear integer arithmetic. The work is split in two, and each
half is checked on its own:

* the kernel runs the verified checker `LinearInt.refutes` on the rows and multipliers, which
  shows that the rows have no common solution;
* each row is derived from the one hypothesis named for it, with every other hypothesis out of
  sight, by `omega`. A row need not be its hypothesis word for word: `x + 2 * y ≤ 3` follows from
  `2 * x + 4 * y < 7` over the integers, and that step is what `omega` confirms.

Because a row sees a single hypothesis, the contradiction itself can only come from the
certificate.
-/

namespace Maithic.Oracle.Logic.LinearInt

open Lean Meta Elab Tactic

/--
`maithic_linear_int [x, y] [(m₁, ⟨[a, b], .le, c⟩), …] using [h₁, …]` closes any goal from
contradictory integer hypotheses. The second list is the certificate: rows over the variables of
the first list, each with its multiplier. The third names, for every row, the hypothesis the row
follows from.

To prove a goal that is not `False`, negate it first:
`apply Classical.byContradiction; intro h`, and name `h` for the row of the negated goal.
-/
syntax (name := maithicLinearInt)
  "maithic_linear_int " term:max ppSpace term:max " using " "[" term,* "]" : tactic

/-- Proves the main goal, a row, from the given proof alone: every other propositional
hypothesis is cleared before `omega` runs. -/
private def proveRowFrom (proof : Term) (index : Nat) : TacticM Unit := withMainContext do
  let h ← elabTerm proof none
  let goal ← (← getMainGoal).assert `source (← inferType h) h
  let (source, goal) ← goal.intro1P
  let others ← goal.withContext do
    let mut others : Array FVarId := #[]
    for decl in ← getLCtx do
      if decl.isImplementationDetail || decl.fvarId == source then continue
      if ← isProp decl.type then others := others.push decl.fvarId
    return others
  setGoals [← goal.tryClearMany others]
  try
    evalTactic (← `(tactic| omega))
  catch _ =>
    throwError "maithic_linear_int: row {index} does not follow from{indentD proof}\nby integer reasoning"

@[tactic maithicLinearInt] def evalMaithicLinearInt : Tactic := fun stx => do
  match stx with
  | `(tactic| maithic_linear_int $vars $cert using [$proofs,*]) => withMainContext do
    let certificate ← elabTermEnsuringType cert (mkConst ``Certificate)
    Term.synthesizeSyntheticMVarsNoPostponing
    let accepted ← whnf (mkApp (mkConst ``refutes) (← instantiateMVars certificate))
    unless accepted.isConstOf ``Bool.true do
      throwError "maithic_linear_int: the checker does not accept the certificate: \
        with these multipliers the rows do not add up to a contradiction"
    evalTactic (← `(tactic|
      refine False.elim (false_of_refutes (c := $cert) (valuation $vars) (by decide) ?_)))
    evalTactic (← `(tactic| dsimp only [AllHold, Row.Holds, dot, valuation]))
    evalTactic (← `(tactic| and_intros))
    let goals ← getGoals
    let proofs := proofs.getElems
    unless goals.length == proofs.size + 1 do
      throwError "maithic_linear_int: the certificate has {goals.length - 1} row(s) and \
        {proofs.size} hypothesis name(s)"
    let mut index := 0
    for (goal, proof) in goals.zip proofs.toList do
      setGoals [goal]
      proveRowFrom proof index
      index := index + 1
    setGoals [goals.getLast!]
    evalTactic (← `(tactic| trivial))
  | _ => throwUnsupportedSyntax

end Maithic.Oracle.Logic.LinearInt
