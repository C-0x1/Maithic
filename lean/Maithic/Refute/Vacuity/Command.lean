/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Vacuity.Analysis

/-!
# `#maithic_vacuous`, `#maithic_necessity` and `#maithic_unused`

Three questions to put to a statement before, or after, proving it.

```
#maithic_vacuous ∀ x : Nat, x < 3 → x > 5 → x = 4
-- warning: vacuous: the hypotheses are contradictory (found by omega, from x < 3 and x > 5)

#maithic_necessity ∀ (a b : Nat) (h : a ≤ b), b - a + a = b
-- h : a ≤ b is needed: without it the statement fails at
--   a := 1
--   b := 0

#maithic_unused my_theorem
-- warning: my_theorem: the proof does not mention h₂
```

Each command takes a statement, or the name of a declared theorem, whose statement is then
examined. `#maithic_vacuous json` prints the findings as JSON.
-/

namespace Maithic.Refute

open Lean Meta Elab Command

/--
`#maithic_vacuous p` looks for the ways in which the statement `p` can be true for the wrong
reasons: hypotheses that contradict each other, a variable that ranges over an empty type, and a
conclusion that holds whatever the hypotheses say. `p` is a proposition or the name of a theorem.
A finding is a warning and is certain, because Lean checked the proof behind it. Finding nothing
is not a proof of the opposite. `#maithic_vacuous json p` prints the findings as JSON.
-/
syntax (name := maithicVacuous) "#maithic_vacuous " (&"json ")? term : command

@[command_elab maithicVacuous] def elabMaithicVacuous : CommandElab := fun stx =>
  runTermElabM fun variables => do
    let result ← analyzeVacuity (← elabStatement stx[2] variables)
    if !stx[1].isNone then logInfo result.toJson.pretty
    else if result.vacuous || result.trivialBy.isSome then logWarning result.toString
    else logInfo result.toString

/--
`#maithic_necessity p` asks of every hypothesis of the statement `p` whether it is needed. The
statement without the hypothesis is first tried by automation: a proof shows that the hypothesis
can be dropped. It is then tested by sampling: a counterexample shows that it cannot. `p` is a
proposition or the name of a theorem. The options of `#maithic_test` may be given before it.
-/
syntax (name := maithicNecessity) "#maithic_necessity " (testOption)* term : command

@[command_elab maithicNecessity] def elabMaithicNecessity : CommandElab := fun stx => do
  let cfg ← liftCoreM (readConfig stx[1].getArgs)
  runTermElabM fun variables => do
    let findings ← analyzeNecessity (← elabStatement stx[2] variables) cfg
    if findings.isEmpty then logInfo "the statement has no hypotheses"
    else logInfo (String.intercalate "\n" (findings.toList.map fun (variant, n) => n.describe variant))

/-- The hypotheses of a statement given to `#maithic_vacuous` or `#maithic_necessity` are named
so that the findings can name them. Nothing in the statement has to refer to them. -/
@[unused_variables_ignore_fn]
def ignoreAnalysedStatement : Lean.Linter.IgnoreFunction := fun _ stack _ =>
  stack.any fun (stx, _) => stx.isOfKind ``maithicVacuous || stx.isOfKind ``maithicNecessity

/--
`#maithic_unused t` lists the hypotheses of the theorem `t` that its proof term does not mention.
Such a hypothesis plays no part in the proof as it stands. The converse does not hold: automation
that gathers every hypothesis mentions them all, and `#maithic_necessity` is the command that
asks whether the statement needs them.
-/
syntax (name := maithicUnused) "#maithic_unused " ident : command

@[command_elab maithicUnused] def elabMaithicUnused : CommandElab := fun stx =>
  runTermElabM fun _ => do
    let name ← realizeGlobalConstNoOverloadWithInfo stx[1]
    let unused ← unusedHypotheses name
    if unused.isEmpty then logInfo m!"{name}: the proof mentions every hypothesis"
    else logWarning m!"{name}: the proof does not mention {nameList unused}"

end Maithic.Refute
