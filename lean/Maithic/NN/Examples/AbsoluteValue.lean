/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.NN.Cert.Property

/-!
# A property that is true, and that interval bounds cannot prove

The network of `examples/nnv/absolute-value.json` computes the absolute value as
`relu x + relu (-x)`. On inputs from `-2` to `3` it never outputs more than `3`. Interval
propagation bounds the two neurons one at a time, by `3` and by `2`, and so bounds their sum by
`5`: it cannot see that the two are never large together.

This file states the three recorded verdicts of `maithic nnv verify` on that network as theorems,
and adds the fourth fact, which no certificate of this kind can carry:

* `within_six`: the outputs stay within `[0, 6]`, by the certificate checker;
* `not_within_two`: they do not stay within `[0, 2]`, by a counterexample;
* `checker_rejects_four`: the checker does not accept `[0, 4]`, so the verifier answers *unknown*;
* `within_three`: and yet the outputs stay within `[0, 3]`, proved here by cases on the sign of
  the input.

Sound and incomplete: what the checker accepts is true, and what is true is not always accepted.
-/

namespace Maithic.NN.Examples

/-- The network `relu x + relu (-x)`: two neurons with a ReLU, and their sum. -/
def absNet : Network := [
  { neurons := [⟨[1], 0⟩, ⟨[-1], 0⟩], activation := .relu },
  { neurons := [⟨[1, 1], 0⟩], activation := .identity }]

/-- The inputs from `-2` to `3`. -/
def absInput : Box := [⟨-2, 3⟩]

/-- What the network computes on one number, exactly. -/
theorem absNet_eval (x : Rat) : Network.eval absNet [x] = [relu x + relu (-x)] := by
  simp [absNet, Network.eval, Layer.eval, Neuron.eval, dot, Activation.apply, Rat.add_zero,
    Rat.neg_mul, Rat.one_mul]

/-- The sum of the two neurons is the absolute value. -/
theorem relu_add_relu_neg (x : Rat) : relu x + relu (-x) = if 0 ≤ x then x else -x := by
  unfold relu
  grind

/-- Interval propagation bounds the outputs by `5`, and by nothing smaller. -/
theorem propagated_bound :
    Box.subset (finalBox absInput (propagated absNet absInput)) [⟨0, 5⟩] = true
      ∧ Box.subset [⟨0, 5⟩] (finalBox absInput (propagated absNet absInput)) = true := by
  decide +kernel

/-- The outputs stay within `[0, 6]`: the checker accepts the certificate that propagation gives. -/
theorem within_six : Holds absNet absInput [⟨0, 6⟩] :=
  checkProperty_sound (cert := propagated absNet absInput) (by decide +kernel)

/-- The outputs do not stay within `[0, 2]`: the input `3` is mapped to `3`. -/
theorem not_within_two : ¬ Holds absNet absInput [⟨0, 2⟩] :=
  not_holds_of_counterexample (xs := [3]) (by decide +kernel)

/-- The checker does not accept `[0, 4]`: the proved bound is `5`, and `5` is more than `4`. -/
theorem checker_rejects_four :
    checkProperty absNet absInput [⟨0, 4⟩] (propagated absNet absInput) = false := by
  decide +kernel

/-- And yet the outputs stay within `[0, 3]`, and so within `[0, 4]`: a property that is true and
out of the reach of interval bounds. -/
theorem within_three : Holds absNet absInput [⟨0, 3⟩] := by
  intro xs hx
  cases hx with
  | cons hi hrest =>
    cases hrest
    rw [absNet_eval]
    refine Box.mem_cons ?_ Box.mem_nil
    rw [relu_add_relu_neg]
    rw [Interval.mem_def] at hi ⊢
    grind

/-- What is true of a smaller box is true of a larger one. -/
theorem within_four : Holds absNet absInput [⟨0, 4⟩] := by
  intro xs hx
  exact Box.subset_sound (b := [⟨0, 3⟩]) (by decide +kernel) (within_three xs hx)

end Maithic.NN.Examples
