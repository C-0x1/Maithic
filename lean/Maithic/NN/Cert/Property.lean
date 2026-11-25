/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.NN.Cert.Bounds

/-!
# Properties of networks, and the two ways of settling one

A property says that a network maps every point of an input box into a claimed box. A verifier
that is not trusted settles it one of two ways, and each way has a checker here with a proof that
the checker is right:

* with a **certificate**, a box for every layer. `checkProperty` accepts it when the layer-wise
  checker accepts the boxes and the last one lies inside the claimed box, and
  `checkProperty_sound` concludes that the property holds;
* with a **counterexample**, a point of the input box. `isCounterexample` accepts it when its exact
  output leaves the claimed box, and `not_holds_of_counterexample` concludes that the property
  fails.

No verdict of a verifier is believed. `maithic nnv verify` runs these checkers on what the
verifier returned, and reports what they say.
-/

namespace Maithic.NN

/-- The property: every point of the input box is mapped into the claimed box. -/
def Holds (net : Network) (input claimed : Box) : Prop :=
  ∀ xs, xs ∈ input → Network.eval net xs ∈ claimed

/-- Checks a certificate for a property: the boxes must be accepted layer by layer, and the last
of them must lie inside the claimed box. -/
def checkProperty (net : Network) (input claimed : Box) (cert : Certificate) : Bool :=
  checkCertificate net input cert && Box.subset (finalBox input cert) claimed

/-- **Soundness of the property checker.** An accepted certificate proves the property. -/
theorem checkProperty_sound {net : Network} {input claimed : Box} {cert : Certificate}
    (h : checkProperty net input claimed cert = true) : Holds net input claimed := by
  simp only [checkProperty, Bool.and_eq_true] at h
  exact fun xs hx => Box.subset_sound h.2 (checkCertificate_sound h.1 xs hx)

/-- The certificate that propagation itself gives: the box after every layer. -/
def propagated : Network → Box → Certificate
  | [], _ => []
  | l :: ls, b => l.bound b :: propagated ls (l.bound b)

theorem Box.subset_refl : ∀ b : Box, Box.subset b b = true
  | [] => rfl
  | i :: is => by
    simp only [Box.subset, Bool.and_eq_true]
    exact ⟨by simp [Interval.subset], Box.subset_refl is⟩

/-- The checker accepts what propagation computes: a verifier that returns the propagated boxes
is never turned away. -/
theorem checkCertificate_propagated (net : Network) (input : Box) :
    checkCertificate net input (propagated net input) = true := by
  induction net generalizing input with
  | nil => rfl
  | cons l ls ih => simp [propagated, checkCertificate, Box.subset_refl, ih]

/-- The last box of the propagated certificate is the bound of the network. -/
theorem finalBox_propagated (net : Network) (input : Box) :
    finalBox input (propagated net input) = Network.bound net input := by
  induction net generalizing input with
  | nil => rfl
  | cons l ls ih => simp [propagated, finalBox, Network.bound, ih]

/-- Whenever the plain bound check accepts, the property checker accepts the propagated
certificate: the layer-wise checker is at least as strong as the check it refines. -/
theorem checkProperty_of_checkBounds {net : Network} {input claimed : Box}
    (h : checkBounds net input claimed = true) : checkProperty net input claimed (propagated net input) = true := by
  simp only [checkProperty, Bool.and_eq_true]
  exact ⟨checkCertificate_propagated net input, by rw [finalBox_propagated]; exact h⟩

/-- **Soundness of the counterexample checker, for properties.** An accepted point shows that
the property fails. -/
theorem not_holds_of_counterexample {net : Network} {input claimed : Box} {xs : List Rat}
    (h : isCounterexample net input claimed xs = true) : ¬ Holds net input claimed := by
  intro hall
  have ⟨hin, hout⟩ := isCounterexample_sound h
  exact hout (hall xs hin)

/-- A property cannot have both an accepted certificate and an accepted counterexample. -/
theorem not_counterexample_of_checkProperty {net : Network} {input claimed : Box} {cert : Certificate}
    {xs : List Rat} (h : checkProperty net input claimed cert = true) :
    isCounterexample net input claimed xs = false := by
  cases hc : isCounterexample net input claimed xs with
  | false => rfl
  | true => exact absurd (checkProperty_sound h) (not_holds_of_counterexample hc)

/-- What a verifier may claim about a property. -/
inductive Claim where
  /-- The property holds, and these boxes show it. -/
  | verified (cert : Certificate)
  /-- The property fails at this point. -/
  | violated (point : List Rat)

/-- Runs the checker that fits the claim. -/
def Claim.accepted (net : Network) (input claimed : Box) : Claim → Bool
  | .verified cert => checkProperty net input claimed cert
  | .violated point => isCounterexample net input claimed point

/-- What an accepted claim establishes. -/
theorem Claim.accepted_sound {net : Network} {input claimed : Box} {c : Claim}
    (h : c.accepted net input claimed = true) :
    match c with
    | .verified _ => Holds net input claimed
    | .violated _ => ¬ Holds net input claimed := by
  cases c with
  | verified cert => exact checkProperty_sound h
  | violated point => exact not_holds_of_counterexample h

end Maithic.NN
