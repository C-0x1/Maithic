/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.NN.Cert.Interval

/-!
# Interval bound propagation and its certificate checker

A *box* is a list of intervals, one per coordinate. Propagating a box through a network gives a
box that contains the network's output for every input in the original box. This file defines
the propagation, proves that statement, and builds the certificate checker on top of it:

`checkBounds net input claimed` recomputes the output box and accepts when it lies inside the
claimed one. The soundness theorem `checkBounds_sound` says that acceptance implies the claimed
bounds hold for every input in the box, for the exact semantics of the network.

The bounds may come from anywhere, in particular from fast floating-point code that is not
trusted. Only this checker and Lean's kernel need to be.
-/

namespace Maithic.NN

/-- A box: one interval per coordinate. -/
abbrev Box := List Interval

/-- `xs` lies in the box: same length, each coordinate in its interval. -/
inductive Box.Contains : Box → List Rat → Prop where
  | nil : Box.Contains [] []
  | cons {i : Interval} {b : Box} {x : Rat} {xs : List Rat} :
      x ∈ i → Box.Contains b xs → Box.Contains (i :: b) (x :: xs)

instance : Membership (List Rat) Box := ⟨Box.Contains⟩

theorem Box.mem_nil : ([] : List Rat) ∈ ([] : Box) := Box.Contains.nil

theorem Box.mem_cons {i : Interval} {b : Box} {x : Rat} {xs : List Rat}
    (hx : x ∈ i) (hxs : xs ∈ b) : (x :: xs) ∈ (i :: b : Box) := Box.Contains.cons hx hxs

/-- A vector in a box has as many coordinates as the box. -/
theorem Box.length_eq {b : Box} {xs : List Rat} (h : xs ∈ b) : xs.length = b.length := by
  induction h with
  | nil => rfl
  | cons _ _ ih => simp [ih]

/-- The interval dot product of a weight row with a box. -/
def Interval.dot : List Rat → Box → Interval
  | w :: ws, i :: is => Interval.add (Interval.scale w i) (Interval.dot ws is)
  | _, _ => Interval.point 0

theorem Interval.dot_sound (ws : List Rat) {b : Box} {xs : List Rat} (h : xs ∈ b) :
    NN.dot ws xs ∈ Interval.dot ws b := by
  induction h generalizing ws with
  | nil =>
    cases ws <;> exact Interval.mem_point 0
  | cons hx _ ih =>
    cases ws with
    | nil => exact Interval.mem_point 0
    | cons w ws => exact Interval.add_sound (Interval.scale_sound w hx) (ih ws)

/-- The interval of a neuron's affine value over a box. -/
def Neuron.bound (n : Neuron) (b : Box) : Interval := Interval.shift n.bias (Interval.dot n.weights b)

theorem Neuron.bound_sound (n : Neuron) {b : Box} {xs : List Rat} (h : xs ∈ b) :
    n.eval xs ∈ n.bound b :=
  Interval.shift_sound n.bias (Interval.dot_sound n.weights h)

/-- The output box of a layer over an input box. -/
def Layer.bound (l : Layer) (b : Box) : Box :=
  l.neurons.map fun n => Interval.activate l.activation (n.bound b)

theorem Layer.bound_sound (l : Layer) {b : Box} {xs : List Rat} (h : xs ∈ b) :
    l.eval xs ∈ l.bound b := by
  unfold Layer.eval Layer.bound
  induction l.neurons with
  | nil => exact Box.mem_nil
  | cons n ns ih => exact Box.mem_cons (Interval.activate_sound l.activation (n.bound_sound h)) ih

/-- The output box of a network over an input box. -/
def Network.bound : Network → Box → Box
  | [], b => b
  | l :: ls, b => Network.bound ls (l.bound b)

/-- Interval bound propagation is sound: outputs of inputs in the box lie in the output box. -/
theorem Network.bound_sound (net : Network) {b : Box} {xs : List Rat} (h : xs ∈ b) :
    Network.eval net xs ∈ Network.bound net b := by
  induction net generalizing b xs with
  | nil => exact h
  | cons l ls ih => exact ih (l.bound_sound h)

/-- Coordinate-wise inclusion of boxes of the same length, as a Boolean test. -/
def Box.subset : Box → Box → Bool
  | [], [] => true
  | i :: is, j :: js => Interval.subset i j && Box.subset is js
  | _, _ => false

theorem Box.subset_sound {b c : Box} {xs : List Rat} (h : Box.subset b c = true) (hx : xs ∈ b) :
    xs ∈ c := by
  induction hx generalizing c with
  | nil =>
    cases c with
    | nil => exact Box.mem_nil
    | cons _ _ => simp [Box.subset] at h
  | cons hx _ ih =>
    cases c with
    | nil => simp [Box.subset] at h
    | cons j js =>
      simp only [Box.subset, Bool.and_eq_true] at h
      exact Box.mem_cons (Interval.subset_sound h.1 hx) (ih h.2)

/-- The certificate checker: recompute the output box and compare it with the claimed one. -/
def checkBounds (net : Network) (input claimed : Box) : Bool :=
  Box.subset (Network.bound net input) claimed

/-- **Soundness of the checker.** If `checkBounds` accepts, every input in the input box is
mapped by the network into the claimed output box. -/
theorem checkBounds_sound {net : Network} {input claimed : Box}
    (h : checkBounds net input claimed = true) :
    ∀ xs, xs ∈ input → Network.eval net xs ∈ claimed :=
  fun _ hx => Box.subset_sound h (Network.bound_sound net hx)

/-- A bound certificate: one claimed box per layer, in order. The boxes may be looser than what
propagation computes, which lets a producer round outwards. -/
abbrev Certificate := List Box

/-- The box a certificate ends with: the last claimed box, or the input box for no layers. -/
def finalBox : Box → Certificate → Box
  | b, [] => b
  | _, c :: cs => finalBox c cs

/-- Checks a certificate layer by layer: the bounds recomputed from the previous box must lie
inside the claimed box, and checking continues from the claimed box. -/
def checkCertificate : Network → Box → Certificate → Bool
  | [], _, [] => true
  | l :: ls, b, c :: cs => Box.subset (l.bound b) c && checkCertificate ls c cs
  | _, _, _ => false

/-- **Soundness of the layer-wise checker.** If `checkCertificate` accepts, every input in the
input box is mapped by the network into the last box of the certificate. -/
theorem checkCertificate_sound {net : Network} {input : Box} {cert : Certificate}
    (h : checkCertificate net input cert = true) :
    ∀ xs, xs ∈ input → Network.eval net xs ∈ finalBox input cert := by
  induction net generalizing input cert with
  | nil =>
    cases cert with
    | nil => exact fun _ hx => hx
    | cons _ _ => simp [checkCertificate] at h
  | cons l ls ih =>
    cases cert with
    | nil => simp [checkCertificate] at h
    | cons c cs =>
      simp only [checkCertificate, Bool.and_eq_true] at h
      exact fun xs hx => ih h.2 _ (Box.subset_sound h.1 (l.bound_sound hx))

/-- Membership of a vector in a box, as a Boolean test. -/
def Box.containsB : Box → List Rat → Bool
  | [], [] => true
  | i :: is, x :: xs => decide (x ∈ i) && Box.containsB is xs
  | _, _ => false

theorem Box.containsB_iff {b : Box} {xs : List Rat} : Box.containsB b xs = true ↔ xs ∈ b := by
  constructor
  · intro h
    induction b generalizing xs with
    | nil =>
      cases xs with
      | nil => exact Box.mem_nil
      | cons _ _ => simp [Box.containsB] at h
    | cons i is ih =>
      cases xs with
      | nil => simp [Box.containsB] at h
      | cons x xs =>
        simp only [Box.containsB, Bool.and_eq_true, decide_eq_true_eq] at h
        exact Box.mem_cons h.1 (ih h.2)
  · intro h
    induction h with
    | nil => rfl
    | cons hx _ ih => simp [Box.containsB, hx, ih]

instance (b : Box) (xs : List Rat) : Decidable (xs ∈ b) :=
  decidable_of_iff _ Box.containsB_iff

/-- A counterexample to claimed bounds: a point of the input box whose exact output leaves the
claimed box. Checking one is a matter of evaluating the network. -/
def isCounterexample (net : Network) (input claimed : Box) (xs : List Rat) : Bool :=
  Box.containsB input xs && !Box.containsB claimed (Network.eval net xs)

/-- **Soundness of the counterexample checker.** An accepted witness lies in the input box and
its exact output lies outside the claimed box, so the claimed bounds are false. -/
theorem isCounterexample_sound {net : Network} {input claimed : Box} {xs : List Rat}
    (h : isCounterexample net input claimed xs = true) :
    xs ∈ input ∧ Network.eval net xs ∉ claimed := by
  simp only [isCounterexample, Bool.and_eq_true, Bool.not_eq_true'] at h
  refine ⟨Box.containsB_iff.mp h.1, fun hmem => ?_⟩
  have := Box.containsB_iff.mpr hmem
  simp [this] at h

/-- Claimed bounds cannot be both certified and refuted. -/
theorem not_counterexample_of_checkBounds {net : Network} {input claimed : Box} {xs : List Rat}
    (h : checkBounds net input claimed = true) : isCounterexample net input claimed xs = false := by
  cases hc : isCounterexample net input claimed xs with
  | false => rfl
  | true =>
    have := isCounterexample_sound hc
    exact absurd (checkBounds_sound h xs this.1) this.2

end Maithic.NN
