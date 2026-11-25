/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.NN.Layers.Dense

/-!
# Rational intervals

Closed intervals with rational endpoints and the arithmetic that bound propagation needs. Every
operation comes with its soundness theorem: if the inputs lie in the input intervals, the exact
result lies in the output interval.
-/

namespace Maithic.NN

/-- A closed interval `[lo, hi]` with rational endpoints. An interval with `hi < lo` is empty. -/
structure Interval where
  lo : Rat
  hi : Rat
  deriving Repr, BEq, DecidableEq, Inhabited

namespace Interval

/-- `x` lies in the interval. -/
def Contains (i : Interval) (x : Rat) : Prop := i.lo ≤ x ∧ x ≤ i.hi

instance : Membership Rat Interval := ⟨Contains⟩

theorem mem_def {i : Interval} {x : Rat} : x ∈ i ↔ i.lo ≤ x ∧ x ≤ i.hi := Iff.rfl

instance (i : Interval) (x : Rat) : Decidable (x ∈ i) :=
  inferInstanceAs (Decidable (i.lo ≤ x ∧ x ≤ i.hi))

/-- The interval holding one point. -/
def point (x : Rat) : Interval := ⟨x, x⟩

/-- Interval addition. -/
def add (i j : Interval) : Interval := ⟨i.lo + j.lo, i.hi + j.hi⟩

/-- Interval negation. -/
def neg (i : Interval) : Interval := ⟨-i.hi, -i.lo⟩

/-- Interval subtraction. -/
def sub (i j : Interval) : Interval := ⟨i.lo - j.hi, i.hi - j.lo⟩

/-- Multiplication by a constant. -/
def scale (c : Rat) (i : Interval) : Interval :=
  if 0 ≤ c then ⟨c * i.lo, c * i.hi⟩ else ⟨c * i.hi, c * i.lo⟩

/-- Adding a constant. -/
def shift (c : Rat) (i : Interval) : Interval := ⟨i.lo + c, i.hi + c⟩

/-- The image of an interval under the rectified linear unit. -/
def relu (i : Interval) : Interval := ⟨NN.relu i.lo, NN.relu i.hi⟩

/-- The smallest interval containing both. -/
def hull (i j : Interval) : Interval := ⟨min i.lo j.lo, max i.hi j.hi⟩

/-- `i` is included in `j`, as a Boolean test. -/
def subset (i j : Interval) : Bool := decide (j.lo ≤ i.lo) && decide (i.hi ≤ j.hi)

/-- The width of an interval. -/
def width (i : Interval) : Rat := i.hi - i.lo

theorem mem_point (x : Rat) : x ∈ point x := by
  simp only [mem_def, point]; grind

theorem add_sound {i j : Interval} {x y : Rat} (hx : x ∈ i) (hy : y ∈ j) : x + y ∈ add i j := by
  simp only [mem_def, add] at *; grind

theorem neg_sound {i : Interval} {x : Rat} (hx : x ∈ i) : -x ∈ neg i := by
  simp only [mem_def, neg] at *; grind

theorem sub_sound {i j : Interval} {x y : Rat} (hx : x ∈ i) (hy : y ∈ j) : x - y ∈ sub i j := by
  simp only [mem_def, sub] at *; grind

theorem shift_sound {i : Interval} {x : Rat} (c : Rat) (hx : x ∈ i) : x + c ∈ shift c i := by
  simp only [mem_def, shift] at *; grind

theorem scale_sound {i : Interval} {x : Rat} (c : Rat) (hx : x ∈ i) : c * x ∈ scale c i := by
  simp only [mem_def] at hx
  unfold scale
  split
  · rename_i hc
    exact ⟨Rat.mul_le_mul_of_nonneg_left hx.1 hc, Rat.mul_le_mul_of_nonneg_left hx.2 hc⟩
  · rename_i hc
    have hneg : 0 ≤ -c := by grind
    have h1 := Rat.mul_le_mul_of_nonneg_left hx.1 hneg
    have h2 := Rat.mul_le_mul_of_nonneg_left hx.2 hneg
    simp only [mem_def]
    grind

theorem relu_sound {i : Interval} {x : Rat} (hx : x ∈ i) : NN.relu x ∈ relu i := by
  simp only [mem_def, relu] at *
  exact ⟨relu_mono hx.1, relu_mono hx.2⟩

theorem hull_sound_left {i j : Interval} {x : Rat} (hx : x ∈ i) : x ∈ hull i j := by
  simp only [mem_def, hull] at *; grind

theorem hull_sound_right {i j : Interval} {x : Rat} (hx : x ∈ j) : x ∈ hull i j := by
  simp only [mem_def, hull] at *; grind

theorem subset_sound {i j : Interval} {x : Rat} (h : subset i j = true) (hx : x ∈ i) : x ∈ j := by
  simp only [subset, Bool.and_eq_true, decide_eq_true_eq] at h
  simp only [mem_def] at *; grind

/-- The image of an interval under an activation. -/
def activate : Activation → Interval → Interval
  | .identity, i => i
  | .relu, i => relu i

theorem activate_sound (a : Activation) {i : Interval} {x : Rat} (hx : x ∈ i) :
    a.apply x ∈ activate a i := by
  cases a with
  | identity => exact hx
  | relu => exact relu_sound hx

end Interval

end Maithic.NN
