/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Checking certificates of linear integer arithmetic

A system of linear constraints over the integers has no solution when its rows, multiplied by
suitable numbers and added up, give `0 ≤ b` with `b` negative, or `0 < 0`. The multipliers are
the certificate; finding them is the work of a solver, and checking them is a few additions and
multiplications. This file has the checker, `LinearInt.refutes`, and the proof that it is right,
`LinearInt.not_allHold_of_refutes`.

A row is `a₀ * x₀ + a₁ * x₁ + … ≤ b`, or the same with `<` or `=`. A multiplier of a row with `≤`
or `<` must not be negative; a multiplier of an equation may have either sign.

The tactic that applies the theorem to a goal is `maithic_linear_int`, in
`Maithic.Oracle.Logic.LinearIntTactic`.
-/

namespace Maithic.Oracle.Logic.LinearInt

/-- How a row compares its left side with its bound. -/
inductive Rel where
  /-- At most. -/
  | le
  /-- Less than. -/
  | lt
  /-- Equal. -/
  | eq
  deriving DecidableEq, Repr, Inhabited

/-- A linear constraint: `coeffs[0] * x 0 + coeffs[1] * x 1 + …` compared with `bound`. -/
structure Row where
  /-- The coefficients of the variables, in order. Missing ones are zero. -/
  coeffs : List Int
  /-- The comparison. -/
  rel : Rel
  /-- The right side. -/
  bound : Int
  deriving DecidableEq, Repr, Inhabited

/-- The value of `as[0] * x i + as[1] * x (i + 1) + …`. -/
def dot : List Int → (Nat → Int) → Nat → Int
  | [], _, _ => 0
  | a :: as, x, i => a * x i + dot as x (i + 1)

/-- Whether a row holds at a point. -/
def Row.Holds (x : Nat → Int) (r : Row) : Prop :=
  match r.rel with
  | .le => dot r.coeffs x 0 ≤ r.bound
  | .lt => dot r.coeffs x 0 < r.bound
  | .eq => dot r.coeffs x 0 = r.bound

/-- A certificate: rows, each with its multiplier. -/
abbrev Certificate := List (Int × Row)

/-- All rows of a certificate hold at a point. -/
def AllHold (x : Nat → Int) : Certificate → Prop
  | [] => True
  | (_, r) :: rest => r.Holds x ∧ AllHold x rest

/-- The integers of a list by position. Positions past the end stand for zero. -/
def valuation : List Int → Nat → Int
  | [], _ => 0
  | v :: _, 0 => v
  | _ :: vs, i + 1 => valuation vs i

/-- The sum of two lists of coefficients, the shorter one continued with zeros. -/
def addCoeffs : List Int → List Int → List Int
  | [], bs => bs
  | a :: as, [] => a :: as
  | a :: as, b :: bs => (a + b) :: addCoeffs as bs

/-- The coefficients multiplied by a number. -/
def scaleCoeffs (m : Int) : List Int → List Int
  | [] => []
  | a :: as => m * a :: scaleCoeffs m as

theorem dot_addCoeffs : ∀ (as bs : List Int) (x : Nat → Int) (i : Nat),
    dot (addCoeffs as bs) x i = dot as x i + dot bs x i
  | [], bs, x, i => by simp [addCoeffs, dot]
  | a :: as, [], x, i => by simp [addCoeffs, dot]
  | a :: as, b :: bs, x, i => by
    simp only [addCoeffs, dot, dot_addCoeffs as bs x (i + 1), Int.add_mul]
    omega

theorem dot_scaleCoeffs (m : Int) : ∀ (as : List Int) (x : Nat → Int) (i : Nat),
    dot (scaleCoeffs m as) x i = m * dot as x i
  | [], x, i => by simp [scaleCoeffs, dot]
  | a :: as, x, i => by
    simp only [scaleCoeffs, dot, dot_scaleCoeffs m as x (i + 1), Int.mul_add, Int.mul_assoc]

theorem dot_eq_zero : ∀ (as : List Int) (x : Nat → Int) (i : Nat),
    as.all (· == 0) = true → dot as x i = 0
  | [], _, _, _ => rfl
  | a :: as, x, i, h => by
    simp only [List.all_cons, Bool.and_eq_true, beq_iff_eq] at h
    simp [dot, h.1, dot_eq_zero as x (i + 1) h.2]

/-- The weighted sum of the left sides. -/
def sumCoeffs : Certificate → List Int
  | [] => []
  | (m, r) :: rest => addCoeffs (scaleCoeffs m r.coeffs) (sumCoeffs rest)

/-- The weighted sum of the bounds. -/
def sumBounds : Certificate → Int
  | [] => 0
  | (m, r) :: rest => m * r.bound + sumBounds rest

/-- Whether every multiplier of an inequality is nonnegative. -/
def signsOk : Certificate → Bool
  | [] => true
  | (m, r) :: rest => (r.rel == .eq || decide (0 ≤ m)) && signsOk rest

/-- Whether some strict row has a positive multiplier. -/
def strictUsed : Certificate → Bool
  | [] => false
  | (m, r) :: rest => (r.rel == .lt && decide (0 < m)) || strictUsed rest

/-- Whether the certificate shows that its rows have no common solution: the signs are right, the
weighted left sides cancel, and the weighted bounds are negative, or zero with a strict row in
use. -/
def refutes (c : Certificate) : Bool :=
  signsOk c && (sumCoeffs c).all (· == 0) &&
    (decide (sumBounds c < 0) || (sumBounds c == 0 && strictUsed c))

/-- One weighted row: the left side times the multiplier is at most the bound times it. -/
theorem scaled_le {x : Nat → Int} {m : Int} {r : Row} (hsign : (r.rel == .eq || decide (0 ≤ m)) = true)
    (hr : r.Holds x) : m * dot r.coeffs x 0 ≤ m * r.bound := by
  unfold Row.Holds at hr
  cases hrel : r.rel <;> simp only [hrel] at hr <;>
    simp [hrel] at hsign
  · exact Int.mul_le_mul_of_nonneg_left hr hsign
  · exact Int.mul_le_mul_of_nonneg_left (Int.le_of_lt hr) hsign
  · rw [hr]; exact Int.le_refl _

theorem weighted_le {x : Nat → Int} : ∀ {c : Certificate}, signsOk c = true → AllHold x c →
    dot (sumCoeffs c) x 0 ≤ sumBounds c
  | [], _, _ => by simp [sumCoeffs, sumBounds, dot]
  | (m, r) :: rest, hsigns, hall => by
    simp only [signsOk, Bool.and_eq_true] at hsigns
    have head := scaled_le hsigns.1 hall.1
    have tail := weighted_le hsigns.2 hall.2
    simp only [sumCoeffs, sumBounds, dot_addCoeffs, dot_scaleCoeffs]
    omega

theorem weighted_lt {x : Nat → Int} : ∀ {c : Certificate}, signsOk c = true → strictUsed c = true →
    AllHold x c → dot (sumCoeffs c) x 0 < sumBounds c
  | [], _, hstrict, _ => by simp [strictUsed] at hstrict
  | (m, r) :: rest, hsigns, hstrict, hall => by
    simp only [signsOk, Bool.and_eq_true] at hsigns
    simp only [sumCoeffs, sumBounds, dot_addCoeffs, dot_scaleCoeffs]
    simp only [strictUsed, Bool.or_eq_true, Bool.and_eq_true, decide_eq_true_eq] at hstrict
    rcases hstrict with ⟨hrel, hm⟩ | hstrict
    · have hr := hall.1
      unfold Row.Holds at hr
      have hlt : r.rel = .lt := by simpa using hrel
      simp only [hlt] at hr
      have head := Int.mul_lt_mul_of_pos_left hr hm
      have tail := weighted_le hsigns.2 hall.2
      omega
    · have head := scaled_le hsigns.1 hall.1
      have tail := weighted_lt hsigns.2 hstrict hall.2
      omega

/-- Soundness: the rows of an accepted certificate have no common solution. -/
theorem not_allHold_of_refutes {c : Certificate} (h : refutes c = true) (x : Nat → Int) :
    ¬AllHold x c := by
  intro hall
  simp only [refutes, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, beq_iff_eq] at h
  obtain ⟨⟨hsigns, hzero⟩, hbound⟩ := h
  have hle := weighted_le hsigns hall
  rw [dot_eq_zero _ x 0 hzero] at hle
  rcases hbound with hneg | ⟨hz, hstrict⟩
  · omega
  · have hlt := weighted_lt hsigns hstrict hall
    rw [dot_eq_zero _ x 0 hzero] at hlt
    omega

/-- The form the tactic applies: an accepted certificate whose rows hold proves anything. -/
theorem false_of_refutes {c : Certificate} (x : Nat → Int) (h : refutes c = true)
    (hall : AllHold x c) : False :=
  not_allHold_of_refutes h x hall

end Maithic.Oracle.Logic.LinearInt
