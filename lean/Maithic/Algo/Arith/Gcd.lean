/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Euclid's algorithm

The oldest algorithm that is still in daily use: the greatest common divisor of two numbers by
repeated remainders. `euclid` is the program, and `euclid_eq_gcd` says that it computes what
Lean's library calls the greatest common divisor. Everything else follows from that one theorem:
the result divides both numbers, and everything that divides both divides the result.
-/

namespace Maithic.Algo

/-- Euclid's algorithm: the greatest common divisor by repeated remainders. -/
def euclid (a b : Nat) : Nat :=
  if b = 0 then a else euclid b (a % b)
termination_by b
decreasing_by exact Nat.mod_lt _ (by omega)

/-- **Correctness of Euclid's algorithm.** -/
theorem euclid_eq_gcd (a b : Nat) : euclid a b = Nat.gcd a b := by
  induction b using Nat.strongRecOn generalizing a with
  | _ b ih =>
    unfold euclid
    split
    · next h => subst h; simp
    · next h =>
      rw [ih (a % b) (Nat.mod_lt _ (Nat.pos_of_ne_zero h)) b, Nat.gcd_comm a b, Nat.gcd_rec b a, Nat.gcd_comm]

theorem euclid_zero_right (a : Nat) : euclid a 0 = a := by simp [euclid_eq_gcd]
theorem euclid_zero_left (b : Nat) : euclid 0 b = b := by simp [euclid_eq_gcd]

/-- The order of the arguments does not matter. -/
theorem euclid_comm (a b : Nat) : euclid a b = euclid b a := by
  rw [euclid_eq_gcd, euclid_eq_gcd, Nat.gcd_comm]

/-- The result divides the first number. -/
theorem euclid_dvd_left (a b : Nat) : euclid a b ∣ a := by
  rw [euclid_eq_gcd]; exact Nat.gcd_dvd_left a b

/-- The result divides the second number. -/
theorem euclid_dvd_right (a b : Nat) : euclid a b ∣ b := by
  rw [euclid_eq_gcd]; exact Nat.gcd_dvd_right a b

/-- Whatever divides both numbers divides the result: it is the greatest of the common divisors. -/
theorem dvd_euclid {d a b : Nat} (ha : d ∣ a) (hb : d ∣ b) : d ∣ euclid a b := by
  rw [euclid_eq_gcd]; exact Nat.dvd_gcd ha hb

/-- The result is positive unless both numbers are zero. -/
theorem euclid_pos {a b : Nat} (h : 0 < a ∨ 0 < b) : 0 < euclid a b := by
  rw [euclid_eq_gcd]
  cases h with
  | inl ha => exact Nat.gcd_pos_of_pos_left b ha
  | inr hb => exact Nat.gcd_pos_of_pos_right a hb

/-- Two numbers are coprime when one is the only divisor they share. -/
def areCoprime (a b : Nat) : Bool := euclid a b == 1

theorem areCoprime_iff (a b : Nat) : areCoprime a b = true ↔ Nat.gcd a b = 1 := by
  simp [areCoprime, euclid_eq_gcd]

/-- A fraction in lowest terms: both numbers divided by what they share. -/
def reduce (a b : Nat) : Nat × Nat := (a / euclid a b, b / euclid a b)

/-- Reducing loses nothing: multiplying back gives the numbers. -/
theorem reduce_mul (a b : Nat) : (reduce a b).1 * euclid a b = a ∧ (reduce a b).2 * euclid a b = b :=
  ⟨Nat.div_mul_cancel (euclid_dvd_left a b), Nat.div_mul_cancel (euclid_dvd_right a b)⟩

end Maithic.Algo
