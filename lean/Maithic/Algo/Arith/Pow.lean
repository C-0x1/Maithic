/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Exponentiation by squaring

`b ^ n` by `n` multiplications is the definition; by squaring it takes about `2 log₂ n`. The two
programs here halve the exponent at every step, and each has a theorem that says it computes the
power: `fastPow_eq` for the power itself and `powMod_eq` for the power modulo a number, which is
the form cryptography and primality tests use, where the power itself would not fit in memory.
-/

namespace Maithic.Algo

/-- The square of a number. A function of its own so that the half power is computed once. -/
@[inline] def sq (x : Nat) : Nat := x * x

/-- `b ^ n` by squaring: the exponent is halved at every step. -/
def fastPow (b n : Nat) : Nat :=
  if n = 0 then 1
  else if n % 2 = 0 then sq (fastPow b (n / 2))
  else sq (fastPow b (n / 2)) * b
termination_by n
decreasing_by all_goals omega

/-- **Correctness of exponentiation by squaring.** -/
theorem fastPow_eq (b n : Nat) : fastPow b n = b ^ n := by
  induction n using Nat.strongRecOn with
  | _ n ih =>
    unfold fastPow
    split
    · next h => subst h; rfl
    · next h =>
      have hlt : n / 2 < n := by omega
      split
      · next he =>
        rw [ih _ hlt, sq, ← Nat.pow_add]
        congr 1; omega
      · next ho =>
        rw [ih _ hlt, sq, ← Nat.pow_add, ← Nat.pow_succ]
        congr 1; omega

/-- `b ^ n % m` by squaring, with every intermediate result below `m`. -/
def powMod (b n m : Nat) : Nat :=
  if n = 0 then 1 % m
  else if n % 2 = 0 then sq (powMod b (n / 2) m) % m
  else sq (powMod b (n / 2) m) % m * (b % m) % m
termination_by n
decreasing_by all_goals omega

/-- **Correctness of modular exponentiation.** -/
theorem powMod_eq (b n m : Nat) : powMod b n m = b ^ n % m := by
  induction n using Nat.strongRecOn with
  | _ n ih =>
    unfold powMod
    split
    · next h => subst h; rfl
    · next h =>
      have hlt : n / 2 < n := by omega
      have hsq : sq (b ^ (n / 2) % m) % m = b ^ (n / 2) * b ^ (n / 2) % m := by rw [sq, ← Nat.mul_mod]
      split
      · next he =>
        rw [ih _ hlt, hsq, ← Nat.pow_add]
        congr 2; omega
      · next ho =>
        rw [ih _ hlt, hsq, ← Nat.mul_mod, ← Nat.pow_add, ← Nat.pow_succ]
        congr 2; omega

/-- Every result of `powMod` is below the modulus. -/
theorem powMod_lt (b n : Nat) {m : Nat} (hm : 0 < m) : powMod b n m < m := by
  rw [powMod_eq]; exact Nat.mod_lt _ hm

/-- The two programs agree. -/
theorem powMod_eq_fastPow_mod (b n m : Nat) : powMod b n m = fastPow b n % m := by
  rw [powMod_eq, fastPow_eq]

/-- Powers of two, as a check of the program on a familiar table. -/
theorem fastPow_two_ten : fastPow 2 10 = 1024 := by rw [fastPow_eq]

end Maithic.Algo
