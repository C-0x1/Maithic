/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Fibonacci numbers, three ways

The definition by its recurrence is the specification: it is what the numbers *are*, and it takes
exponential time to run. Two faster programs follow. `fibMemo` fills a table from the bottom, each
entry computed once from the two before it, which is what memoization does to the recurrence.
`fibFast` carries only the last two numbers along. Each is proved equal to the definition for
every argument, so the fast programs may stand in for the slow one anywhere.
-/

namespace Maithic.Algo

/-- The Fibonacci numbers, by the recurrence that defines them. -/
def fib : Nat → Nat
  | 0 => 0
  | 1 => 1
  | n + 2 => fib n + fib (n + 1)

@[simp] theorem fib_zero : fib 0 = 0 := rfl

@[simp] theorem fib_one : fib 1 = 1 := rfl

theorem fib_add_two (n : Nat) : fib (n + 2) = fib n + fib (n + 1) := rfl

/-- The table of `fib 0`, …, `fib n`: each new entry is the sum of the two entries before it,
read from the table and not computed again. -/
def fibTable : Nat → Array Nat
  | 0 => #[0]
  | 1 => #[0, 1]
  | n + 2 =>
    let table := fibTable (n + 1)
    table.push (table[n]?.getD 0 + table[n + 1]?.getD 0)

/-- The `n`th Fibonacci number, read from the table. -/
def fibMemo (n : Nat) : Nat := (fibTable n)[n]?.getD 0

/-- The table has an entry for each of `0`, …, `n`, and each entry is the Fibonacci number of its
position. -/
theorem fibTable_spec : ∀ n, (fibTable n).size = n + 1 ∧ ∀ i, i ≤ n → (fibTable n)[i]? = some (fib i)
  | 0 => by
    refine ⟨rfl, fun i hi => ?_⟩
    have : i = 0 := by omega
    subst this
    rfl
  | 1 => by
    refine ⟨rfl, fun i hi => ?_⟩
    have : i = 0 ∨ i = 1 := by omega
    rcases this with rfl | rfl <;> rfl
  | n + 2 => by
    obtain ⟨hsize, hget⟩ := fibTable_spec (n + 1)
    refine ⟨by simp [fibTable, hsize], fun i hi => ?_⟩
    simp only [fibTable, Array.getElem?_push, hsize]
    split
    · rename_i hlast
      subst hlast
      rw [hget n (by omega), hget (n + 1) (by omega)]
      rfl
    · exact hget i (by omega)

theorem size_fibTable (n : Nat) : (fibTable n).size = n + 1 := (fibTable_spec n).1

/-- The memoized program computes the Fibonacci numbers. -/
theorem fibMemo_eq_fib (n : Nat) : fibMemo n = fib n := by
  simp [fibMemo, (fibTable_spec n).2 n (Nat.le_refl n)]

/-- Two consecutive Fibonacci numbers, each step made from the step before. -/
def fibPair : Nat → Nat × Nat
  | 0 => (0, 1)
  | n + 1 =>
    let p := fibPair n
    (p.2, p.1 + p.2)

/-- The `n`th Fibonacci number in `n` additions and constant space. -/
def fibFast (n : Nat) : Nat := (fibPair n).1

theorem fibPair_eq : ∀ n, fibPair n = (fib n, fib (n + 1))
  | 0 => rfl
  | n + 1 => by
    simp only [fibPair, fibPair_eq n]
    rfl

/-- The iterative program computes the Fibonacci numbers. -/
theorem fibFast_eq_fib (n : Nat) : fibFast n = fib n := by
  simp [fibFast, fibPair_eq]

/-- The two fast programs agree, since each agrees with the definition. -/
theorem fibMemo_eq_fibFast (n : Nat) : fibMemo n = fibFast n := by
  rw [fibMemo_eq_fib, fibFast_eq_fib]

/-- The Fibonacci numbers never decrease. -/
theorem fib_le_fib_succ : ∀ n, fib n ≤ fib (n + 1)
  | 0 => by decide
  | n + 1 => by
    rw [fib_add_two]
    omega

end Maithic.Algo
