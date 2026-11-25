/-!
Doubling by recursion agrees with multiplying by two.

The automation portfolio does not find this proof: it needs induction. The step-wise search
does, with no model: run `maithic prove examples/double.lean --search`.
-/

/-- Twice `n`, by recursion. -/
def double : Nat → Nat
  | 0 => 0
  | n + 1 => double n + 2

theorem double_eq (n : Nat) : double n = 2 * n := by
  sorry
