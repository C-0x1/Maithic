/-!
The first `n` odd numbers add up to `n` squared: `1 + 3 + 5 + 7 = 16`.

A problem for a team. The automation portfolio does not prove it: the proof is an induction whose
step compares two polynomials in `n`. Run `maithic council examples/council.lean` with a model
provider configured, or with `--mode replay` to use the answers stored in `fixtures/cassettes/`.
-/

/-- The sum of the first `n` odd numbers. -/
def sumOdd : Nat → Nat
  | 0 => 0
  | n + 1 => sumOdd n + (2 * n + 1)

/-- The first `n` odd numbers add up to `n` squared. -/
theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by
  sorry
