/-!
The sum of the first `n` positive integers is `n * (n + 1) / 2`, stated without division.

The automation portfolio does not find this proof: it needs induction. Run
`maithic prove examples/triangle.lean` with a model provider configured, or with
`--mode replay` to use the answers stored in `fixtures/cassettes/`.
-/

/-- The `n`-th triangular number: `0 + 1 + ... + n`. -/
def tri : Nat → Nat
  | 0 => 0
  | n + 1 => tri n + (n + 1)

theorem tri_formula (n : Nat) : 2 * tri n = n * (n + 1) := by
  sorry
