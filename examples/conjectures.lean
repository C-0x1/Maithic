/-!
# Conjectures

Statements as they come out of a first attempt at writing them down: some are false, one is
false by a slip of the key, one says nothing, and one is fine. None is proved yet.
`maithic refute examples/conjectures.lean` tells them apart before anyone spends time on a proof.
-/

/-- False: fails when one of the two is zero. -/
theorem sum_le_product (a b : Nat) : a + b ≤ a * b := by
  sorry

/-- False: squaring does not keep the order of negative numbers. -/
theorem square_monotone (x y : Int) (h : x ≤ y) : x * x ≤ y * y := by
  sorry

/-- False, and the failing point is not small: a solver finds it at once. -/
theorem within_budget (x y : Rat) (h₁ : 2 * x + 3 * y ≤ 6) (h₂ : 0 ≤ x) (h₃ : 0 ≤ y) : x + y ≤ 2 := by
  sorry

/-- False by a slip: `m` was meant to be `n`. Lean takes a name it does not know for a new
variable, so the statement elaborates, and says something else. -/
theorem pred_lt (n : Nat) (h : 0 < n) : n - 1 < m := by
  sorry

/-- True and empty: no number is below three and above five. -/
theorem small_and_large (n : Nat) (h₁ : n < 3) (h₂ : n > 5) : n * n = 7 := by
  sorry

/-- True. Nothing is found, which is not a proof. -/
theorem reverse_append (l₁ l₂ : List Nat) : (l₁ ++ l₂).reverse = l₂.reverse ++ l₁.reverse := by
  sorry
