import Maithic.Oracle.Logic.LinearIntTactic

/-- The half-planes `2 * x + 4 * y < 7` and `x + 2 * y ≥ 4` have no point in common. -/
theorem no_point (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  sorry

/-- Not a theorem: it fails wherever `x = y`. -/
theorem wrong (x y : Int) (h : x ≤ y) : x + 1 ≤ y := by
  sorry
