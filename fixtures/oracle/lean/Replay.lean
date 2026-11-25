-- Oracle certificates replayed through the verified checkers of the Lean library.
-- Written by `make julia-fixtures` from julia/MaithicOracle; do not edit.
import Maithic.Oracle.Logic.LinearIntTactic

set_option linter.unusedVariables false

-- ∀ x y : ℤ, 2 * x + 4 * y < 7 → x + 2 * y > 2 → x + 2 * y = 3
theorem replay_linear_integer_parity (x y : Int) (h1 : (2 * x + 4 * y : Int) < 7) (h2 : (x + 2 * y : Int) > 2) : (x + 2 * y : Int) = 3 := by
  apply Classical.byContradiction
  intro negated
  rcases Int.lt_or_gt_of_ne negated with negated | negated
  · maithic_linear_int [x, y] [(1, ⟨[-1, -2], .le, -3⟩), (1, ⟨[1, 2], .le, 2⟩)] using [h2, negated]
  · maithic_linear_int [x, y] [(2, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h1, h2, negated]

-- ∀ x y : ℤ, 2 * x + 4 * y < 7 → x + 2 * y ≥ 4 → False
theorem replay_linear_integer_gap (x y : Int) (h1 : (2 * x + 4 * y : Int) < 7) (h2 : (x + 2 * y : Int) ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h1, h2]

-- ∀ x y : ℤ, 3 * x ≥ 4 → y ≥ x → y ≥ 2
theorem replay_linear_integer_bound (x y : Int) (h1 : (3 * x : Int) ≥ 4) (h2 : (y : Int) ≥ x) : (y : Int) ≥ 2 := by
  apply Classical.byContradiction
  intro negated
  maithic_linear_int [x, y] [(1, ⟨[-1, 0], .le, -2⟩), (1, ⟨[1, -1], .le, 0⟩), (1, ⟨[0, 1], .le, 1⟩)] using [h1, h2, negated]

-- ∀ x y z : ℤ, x - y < 3 → y - z < -1 → z - x ≤ -1 → False
theorem replay_linear_integer_cycle (x y z : Int) (h1 : (x - y : Int) < 3) (h2 : (y - z : Int) < -1) (h3 : (z - x : Int) ≤ -1) : False := by
  maithic_linear_int [x, y, z] [(1, ⟨[1, -1, 0], .le, 2⟩), (1, ⟨[0, 1, -1], .le, -2⟩), (1, ⟨[-1, 0, 1], .le, -1⟩)] using [h1, h2, h3]

-- ∀ a b c : ℤ, 7 * a + 12 * b + 31 * c = 17 → 3 * a + 5 * b + 14 * c = 7 → 1 ≤ a → a ≤ 40 → -50 ≤ b → b ≤ 50 → c ≥ -1000
theorem replay_linear_integer_system (a b c : Int) (h1 : (7 * a + 12 * b + 31 * c : Int) = 17) (h2 : (3 * a + 5 * b + 14 * c : Int) = 7) (h3 : (1 : Int) ≤ a) (h4 : (a : Int) ≤ 40) (h5 : (-50 : Int) ≤ b) (h6 : (b : Int) ≤ 50) : (c : Int) ≥ -1000 := by
  apply Classical.byContradiction
  intro negated
  maithic_linear_int [a, b, c] [(5, ⟨[7, 12, 31], .eq, 17⟩), (-12, ⟨[3, 5, 14], .eq, 7⟩), (1, ⟨[1, 0, 0], .le, 40⟩), (13, ⟨[0, 0, 1], .le, -1001⟩)] using [h1, h2, h4, negated]

-- ∀ x y : ℤ, 2 * x + 1 = 2 * y → x ≠ y
theorem replay_linear_integer_distinct (x y : Int) (h1 : (2 * x + 1 : Int) = 2 * y) : (x : Int) ≠ y := by
  intro negated
  maithic_linear_int [x, y] [(1, ⟨[0, 0], .le, -1⟩)] using [h1]
