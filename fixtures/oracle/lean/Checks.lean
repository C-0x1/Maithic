-- Statements that restate oracle certificates for the Lean kernel.
-- Written by `make julia-fixtures` from julia/MaithicOracle; do not edit.

set_option linter.unusedVariables false

-- ∀ x y : ℚ, 2 * x + 3 * y ≤ 6 → x ≥ 0 → y ≥ 0 → x + y ≤ 3
theorem linear_budget (x y : Rat) (h1 : (2 * x + 3 * y : Rat) ≤ 6) (h2 : (x : Rat) ≥ 0) (h3 : (y : Rat) ≥ 0) : (x + y : Rat) ≤ 3 := by grind

-- ∀ x y : ℚ, x + y ≤ 1 → x ≥ 1 → y ≥ 1 / 2 → False
theorem linear_contradiction (x y : Rat) (h1 : (x + y : Rat) ≤ 1) (h2 : (x : Rat) ≥ 1) (h3 : (y : Rat) ≥ 1 / 2) : False := by grind

-- ∀ x y : ℚ, x + y = 10 → x - y = 2 → x = 6
theorem linear_equation (x y : Rat) (h1 : (x + y : Rat) = 10) (h2 : (x - y : Rat) = 2) : (x : Rat) = 6 := by grind

-- ∀ x y : ℤ, 2 * x + 4 * y < 7 → x + 2 * y > 2 → x + 2 * y = 3
theorem linear_integer_parity (x y : Int) (h1 : (2 * x + 4 * y : Int) < 7) (h2 : (x + 2 * y : Int) > 2) : (x + 2 * y : Int) = 3 := by omega

-- ∀ x y : ℤ, 2 * x + 4 * y < 7 → x + 2 * y ≥ 4 → False
theorem linear_integer_gap (x y : Int) (h1 : (2 * x + 4 * y : Int) < 7) (h2 : (x + 2 * y : Int) ≥ 4) : False := by omega

-- ∀ x y : ℤ, 3 * x ≥ 4 → y ≥ x → y ≥ 2
theorem linear_integer_bound (x y : Int) (h1 : (3 * x : Int) ≥ 4) (h2 : (y : Int) ≥ x) : (y : Int) ≥ 2 := by omega

-- ∀ x y z : ℤ, x - y < 3 → y - z < -1 → z - x ≤ -1 → False
theorem linear_integer_cycle (x y z : Int) (h1 : (x - y : Int) < 3) (h2 : (y - z : Int) < -1) (h3 : (z - x : Int) ≤ -1) : False := by omega

-- ∀ a b c : ℤ, 7 * a + 12 * b + 31 * c = 17 → 3 * a + 5 * b + 14 * c = 7 → 1 ≤ a → a ≤ 40 → -50 ≤ b → b ≤ 50 → c ≥ -1000
theorem linear_integer_system (a b c : Int) (h1 : (7 * a + 12 * b + 31 * c : Int) = 17) (h2 : (3 * a + 5 * b + 14 * c : Int) = 7) (h3 : (1 : Int) ≤ a) (h4 : (a : Int) ≤ 40) (h5 : (-50 : Int) ≤ b) (h6 : (b : Int) ≤ 50) : (c : Int) ≥ -1000 := by omega

-- ∀ x y : ℤ, 2 * x + 1 = 2 * y → x ≠ y
theorem linear_integer_distinct (x y : Int) (h1 : (2 * x + 1 : Int) = 2 * y) : (x : Int) ≠ y := by omega

-- ∀ a b c d : ℚ, (a ^ 2 + b ^ 2) * (c ^ 2 + d ^ 2) = (a * c - b * d) ^ 2 + (a * d + b * c) ^ 2
theorem polynomial_identity (a b c d : Rat) : ((a ^ 2 + b ^ 2) * (c ^ 2 + d ^ 2) : Rat) = (a * c - b * d) ^ 2 + (a * d + b * c) ^ 2 := by grind

-- ∀ x y : ℚ, x + y = 3 → x * y = 2 → x ^ 2 + y ^ 2 = 5
theorem polynomial_symmetric (x y : Rat) : (x ^ 2 + y ^ 2 - 5 : Rat) = (x + y + 3) * (x + y - 3) + -2 * (x * y - 2) := by grind

-- ∀ x y : ℤ, 2 * x = 3 * y + 1 → y = 1 → x = 2
theorem polynomial_integer (x y : Int) : (2 * (x - 2) : Int) = 1 * (2 * x - (3 * y + 1)) + 3 * (y - 1) := by grind

-- 360 = 2^3 * 3^2 * 5
theorem discrete_factorization : (360 : Nat) = 2 ^ 3 * 3 ^ 2 * 5 ^ 1 := by decide

-- 18446744073709551617 = 274177 * 67280421310721
theorem discrete_factorization_fermat : (18446744073709551617 : Nat) = 274177 ^ 1 * 67280421310721 ^ 1 := by decide

-- 3 * 5 ≡ 1 [ZMOD 7]
theorem discrete_inverse : (3 * 5 : Int) % 7 = 1 := by decide

-- 659791110852991619 ^ 2 ≡ 5 [ZMOD 2305843009213693951]
theorem discrete_square_root : ((659791110852991619 : Int) ^ 2 - 5) % 2305843009213693951 = 0 := by decide

-- Int.gcd 1606938044258990275541962092341162602522202993782792835301375 (-1329227995784915872903807060280344575) = 1099511627775
theorem discrete_bezout : (1606938044258990275541962092341162602522202993782792835301375 : Int) * (-1099511627776) + (-1329227995784915872903807060280344575) * (-1329227995784915872903807060280344577) = 1099511627775 ∧ (1099511627775 : Int) * 1461501637332232146199470957515006442444899221505 = 1606938044258990275541962092341162602522202993782792835301375 ∧ (1099511627775 : Int) * (-1208925819615728686333953) = (-1329227995784915872903807060280344575) := by decide

-- ∃ x y : ℤ, x ^ 3 + y ^ 3 = 1729 ∧ 0 < x ∧ x < 9 ∧ x < y
theorem discrete_witness : ∃ x y : Int, x ^ 3 + y ^ 3 = 1729 ∧ 0 < x ∧ x < 9 ∧ x < y := ⟨1, 12, by decide⟩

-- ∃ x y : ℕ, x ^ 2 = 2 * y ^ 2 + 1 ∧ y > 0
theorem discrete_witness_pell : ∃ x y : Nat, x ^ 2 = 2 * y ^ 2 + 1 ∧ y > 0 := ⟨3, 2, by decide⟩
