-- The blueprint "Odd numbers, squares and triangles", with one proof left.
-- Every declaration carries the identifier of its node; `maithic blueprint status` reads what Lean says about each.

namespace Triangles

-- blueprint: tri
/-- Triangular numbers.

The n-th triangular number is the sum 1 + 2 + ... + n, defined by recursion on n. -/
def tri : Nat → Nat
  | 0 => 0
  | n + 1 => tri n + (n + 1)

-- blueprint: sum-odd
/-- The sum of the first odd numbers.

The sum of the first n odd numbers, 1 + 3 + ... + (2n - 1), defined by recursion on n. -/
def sumOdd : Nat → Nat
  | 0 => 0
  | n + 1 => sumOdd n + (2 * n + 1)

-- blueprint: tri-step
/-- The next triangular number.

The triangular number after the n-th is the n-th plus n + 1. -/
theorem tri_succ (n : Nat) : tri (n + 1) = tri n + (n + 1) := by
  rfl

-- blueprint: tri-formula
/-- Twice a triangular number.

Twice the n-th triangular number is n (n + 1). -/
theorem two_mul_tri (n : Nat) : 2 * tri n = n * (n + 1) := by
  sorry

-- blueprint: odd-step
/-- The next odd number.

The sum of the first n + 1 odd numbers is the sum of the first n plus 2n + 1. -/
theorem sumOdd_succ (n : Nat) : sumOdd (n + 1) = sumOdd n + (2 * n + 1) := by
  rfl

-- blueprint: odd-square
/-- Odd numbers add up to a square.

The first n odd numbers add up to n squared. -/
theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by
  induction n with
  | zero => rfl
  | succ k ih =>
    simp only [sumOdd_succ, ih, Nat.succ_mul, Nat.mul_succ]
    omega

-- blueprint: two-triangles
/-- Two triangles make a square.

The n-th triangular number and the next add up to the square of n + 1. -/
theorem tri_add_tri_succ (n : Nat) : tri n + tri (n + 1) = (n + 1) * (n + 1) := by
  have h := two_mul_tri n
  rw [tri_succ, Nat.succ_mul]
  omega

-- blueprint: odd-triangles
/-- Odd numbers add up to two triangles.

The first n + 1 odd numbers add up to the n-th triangular number plus the next. -/
theorem sumOdd_eq_two_triangles (n : Nat) : sumOdd (n + 1) = tri n + tri (n + 1) := by
  rw [sumOdd_eq_square, tri_add_tri_succ]

end Triangles
