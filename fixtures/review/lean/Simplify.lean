/-! Proofs that say more than they need to. -/

/-- A step that restates a hypothesis: one call of automation does the whole proof. -/
theorem le_succ_of_le' (a b : Nat) (h : a ≤ b) : a ≤ b + 1 := by
  have h2 : a ≤ b := h
  omega

/-- Triangular numbers, by recursion. -/
def tri : Nat → Nat
  | 0 => 0
  | n + 1 => tri n + (n + 1)

/-- The induction is needed, and so is the arithmetic; the fact about zero is not. -/
theorem two_tri (n : Nat) : 2 * tri n = n * (n + 1) := by
  have zero : tri 0 = 0 := rfl
  induction n with
  | zero => rfl
  | succ k ih => simp only [tri]; grind

-- maithic: allow docstring (an example of a suppression with its reason)
theorem tri_zero : tri 0 = 0 := rfl

-- maithic: allow docstring
theorem tri_one : tri 1 = 1 := rfl
