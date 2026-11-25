/-!
A file that stopped compiling after the code it rests on moved on: a demonstration for
`maithic mend`. Each theorem is broken in a different way, and each way has its own remedy.

    maithic mend scan fixtures/mend/lean/Upgrade.lean
    maithic mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml
-/

/-- Twice a number. -/
def twice (n : Nat) : Nat := n + n

theorem twice_eq (n : Nat) : twice n = 2 * n := by
  simp [twice]
  omega

/-- The old name of `twice_eq`, kept for a while so that its users have time to move. -/
@[deprecated twice_eq (since := "2026-01-01")]
theorem twice_is_double (n : Nat) : twice n = 2 * n := twice_eq n

/-- A name that is gone. The table of renames knows its successor. -/
theorem sum_comm (a b : Nat) : a + b = b + a := by
  exact Nat.add_commutes a b

/-- A name that is deprecated. Lean says what to use instead. -/
theorem twice_add_zero (n : Nat) : twice n + 0 = 2 * n := by
  rw [Nat.add_zero]
  exact twice_is_double n

/-- A search that was never written down. Lean says what it found. -/
theorem le_succ_self (n : Nat) : n ≤ n + 1 := by
  exact?

/-- A tactic that no longer closes its goal. One step of the proof has to change. -/
theorem twice_add (m n : Nat) : twice (m + n) = twice m + twice n := by
  induction n with
  | zero => simp [twice]
  | succ k ih =>
    simp [twice] at *
    simp

/-- A term that no longer has the type its place asks for. -/
theorem add_zero_eq (n : Nat) : n + 0 = n := by
  exact Nat.zero_add n
