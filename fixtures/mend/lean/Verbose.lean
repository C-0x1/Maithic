/-!
Proofs that work and carry more than they need: a demonstration for `maithic mend minimize`.

    maithic mend minimize fixtures/mend/lean/Verbose.lean
    maithic mend minimize fixtures/mend/lean/Verbose.lean --automation
-/

/-- Twice a number. -/
def twice (n : Nat) : Nat := n + n

/-- A fact nobody uses, a step that changes nothing that matters, and a lemma `simp` never needs. -/
theorem twice_eq (n : Nat) : twice n = 2 * n := by
  unfold twice
  have bound : n ≤ n + n := by
    omega
  simp only [Nat.two_mul, Nat.add_zero]

/-- Lemmas handed to `simp` that it finds by itself, or does not use. -/
theorem length_twice (xs : List Nat) : (xs ++ xs).length = twice xs.length := by
  simp [twice, List.length_append, Nat.add_comm]

/-- Facts stated on the way that the last step does not need. -/
theorem twice_mono (m n : Nat) (h : m ≤ n) : twice m ≤ twice n := by
  unfold twice
  have h2 : m + m ≤ n + m := by
    omega
  have h3 : n + m ≤ n + n := by
    omega
  omega

/-- No step can go, and yet one call of automation proves the same: `--automation` finds it. -/
theorem sum_le (a b c : Nat) (h₁ : a ≤ b) (h₂ : b ≤ c) : a + a ≤ c + c := by
  have h : a ≤ c := by omega
  exact Nat.add_le_add h h

/-- The same statement, proved from two lemmas and no axiom. `omega` would prove it in one call,
and would bring in axioms this proof does without: the shorter proof is refused. -/
theorem sum_le_plain (a b c : Nat) (h₁ : a ≤ b) (h₂ : b ≤ c) : a + a ≤ c + c := by
  have h : a ≤ c := Nat.le_trans h₁ h₂
  exact Nat.add_le_add h h

/-- Nothing to take out. -/
theorem le_twice (n : Nat) : n ≤ twice n := by
  unfold twice
  omega
