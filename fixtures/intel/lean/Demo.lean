/-! Notes about doubling. -/

/-- Doubles a number. -/
def double : Nat → Nat
  | 0 => 0
  | n + 1 => double n + 2

theorem double_eq (n : Nat) : double n = 2 * n := by
  induction n with
  | zero => rfl
  | succ n ih =>
    simp [double]
    omega

theorem Bad_Name (a b : Nat) : a + b = b + a := Nat.add_comm a b

theorem open_one (n : Nat) : n + 0 = n := by
  sorry

theorem uses_open (n : Nat) : n + 0 = n := open_one n

structure Point where
  x : Nat
  y : Nat

instance : Inhabited Point := ⟨⟨0, 0⟩⟩

axiom shortcut : ∀ n : Nat, n = n + 0

theorem by_shortcut (n : Nat) : n = n + 0 := shortcut n

/-- A point in space. -/
structure Point3 extends Point where
  z : Nat

theorem term_gap : 2 + 2 = 4 := sorry
