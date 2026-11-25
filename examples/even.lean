/-!
The sum of two even numbers is even.

Run `maithic prove examples/even.lean` to have the proof found and verified.
-/

theorem even_add_even (a b : Nat) (ha : a % 2 = 0) (hb : b % 2 = 0) : (a + b) % 2 = 0 := by
  sorry
