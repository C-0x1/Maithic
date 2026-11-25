/-
The problem, as it was posed:

  Show that the square of a negative integer is positive.

The statement below was chosen from 4 candidates. Lean elaborates it; nothing here proves it.
Its reading of the problem: The number is an integer below zero, and positive means above zero.

Read back into English, the statement says:

  For every integer n that is less than zero, the square of n is greater than zero.

What the checks found:

  vacuity           passed
  counterexample    passed   no counterexample in 300 sampled cases
  triviality        passed
  numbers           passed
  variables         passed
  traps             passed
  back-translation  passed
  round-trip        passed   agreement 10 of 10

That a statement says what was meant is a matter of evidence. Read it before you rely on it.
-/
set_option autoImplicit false in
theorem problem (n : Int) (h : n < 0) : 0 < n ^ 2 := by
  sorry
