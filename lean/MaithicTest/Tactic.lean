/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Tactic.Auto

/-!
# Tests of the tactic suite

Each `example` is a test: the file compiles only if every tactic behaves as stated. Negative
cases use `fail_if_success`.
-/

namespace MaithicTest.Tactic

-- cap: P01-04-01
example (a b : Nat) (ha : a % 2 = 0) (hb : b % 2 = 0) : (a + b) % 2 = 0 := by maithic_auto

-- cap: P01-04-01
example : ∀ p q : Prop, p ∧ q → q ∧ p := by maithic_auto

-- cap: P01-04-01
example (xs ys : List Nat) : (xs ++ ys).length = xs.length + ys.length := by maithic_auto

-- cap: P01-04-01
example : (List.range 10).foldl (· + ·) 0 = 45 := by maithic_auto

/-- A proposition behind a definition: `maithic_auto` unfolds the head once. -/
def hidden : Prop := ∀ n : Nat, 2 * n = n + n

-- cap: P01-04-01
example : hidden := by maithic_auto

-- cap: P01-04-01
example (x y : Int) (h : x < y) : x + 1 ≤ y := by maithic_auto (heartbeats := 20000)

-- cap: P01-04-01
-- Every step runs under its own budget: one thousand heartbeats are not enough for this goal,
-- the failure leaves the goal untouched, and the default budget closes it.
example (a b c d : Nat) (h : a + b = c) (h' : b = 2 * a) (h2 : d = c + a) : d = 4 * a := by
  fail_if_success maithic_auto (heartbeats := 1)
  maithic_auto

-- cap: P01-04-01
-- A false statement is not closed by any step.
example : True := by
  fail_if_success
    have : ∀ a b : Nat, a - b + b = a := by maithic_auto
  trivial

-- cap: P01-04-02
example (p q : Prop) (hp : p) (hq : q) : p ∧ q := by
  constructor
  · maithic_close exact hp
  · maithic_close exact hq

-- cap: P01-04-02
-- `constructor` makes progress but leaves goals, so `maithic_close` fails and restores the state.
example (p q : Prop) (hp : p) (hq : q) : p ∧ q := by
  fail_if_success maithic_close constructor
  exact ⟨hp, hq⟩

-- cap: P01-04-03
example (a b : Nat) (h : a ≤ b) : a < b + 1 := by
  maithic_first [rfl, skip, omega]

-- cap: P01-04-03
-- The first tactic that makes progress wins even if it does not finish the proof.
example (p q : Prop) (hp : p) (hq : q) : p ∧ q := by
  maithic_first [skip, constructor, exact ⟨hp, hq⟩]
  · exact hp
  · exact hq

-- cap: P01-04-03
example (p : Prop) (hp : p) : p := by
  fail_if_success maithic_first [skip, rfl]
  exact hp

-- cap: P01-04-04
example (p : Prop) (hp : p) : p := by
  fail_if_success maithic_progress skip
  maithic_progress exact hp

-- cap: P01-04-04
example (a : Nat) : a + 0 = a := by
  maithic_progress simp

end MaithicTest.Tactic
