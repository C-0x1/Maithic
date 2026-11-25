/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBench.Problem

/-!
# The basics suite

Statements that the automation shipped with Lean can prove: propositional logic, arithmetic on
natural numbers and integers, list facts and small computations. The suite exists so that the
whole pipeline can be exercised end to end with no model provider, and so that any change which
makes the prover worse on easy problems is caught at once.

Reference solutions are in `MaithicBench.Basics.Solutions`.
-/

namespace MaithicBench.Basics

@[maithic_problem "basics-001" (tags := ["logic"])]
def and_comm' : Prop := ∀ p q : Prop, p ∧ q → q ∧ p

@[maithic_problem "basics-002" (tags := ["logic"])]
def or_comm' : Prop := ∀ p q : Prop, p ∨ q → q ∨ p

@[maithic_problem "basics-003" (tags := ["logic"])]
def imp_trans : Prop := ∀ p q r : Prop, (p → q) → (q → r) → p → r

@[maithic_problem "basics-004" (tags := ["nat"])]
def add_comm' : Prop := ∀ a b : Nat, a + b = b + a

@[maithic_problem "basics-005" (tags := ["nat"])]
def even_add_even : Prop := ∀ a b : Nat, a % 2 = 0 → b % 2 = 0 → (a + b) % 2 = 0

@[maithic_problem "basics-006" (tags := ["nat"])]
def lt_succ_of_le' : Prop := ∀ a b : Nat, a ≤ b → a < b + 1

@[maithic_problem "basics-007" (tags := ["nat"])]
def mul_two : Prop := ∀ n : Nat, 2 * n = n + n

@[maithic_problem "basics-008" (tags := ["nat"])]
def succ_ne_zero' : Prop := ∀ n : Nat, n + 1 ≠ 0

@[maithic_problem "basics-009" (tags := ["int"])]
def add_one_le_of_lt : Prop := ∀ x y : Int, x < y → x + 1 ≤ y

@[maithic_problem "basics-010" (tags := ["list"])]
def length_append' : Prop := ∀ (xs ys : List Nat), (xs ++ ys).length = xs.length + ys.length

@[maithic_problem "basics-011" (tags := ["list"])]
def reverse_reverse' : Prop := ∀ xs : List Nat, xs.reverse.reverse = xs

@[maithic_problem "basics-012" (tags := ["decide"])]
def small_sum : Prop := (List.range 10).foldl (· + ·) 0 = 45

@[maithic_problem "basics-013" (tags := ["logic"])]
def not_not_intro' : Prop := ∀ p : Prop, p → ¬¬p

@[maithic_problem "basics-014" (tags := ["logic"])]
def and_assoc' : Prop := ∀ p q r : Prop, (p ∧ q) ∧ r → p ∧ (q ∧ r)

@[maithic_problem "basics-015" (tags := ["logic"]) (difficulty := 2)]
def and_or_distrib : Prop := ∀ p q r : Prop, p ∧ (q ∨ r) → (p ∧ q) ∨ (p ∧ r)

@[maithic_problem "basics-016" (tags := ["logic"])]
def contrapositive : Prop := ∀ p q : Prop, (p → q) → ¬q → ¬p

@[maithic_problem "basics-017" (tags := ["nat"])]
def odd_add_odd : Prop := ∀ a b : Nat, a % 2 = 1 → b % 2 = 1 → (a + b) % 2 = 0

@[maithic_problem "basics-018" (tags := ["nat"])]
def sub_add_cancel_of_le : Prop := ∀ a b : Nat, b ≤ a → a - b + b = a

@[maithic_problem "basics-019" (tags := ["nat"]) (difficulty := 2)]
def max_comm' : Prop := ∀ a b : Nat, max a b = max b a

@[maithic_problem "basics-020" (tags := ["int"])]
def le_antisymm' : Prop := ∀ x y : Int, x ≤ y → y ≤ x → x = y

@[maithic_problem "basics-021" (tags := ["int"])]
def int_add_self : Prop := ∀ x : Int, x + x = 2 * x

@[maithic_problem "basics-022" (tags := ["list"])]
def length_map' : Prop := ∀ (f : Nat → Nat) (xs : List Nat), (xs.map f).length = xs.length

@[maithic_problem "basics-023" (tags := ["list"])]
def append_nil' : Prop := ∀ xs : List Nat, xs ++ [] = xs

@[maithic_problem "basics-024" (tags := ["decide"])]
def two_pow_ten : Prop := 2 ^ 10 = 1024

end MaithicBench.Basics
