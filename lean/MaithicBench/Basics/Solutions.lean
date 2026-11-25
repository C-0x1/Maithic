/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBench.Basics

/-!
# Reference solutions of the basics suite

One kernel-checked proof per problem. The proofs are written out rather than left to a single
portfolio call, so that they double as small worked examples.
-/

namespace MaithicBench.Basics.Solutions

open MaithicBench.Basics

theorem and_comm' : and_comm' := fun _ _ h => ⟨h.2, h.1⟩

theorem or_comm' : or_comm' := fun _ _ h => h.elim Or.inr Or.inl

theorem imp_trans : imp_trans := fun _ _ _ hpq hqr hp => hqr (hpq hp)

theorem add_comm' : add_comm' := fun a b => Nat.add_comm a b

theorem even_add_even : even_add_even := by
  intro a b ha hb
  omega

theorem lt_succ_of_le' : lt_succ_of_le' := fun _ _ h => Nat.lt_succ_of_le h

theorem mul_two : mul_two := by
  intro n
  omega

theorem succ_ne_zero' : succ_ne_zero' := fun n => Nat.succ_ne_zero n

theorem add_one_le_of_lt : add_one_le_of_lt := by
  intro x y h
  omega

theorem length_append' : length_append' := fun _ _ => List.length_append

theorem reverse_reverse' : reverse_reverse' := fun xs => List.reverse_reverse xs

theorem small_sum : small_sum := by
  unfold Basics.small_sum
  decide

theorem not_not_intro' : not_not_intro' := fun _ hp hnp => hnp hp

theorem and_assoc' : and_assoc' := fun _ _ _ h => ⟨h.1.1, h.1.2, h.2⟩

theorem and_or_distrib : and_or_distrib := by
  intro p q r ⟨hp, hqr⟩
  rcases hqr with hq | hr
  · exact Or.inl ⟨hp, hq⟩
  · exact Or.inr ⟨hp, hr⟩

theorem contrapositive : contrapositive := fun _ _ hpq hnq hp => hnq (hpq hp)

theorem odd_add_odd : odd_add_odd := by
  intro a b ha hb
  omega

theorem sub_add_cancel_of_le : sub_add_cancel_of_le := fun _ _ h => Nat.sub_add_cancel h

theorem max_comm' : max_comm' := fun a b => Nat.max_comm a b

theorem le_antisymm' : le_antisymm' := fun _ _ h₁ h₂ => Int.le_antisymm h₁ h₂

theorem int_add_self : int_add_self := by
  intro x
  omega

theorem length_map' : length_map' := fun _ _ => List.length_map ..

theorem append_nil' : append_nil' := fun xs => List.append_nil xs

theorem two_pow_ten : two_pow_ten := by
  unfold Basics.two_pow_ten
  decide

end MaithicBench.Basics.Solutions
