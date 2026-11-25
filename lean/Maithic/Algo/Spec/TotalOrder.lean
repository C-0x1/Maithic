/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.Sorted

/-!
# Total orders, and sortedness by position

Sorting needs a total preorder: any two elements can be compared, and `≤` is transitive. Searching
for a value by comparisons needs one fact more, that two elements which are each at most the other
are the same element; a `TotalOrder` has it. This file also holds two forms of sortedness that the
algorithms work with: by position, for code that indexes, and as a check that compares neighbours
only, for code that runs.
-/

namespace Maithic.Algo

universe u

/-- A total order: a total preorder in which two elements that are each at most the other are
the same element. Searching for a value by comparisons needs this to conclude that what it found
*is* the value. -/
class TotalOrder (α : Type u) [LE α] : Prop extends TotalPreorder α where
  le_antisymm : ∀ a b : α, a ≤ b → b ≤ a → a = b

instance : TotalOrder Nat := { le_antisymm := fun _ _ => Nat.le_antisymm }

instance : TotalOrder Int := { le_antisymm := fun _ _ => Int.le_antisymm }

/-- Every element is at most itself. -/
theorem TotalPreorder.le_refl {α : Type u} [LE α] [TotalPreorder α] (a : α) : a ≤ a :=
  (TotalPreorder.le_total a a).elim id id

variable {α : Type u} [LE α]

/-- In a sorted list, an earlier position holds at most what a later position holds. -/
theorem Sorted.getElem_le [TotalPreorder α] {l : List α} (h : Sorted l) {i j : Nat} (hij : i ≤ j)
    (hj : j < l.length) : l[i]'(Nat.lt_of_le_of_lt hij hj) ≤ l[j] := by
  rcases Nat.lt_or_eq_of_le hij with hlt | rfl
  · exact List.pairwise_iff_getElem.mp h i j (Nat.lt_trans hlt hj) hj hlt
  · exact TotalPreorder.le_refl _

/-- Checks sortedness by comparing neighbours: one comparison for each adjacent pair, where the
definition compares every pair. -/
def isSorted [DecidableLE α] : List α → Bool
  | [] => true
  | [_] => true
  | x :: y :: rest => decide (x ≤ y) && isSorted (y :: rest)

/-- Comparing neighbours is enough: where `≤` is transitive, the check that looks at adjacent
pairs agrees with the definition, which looks at all pairs. -/
theorem isSorted_iff [DecidableLE α] [TotalPreorder α] : ∀ {l : List α}, isSorted l = true ↔ Sorted l
  | [] => by simp [isSorted]
  | [_] => by simp [isSorted]
  | x :: y :: rest => by
    have ih := isSorted_iff (l := y :: rest)
    simp only [isSorted, Bool.and_eq_true, decide_eq_true_eq, ih]
    constructor
    · rintro ⟨hxy, hrest⟩
      refine sorted_cons.mpr ⟨fun b hb => ?_, hrest⟩
      rcases List.mem_cons.mp hb with rfl | hb
      · exact hxy
      · exact TotalPreorder.le_trans _ _ _ hxy (hrest.head_le hb)
    · intro h
      exact ⟨h.head_le (List.mem_cons_self ..), h.tail⟩

end Maithic.Algo
