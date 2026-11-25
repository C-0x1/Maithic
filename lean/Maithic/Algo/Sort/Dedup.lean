/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.TotalOrder

/-!
# Removing duplicates from a sorted list

In a sorted list, equal elements stand next to each other, so one pass that compares neighbours
removes every duplicate. The theorems say what the pass keeps and what it achieves: it keeps
exactly the values of the list, it keeps the list sorted, and on a sorted list in a total order
no value is left twice. The last holds because the input is sorted: on `[1, 2, 1]` the same
function leaves both ones, and the proof shows where sortedness is used.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [DecidableEq α]

/-- Drops each element that equals the element after it. -/
def dedupSorted : List α → List α
  | [] => []
  | [x] => [x]
  | x :: y :: ys => if x = y then dedupSorted (y :: ys) else x :: dedupSorted (y :: ys)

/-- What is left is a sublist: elements are dropped, never moved or added. -/
theorem dedupSorted_sublist : ∀ l : List α, (dedupSorted l).Sublist l
  | [] => List.Sublist.refl _
  | [_] => List.Sublist.refl _
  | x :: y :: ys => by
    have ih := dedupSorted_sublist (y :: ys)
    simp only [dedupSorted]
    split
    · exact List.Sublist.cons x ih
    · exact List.Sublist.cons_cons x ih

/-- The values of the list are the values of what is left. -/
theorem mem_dedupSorted {a : α} : ∀ {l : List α}, a ∈ dedupSorted l ↔ a ∈ l
  | [] => by simp [dedupSorted]
  | [_] => by simp [dedupSorted]
  | x :: y :: ys => by
    have ih := @mem_dedupSorted a (y :: ys)
    simp only [dedupSorted]
    split
    · rename_i hxy
      subst hxy
      rw [ih]
      simp
    · rw [List.mem_cons, ih, List.mem_cons (l := y :: ys)]

/-- What is left is no longer than the list. -/
theorem length_dedupSorted_le (l : List α) : (dedupSorted l).length ≤ l.length :=
  (dedupSorted_sublist l).length_le

/-- A sorted list stays sorted. -/
theorem sorted_dedupSorted [LE α] {l : List α} (h : Sorted l) : Sorted (dedupSorted l) :=
  h.sublist (dedupSorted_sublist l)

/-- On a sorted list in a total order, no value is left twice. -/
theorem nodup_dedupSorted [LE α] [TotalOrder α] : ∀ {l : List α}, Sorted l → (dedupSorted l).Nodup
  | [], _ => by simp [dedupSorted]
  | [_], _ => by simp [dedupSorted]
  | x :: y :: ys, h => by
    have ih := @nodup_dedupSorted _ _ (y :: ys) h.tail
    simp only [dedupSorted]
    split
    · exact ih
    · rename_i hxy
      refine List.nodup_cons.mpr ⟨?_, ih⟩
      intro hx
      rcases List.mem_cons.mp (mem_dedupSorted.mp hx) with hx | hx
      · exact hxy hx
      · -- `x` stands again after `y`: sortedness puts `y` between two copies of `x`.
        have hxy' : x ≤ y := h.head_le (List.mem_cons_self ..)
        have hyx : y ≤ x := h.tail.head_le hx
        exact hxy (TotalOrder.le_antisymm _ _ hxy' hyx)

/-- A list without duplicates is left as it is. -/
theorem dedupSorted_eq_self : ∀ {l : List α}, l.Nodup → dedupSorted l = l
  | [], _ => rfl
  | [_], _ => rfl
  | x :: y :: ys, h => by
    have hne : x ≠ y := fun hxy => (List.nodup_cons.mp h).1 (hxy ▸ List.mem_cons_self ..)
    simp only [dedupSorted, hne, ↓reduceIte]
    rw [dedupSorted_eq_self (List.nodup_cons.mp h).2]

end Maithic.Algo
