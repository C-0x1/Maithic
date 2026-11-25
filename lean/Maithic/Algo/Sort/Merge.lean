/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.Sorted

/-!
# Merging two sorted lists

The step that merge sort is built from: two sorted lists become one. The specification has the
same two halves as that of a sort. The result holds exactly the elements of both inputs, each as
often as they hold it together; and when both inputs are sorted, so is the result.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [LE α] [DecidableLE α]

/-- Merges two lists by taking, again and again, the smaller of their two heads; of two equal
heads the one of the first list goes first. -/
def merge : List α → List α → List α
  | [], ys => ys
  | x :: xs, [] => x :: xs
  | x :: xs, y :: ys => if x ≤ y then x :: merge xs (y :: ys) else y :: merge (x :: xs) ys
termination_by xs ys => xs.length + ys.length

@[simp] theorem merge_nil_left (ys : List α) : merge [] ys = ys := by
  simp [merge]

@[simp] theorem merge_nil_right (xs : List α) : merge xs [] = xs := by
  cases xs <;> simp [merge]

/-- The merged list is a permutation of the two lists joined: nothing is lost and nothing is
added. -/
theorem merge_perm (xs ys : List α) : (merge xs ys).Perm (xs ++ ys) := by
  fun_induction merge xs ys with
  | case1 ys => simp
  | case2 x xs => simp
  | case3 x xs y ys hxy ih => exact List.Perm.cons x ih
  | case4 x xs y ys hxy ih =>
    exact (List.Perm.cons y ih).trans (List.perm_middle (l₁ := x :: xs) (l₂ := ys) (a := y)).symm

/-- The merged list holds what the two lists hold. -/
theorem mem_merge {a : α} {xs ys : List α} : a ∈ merge xs ys ↔ a ∈ xs ∨ a ∈ ys := by
  rw [(merge_perm xs ys).mem_iff, List.mem_append]

/-- The merged list is as long as the two lists together. -/
theorem length_merge (xs ys : List α) : (merge xs ys).length = xs.length + ys.length := by
  rw [(merge_perm xs ys).length_eq, List.length_append]

/-- Merging two sorted lists gives a sorted list. -/
theorem sorted_merge [TotalPreorder α] {xs ys : List α} (hx : Sorted xs) (hy : Sorted ys) :
    Sorted (merge xs ys) := by
  fun_induction merge xs ys with
  | case1 ys => exact hy
  | case2 x xs => exact hx
  | case3 x xs y ys hxy ih =>
    refine sorted_cons.mpr ⟨?_, ih hx.tail hy⟩
    intro z hz
    rcases mem_merge.mp hz with hz | hz
    · exact hx.head_le hz
    · rcases List.mem_cons.mp hz with rfl | hz
      · exact hxy
      · exact TotalPreorder.le_trans _ _ _ hxy (hy.head_le hz)
  | case4 x xs y ys hxy ih =>
    have hyx : y ≤ x := (TotalPreorder.le_total x y).resolve_left hxy
    refine sorted_cons.mpr ⟨?_, ih hx hy.tail⟩
    intro z hz
    rcases mem_merge.mp hz with hz | hz
    · rcases List.mem_cons.mp hz with rfl | hz
      · exact hyx
      · exact TotalPreorder.le_trans _ _ _ hyx (hx.head_le hz)
    · exact hy.head_le hz

end Maithic.Algo
