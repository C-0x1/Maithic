/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.Sorted

/-!
# Insertion sort

Insertion sort on lists, with the two halves of its specification: the output is sorted, and the
output is a permutation of the input. Together they pin the result down completely.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [LE α] [DecidableLE α]

/-- Inserts `x` in front of the first element that is not smaller than it. -/
def insertSorted (x : α) : List α → List α
  | [] => [x]
  | y :: ys => if x ≤ y then x :: y :: ys else y :: insertSorted x ys

/-- Insertion sort. -/
def insertionSort : List α → List α
  | [] => []
  | x :: xs => insertSorted x (insertionSort xs)

/-- Inserting adds exactly the new element. -/
theorem mem_insertSorted {x z : α} {l : List α} : z ∈ insertSorted x l ↔ z = x ∨ z ∈ l := by
  induction l with
  | nil => simp [insertSorted]
  | cons y ys ih =>
    simp only [insertSorted]
    split
    · simp
    · simp only [List.mem_cons, ih]
      constructor
      · rintro (h | h | h)
        · exact Or.inr (Or.inl h)
        · exact Or.inl h
        · exact Or.inr (Or.inr h)
      · rintro (h | h | h)
        · exact Or.inr (Or.inl h)
        · exact Or.inl h
        · exact Or.inr (Or.inr h)

/-- Inserting into a list gives a permutation of the list with the element in front. -/
theorem insertSorted_perm (x : α) (l : List α) : (insertSorted x l).Perm (x :: l) := by
  induction l with
  | nil => exact List.Perm.refl _
  | cons y ys ih =>
    simp only [insertSorted]
    split
    · exact List.Perm.refl _
    · exact (List.Perm.cons y ih).trans (List.Perm.swap x y ys)

/-- Inserting into a sorted list keeps it sorted. -/
theorem sorted_insertSorted [TotalPreorder α] (x : α) {l : List α} (h : Sorted l) :
    Sorted (insertSorted x l) := by
  induction l with
  | nil => simp [insertSorted]
  | cons y ys ih =>
    have hy := sorted_cons.mp h
    simp only [insertSorted]
    split
    · rename_i hxy
      refine sorted_cons.mpr ⟨?_, h⟩
      intro z hz
      rcases List.mem_cons.mp hz with rfl | hz
      · exact hxy
      · exact TotalPreorder.le_trans _ _ _ hxy (hy.1 z hz)
    · rename_i hxy
      have hyx : y ≤ x := (TotalPreorder.le_total x y).resolve_left hxy
      refine sorted_cons.mpr ⟨?_, ih hy.2⟩
      intro z hz
      rcases mem_insertSorted.mp hz with rfl | hz
      · exact hyx
      · exact hy.1 z hz

/-- The output of insertion sort is sorted. -/
theorem sorted_insertionSort [TotalPreorder α] (l : List α) : Sorted (insertionSort l) := by
  induction l with
  | nil => exact sorted_nil
  | cons x xs ih => exact sorted_insertSorted x ih

/-- The output of insertion sort is a permutation of its input. -/
theorem insertionSort_perm (l : List α) : (insertionSort l).Perm l := by
  induction l with
  | nil => exact List.Perm.refl _
  | cons x xs ih => exact (insertSorted_perm x _).trans (List.Perm.cons x ih)

/-- Insertion sort keeps the length. -/
theorem length_insertionSort (l : List α) : (insertionSort l).length = l.length :=
  (insertionSort_perm l).length_eq

/-- Insertion sort keeps the elements. -/
theorem mem_insertionSort {x : α} {l : List α} : x ∈ insertionSort l ↔ x ∈ l :=
  (insertionSort_perm l).mem_iff

/-- Sorting a sorted list changes nothing. -/
theorem insertionSort_of_sorted {l : List α} (h : Sorted l) : insertionSort l = l := by
  induction l with
  | nil => rfl
  | cons x xs ih =>
    have hx := sorted_cons.mp h
    simp only [insertionSort, ih hx.2]
    cases xs with
    | nil => rfl
    | cons y ys => simp [insertSorted, hx.1 y (List.mem_cons_self ..)]

/-- Insertion sort is idempotent. -/
theorem insertionSort_idem [TotalPreorder α] (l : List α) :
    insertionSort (insertionSort l) = insertionSort l :=
  insertionSort_of_sorted (sorted_insertionSort l)

end Maithic.Algo
