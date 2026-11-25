/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.TotalOrder

/-!
# Minimum and maximum

The least and the greatest element of a collection that is not empty. That the collection is not
empty is an argument of the functions, so neither needs a default value or an `Option`: there is
always an answer, and the theorems say it is the right one. The minimum is an element of the
collection and is at most every element; the maximum is an element and is at least every element.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [LE α] [DecidableLE α]

/-- The least element of a list that is not empty; of several least elements, the first. -/
def minimum : (l : List α) → l ≠ [] → α
  | [x], _ => x
  | x :: y :: ys, _ =>
    let m := minimum (y :: ys) (List.cons_ne_nil y ys)
    if x ≤ m then x else m

/-- The greatest element of a list that is not empty; of several greatest elements, the first. -/
def maximum : (l : List α) → l ≠ [] → α
  | [x], _ => x
  | x :: y :: ys, _ =>
    let m := maximum (y :: ys) (List.cons_ne_nil y ys)
    if m ≤ x then x else m

/-- The minimum is one of the elements. -/
theorem minimum_mem : ∀ (l : List α) (h : l ≠ []), minimum l h ∈ l
  | [x], _ => by simp [minimum]
  | x :: y :: ys, _ => by
    have ih := minimum_mem (y :: ys) (List.cons_ne_nil y ys)
    simp only [minimum]
    split
    · exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ ih

/-- The minimum is at most every element. -/
theorem minimum_le [TotalPreorder α] : ∀ (l : List α) (h : l ≠ []) {a : α}, a ∈ l → minimum l h ≤ a
  | [x], _, a, ha => by
    have : a = x := by simpa using ha
    subst this
    exact TotalPreorder.le_refl _
  | x :: y :: ys, _, a, ha => by
    have ih := @minimum_le _ (y :: ys) (List.cons_ne_nil y ys)
    simp only [minimum]
    split
    · rename_i hxm
      rcases List.mem_cons.mp ha with rfl | ha
      · exact TotalPreorder.le_refl _
      · exact TotalPreorder.le_trans _ _ _ hxm (ih ha)
    · rename_i hxm
      rcases List.mem_cons.mp ha with rfl | ha
      · exact (TotalPreorder.le_total _ _).resolve_left hxm
      · exact ih ha

/-- The maximum is one of the elements. -/
theorem maximum_mem : ∀ (l : List α) (h : l ≠ []), maximum l h ∈ l
  | [x], _ => by simp [maximum]
  | x :: y :: ys, _ => by
    have ih := maximum_mem (y :: ys) (List.cons_ne_nil y ys)
    simp only [maximum]
    split
    · exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ ih

/-- The maximum is at least every element. -/
theorem le_maximum [TotalPreorder α] : ∀ (l : List α) (h : l ≠ []) {a : α}, a ∈ l → a ≤ maximum l h
  | [x], _, a, ha => by
    have : a = x := by simpa using ha
    subst this
    exact TotalPreorder.le_refl _
  | x :: y :: ys, _, a, ha => by
    have ih := @le_maximum _ (y :: ys) (List.cons_ne_nil y ys)
    simp only [maximum]
    split
    · rename_i hmx
      rcases List.mem_cons.mp ha with rfl | ha
      · exact TotalPreorder.le_refl _
      · exact TotalPreorder.le_trans _ _ _ (ih ha) hmx
    · rename_i hmx
      rcases List.mem_cons.mp ha with rfl | ha
      · exact (TotalPreorder.le_total _ _).resolve_left hmx
      · exact ih ha

/-- The minimum is at most the maximum. -/
theorem minimum_le_maximum [TotalPreorder α] (l : List α) (h : l ≠ []) : minimum l h ≤ maximum l h :=
  minimum_le l h (maximum_mem l h)

/-- In a total order the two properties leave one answer: an element that is at most every
element is the minimum. -/
theorem eq_minimum [TotalOrder α] {l : List α} (h : l ≠ []) {m : α} (hm : m ∈ l)
    (hle : ∀ a ∈ l, m ≤ a) : m = minimum l h :=
  TotalOrder.le_antisymm _ _ (hle _ (minimum_mem l h)) (minimum_le l h hm)

/-- In a total order an element that is at least every element is the maximum. -/
theorem eq_maximum [TotalOrder α] {l : List α} (h : l ≠ []) {m : α} (hm : m ∈ l)
    (hle : ∀ a ∈ l, a ≤ m) : m = maximum l h :=
  TotalOrder.le_antisymm _ _ (le_maximum l h hm) (hle _ (maximum_mem l h))

/-- The head of a sorted list is at most its minimum, and so, in a total order, is its minimum. -/
theorem Sorted.head_eq_minimum [TotalOrder α] {x : α} {l : List α} (h : Sorted (x :: l)) :
    x = minimum (x :: l) (List.cons_ne_nil x l) :=
  eq_minimum _ (List.mem_cons_self ..) fun a ha => by
    rcases List.mem_cons.mp ha with rfl | ha
    · exact TotalPreorder.le_refl _
    · exact h.head_le ha

/-- The least element of an array that is not empty. -/
def arrayMinimum (a : Array α) (h : 0 < a.size) : α :=
  minimum a.toList (by intro hnil; simp [← Array.length_toList, hnil] at h)

/-- The greatest element of an array that is not empty. -/
def arrayMaximum (a : Array α) (h : 0 < a.size) : α :=
  maximum a.toList (by intro hnil; simp [← Array.length_toList, hnil] at h)

/-- The minimum of an array is at most each of its elements. -/
theorem arrayMinimum_le [TotalPreorder α] (a : Array α) (h : 0 < a.size) {i : Nat} (hi : i < a.size) :
    arrayMinimum a h ≤ a[i] :=
  minimum_le _ _ (by simp)

/-- The maximum of an array is at least each of its elements. -/
theorem le_arrayMaximum [TotalPreorder α] (a : Array α) (h : 0 < a.size) {i : Nat} (hi : i < a.size) :
    a[i] ≤ arrayMaximum a h :=
  le_maximum _ _ (by simp)

end Maithic.Algo
