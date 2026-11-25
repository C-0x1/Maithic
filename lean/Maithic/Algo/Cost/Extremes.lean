/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Cost.Comparisons

/-!
# The best case and the worst case of insertion sort

`two_mul_insertionSortCost_le` bounds the comparisons of insertion sort by `n (n - 1) / 2`. A
bound says what cannot happen; it does not say that the bound is ever met. This file finds the two
ends of the range, each with the inputs that reach it:

* `two_mul_insertionSortCost_of_falling`: on a falling list, in which every element is above all
  that come after it, the count is `n (n - 1) / 2` exactly, so the upper bound cannot be improved;
* `pred_length_le_insertionSortCost`: no list of `n` elements is sorted with fewer than `n - 1`
  comparisons;
* `insertionSortCost_of_sorted`: on a list that is sorted already the count is `n - 1` exactly,
  so the lower bound cannot be improved either, and the algorithm is linear on sorted input.

`insertionSortCost_reverse_range` and `insertionSortCost_range` state the two ends for the lists
that the measurements of `maithic algo differential` use, `n - 1, …, 1, 0` and `0, 1, …, n - 1`:
the points of the measured curve at these inputs are not observations, they are theorems.
-/

namespace Maithic.Algo.Cost

open Maithic.Algo

universe u

variable {α : Type u} [LE α]

/-- Every element is above all the elements that come after it. -/
def Falling : List α → Prop
  | [] => True
  | x :: xs => (∀ y ∈ xs, ¬ x ≤ y) ∧ Falling xs

@[simp] theorem falling_nil : Falling ([] : List α) := trivial

theorem falling_cons {x : α} {xs : List α} : Falling (x :: xs) ↔ (∀ y ∈ xs, ¬ x ≤ y) ∧ Falling xs :=
  Iff.rfl

variable [DecidableLE α]

/-- In a falling list the new element is compared with every element sorted so far. -/
theorem insertionSortCost_cons_of_falling {x : α} {xs : List α} (h : ∀ y ∈ xs, ¬ x ≤ y) :
    insertionSortCost (x :: xs) = insertionSortCost xs + xs.length := by
  have hcost : insertCost x (insertionSort xs) = (insertionSort xs).length :=
    insertCost_eq_length fun y hy => h y (mem_insertionSort.mp hy)
  rw [length_insertionSort] at hcost
  simp only [insertionSortCost, hcost]

private theorem triangle (n : Nat) : n * (n - 1) + 2 * n = (n + 1) * n := by
  cases n with
  | zero => rfl
  | succ m =>
    simp only [Nat.add_sub_cancel]
    grind

/-- The worst case: a falling list of `n` elements costs `n (n - 1) / 2` comparisons, written
without the division. The bound of `two_mul_insertionSortCost_le` is reached for every length. -/
theorem two_mul_insertionSortCost_of_falling {l : List α} (h : Falling l) :
    2 * insertionSortCost l = l.length * (l.length - 1) := by
  induction l with
  | nil => rfl
  | cons x xs ih =>
    have hcons := insertionSortCost_cons_of_falling h.1
    have hrest := ih h.2
    have key := triangle xs.length
    simp only [List.length_cons, Nat.add_sub_cancel]
    omega

/-- The best case: a sorted list of `n` elements costs `n - 1` comparisons, one for each element
after the first. -/
theorem insertionSortCost_of_sorted {l : List α} (h : Sorted l) : insertionSortCost l = l.length - 1 := by
  induction l with
  | nil => rfl
  | cons x xs ih =>
    have hx := sorted_cons.mp h
    have hrest := ih hx.2
    simp only [insertionSortCost, insertionSort_of_sorted hx.2, hrest]
    cases xs with
    | nil => rfl
    | cons y ys =>
      have hxy : x ≤ y := hx.1 y (List.mem_cons_self ..)
      simp only [insertCost, hxy, ↓reduceIte, List.length_cons]
      omega

/-- Inserting into a list that is not empty compares at least once. -/
theorem insertCost_pos (x : α) {l : List α} (h : l ≠ []) : 0 < insertCost x l := by
  cases l with
  | nil => exact absurd rfl h
  | cons y ys =>
    simp only [insertCost]
    split <;> omega

/-- No list is sorted for less: every element after the first costs at least one comparison. -/
theorem pred_length_le_insertionSortCost (l : List α) : l.length - 1 ≤ insertionSortCost l := by
  induction l with
  | nil => exact Nat.le_refl 0
  | cons x xs ih =>
    simp only [insertionSortCost, List.length_cons, Nat.add_sub_cancel]
    cases xs with
    | nil => exact Nat.zero_le _
    | cons y ys =>
      have hne : insertionSort (y :: ys) ≠ [] := by
        intro hnil
        have hlen := length_insertionSort (y :: ys)
        rw [hnil] at hlen
        simp at hlen
      have hpos := insertCost_pos x hne
      simp only [List.length_cons, Nat.add_sub_cancel] at ih ⊢
      omega

/-- Every input lies between the two ends, and each end is reached. -/
theorem insertionSortCost_bounds (l : List α) :
    l.length - 1 ≤ insertionSortCost l ∧ 2 * insertionSortCost l ≤ l.length * (l.length - 1) :=
  ⟨pred_length_le_insertionSortCost l, two_mul_insertionSortCost_le l⟩

/-- The numbers below `n`, largest first, are a falling list. -/
theorem falling_reverse_range : ∀ n : Nat, Falling (List.range n).reverse
  | 0 => trivial
  | n + 1 => by
    simp only [List.range_succ, List.reverse_append, List.reverse_cons, List.reverse_nil,
      List.nil_append, List.singleton_append]
    refine ⟨fun y hy => ?_, falling_reverse_range n⟩
    have hlt : y < n := by simpa using hy
    omega

/-- The numbers below `n`, smallest first, are a sorted list. -/
theorem sorted_range (n : Nat) : Sorted (List.range n) :=
  List.pairwise_lt_range.imp Nat.le_of_lt

/-- Sorting `n - 1, …, 1, 0` costs `n (n - 1) / 2` comparisons. -/
theorem insertionSortCost_reverse_range (n : Nat) :
    2 * insertionSortCost (List.range n).reverse = n * (n - 1) := by
  have h := two_mul_insertionSortCost_of_falling (falling_reverse_range n)
  simpa using h

/-- Sorting `0, 1, …, n - 1` costs `n - 1` comparisons. -/
theorem insertionSortCost_range (n : Nat) : insertionSortCost (List.range n) = n - 1 := by
  have h := insertionSortCost_of_sorted (sorted_range n)
  simpa using h

end Maithic.Algo.Cost
