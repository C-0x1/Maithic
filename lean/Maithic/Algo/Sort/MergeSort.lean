/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Sort.Merge
import Maithic.Algo.Spec.TotalOrder
import Maithic.Algo.Sort.Insertion

/-!
# Merge sort, and why two sorts cannot disagree

Merge sort is the verified merge applied to two halves that were sorted the same way. Its
specification is the one insertion sort was proved against: the result is sorted, and it is a
permutation of the input.

In a total order that specification has exactly one solution. Two sorted lists that are
permutations of each other are the same list (`Sorted.eq_of_perm`), so any two functions that meet
the specification are the same function. `mergeSort_eq_insertionSort` is the instance for the two
sorts of this library: the fast one may stand in for the simple one anywhere, and a property
proved of one holds of the other.
-/

namespace Maithic.Algo

universe u

variable {α : Type u}

/-- Deals the elements of a list out in turn into two lists. -/
def halve : List α → List α × List α
  | [] => ([], [])
  | [x] => ([x], [])
  | x :: y :: rest => (x :: (halve rest).1, y :: (halve rest).2)

/-- Nothing is lost and nothing is added: the two halves are as long as the list together. -/
theorem length_halve : ∀ l : List α, (halve l).1.length + (halve l).2.length = l.length
  | [] => rfl
  | [_] => rfl
  | _ :: _ :: rest => by
    have ih := length_halve rest
    simp only [halve, List.length_cons]
    omega

/-- The two halves hold what the list holds. -/
theorem halve_perm : ∀ l : List α, ((halve l).1 ++ (halve l).2).Perm l
  | [] => List.Perm.refl _
  | [_] => List.Perm.refl _
  | x :: y :: rest => by
    have ih := halve_perm rest
    simp only [halve, List.cons_append]
    exact List.Perm.cons x ((List.perm_middle (a := y)).trans (List.Perm.cons y ih))

variable [LE α] [DecidableLE α]

/-- Merge sort: a list of two or more elements is dealt into two halves, each half is sorted, and
the two sorted halves are merged. -/
def mergeSort : List α → List α
  | [] => []
  | [x] => [x]
  | x :: y :: rest => merge (mergeSort (x :: (halve rest).1)) (mergeSort (y :: (halve rest).2))
termination_by l => l.length
decreasing_by
  all_goals simp_wf
  all_goals have := length_halve rest
  all_goals omega

/-- Merge sort returns a permutation of its input. -/
theorem mergeSort_perm (l : List α) : (mergeSort l).Perm l := by
  fun_induction mergeSort l with
  | case1 => exact List.Perm.refl _
  | case2 x => exact List.Perm.refl _
  | case3 x y rest ih₁ ih₂ =>
    refine (merge_perm _ _).trans ((List.Perm.append ih₁ ih₂).trans ?_)
    simp only [List.cons_append]
    exact List.Perm.cons x ((List.perm_middle (a := y)).trans (List.Perm.cons y (halve_perm rest)))

/-- Merge sort returns a sorted list. -/
theorem sorted_mergeSort [TotalPreorder α] (l : List α) : Sorted (mergeSort l) := by
  fun_induction mergeSort l with
  | case1 => exact sorted_nil
  | case2 x => exact sorted_singleton x
  | case3 x y rest ih₁ ih₂ => exact sorted_merge ih₁ ih₂

omit [DecidableLE α] in
/-- In a total order, two sorted lists that are permutations of each other are the same list. -/
theorem Sorted.eq_of_perm [TotalOrder α] : ∀ {l₁ l₂ : List α}, Sorted l₁ → Sorted l₂ → l₁.Perm l₂ → l₁ = l₂
  | [], l₂, _, _, hp => (List.Perm.nil_eq hp)
  | a :: t₁, [], _, _, hp => absurd hp.length_eq (by simp)
  | a :: t₁, b :: t₂, h₁, h₂, hp => by
    have hab : a ≤ b := by
      have hb : b ∈ a :: t₁ := hp.mem_iff.mpr (List.mem_cons_self ..)
      rcases List.mem_cons.mp hb with rfl | hb
      · exact TotalPreorder.le_refl _
      · exact h₁.head_le hb
    have hba : b ≤ a := by
      have ha : a ∈ b :: t₂ := hp.mem_iff.mp (List.mem_cons_self ..)
      rcases List.mem_cons.mp ha with rfl | ha
      · exact TotalPreorder.le_refl _
      · exact h₂.head_le ha
    have heq : a = b := TotalOrder.le_antisymm _ _ hab hba
    subst heq
    rw [Sorted.eq_of_perm h₁.tail h₂.tail hp.cons_inv]

/-- In a total order, a function that returns a sorted permutation of its input is insertion
sort: the specification of sorting has one solution. -/
theorem eq_insertionSort_of_spec [TotalOrder α] (sort : List α → List α)
    (sorted : ∀ l, Sorted (sort l)) (perm : ∀ l, (sort l).Perm l) (l : List α) :
    sort l = insertionSort l :=
  (sorted l).eq_of_perm (sorted_insertionSort l) ((perm l).trans (insertionSort_perm l).symm)

/-- Merge sort and insertion sort return the same list. -/
theorem mergeSort_eq_insertionSort [TotalOrder α] (l : List α) : mergeSort l = insertionSort l :=
  eq_insertionSort_of_spec mergeSort sorted_mergeSort mergeSort_perm l

/-- Sorting a list that is sorted changes nothing. -/
theorem insertionSort_eq_self [TotalOrder α] {l : List α} (h : Sorted l) : insertionSort l = l :=
  (sorted_insertionSort l).eq_of_perm h (insertionSort_perm l)

/-- Sorting twice is sorting once. -/
theorem insertionSort_idempotent [TotalOrder α] (l : List α) :
    insertionSort (insertionSort l) = insertionSort l :=
  insertionSort_eq_self (sorted_insertionSort l)

/-- Merge sort leaves a sorted list as it is. -/
theorem mergeSort_eq_self [TotalOrder α] {l : List α} (h : Sorted l) : mergeSort l = l :=
  (sorted_mergeSort l).eq_of_perm h (mergeSort_perm l)

/-- Two lists that are permutations of each other are sorted to the same list. -/
theorem mergeSort_eq_of_perm [TotalOrder α] {l₁ l₂ : List α} (h : l₁.Perm l₂) : mergeSort l₁ = mergeSort l₂ :=
  (sorted_mergeSort l₁).eq_of_perm (sorted_mergeSort l₂) ((mergeSort_perm l₁).trans (h.trans (mergeSort_perm l₂).symm))

end Maithic.Algo
