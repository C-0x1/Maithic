/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Sort.MinMax
import Maithic.Algo.Sort.MergeSort

/-!
# Selection sort and quicksort

Two more ways to sort, each proved to return a sorted permutation of its input. Since a sorted
permutation of a list is unique in a total order (`Sorted.eq_of_perm`), each is then the same
function as insertion sort: four programs, one specification, one result.

* `selectionSort` takes the minimum out of the list and puts it in front of the rest, sorted.
* `quickSort` splits the rest of the list around its first element and sorts both parts.
-/

namespace Maithic.Algo

universe u

section Selection

variable {α : Type u} [LE α] [DecidableLE α] [DecidableEq α]

/-- Selection sort: the minimum first, then the rest, sorted. -/
def selectionSort (l : List α) : List α :=
  if h : l = [] then [] else minimum l h :: selectionSort (l.erase (minimum l h))
termination_by l.length
decreasing_by
  rw [List.length_erase_of_mem (minimum_mem l h)]
  have := List.length_pos_iff.mpr h
  omega

/-- Selection sort returns a permutation of its input. -/
theorem selectionSort_perm (l : List α) : (selectionSort l).Perm l := by
  induction hn : l.length using Nat.strongRecOn generalizing l with
  | _ n ih =>
    unfold selectionSort
    split
    · next h => subst h; exact List.Perm.refl _
    · next h =>
      have hm := minimum_mem l h
      have hlen : (l.erase (minimum l h)).length < n := by
        rw [List.length_erase_of_mem hm, ← hn]
        have := List.length_pos_iff.mpr h
        omega
      exact ((ih _ hlen _ rfl).cons _).trans (List.perm_cons_erase hm).symm

/-- Selection sort returns a sorted list. -/
theorem sorted_selectionSort [TotalPreorder α] (l : List α) : Sorted (selectionSort l) := by
  induction hn : l.length using Nat.strongRecOn generalizing l with
  | _ n ih =>
    unfold selectionSort
    split
    · next h => exact List.Pairwise.nil
    · next h =>
      have hm := minimum_mem l h
      have hlen : (l.erase (minimum l h)).length < n := by
        rw [List.length_erase_of_mem hm, ← hn]
        have := List.length_pos_iff.mpr h
        omega
      refine sorted_cons.mpr ⟨fun b hb => ?_, ih _ hlen _ rfl⟩
      exact minimum_le l h (List.mem_of_mem_erase ((selectionSort_perm _).mem_iff.mp hb))

/-- Selection sort and insertion sort return the same list. -/
theorem selectionSort_eq_insertionSort [TotalOrder α] (l : List α) : selectionSort l = insertionSort l :=
  eq_insertionSort_of_spec selectionSort sorted_selectionSort selectionSort_perm l

end Selection

section Quick

variable {α : Type u} [LE α] [DecidableLE α]

/-- Quicksort: what is below the first element, sorted, then the element, then the rest, sorted. -/
def quickSort : List α → List α
  | [] => []
  | p :: rest => quickSort (rest.filter fun x => !decide (p ≤ x)) ++ p :: quickSort (rest.filter fun x => decide (p ≤ x))
termination_by l => l.length
decreasing_by
  all_goals simp only [List.length_cons, List.length_unattach]
  all_goals exact Nat.lt_succ_of_le (Nat.le_trans (List.length_filter_le _ _) (Nat.le_of_eq List.length_attach))

/-- Quicksort returns a permutation of its input. -/
theorem quickSort_perm (l : List α) : (quickSort l).Perm l := by
  induction hn : l.length using Nat.strongRecOn generalizing l with
  | _ n ih =>
    cases l with
    | nil => simp [quickSort]
    | cons p rest =>
      have hlt : ∀ q : α → Bool, (rest.filter q).length < n := fun q => by
        rw [← hn, List.length_cons]; exact Nat.lt_succ_of_le (List.length_filter_le _ _)
      rw [quickSort]
      have hl := ih _ (hlt fun x => !decide (p ≤ x)) _ rfl
      have hr := ih _ (hlt fun x => decide (p ≤ x)) _ rfl
      have hsplit : (rest.filter (fun x => !decide (p ≤ x)) ++ rest.filter (fun x => decide (p ≤ x))).Perm rest :=
        List.perm_append_comm.trans (List.filter_append_perm _ rest)
      exact ((hl.append (hr.cons p)).trans List.perm_middle).trans (hsplit.cons p)

/-- Quicksort returns a sorted list. -/
theorem sorted_quickSort [TotalPreorder α] (l : List α) : Sorted (quickSort l) := by
  induction hn : l.length using Nat.strongRecOn generalizing l with
  | _ n ih =>
    cases l with
    | nil => simp [quickSort, Sorted]
    | cons p rest =>
      have hlt : ∀ q : α → Bool, (rest.filter q).length < n := fun q => by
        rw [← hn, List.length_cons]; exact Nat.lt_succ_of_le (List.length_filter_le _ _)
      rw [quickSort]
      have below : ∀ a ∈ quickSort (rest.filter fun x => !decide (p ≤ x)), a ≤ p := fun a ha => by
        have := (List.mem_filter.mp ((quickSort_perm _).mem_iff.mp ha)).2
        cases TotalPreorder.le_total a p with
        | inl h => exact h
        | inr h => simp [h] at this
      have above : ∀ b ∈ quickSort (rest.filter fun x => decide (p ≤ x)), p ≤ b := fun b hb => by
        simpa using (List.mem_filter.mp ((quickSort_perm _).mem_iff.mp hb)).2
      refine sorted_append.mpr ⟨ih _ (hlt _) _ rfl, sorted_cons.mpr ⟨above, ih _ (hlt _) _ rfl⟩, fun a ha b hb => ?_⟩
      cases List.mem_cons.mp hb with
      | inl h => exact h ▸ below a ha
      | inr h => exact TotalPreorder.le_trans _ _ _ (below a ha) (above b h)

/-- Quicksort and insertion sort return the same list. -/
theorem quickSort_eq_insertionSort [TotalOrder α] (l : List α) : quickSort l = insertionSort l :=
  eq_insertionSort_of_spec quickSort sorted_quickSort quickSort_perm l

end Quick

/-- All four sorting programs of this library are one function. -/
theorem sorts_agree {α : Type u} [LE α] [DecidableLE α] [DecidableEq α] [TotalOrder α] (l : List α) :
    selectionSort l = insertionSort l ∧ quickSort l = insertionSort l ∧ mergeSort l = insertionSort l :=
  ⟨selectionSort_eq_insertionSort l, quickSort_eq_insertionSort l, mergeSort_eq_insertionSort l⟩

end Maithic.Algo
