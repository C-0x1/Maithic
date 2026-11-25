/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Sort.Insertion
import Maithic.Algo.Sort.MergeSort
import Maithic.Algo.Sort.Linear
import Maithic.Algo.Sort.Binary

/-!
# How much work: counted, and bounded by proof

The theorems elsewhere in this library say what an algorithm returns. These say how much it does
to get there. For each of four algorithms there is a function that follows the algorithm step by
step and counts where the algorithm compares, and a theorem that bounds the count by the size of
the input:

* `linearSearchCost_le`: linear search compares at most `n` times;
* `binarySearchCost_le`: binary search looks at no more than `log₂ n + 1` elements;
* `two_mul_insertionSortCost_le`: insertion sort compares at most `n (n - 1) / 2` times;
* `mergeSortCost_le`: merge sort compares at most `n (log₂ n + 1)` times.

A counting function has the shape of the algorithm it counts, branch for branch, so that what it
counts is what the algorithm does; it is a model of the cost, written by hand, and that match is
the one thing here that is read and not proved. The executable `maithic-algo` answers a request
`cost merge_sort 3 1 2` with the count, so the growth of the counts can be measured on inputs of
any size and laid beside the bounds.
-/

namespace Maithic.Algo.Cost

open Maithic.Algo

universe u

section Search

variable {α : Type u}

/-- The comparisons `linearSearch x l` makes: one for each element, up to and including the first
that equals `x`. -/
def linearSearchCost [BEq α] (x : α) : List α → Nat
  | [] => 0
  | y :: ys => if y == x then 1 else 1 + linearSearchCost x ys

/-- Linear search looks at no element twice. -/
theorem linearSearchCost_le [BEq α] (x : α) (l : List α) : linearSearchCost x l ≤ l.length := by
  induction l with
  | nil => exact Nat.le_refl 0
  | cons y ys ih =>
    simp only [linearSearchCost, List.length_cons]
    split <;> omega

/-- A search that finds nothing has looked at everything: the bound is reached. -/
theorem linearSearchCost_of_none [BEq α] {x : α} {l : List α} (h : linearSearch x l = none) :
    linearSearchCost x l = l.length := by
  induction l with
  | nil => rfl
  | cons y ys ih =>
    by_cases hy : (y == x) = true
    · simp [linearSearch, hy] at h
    · have hrest : linearSearch x ys = none := by simpa [linearSearch, hy] using h
      simp [linearSearchCost, hy, ih hrest, Nat.add_comm]

variable [LE α] [DecidableLE α]

/-- The elements `binarySearch.go` looks at between `lo` and `hi`: one in every round. -/
def binarySearchCost.go (a : Array α) (x : α) (lo hi : Nat) (h : hi ≤ a.size) : Nat :=
  if hlt : lo < hi then
    let mid := lo + (hi - lo) / 2
    have hmid : mid < a.size := by omega
    if a[mid] ≤ x then
      if x ≤ a[mid] then 1
      else 1 + go a x (mid + 1) hi h
    else 1 + go a x lo mid (by omega)
  else 0
termination_by hi - lo

/-- The elements of `a` that `binarySearch a x` looks at. -/
def binarySearchCost (a : Array α) (x : α) : Nat := binarySearchCost.go a x 0 a.size (Nat.le_refl _)

/-- An interval of fewer than `2 ^ k` positions is searched in at most `k` rounds: every round
halves what is left. -/
theorem binarySearchCost.go_le (a : Array α) (x : α) :
    ∀ (k lo hi : Nat) (h : hi ≤ a.size), hi - lo < 2 ^ k → binarySearchCost.go a x lo hi h ≤ k := by
  intro k
  induction k with
  | zero =>
    intro lo hi h hk
    rw [Nat.pow_zero] at hk
    rw [binarySearchCost.go]
    split
    · next hlt => exact absurd hlt (by omega)
    · exact Nat.le_refl 0
  | succ k ih =>
    intro lo hi h hk
    rw [Nat.pow_succ] at hk
    rw [binarySearchCost.go]
    split
    · simp only []
      split
      · split
        · omega
        · have := ih (lo + (hi - lo) / 2 + 1) hi h (by omega)
          omega
      · have := ih lo (lo + (hi - lo) / 2) (by omega) (by omega)
        omega
    · omega

/-- Binary search looks at no more than `log₂ n + 1` of the `n` elements. -/
theorem binarySearchCost_le (a : Array α) (x : α) : binarySearchCost a x ≤ Nat.log2 a.size + 1 :=
  binarySearchCost.go_le a x _ 0 a.size (Nat.le_refl _) Nat.lt_log2_self

end Search

section Halves

variable {α : Type u}

/-- Dealing a list out gives the first half the extra element when there is one. -/
theorem length_halve_fst : ∀ l : List α, (halve l).1.length = (l.length + 1) / 2
  | [] => by simp [halve]
  | [_] => by simp [halve]
  | _ :: _ :: rest => by
    have ih := length_halve_fst rest
    simp only [halve, List.length_cons]
    omega

/-- The second half has half of the elements, rounded down. -/
theorem length_halve_snd : ∀ l : List α, (halve l).2.length = l.length / 2
  | [] => by simp [halve]
  | [_] => by simp [halve]
  | _ :: _ :: rest => by
    have ih := length_halve_snd rest
    simp only [halve, List.length_cons]
    omega

end Halves

section Sorting

variable {α : Type u} [LE α] [DecidableLE α]

/-- The comparisons `insertSorted x l` makes: one for each element it passes, and one for the
element it stops in front of. -/
def insertCost (x : α) : List α → Nat
  | [] => 0
  | y :: ys => if x ≤ y then 1 else 1 + insertCost x ys

/-- Inserting compares with each element at most once. -/
theorem insertCost_le (x : α) (l : List α) : insertCost x l ≤ l.length := by
  induction l with
  | nil => exact Nat.le_refl 0
  | cons y ys ih =>
    simp only [insertCost, List.length_cons]
    split <;> omega

/-- The bound is reached: an element that is not below any element of the list is compared with
every one of them on its way to the end. -/
theorem insertCost_eq_length {x : α} {l : List α} (h : ∀ y ∈ l, ¬ x ≤ y) : insertCost x l = l.length := by
  induction l with
  | nil => rfl
  | cons y ys ih =>
    have hy : ¬ x ≤ y := h y (List.mem_cons_self ..)
    have hrest := ih (fun z hz => h z (List.mem_cons_of_mem _ hz))
    simp only [insertCost, hy, ↓reduceIte, hrest, List.length_cons]
    omega

/-- The comparisons `insertionSort l` makes. -/
def insertionSortCost : List α → Nat
  | [] => 0
  | x :: xs => insertionSortCost xs + insertCost x (insertionSort xs)

/-- One more element costs at most as many comparisons as there were elements. -/
theorem insertionSortCost_cons_le (x : α) (xs : List α) :
    insertionSortCost (x :: xs) ≤ insertionSortCost xs + xs.length := by
  have h := insertCost_le x (insertionSort xs)
  rw [length_insertionSort] at h
  simp only [insertionSortCost]
  omega

private theorem triangle_step (n : Nat) : n * (n - 1) + 2 * n = (n + 1) * n := by
  cases n with
  | zero => rfl
  | succ m =>
    simp only [Nat.add_sub_cancel]
    grind

/-- Insertion sort makes at most `n (n - 1) / 2` comparisons, written without the division. -/
theorem two_mul_insertionSortCost_le (l : List α) : 2 * insertionSortCost l ≤ l.length * (l.length - 1) := by
  induction l with
  | nil => exact Nat.le_refl 0
  | cons x xs ih =>
    have h := insertionSortCost_cons_le x xs
    have key := triangle_step xs.length
    simp only [List.length_cons, Nat.add_sub_cancel]
    omega

/-- The same bound in the form that is quoted: quadratic in the length. -/
theorem insertionSortCost_le_sq (l : List α) : insertionSortCost l ≤ l.length * l.length := by
  have h := two_mul_insertionSortCost_le l
  have : l.length * (l.length - 1) ≤ l.length * l.length := Nat.mul_le_mul_left _ (Nat.sub_le _ _)
  omega

/-- The comparisons `merge xs ys` makes: one for every element it moves while both lists still
have one. -/
def mergeCost : List α → List α → Nat
  | [], _ => 0
  | _ :: _, [] => 0
  | x :: xs, y :: ys => if x ≤ y then 1 + mergeCost xs (y :: ys) else 1 + mergeCost (x :: xs) ys
termination_by xs ys => xs.length + ys.length

/-- Merging compares at most once for each element. -/
theorem mergeCost_le (xs ys : List α) : mergeCost xs ys ≤ xs.length + ys.length := by
  fun_induction mergeCost xs ys with
  | case1 => exact Nat.zero_le _
  | case2 => exact Nat.zero_le _
  | case3 x xs y ys h ih =>
    simp only [List.length_cons] at ih ⊢
    omega
  | case4 x xs y ys h ih =>
    simp only [List.length_cons] at ih ⊢
    omega

/-- The comparisons `mergeSort l` makes: those of sorting the two halves, and those of merging
them. -/
def mergeSortCost : List α → Nat
  | [] => 0
  | [_] => 0
  | x :: y :: rest =>
    mergeSortCost (x :: (halve rest).1) + mergeSortCost (y :: (halve rest).2)
      + mergeCost (mergeSort (x :: (halve rest).1)) (mergeSort (y :: (halve rest).2))
termination_by l => l.length
decreasing_by
  all_goals simp_wf
  all_goals have := length_halve rest
  all_goals omega

/-- Merge sort returns as many elements as it was given. -/
theorem length_mergeSort (l : List α) : (mergeSort l).length = l.length :=
  (mergeSort_perm l).length_eq

/-- A list of at most `2 ^ k` elements is sorted with at most `n k` comparisons: there are `k`
levels of merging, and each level compares at most once for each element. -/
theorem mergeSortCost_le_of_le_pow :
    ∀ (k : Nat) (l : List α), l.length ≤ 2 ^ k → mergeSortCost l ≤ l.length * k := by
  intro k
  induction k with
  | zero =>
    intro l h
    match l, h with
    | [], _ => simp [mergeSortCost]
    | [_], _ => simp [mergeSortCost]
    | _ :: _ :: _, h => simp at h
  | succ k ih =>
    intro l h
    match l, h with
    | [], _ => simp [mergeSortCost]
    | [_], _ => simp [mergeSortCost]
    | x :: y :: rest, h =>
      have hf := length_halve_fst rest
      have hs := length_halve_snd rest
      have hsum := length_halve rest
      simp only [List.length_cons, Nat.pow_succ] at h
      have h1 := ih (x :: (halve rest).1) (by simp only [List.length_cons]; omega)
      have h2 := ih (y :: (halve rest).2) (by simp only [List.length_cons]; omega)
      have h3 := mergeCost_le (mergeSort (x :: (halve rest).1)) (mergeSort (y :: (halve rest).2))
      rw [length_mergeSort, length_mergeSort] at h3
      simp only [List.length_cons] at h1 h2 h3
      have expand : (rest.length + 1 + 1) * (k + 1)
          = ((halve rest).1.length + 1) * k + ((halve rest).2.length + 1) * k + (rest.length + 2) := by
        rw [← hsum]
        grind
      rw [mergeSortCost]
      simp only [List.length_cons]
      omega

/-- Merge sort makes at most `n (log₂ n + 1)` comparisons. -/
theorem mergeSortCost_le (l : List α) : mergeSortCost l ≤ l.length * (Nat.log2 l.length + 1) :=
  mergeSortCost_le_of_le_pow _ l (Nat.le_of_lt Nat.lt_log2_self)

/-- For a list whose length is a power of two this is the bound of the textbooks, `n log₂ n`. -/
theorem mergeSortCost_le_of_eq_pow (k : Nat) (l : List α) (h : l.length = 2 ^ k) :
    mergeSortCost l ≤ l.length * Nat.log2 l.length := by
  have hk : Nat.log2 l.length = k := by rw [h, Nat.log2_two_pow]
  rw [hk]
  exact mergeSortCost_le_of_le_pow k l (Nat.le_of_eq h)

end Sorting

end Maithic.Algo.Cost
