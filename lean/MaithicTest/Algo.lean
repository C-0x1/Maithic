/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.Perm
import Maithic.Algo.Sort.Insertion
import Maithic.Algo.Sort.Linear
import Maithic.Algo.Sort.LinearSpec
import Maithic.Algo.Sort.Binary
import Maithic.Algo.Sort.Merge
import Maithic.Algo.Sort.MergeSort
import Maithic.Algo.Sort.MinMax
import Maithic.Algo.Sort.Dedup
import Maithic.Algo.Graph.Fib
import Maithic.Algo.Graph.PrefixSum
import Maithic.Algo.Runner
import Maithic.Algo.Cost.Extremes
import Maithic.Algo.Lab.RunLengths

/-!
# Tests of the verified algorithms

The theorems are proved in the library; these checks run the same functions on concrete inputs,
so that a change of definition that keeps the proofs but breaks the behaviour is noticed.
-/

namespace MaithicTest.Algo

open Maithic.Algo

-- cap: P05-01-01
#guard decide (Sorted [1, 2, 2, 5])
-- cap: P05-01-01
#guard !decide (Sorted [1, 3, 2])
-- cap: P05-01-01
#guard decide (Sorted ([] : List Nat))
-- cap: P05-01-01
#guard decide (SortedArray #[(-3 : Int), 0, 7])
-- cap: P05-01-01
example : Sorted [1, 2, 3] := by decide
-- cap: P05-01-01
example (a b : Nat) (l : List Nat) (h : Sorted (a :: b :: l)) : a ≤ b :=
  h.head_le (List.mem_cons_self ..)

-- cap: P05-01-01
-- The check that compares neighbours agrees with the definition, which compares all pairs.
#guard [[1, 2, 2, 5], [1, 3, 2], [], [4], [2, 1], [1, 2, 3, 0]].all fun l => isSorted l == decide (Sorted l)
-- cap: P05-01-01
#guard isSorted (List.range 2000)
-- cap: P05-01-01
example (l : List Int) (h : isSorted l = true) : Sorted l := isSorted_iff.mp h

-- cap: P05-02-03
#guard insertionSort [3, 1, 2] == [1, 2, 3]
-- cap: P05-02-03
#guard insertionSort ([] : List Nat) == []
-- cap: P05-02-03
#guard insertionSort [5, 5, 1, 5] == [1, 5, 5, 5]
-- cap: P05-02-03
#guard insertionSort [(2 : Int), -7, 0, -7] == [-7, -7, 0, 2]
-- cap: P05-02-03
#guard decide (Sorted (insertionSort [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]))
-- cap: P05-02-03
example (l : List Int) : Sorted (insertionSort l) ∧ (insertionSort l).Perm l :=
  ⟨sorted_insertionSort l, insertionSort_perm l⟩

-- cap: P05-02-01
#guard linearSearch 7 [4, 7, 9, 7] == some 1
-- cap: P05-02-01
#guard linearSearch 3 [4, 7, 9] == none
-- cap: P05-02-01
#guard linearSearch 'c' "abc".toList == some 2
-- cap: P05-02-01
example (x : Nat) (l : List Nat) (h : linearSearch x l = none) : x ∉ l := linearSearch_none h

/-! ## Rearrangements -/

-- cap: P05-01-02
#guard decide (SameCounts [3, 1, 2, 1] [1, 1, 2, 3])
-- cap: P05-01-02
#guard !decide (SameCounts [1, 1, 2] [1, 2, 2])
-- cap: P05-01-02
#guard countsAgree "listen".toList "silent".toList
-- cap: P05-01-02
#guard !countsAgree [1, 2] [1, 2, 2]
-- cap: P05-01-02
-- The check and the relation never disagree.
#guard [([1, 2, 2], [2, 1, 2]), ([1], [2]), ([], []), ([1, 1], [1])].all fun (a, b) =>
  countsAgree a b == decide (SameCounts a b)
-- cap: P05-01-02
-- Counting is the permutation relation of the core library, in both directions.
example (l₁ l₂ : List Nat) (h : l₁.Perm l₂) : SameCounts l₁ l₂ := sameCounts_iff_perm.mpr h
-- cap: P05-01-02
example (l₁ l₂ : List Nat) (h : SameCounts l₁ l₂) : l₁.length = l₂.length := h.length_eq
-- cap: P05-01-02
-- Insertion sort changes the order of a list and nothing else.
example (l : List Int) : SameCounts (insertionSort l) l := sameCounts_iff_perm.mpr (insertionSort_perm l)

/-! ## What a search may answer -/

-- cap: P05-01-03
#guard decide (SearchSpec [4, 7, 9] 7 (some 1))
-- cap: P05-01-03
#guard !decide (SearchSpec [4, 7, 9] 7 (some 2))
-- cap: P05-01-03
-- A position outside the list is never a right answer.
#guard !decide (SearchSpec [4, 7, 9] 7 (some 3))
-- cap: P05-01-03
#guard decide (SearchSpec [4, 7, 9] 5 none)
-- cap: P05-01-03
-- `none` is a wrong answer when the target is there.
#guard !decide (SearchSpec [4, 7, 9] 9 none)
-- cap: P05-01-03
example (l : List Nat) (x i : Nat) (h : SearchSpec l x (some i)) : i < l.length ∧ x ∈ l := ⟨h.lt_length, h.mem⟩
-- cap: P05-01-03
-- Linear search meets the specification of a first position, and nothing else does.
example (l : List Nat) (x : Nat) (r : Option Nat) (h : FirstSearchSpec l x r) : r = linearSearch x l :=
  h.unique (linearSearch_spec x l)

/-! ## Binary search -/

-- cap: P05-02-02
#guard binarySearch #[1, 3, 5, 7, 9, 11] 7 == some 3
-- cap: P05-02-02
#guard binarySearch #[1, 3, 5, 7, 9, 11] 1 == some 0
-- cap: P05-02-02
#guard binarySearch #[1, 3, 5, 7, 9, 11] 11 == some 5
-- cap: P05-02-02
#guard binarySearch #[1, 3, 5, 7, 9, 11] 4 == none
-- cap: P05-02-02
#guard binarySearch #[1, 3, 5, 7, 9, 11] 0 == none
-- cap: P05-02-02
#guard binarySearch #[1, 3, 5, 7, 9, 11] 12 == none
-- cap: P05-02-02
#guard binarySearch (#[] : Array Nat) 3 == none
-- cap: P05-02-02
#guard binarySearch #[(-4 : Int), -4, 0, 2] (-4) |>.isSome
-- cap: P05-02-02
-- Every element of a sorted array is found, and nothing else is.
#guard (List.range 40).all fun x =>
  (binarySearch (Array.range 20 |>.map (· * 2)) x).isSome == (x % 2 == 0 && x < 40)
-- cap: P05-02-02
-- On any array, sorted or not, an answer is a position that holds the target.
example (a : Array Nat) (x i : Nat) (h : binarySearch a x = some i) : ∃ hi : i < a.size, a[i] = x := by
  obtain ⟨hi, hle, hge⟩ := binarySearch_some h
  exact ⟨hi, Nat.le_antisymm hle hge⟩
-- cap: P05-02-02
-- On a sorted array the answer meets the specification of a search.
example (a : Array Int) (x : Int) (h : SortedArray a) : SearchSpec a.toList x (binarySearch a x) :=
  binarySearch_spec x h

/-! ## Merging -/

-- cap: P05-02-04
#guard merge [1, 4, 6] [2, 3, 7, 8] == [1, 2, 3, 4, 6, 7, 8]
-- cap: P05-02-04
#guard merge ([] : List Nat) [2, 3] == [2, 3]
-- cap: P05-02-04
#guard merge [2, 3] ([] : List Nat) == [2, 3]
/-- A value with a tag that the order does not look at, to see which of two equal elements a
merge puts first. -/
structure Tagged where
  key : Nat
  tag : Char

instance : LE Tagged := ⟨fun a b => a.key ≤ b.key⟩

instance : DecidableLE Tagged := fun a b => inferInstanceAs (Decidable (a.key ≤ b.key))

-- cap: P05-02-04
-- Of two equal elements the one of the first list comes first.
#guard (merge [⟨1, 'a'⟩, ⟨2, 'a'⟩] [⟨1, 'b'⟩, ⟨2, 'b'⟩] : List Tagged).map (·.tag) == ['a', 'b', 'a', 'b']
-- cap: P05-02-04
#guard decide (Sorted (merge [1, 3, 5, 7] [0, 2, 4, 6, 8]))
-- cap: P05-02-04
example (xs ys : List Int) (hx : Sorted xs) (hy : Sorted ys) :
    Sorted (merge xs ys) ∧ (merge xs ys).Perm (xs ++ ys) :=
  ⟨sorted_merge hx hy, merge_perm xs ys⟩
-- cap: P05-02-04
example (xs ys : List Nat) : (merge xs ys).length = xs.length + ys.length := length_merge xs ys

/-! ## Merge sort -/

-- cap: P05-02-04
#guard mergeSort [5, 2, 9, 1, 5, 6] == [1, 2, 5, 5, 6, 9]
-- cap: P05-02-04
#guard mergeSort ([] : List Nat) == [] && mergeSort [7] == [7]
-- cap: P05-02-04
#guard mergeSort [(3 : Int), -1, 0, -1] == [-1, -1, 0, 3]
-- cap: P05-02-04
-- Dealing a list out gives two halves whose lengths differ by one at most.
#guard halve [1, 2, 3, 4, 5] == ([1, 3, 5], [2, 4])
-- cap: P05-02-04
-- Two hundred elements, against the simple sort.
#guard mergeSort ((List.range 200).map fun i => (i * 7919) % 211) == insertionSort ((List.range 200).map fun i => (i * 7919) % 211)
-- cap: P05-02-04
example (l : List Int) : Sorted (mergeSort l) ∧ (mergeSort l).Perm l := ⟨sorted_mergeSort l, mergeSort_perm l⟩
-- cap: P05-01-02
-- A sorted list has no other sorted rearrangement, so the two sorts are one function.
example (l : List Nat) : mergeSort l = insertionSort l := mergeSort_eq_insertionSort l
-- cap: P05-01-02
example (l₁ l₂ : List Int) (h₁ : Sorted l₁) (h₂ : Sorted l₂) (h : SameCounts l₁ l₂) : l₁ = l₂ :=
  h₁.eq_of_perm h₂ (sameCounts_iff_perm.mp h)

-- cap: P05-02-03
-- Sorting a sorted list changes nothing, so sorting twice is sorting once.
example (l : List Nat) : insertionSort (insertionSort l) = insertionSort l := insertionSort_idempotent l
-- cap: P05-02-04
-- The order of the input does not matter to the result.
example (l₁ l₂ : List Nat) (h : l₁.Perm l₂) : mergeSort l₁ = mergeSort l₂ := mergeSort_eq_of_perm h

/-! ## Minimum and maximum -/

-- cap: P05-02-05
#guard minimum [3, 1, 2] (by simp) == 1
-- cap: P05-02-05
#guard maximum [3, 1, 2] (by simp) == 3
-- cap: P05-02-05
#guard minimum [(5 : Int)] (by simp) == 5 && maximum [(5 : Int)] (by simp) == 5
-- cap: P05-02-05
#guard arrayMinimum #[(4 : Int), -2, 9] (by decide) == -2 && arrayMaximum #[(4 : Int), -2, 9] (by decide) == 9
-- cap: P05-02-05
example (l : List Nat) (h : l ≠ []) : minimum l h ∈ l ∧ ∀ a ∈ l, minimum l h ≤ a :=
  ⟨minimum_mem l h, fun _ ha => minimum_le l h ha⟩
-- cap: P05-02-05
example (l : List Int) (h : l ≠ []) : maximum l h ∈ l ∧ ∀ a ∈ l, a ≤ maximum l h :=
  ⟨maximum_mem l h, fun _ ha => le_maximum l h ha⟩
-- cap: P05-02-05
-- The first element of a sorted list is its minimum.
example (x : Nat) (l : List Nat) (h : Sorted (x :: l)) : x = minimum (x :: l) (by simp) := h.head_eq_minimum

/-! ## Removing duplicates -/

-- cap: P05-02-06
#guard dedupSorted [1, 1, 2, 3, 3, 3, 7] == [1, 2, 3, 7]
-- cap: P05-02-06
#guard dedupSorted ([] : List Nat) == []
-- cap: P05-02-06
#guard dedupSorted [4, 4, 4] == [4]
-- cap: P05-02-06
-- Sorting first makes it a removal of every duplicate.
#guard dedupSorted (insertionSort [3, 1, 3, 2, 1]) == [1, 2, 3]
-- cap: P05-02-06
-- On a list that is not sorted, equal elements that are apart both stay: the theorem needs its hypothesis.
#guard dedupSorted [1, 2, 1] == [1, 2, 1]
-- cap: P05-02-06
example (l : List Nat) (h : Sorted l) : (dedupSorted l).Nodup ∧ Sorted (dedupSorted l) :=
  ⟨nodup_dedupSorted h, sorted_dedupSorted h⟩
-- cap: P05-02-06
example (l : List Int) (a : Int) : a ∈ dedupSorted l ↔ a ∈ l := mem_dedupSorted

/-! ## Fibonacci numbers -/

-- cap: P05-03-01
#guard (List.range 12).map fib == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]
-- cap: P05-03-01
#guard (List.range 25).all fun n => fibMemo n == fib n && fibFast n == fib n
-- cap: P05-03-01
-- Far beyond what the recurrence can compute in the time of a build.
#guard fibMemo 90 == 2880067194370816120
-- cap: P05-03-01
#guard fibFast 90 == 2880067194370816120
-- cap: P05-03-01
#guard fibTable 6 == #[0, 1, 1, 2, 3, 5, 8]
-- cap: P05-03-01
example (n : Nat) : fibMemo n = fib n ∧ fibFast n = fib n := ⟨fibMemo_eq_fib n, fibFast_eq_fib n⟩
-- cap: P05-03-01
example (n : Nat) : (fibTable n).size = n + 1 := size_fibTable n

/-! ## Prefix sums -/

-- cap: P05-03-02
#guard prefixSums [3, 1, 4, 1, 5] == [0, 3, 4, 8, 9, 14]
-- cap: P05-03-02
#guard prefixSums [] == [0]
-- cap: P05-03-02
#guard rangeSum (prefixSums [3, 1, 4, 1, 5]) 1 4 == 6
-- cap: P05-03-02
#guard rangeSum (prefixSums [3, -1, 4, -1, 5]) 0 5 == 10
-- cap: P05-03-02
-- An empty stretch sums to zero.
#guard rangeSum (prefixSums [3, 1, 4]) 2 2 == 0
-- cap: P05-03-02
#guard rangeSumArray (prefixSumArray #[2, 7, 1, 8, 2, 8]) 2 5 == 11
-- cap: P05-03-02
-- Every stretch of a list, against the sum computed the long way.
#guard (List.range 6).all fun i => (List.range 6).all fun j =>
  i > j || rangeSum (prefixSums [3, -1, 4, -1, 5]) i j == (([3, -1, 4, -1, 5].drop i).take (j - i)).sum
-- cap: P05-03-02
example (l : List Int) (i j : Nat) (hij : i ≤ j) (hj : j ≤ l.length) :
    rangeSum (prefixSums l) i j = ((l.drop i).take (j - i)).sum :=
  rangeSum_prefixSums l hij hj
-- cap: P05-03-02
example (l : List Int) : (prefixSums l).length = l.length + 1 := length_prefixSums l

/-! ## Selection sort and quicksort -/

#guard selectionSort [3, 1, 2, 1] == [1, 1, 2, 3]
#guard selectionSort ([] : List Nat) == []
#guard quickSort [5, -1, 3, 3, 0] == ([-1, 0, 3, 3, 5] : List Int)
#guard quickSort [7] == [7]
-- Four programs, one function: each returns the sorted permutation, and there is only one.
example (l : List Int) : selectionSort l = insertionSort l ∧ quickSort l = insertionSort l ∧ mergeSort l = insertionSort l :=
  sorts_agree l
example (l : List Nat) : Sorted (quickSort l) ∧ (quickSort l).Perm l := ⟨sorted_quickSort l, quickSort_perm l⟩
example (l : List Nat) : Sorted (selectionSort l) ∧ (selectionSort l).Perm l := ⟨sorted_selectionSort l, selectionSort_perm l⟩
#guard (List.range 40).reverse |> quickSort |> (· == List.range 40)
#guard (List.range 40).reverse |> selectionSort |> (· == List.range 40)

/-! ## Searching for a pattern -/

#guard findSub [2, 3] [1, 2, 3, 2, 3] == some 1
#guard findSub [3, 2, 3] [1, 2, 3, 2, 3] == some 2
#guard findSub [3, 3] [1, 2, 3, 2, 3] == none
#guard findSub ([] : List Nat) [1, 2] == some 0
#guard findSub [1] ([] : List Nat) == none
#guard findInString "sort" "insertion sort" == some 10
#guard findInString "merge" "insertion sort" == none
#guard containsSub "ab".toList "cabbage".toList
-- What is returned is an occurrence, and the first one.
example (p t : List Nat) (i : Nat) (h : findSub p t = some i) : OccursAt p t i ∧ ∀ j, j < i → ¬ OccursAt p t j :=
  ⟨findSub_some h, findSub_first h⟩
example (p t : List Nat) (h : findSub p t = none) (i : Nat) : ¬ OccursAt p t i := findSub_none h i

/-! ## Arithmetic -/

#guard euclid 12 18 == 6
#guard euclid 17 5 == 1
#guard euclid 0 9 == 9 && euclid 9 0 == 9 && euclid 0 0 == 0
#guard areCoprime 35 64 && !areCoprime 35 49
#guard reduce 12 18 == (2, 3)
example (a b : Nat) : euclid a b = Nat.gcd a b := euclid_eq_gcd a b
example (a b d : Nat) (ha : d ∣ a) (hb : d ∣ b) : d ∣ euclid a b := dvd_euclid ha hb
#guard fastPow 3 5 == 243
#guard fastPow 2 100 == 1267650600228229401496703205376
#guard fastPow 7 0 == 1 && fastPow 0 0 == 1 && fastPow 0 3 == 0
#guard powMod 3 200 13 == 9
#guard powMod 2 1000 1 == 0
#guard powMod 5 0 7 == 1
example (b n : Nat) : fastPow b n = b ^ n := fastPow_eq b n
example (b n m : Nat) : powMod b n m = b ^ n % m := powMod_eq b n m
-- The modular power of a number with a thousand digits in its plain power, in an instant.
#guard powMod 123456789 100000 1000000007 == fastPow 123456789 100000 % 1000000007

/-! ## Paths in a graph -/

/-- A square with one diagonal and a vertex off to the side: 0 → 1 → 2 → 3 → 0, 0 → 2, and 4 → 0. -/
def square : Graph := Graph.ofEdges [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2), (4, 0)]

#guard square 0 == [1, 2]
#guard square 5 == []
#guard findPath square 3 5 [] 0 == some [0, 1, 2, 3]
#guard findPath square 0 5 [] 4 == some [4, 0]
#guard findPath square 2 5 [] 2 == some [2]
-- Nothing leads into vertex 4, and no search finds a way there.
#guard findPath square 4 5 [] 0 == none
#guard reaches square 5 1 0 && !reaches square 5 0 4
-- One edge in a row is not enough to get from 0 to 3.
#guard findPath square 3 1 [] 0 == none
#guard isPath square [0, 1, 2, 3, 0, 2]
#guard !isPath square [0, 3]
#guard isPath square [] && isPath square [9]
-- Whatever the search returns is a path from where it started to the target.
example (fuel u t : Nat) (p : List Nat) (h : findPath square t fuel [] u = some p) : IsPathFromTo square u t p :=
  findPath_sound square t fuel [] u p h
example : IsPath square [0, 1, 2] := by decide

/-! ## The runner -/

open Runner in
#guard answer "merge_sort 3 1 2" == "1 2 3"
open Runner in
#guard answer "  quick_sort 2 -5 2  " == "-5 2 2"
open Runner in
#guard answer "linear_search 2 | 3 1 2" == "2" && answer "binary_search 9 | 1 2 3" == "none"
open Runner in
#guard answer "minimum" == "none" && answer "maximum 4 9 2" == "9"
open Runner in
#guard answer "range_sum 1 3 | 5 6 7 8" == "13" && answer "prefix_sums 1 2 3" == "0 1 3 6"
open Runner in
#guard answer "gcd 12 18" == "6" && answer "pow 3 5" == "243" && answer "pow_mod 3 200 13" == "9" && answer "fib 30" == "832040"
open Runner in
#guard answer "find_sub 2 3 | 1 2 3 4" == "1" && answer "path 4 0 3 | 0 1 1 2 2 3" == "0 1 2 3" && answer "path 4 3 0 | 0 1 1 2 2 3" == "none"
open Runner in
#guard answer "same_counts 1 2 2 | 2 1 2" == "true" && answer "is_sorted 2 1" == "false" && answer "dedup_sorted 1 1 2" == "1 2"
open Runner in
#guard answer "shuffle 1 2" == "error: 'shuffle' is not an algorithm of this library"
open Runner in
#guard answer "fib x" == "error: the arguments of fib do not fit" && answer "" == "error: an empty request"
open Runner in
#guard algorithms.length == 19 && algorithms.all fun name => !(answer name).startsWith "error: '"

/-! ## Costs: counted, and bounded -/

section Costs

open Cost

-- The worst cases are reached: a search that fails, and a list in falling order.
#guard linearSearchCost 9 [3, 1, 4, 1, 5] == 5 && linearSearchCost 4 [3, 1, 4, 1, 5] == 3
#guard insertionSortCost [5, 4, 3, 2, 1] == 10 && insertionSortCost [1, 2, 3, 4, 5] == 4
#guard insertCost 9 [1, 3, 5] == 3 && insertCost 0 [1, 3, 5] == 1
#guard mergeCost [1, 3, 5] [2, 4, 6] == 5 && mergeCost [1, 2, 3] [4, 5, 6] == 3 && mergeCost ([] : List Nat) [1, 2] == 0
#guard mergeSortCost [8, 7, 6, 5, 4, 3, 2, 1] == 17 && mergeSortCost ([] : List Nat) == 0 && mergeSortCost [1] == 0
#guard binarySearchCost #[1, 3, 5, 7, 9, 11, 13] 7 == 1 && binarySearchCost #[1, 3, 5, 7, 9, 11, 13] 8 == 3
#guard binarySearchCost (#[] : Array Nat) 1 == 0

-- The bounds hold on lists of every length up to 64, in rising, falling and mixed order.
#guard (List.range 65).all fun n =>
  let rising := List.range n
  let falling := rising.reverse
  let mixed := rising.map fun i => (i * 37 + 11) % 64
  [rising, falling, mixed].all fun l =>
    2 * insertionSortCost l ≤ n * (n - 1) && mergeSortCost l ≤ n * (Nat.log2 n + 1)
      && linearSearchCost n l ≤ n && binarySearchCost rising.toArray (2 * n / 3) ≤ Nat.log2 n + 1

-- A list in falling order costs insertion sort exactly n (n - 1) / 2 comparisons.
#guard (List.range 40).all fun n => 2 * insertionSortCost (List.range n).reverse == n * (n - 1)

example (x : Int) (l : List Int) : linearSearchCost x l ≤ l.length := linearSearchCost_le x l
example (l : List Int) (h : linearSearch 7 l = none) : linearSearchCost 7 l = l.length := linearSearchCost_of_none h
example (a : Array Int) (x : Int) : binarySearchCost a x ≤ Nat.log2 a.size + 1 := binarySearchCost_le a x
example (l : List Int) : 2 * insertionSortCost l ≤ l.length * (l.length - 1) := two_mul_insertionSortCost_le l
example (l : List Int) : insertionSortCost l ≤ l.length * l.length := insertionSortCost_le_sq l
example (xs ys : List Int) : mergeCost xs ys ≤ xs.length + ys.length := mergeCost_le xs ys
example (l : List Int) : mergeSortCost l ≤ l.length * (Nat.log2 l.length + 1) := mergeSortCost_le l
example (l : List Int) (h : l.length ≤ 1024) : mergeSortCost l ≤ l.length * 10 :=
  mergeSortCost_le_of_le_pow 10 l h
example (l : List Int) (h : l.length = 2 ^ 10) : mergeSortCost l ≤ l.length * Nat.log2 l.length :=
  mergeSortCost_le_of_eq_pow 10 l h
example : insertCost 9 [1, 3, 5, 7] = 4 := insertCost_eq_length (by decide)
example (l : List Nat) : (halve l).1.length = (l.length + 1) / 2 ∧ (halve l).2.length = l.length / 2 :=
  ⟨length_halve_fst l, length_halve_snd l⟩

-- The two ends of insertion sort, and everything between: a list in falling order reaches the
-- upper bound, a sorted list reaches the lower one, and no list is outside the two.
#guard (List.range 40).all fun n => insertionSortCost (List.range n) == n - 1
example (n : Nat) : 2 * insertionSortCost (List.range n).reverse = n * (n - 1) := insertionSortCost_reverse_range n
example (n : Nat) : insertionSortCost (List.range n) = n - 1 := insertionSortCost_range n
example (l : List Int) : l.length - 1 ≤ insertionSortCost l ∧ 2 * insertionSortCost l ≤ l.length * (l.length - 1) :=
  insertionSortCost_bounds l
example (l : List Int) (h : Sorted l) : insertionSortCost l = l.length - 1 := insertionSortCost_of_sorted h
example : 2 * insertionSortCost [9, 7, 4, 1] = 4 * 3 := two_mul_insertionSortCost_of_falling (by simp [Falling])
example : ¬ Falling [3, 5] := by simp [Falling]
example (x : Int) (l : List Int) (h : l ≠ []) : 0 < insertCost x l := insertCost_pos x h
example (n : Nat) : Falling (List.range n).reverse ∧ Sorted (List.range n) := ⟨falling_reverse_range n, sorted_range n⟩

-- A million sorted elements are searched by looking at twenty of them.
example (a : Array Int) (x : Int) (h : a.size < 2 ^ 20) : binarySearchCost a x ≤ 20 :=
  binarySearchCost.go_le a x 20 0 a.size (Nat.le_refl _) (by omega)

open Runner in
#guard answer "cost insertion_sort 3 2 1" == "3" && answer "cost merge_sort 4 3 2 1" == "5"
open Runner in
#guard answer "cost linear_search 9 | 1 2 3" == "3" && answer "cost binary_search 9 | 1 2 3" == "2"
open Runner in
#guard answer "cost quick_sort 1 2" == "error: the comparisons of 'quick_sort' are not counted"
open Runner in
#guard answer "cost" == "error: cost needs the name of an algorithm" && answer "cost merge_sort x" == "error: the arguments of merge_sort do not fit"
open Runner in
#guard costed.all fun name => algorithms.contains name

end Costs

/-! ## A function a model wrote, and what is proved of it -/

section Lab

open Lab

#guard runLengths [1, 1, 2, 3, 3, 3] == [(1, 2), (2, 1), (3, 3)] && runLengths [] == [] && runLengths [7] == [(7, 1)]
#guard expand [(1, 2), (2, 1), (3, 3)] == [1, 1, 2, 3, 3, 3] && expand [] == [] && expand [(5, 0), (6, 2)] == [6, 6]

-- Encoding and decoding undo each other on every list of bits up to length 10.
#guard (List.range 1024).all fun code =>
  let l := (List.range 10).map fun i => (code >>> i) % 2
  expand (runLengths l) == l && ((runLengths l).map (·.2)).sum == l.length && (runLengths l).all fun p => p.2 > 0

example (l : List Nat) : expand (runLengths l) = l := expand_runLengths l
example (l : List Nat) (p : Nat × Nat) (h : p ∈ runLengths l) : 0 < p.2 := runLengths_pos l p h
example (l : List Nat) : Separated (runLengths l) := separated_runLengths l
example (l : List Nat) : ((runLengths l).map (·.2)).sum = l.length := sum_runLengths l
example (l : List Nat) : (runLengths l).length ≤ l.length := length_runLengths_le l
example (l : List Nat) : runLengths l = [] ↔ l = [] := runLengths_eq_nil
example (x n : Nat) : runLengths (List.replicate (n + 1) x) = [(x, n + 1)] := runLengths_replicate x (Nat.succ_pos n)
example (l m : List Nat) (h : runLengths l = runLengths m) : l = m := runLengths_injective h
#guard runLengths (List.replicate 6 4) == [(4, 6)] && (runLengths [1, 2, 1, 2]).length == 4
example : Separated [(1, 2), (2, 1), (1, 4)] := ⟨by decide, by decide, trivial⟩
example : ¬ Separated [(1, 2), (1, 1)] := fun h => h.1 rfl

-- Any other encoding with the three properties is this one: two runs of ones are not an encoding
-- of four ones, and the one that is cannot be anything but what the function returns.
example (ps : List (Nat × Nat)) (h : expand ps = [1, 1, 1, 1]) (hpos : ∀ p ∈ ps, 0 < p.2) (hsep : Separated ps) :
    ps = [(1, 4)] := by
  have := eq_runLengths h hpos hsep
  simpa [runLengths] using this

end Lab

end MaithicTest.Algo
