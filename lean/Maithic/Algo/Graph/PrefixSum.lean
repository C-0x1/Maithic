/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Prefix sums and range queries

The sum of any stretch of a list in constant time, after one pass that records the sum of every
prefix. The table of a list of length `n` has `n + 1` entries: entry `i` is the sum of the first
`i` elements, so entry `0` is zero. The sum of the elements at positions `i ≤ k < j` is then entry
`j` minus entry `i`, which is the theorem `rangeSum_prefixSums`.
-/

namespace Maithic.Algo

/-- The running sums of a list, starting from `acc`: one more entry than the list has elements. -/
def prefixSums.go (acc : Int) : List Int → List Int
  | [] => [acc]
  | x :: xs => acc :: go (acc + x) xs

/-- The table of prefix sums: entry `i` is the sum of the first `i` elements. -/
def prefixSums (l : List Int) : List Int := prefixSums.go 0 l

/-- The sum of the elements at positions `i ≤ k < j`, read from a table of prefix sums. -/
def rangeSum (table : List Int) (i j : Nat) : Int := table[j]?.getD 0 - table[i]?.getD 0

theorem prefixSums.length_go (acc : Int) (l : List Int) : (prefixSums.go acc l).length = l.length + 1 := by
  induction l generalizing acc with
  | nil => rfl
  | cons x xs ih => simp [prefixSums.go, ih]

/-- The table has one more entry than the list has elements. -/
@[simp] theorem length_prefixSums (l : List Int) : (prefixSums l).length = l.length + 1 :=
  prefixSums.length_go 0 l

theorem prefixSums.getElem?_go (acc : Int) (l : List Int) {i : Nat} (hi : i ≤ l.length) :
    (prefixSums.go acc l)[i]? = some (acc + (l.take i).sum) := by
  induction l generalizing acc i with
  | nil =>
    have : i = 0 := by simpa using hi
    subst this
    simp [prefixSums.go]
  | cons x xs ih =>
    cases i with
    | zero => simp [prefixSums.go]
    | succ k =>
      have hk : k ≤ xs.length := by simpa using hi
      simp [prefixSums.go, ih (acc + x) hk, Int.add_assoc]

/-- Entry `i` of the table is the sum of the first `i` elements. -/
theorem prefixSums_getElem? (l : List Int) {i : Nat} (hi : i ≤ l.length) :
    (prefixSums l)[i]? = some (l.take i).sum := by
  simpa [prefixSums] using prefixSums.getElem?_go 0 l hi

/-- The first entry is zero and the last is the sum of the whole list. -/
theorem prefixSums_last (l : List Int) : (prefixSums l)[l.length]? = some l.sum := by
  simpa using prefixSums_getElem? l (Nat.le_refl _)

/-- A range query on the table answers with the sum of the stretch. -/
theorem rangeSum_prefixSums (l : List Int) {i j : Nat} (hij : i ≤ j) (hj : j ≤ l.length) :
    rangeSum (prefixSums l) i j = ((l.drop i).take (j - i)).sum := by
  have hsplit : l.take j = l.take i ++ (l.drop i).take (j - i) := by
    have : j = i + (j - i) := by omega
    rw [this, List.take_add]
    simp
  have hsum : (l.take j).sum = (l.take i).sum + ((l.drop i).take (j - i)).sum := by
    rw [hsplit, List.sum_append_int]
  simp only [rangeSum, prefixSums_getElem? l hj, prefixSums_getElem? l (Nat.le_trans hij hj), Option.getD_some]
  omega

/-- The whole list is the stretch from `0` to its length. -/
theorem rangeSum_all (l : List Int) : rangeSum (prefixSums l) 0 l.length = l.sum := by
  simpa using rangeSum_prefixSums l (Nat.zero_le _) (Nat.le_refl _)

/-- Two stretches that meet add up to the stretch that covers both. -/
theorem rangeSum_add (table : List Int) (i j k : Nat) :
    rangeSum table i j + rangeSum table j k = rangeSum table i k := by
  simp only [rangeSum]
  omega

/-- The table of prefix sums of an array, as an array. -/
def prefixSumArray (a : Array Int) : Array Int := (prefixSums a.toList).toArray

/-- A range query on an array of prefix sums: two reads and a subtraction. -/
def rangeSumArray (table : Array Int) (i j : Nat) : Int := table[j]?.getD 0 - table[i]?.getD 0

@[simp] theorem size_prefixSumArray (a : Array Int) : (prefixSumArray a).size = a.size + 1 := by
  simp [prefixSumArray]

/-- A range query on the array of prefix sums answers with the sum of the stretch. -/
theorem rangeSumArray_prefixSumArray (a : Array Int) {i j : Nat} (hij : i ≤ j) (hj : j ≤ a.size) :
    rangeSumArray (prefixSumArray a) i j = ((a.toList.drop i).take (j - i)).sum := by
  have := rangeSum_prefixSums a.toList hij (by simpa using hj)
  simpa [rangeSumArray, prefixSumArray, rangeSum] using this

end Maithic.Algo
