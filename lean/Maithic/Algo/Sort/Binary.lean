/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Algo.Spec.TotalOrder
import Maithic.Algo.Spec.Search

/-!
# Binary search

Binary search on an array, with the three things that go wrong in practice ruled out by proof.

* **Bounds.** Every read `a[mid]` carries a proof that `mid` is a position of the array: there is
  no index check at run time and no way to read outside.
* **Termination.** The interval `[lo, hi)` shrinks at every step, and Lean accepts the definition
  only with that argument. The midpoint is `lo + (hi - lo) / 2`, which cannot overflow in a
  language with bounded numbers either.
* **Correctness.** An answered position holds the target, whatever the array. On a sorted array
  `none` means the target is not there.

The search compares with `≤` alone, so what it finds is an element that is at most the target and
at least the target. In a total order that is the target itself, which is the last theorem.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [LE α] [DecidableLE α]

/-- Searches the positions `lo ≤ i < hi` of `a` for an element that is both at most and at least
`x`. -/
def binarySearch.go (a : Array α) (x : α) (lo hi : Nat) (h : hi ≤ a.size) : Option Nat :=
  if hlt : lo < hi then
    let mid := lo + (hi - lo) / 2
    have hmid : mid < a.size := by omega
    if a[mid] ≤ x then
      if x ≤ a[mid] then some mid
      else go a x (mid + 1) hi h
    else go a x lo mid (by omega)
  else none
termination_by hi - lo

/-- Binary search: a position of `a` that holds an element both at most and at least `x`, if the
search finds one. On a sorted array it finds one whenever there is one. -/
def binarySearch (a : Array α) (x : α) : Option Nat := binarySearch.go a x 0 a.size (Nat.le_refl _)

/-- A position the search answers lies in the interval it was given, and what it holds is at most
the target and at least the target. No assumption on the array is needed. -/
theorem binarySearch.go_some {a : Array α} {x : α} {lo hi : Nat} {h : hi ≤ a.size} {i : Nat}
    (hfound : binarySearch.go a x lo hi h = some i) :
    ∃ hi' : i < a.size, lo ≤ i ∧ i < hi ∧ a[i] ≤ x ∧ x ≤ a[i] := by
  induction hd : hi - lo using Nat.strongRecOn generalizing lo hi with
  | _ d ih =>
    unfold binarySearch.go at hfound
    split at hfound
    · rename_i hlt
      simp only at hfound
      split at hfound
      · rename_i hle
        split at hfound
        · rename_i hge
          have hi' : i = lo + (hi - lo) / 2 := by simpa using hfound.symm
          subst hi'
          exact ⟨by omega, by omega, by omega, hle, hge⟩
        · obtain ⟨hb, hlo, hhi, hrest⟩ := ih (hi - (lo + (hi - lo) / 2 + 1)) (by omega) hfound rfl
          exact ⟨hb, by omega, hhi, hrest⟩
      · obtain ⟨hb, hlo, hhi, hrest⟩ := ih (lo + (hi - lo) / 2 - lo) (by omega) hfound rfl
        exact ⟨hb, hlo, by omega, hrest⟩
    · simp at hfound

/-- On a sorted array, when the search answers `none`, no position of the interval holds an
element that is both at most and at least the target. -/
theorem binarySearch.go_none [TotalPreorder α] {a : Array α} {x : α} {lo hi : Nat}
    {h : hi ≤ a.size} (hsorted : SortedArray a) (hnone : binarySearch.go a x lo hi h = none)
    {j : Nat} (hlo : lo ≤ j) (hhi : j < hi) : ¬ (a[j]'(Nat.lt_of_lt_of_le hhi h) ≤ x ∧ x ≤ a[j]) := by
  induction hd : hi - lo using Nat.strongRecOn generalizing lo hi with
  | _ d ih =>
    have hle : ∀ {p q : Nat} (hpq : p ≤ q) (hq : q < a.size), a[p] ≤ a[q] := by
      intro p q hpq hq
      have := hsorted.getElem_le hpq (by simpa using hq)
      simpa using this
    unfold binarySearch.go at hnone
    split at hnone
    · rename_i hlt
      simp only at hnone
      have hmid : lo + (hi - lo) / 2 < a.size := by omega
      split at hnone
      · rename_i hmx
        split at hnone
        · simp at hnone
        · rename_i hxm
          rintro ⟨hjx, hxj⟩
          by_cases hjm : j ≤ lo + (hi - lo) / 2
          · -- Left of the midpoint everything is at most the midpoint, which is below the target.
            exact hxm (TotalPreorder.le_trans _ _ _ hxj (hle hjm hmid))
          · exact ih (hi - (lo + (hi - lo) / 2 + 1)) (by omega) hnone (by omega) hhi rfl ⟨hjx, hxj⟩
      · rename_i hmx
        rintro ⟨hjx, hxj⟩
        by_cases hjm : j < lo + (hi - lo) / 2
        · exact ih (lo + (hi - lo) / 2 - lo) (by omega) hnone hlo hjm rfl ⟨hjx, hxj⟩
        · -- From the midpoint on everything is at least the midpoint, which is above the target.
          exact hmx (TotalPreorder.le_trans _ _ _ (hle (by omega) (Nat.lt_of_lt_of_le hhi h)) hjx)
    · omega

/-- What binary search answers is a position of the array, and the element there is at most the
target and at least the target. -/
theorem binarySearch_some {a : Array α} {x : α} {i : Nat} (h : binarySearch a x = some i) :
    ∃ hi : i < a.size, a[i] ≤ x ∧ x ≤ a[i] := by
  obtain ⟨hb, _, _, hrest⟩ := binarySearch.go_some h
  exact ⟨hb, hrest⟩

/-- On a sorted array, `none` means that no element is both at most and at least the target. -/
theorem binarySearch_none [TotalPreorder α] {a : Array α} {x : α} (hsorted : SortedArray a)
    (h : binarySearch a x = none) {j : Nat} (hj : j < a.size) : ¬ (a[j] ≤ x ∧ x ≤ a[j]) :=
  binarySearch.go_none hsorted h (Nat.zero_le j) hj

/-- On a sorted array the search finds an element exactly when there is one to find. -/
theorem binarySearch_isSome_iff [TotalPreorder α] {a : Array α} {x : α} (hsorted : SortedArray a) :
    (binarySearch a x).isSome ↔ ∃ (j : Nat) (hj : j < a.size), a[j] ≤ x ∧ x ≤ a[j] := by
  constructor
  · intro h
    cases hs : binarySearch a x with
    | none => simp [hs] at h
    | some i =>
      obtain ⟨hb, hrest⟩ := binarySearch_some hs
      exact ⟨i, hb, hrest⟩
  · rintro ⟨j, hj, hrest⟩
    cases hs : binarySearch a x with
    | none => exact absurd hrest (binarySearch_none hsorted hs hj)
    | some i => rfl

/-- In a total order, binary search on a sorted array meets the specification of a search: an
answered position holds the target, and `none` means the target is not in the array. -/
theorem binarySearch_spec [TotalOrder α] {a : Array α} (x : α) (hsorted : SortedArray a) :
    SearchSpec a.toList x (binarySearch a x) := by
  cases hs : binarySearch a x with
  | some i =>
    obtain ⟨hb, hle, hge⟩ := binarySearch_some hs
    have heq : a[i] = x := TotalOrder.le_antisymm _ _ hle hge
    simp [SearchSpec, hb, heq]
  | none =>
    intro hmem
    obtain ⟨j, hj, hjx⟩ := List.getElem_of_mem hmem
    have hj' : j < a.size := by simpa using hj
    have heq : a[j] = x := by simpa using hjx
    exact binarySearch_none hsorted hs hj' ⟨by rw [heq]; exact TotalPreorder.le_refl x,
      by rw [heq]; exact TotalPreorder.le_refl x⟩

end Maithic.Algo
