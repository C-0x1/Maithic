/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Linear search

Finds the first position of an element in a list. The specification has two halves: a returned
index holds the target and nothing before it does, and `none` means the target is absent.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [BEq α]

/-- The index of the first element equal to `x`, if there is one. -/
def linearSearch (x : α) : List α → Option Nat
  | [] => none
  | y :: ys => if y == x then some 0 else (linearSearch x ys).map (· + 1)

/-- A returned index is in bounds and holds the target. -/
theorem linearSearch_some [LawfulBEq α] {x : α} {l : List α} {i : Nat}
    (h : linearSearch x l = some i) : l[i]? = some x := by
  induction l generalizing i with
  | nil => simp [linearSearch] at h
  | cons y ys ih =>
    simp only [linearSearch] at h
    split at h
    · rename_i hyx
      have hi : i = 0 := by simpa using h.symm
      subst hi
      simpa using hyx
    · cases hrec : linearSearch x ys with
      | none => simp [hrec] at h
      | some j =>
        have hi : i = j + 1 := by simpa [hrec] using h.symm
        subst hi
        simpa using ih hrec

/-- No element before a returned index equals the target. -/
theorem linearSearch_first [LawfulBEq α] {x : α} {l : List α} {i j : Nat}
    (h : linearSearch x l = some i) (hj : j < i) : l[j]? ≠ some x := by
  induction l generalizing i j with
  | nil => simp [linearSearch] at h
  | cons y ys ih =>
    simp only [linearSearch] at h
    split at h
    · have hi : i = 0 := by simpa using h.symm
      omega
    · rename_i hyx
      cases hrec : linearSearch x ys with
      | none => simp [hrec] at h
      | some k =>
        have hi : i = k + 1 := by simpa [hrec] using h.symm
        subst hi
        cases j with
        | zero => simpa using hyx
        | succ j' => simpa using ih hrec (by omega)

/-- `none` means the target does not occur. -/
theorem linearSearch_none [LawfulBEq α] {x : α} {l : List α}
    (h : linearSearch x l = none) : x ∉ l := by
  induction l with
  | nil => simp
  | cons y ys ih =>
    simp only [linearSearch] at h
    split at h
    · simp at h
    · rename_i hyx
      have hrec : linearSearch x ys = none := by simpa using h
      intro hmem
      rcases List.mem_cons.mp hmem with rfl | hmem
      · simp at hyx
      · exact ih hrec hmem

/-- The search succeeds exactly when the target occurs. -/
theorem linearSearch_isSome_iff [LawfulBEq α] {x : α} {l : List α} :
    (linearSearch x l).isSome ↔ x ∈ l := by
  constructor
  · intro h
    cases hs : linearSearch x l with
    | none => simp [hs] at h
    | some i => exact List.mem_of_getElem? (linearSearch_some hs)
  · intro hmem
    cases hs : linearSearch x l with
    | none => exact absurd hmem (linearSearch_none hs)
    | some i => rfl

end Maithic.Algo
