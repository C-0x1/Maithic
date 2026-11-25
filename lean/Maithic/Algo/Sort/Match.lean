/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Searching for a pattern in a text

The plain method: try the pattern at the first position, then at the second, and so on.
`findSub pattern text` returns the first position at which the pattern occurs, and three theorems
say that this is what it does:

* `findSub_some`: at a position it returns, the pattern occurs;
* `findSub_first`: it occurs at no earlier position;
* `findSub_none`: when nothing is returned, it occurs nowhere.

Texts are lists, of characters or of anything else that can be compared.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [BEq α]

/-- The first position at which `pattern` occurs in the text, if it occurs. -/
def findSub (pattern : List α) : List α → Option Nat
  | [] => if pattern.isEmpty then some 0 else none
  | c :: rest => if pattern.isPrefixOf (c :: rest) then some 0 else (findSub pattern rest).map (· + 1)

/-- The pattern occurs in the text at position `i`: it is a prefix of what is left after `i` elements. -/
def OccursAt (pattern text : List α) (i : Nat) : Prop := i ≤ text.length ∧ pattern <+: text.drop i

variable [LawfulBEq α]

omit [BEq α] [LawfulBEq α] in
theorem occursAt_zero {pattern text : List α} : OccursAt pattern text 0 ↔ pattern <+: text := by
  simp [OccursAt]

omit [BEq α] [LawfulBEq α] in
theorem occursAt_succ {pattern : List α} {c : α} {rest : List α} {i : Nat} :
    OccursAt pattern (c :: rest) (i + 1) ↔ OccursAt pattern rest i := by
  simp [OccursAt]

/-- At a position that is returned, the pattern occurs. -/
theorem findSub_some {pattern : List α} : ∀ {text : List α} {i : Nat}, findSub pattern text = some i → OccursAt pattern text i
  | [], i, h => by
    simp only [findSub] at h
    split at h
    · next he =>
      cases h
      exact ⟨Nat.le_refl _, by simp [List.isEmpty_iff.mp he]⟩
    · cases h
  | c :: rest, i, h => by
    simp only [findSub] at h
    split at h
    · next hp =>
      cases h
      exact occursAt_zero.mpr (List.isPrefixOf_iff_prefix.mp hp)
    · next hp =>
      cases hr : findSub pattern rest with
      | none => simp [hr] at h
      | some j =>
        simp only [hr, Option.map_some, Option.some.injEq] at h
        subst h
        exact occursAt_succ.mpr (findSub_some hr)

/-- When nothing is returned, the pattern occurs nowhere. -/
theorem findSub_none {pattern : List α} : ∀ {text : List α}, findSub pattern text = none → ∀ i, ¬ OccursAt pattern text i
  | [], h, i, ⟨hi, hp⟩ => by
    simp only [findSub] at h
    split at h
    · cases h
    · next he =>
      have : pattern = [] := by simpa using hp
      exact he (by simp [this])
  | c :: rest, h, i, ho => by
    simp only [findSub] at h
    split at h
    · cases h
    · next hp =>
      cases i with
      | zero => exact hp (List.isPrefixOf_iff_prefix.mpr (occursAt_zero.mp ho))
      | succ j =>
        cases hr : findSub pattern rest with
        | none => exact findSub_none hr j (occursAt_succ.mp ho)
        | some k => simp [hr] at h

/-- The position that is returned is the first: the pattern occurs at no earlier one. -/
theorem findSub_first {pattern : List α} : ∀ {text : List α} {i : Nat}, findSub pattern text = some i →
    ∀ j, j < i → ¬ OccursAt pattern text j
  | [], i, h, j, hj, _ => by
    simp only [findSub] at h
    split at h
    · cases h; omega
    · cases h
  | c :: rest, i, h, j, hj, ho => by
    simp only [findSub] at h
    split at h
    · cases h; omega
    · next hp =>
      cases hr : findSub pattern rest with
      | none => simp [hr] at h
      | some k =>
        simp only [hr, Option.map_some, Option.some.injEq] at h
        subst h
        cases j with
        | zero => exact hp (List.isPrefixOf_iff_prefix.mpr (occursAt_zero.mp ho))
        | succ j' => exact findSub_first hr j' (by omega) (occursAt_succ.mp ho)

/-- The search succeeds exactly when the pattern occurs somewhere. -/
theorem findSub_isSome_iff {pattern text : List α} : (findSub pattern text).isSome ↔ ∃ i, OccursAt pattern text i := by
  constructor
  · intro h
    cases hr : findSub pattern text with
    | none => simp [hr] at h
    | some i => exact ⟨i, findSub_some hr⟩
  · intro ⟨i, hi⟩
    cases hr : findSub pattern text with
    | none => exact absurd hi (findSub_none hr i)
    | some _ => rfl

omit [LawfulBEq α] in
/-- The empty pattern occurs at the start of every text. -/
theorem findSub_nil (text : List α) : findSub ([] : List α) text = some 0 := by
  cases text <;> simp [findSub]

/-- Whether a text contains a pattern. -/
def containsSub (pattern text : List α) : Bool := (findSub pattern text).isSome

/-- The same search on strings, by their characters. -/
def findInString (pattern text : String) : Option Nat := findSub pattern.toList text.toList

end Maithic.Algo
