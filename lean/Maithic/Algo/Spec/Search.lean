/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# What a search may answer

A search over a list answers with a position or with `none`. This file says what each answer has
to mean, once, for every search in the library: a position is in bounds and holds the target, and
`none` means the target is nowhere in the list. A search that promises the *first* position is
held to more, and that stronger specification leaves exactly one right answer.
-/

namespace Maithic.Algo

universe u

variable {α : Type u}

/-- `r` is a right answer to the question where `x` is in `l`: a position that holds `x`, or
`none` when `x` is not in the list. -/
def SearchSpec (l : List α) (x : α) : Option Nat → Prop
  | some i => l[i]? = some x
  | none => x ∉ l

/-- `r` is the right answer to the question where `x` is *first* in `l`. -/
def FirstSearchSpec (l : List α) (x : α) (r : Option Nat) : Prop :=
  SearchSpec l x r ∧ ∀ i, r = some i → ∀ j, j < i → l[j]? ≠ some x

@[simp] theorem searchSpec_some {l : List α} {x : α} {i : Nat} :
    SearchSpec l x (some i) ↔ l[i]? = some x := Iff.rfl

@[simp] theorem searchSpec_none {l : List α} {x : α} : SearchSpec l x none ↔ x ∉ l := Iff.rfl

/-- A position that is answered is a position of the list. -/
theorem SearchSpec.lt_length {l : List α} {x : α} {i : Nat} (h : SearchSpec l x (some i)) :
    i < l.length := by
  have h' : l[i]? = some x := h
  exact (List.getElem?_eq_some_iff.mp h').1

/-- When a position is answered, the target is in the list. -/
theorem SearchSpec.mem {l : List α} {x : α} {i : Nat} (h : SearchSpec l x (some i)) : x ∈ l :=
  List.mem_of_getElem? h

/-- A right answer is a position exactly when the target is in the list. -/
theorem SearchSpec.isSome_iff {l : List α} {x : α} {r : Option Nat} (h : SearchSpec l x r) :
    r.isSome ↔ x ∈ l := by
  cases r with
  | none => simpa using h
  | some i => simpa using h.mem

instance [DecidableEq α] (l : List α) (x : α) (r : Option Nat) : Decidable (SearchSpec l x r) :=
  match r with
  | some i => inferInstanceAs (Decidable (l[i]? = some x))
  | none => inferInstanceAs (Decidable (x ∉ l))

/-- The specification of a first position leaves one right answer. -/
theorem FirstSearchSpec.unique {l : List α} {x : α} {r₁ r₂ : Option Nat}
    (h₁ : FirstSearchSpec l x r₁) (h₂ : FirstSearchSpec l x r₂) : r₁ = r₂ := by
  cases r₁ with
  | none =>
    cases r₂ with
    | none => rfl
    | some j => exact absurd h₂.1.mem h₁.1
  | some i =>
    cases r₂ with
    | none => exact absurd h₁.1.mem h₂.1
    | some j =>
      have hi : l[i]? = some x := h₁.1
      have hj : l[j]? = some x := h₂.1
      have hij : ¬ j < i := fun hlt => h₁.2 i rfl j hlt hj
      have hji : ¬ i < j := fun hlt => h₂.2 j rfl i hlt hi
      have : i = j := by omega
      rw [this]

end Maithic.Algo
