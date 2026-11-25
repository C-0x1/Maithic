/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Sortedness

The specification that every sorting and searching algorithm in the library is proved against.
A list is sorted when each element is at most every later element. The definition is stated for
any type with `≤`; the algorithms additionally ask for a `TotalPreorder`, the two order facts
their proofs use.
-/

namespace Maithic.Algo

universe u

/-- The order facts that sorting needs: any two elements are comparable, and `≤` is transitive. -/
class TotalPreorder (α : Type u) [LE α] : Prop where
  le_total : ∀ a b : α, a ≤ b ∨ b ≤ a
  le_trans : ∀ a b c : α, a ≤ b → b ≤ c → a ≤ c

instance : TotalPreorder Nat := ⟨Nat.le_total, fun _ _ _ => Nat.le_trans⟩

instance : TotalPreorder Int := ⟨Int.le_total, fun _ _ _ => Int.le_trans⟩

variable {α : Type u} [LE α]

/-- A list is sorted when each element is at most every later element. -/
def Sorted (l : List α) : Prop := l.Pairwise (· ≤ ·)

/-- An array is sorted when its list of elements is. -/
def SortedArray (a : Array α) : Prop := Sorted a.toList

instance [DecidableLE α] (l : List α) : Decidable (Sorted l) :=
  inferInstanceAs (Decidable (l.Pairwise (· ≤ ·)))

instance [DecidableLE α] (a : Array α) : Decidable (SortedArray a) :=
  inferInstanceAs (Decidable (Sorted a.toList))

@[simp] theorem sorted_nil : Sorted ([] : List α) := List.Pairwise.nil

@[simp] theorem sorted_singleton (a : α) : Sorted [a] := by
  simp [Sorted]

/-- A list with a new head is sorted exactly when the head is at most every element and the tail
is sorted. -/
theorem sorted_cons {a : α} {l : List α} : Sorted (a :: l) ↔ (∀ b ∈ l, a ≤ b) ∧ Sorted l :=
  List.pairwise_cons

/-- The tail of a sorted list is sorted. -/
theorem Sorted.tail {a : α} {l : List α} (h : Sorted (a :: l)) : Sorted l :=
  (sorted_cons.mp h).2

/-- The head of a sorted list is at most every later element. -/
theorem Sorted.head_le {a b : α} {l : List α} (h : Sorted (a :: l)) (hb : b ∈ l) : a ≤ b :=
  (sorted_cons.mp h).1 b hb

/-- Any sublist of a sorted list is sorted. -/
theorem Sorted.sublist {l₁ l₂ : List α} (h : Sorted l₂) (hs : l₁.Sublist l₂) : Sorted l₁ :=
  List.Pairwise.sublist hs h

/-- Two sorted lists concatenate to a sorted list when everything in the first is at most
everything in the second. -/
theorem sorted_append {l₁ l₂ : List α} :
    Sorted (l₁ ++ l₂) ↔ Sorted l₁ ∧ Sorted l₂ ∧ ∀ a ∈ l₁, ∀ b ∈ l₂, a ≤ b :=
  List.pairwise_append

end Maithic.Algo
