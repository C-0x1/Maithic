/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Rearrangements, by counting

A sorting algorithm may reorder a list and do nothing else to it. "Nothing else" is said here by
counting: two lists are rearrangements of each other when every value occurs in both the same
number of times. This is the form a test can evaluate and a reader can check by hand.
`sameCounts_iff_perm` shows that it is the permutation relation of the core library, so that a
theorem about one is a theorem about the other.
-/

namespace Maithic.Algo

universe u

variable {α : Type u} [DecidableEq α]

/-- Two lists hold the same values, each the same number of times. -/
def SameCounts (l₁ l₂ : List α) : Prop := ∀ a, l₁.count a = l₂.count a

/-- Counting characterizes permutations: two lists have the same counts exactly when one is a
permutation of the other. -/
theorem sameCounts_iff_perm {l₁ l₂ : List α} : SameCounts l₁ l₂ ↔ l₁.Perm l₂ :=
  List.perm_iff_count.symm

instance (l₁ l₂ : List α) : Decidable (SameCounts l₁ l₂) :=
  decidable_of_iff _ sameCounts_iff_perm.symm

theorem SameCounts.refl (l : List α) : SameCounts l l := fun _ => rfl

theorem SameCounts.symm {l₁ l₂ : List α} (h : SameCounts l₁ l₂) : SameCounts l₂ l₁ :=
  fun a => (h a).symm

theorem SameCounts.trans {l₁ l₂ l₃ : List α} (h₁ : SameCounts l₁ l₂) (h₂ : SameCounts l₂ l₃) :
    SameCounts l₁ l₃ :=
  fun a => (h₁ a).trans (h₂ a)

/-- Rearranging does not change the length. -/
theorem SameCounts.length_eq {l₁ l₂ : List α} (h : SameCounts l₁ l₂) : l₁.length = l₂.length :=
  (sameCounts_iff_perm.mp h).length_eq

/-- Rearranging neither adds a value nor loses one. -/
theorem SameCounts.mem_iff {l₁ l₂ : List α} (h : SameCounts l₁ l₂) {a : α} : a ∈ l₁ ↔ a ∈ l₂ :=
  (sameCounts_iff_perm.mp h).mem_iff

/-- The same value in front of two rearrangements gives two rearrangements. -/
theorem SameCounts.cons (a : α) {l₁ l₂ : List α} (h : SameCounts l₁ l₂) :
    SameCounts (a :: l₁) (a :: l₂) :=
  fun b => by simp [List.count_cons, h b]

/-- Rearrangements joined piece by piece are rearrangements. -/
theorem SameCounts.append {l₁ l₂ m₁ m₂ : List α} (h : SameCounts l₁ l₂) (k : SameCounts m₁ m₂) :
    SameCounts (l₁ ++ m₁) (l₂ ++ m₂) :=
  fun a => by simp [List.count_append, h a, k a]

/-- Two lists joined in either order hold the same values. -/
theorem sameCounts_append_comm (l₁ l₂ : List α) : SameCounts (l₁ ++ l₂) (l₂ ++ l₁) :=
  fun a => by simp [List.count_append, Nat.add_comm]

/-- A list read backwards holds the same values. -/
theorem sameCounts_reverse (l : List α) : SameCounts l.reverse l :=
  sameCounts_iff_perm.mpr (List.reverse_perm l)

/-- A check that looks only at the values that occur: it compares the counts of every value of
either list. -/
def countsAgree (l₁ l₂ : List α) : Bool := (l₁ ++ l₂).all fun a => l₁.count a == l₂.count a

/-- The check decides the relation: values that occur in neither list are counted zero times in
both, so nothing is lost by not looking at them. -/
theorem countsAgree_iff {l₁ l₂ : List α} : countsAgree l₁ l₂ = true ↔ SameCounts l₁ l₂ := by
  constructor
  · intro h a
    by_cases ha : a ∈ l₁ ++ l₂
    · simpa using (List.all_eq_true.mp h) a ha
    · have h₁ : a ∉ l₁ := fun h' => ha (List.mem_append_left _ h')
      have h₂ : a ∉ l₂ := fun h' => ha (List.mem_append_right _ h')
      rw [List.count_eq_zero_of_not_mem h₁, List.count_eq_zero_of_not_mem h₂]
  · intro h
    exact List.all_eq_true.mpr fun a _ => by simpa using h a

end Maithic.Algo
