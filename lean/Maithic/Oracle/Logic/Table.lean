/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Formula

/-!
# Deciding validity by truth table

The simplest complete method: evaluate the formula under every assignment of its atoms. It is
exponential in the number of atoms, so it suits small goals; larger ones are closed from a
clausal proof instead (`Maithic.Oracle.Logic.Lrat`). The procedure is proved sound and complete,
which makes validity of a formula a decidable proposition.
-/

namespace Maithic.Oracle.Logic

/-- The assignment that differs from `σ` at atom `i` only, where it is `b`. -/
def Assignment.update (σ : Assignment) (i : Nat) (b : Bool) : Assignment :=
  fun j => if j = i then b else σ j

namespace Formula

/-- Whether `f` is true under every assignment that agrees with `σ` from atom `n` on. -/
def holdsForAll (f : Formula) : Nat → Assignment → Bool
  | 0, σ => f.eval σ
  | n + 1, σ => holdsForAll f n (σ.update n true) && holdsForAll f n (σ.update n false)

/-- Whether `f` is true under every assignment, computed from its truth table. -/
def isTautology (f : Formula) : Bool :=
  holdsForAll f f.bound fun _ => false

theorem eval_of_holdsForAll {f : Formula} :
    ∀ (n : Nat) {σ τ : Assignment}, holdsForAll f n σ = true → (∀ j, n ≤ j → τ j = σ j) →
      f.eval τ = true
  | 0, σ, τ, h, agree => by
    have : τ = σ := funext fun j => agree j (Nat.zero_le j)
    rw [this]; exact h
  | n + 1, σ, τ, h, agree => by
    simp only [holdsForAll, Bool.and_eq_true] at h
    have step : ∀ b, τ n = b → holdsForAll f n (σ.update n b) = true → f.eval τ = true := by
      intro b hb hn
      refine eval_of_holdsForAll n hn fun j hj => ?_
      by_cases hjn : j = n
      · simp [Assignment.update, hjn, hb]
      · simp only [Assignment.update, hjn, ite_false]
        exact agree j (by omega)
    cases hb : τ n
    · exact step false hb h.2
    · exact step true hb h.1

theorem holdsForAll_of_valid {f : Formula} (h : f.Valid) :
    ∀ (n : Nat) (σ : Assignment), holdsForAll f n σ = true
  | 0, σ => h σ
  | n + 1, σ => by
    simp [holdsForAll, holdsForAll_of_valid h n]

/-- A formula whose truth table is all true is valid. -/
theorem valid_of_isTautology {f : Formula} (h : f.isTautology = true) : f.Valid := by
  intro σ
  let τ : Assignment := fun j => if j < f.bound then σ j else false
  have hτ : f.eval τ = true :=
    eval_of_holdsForAll f.bound h fun j hj => by simp [τ, Nat.not_lt.mpr hj]
  rw [eval_congr f (σ := σ) (τ := τ) fun i hi => by simp [τ, hi]]
  exact hτ

/-- The truth table decides validity. -/
theorem isTautology_iff {f : Formula} : f.isTautology = true ↔ f.Valid :=
  ⟨valid_of_isTautology, fun h => holdsForAll_of_valid h _ _⟩

instance (f : Formula) : Decidable f.Valid :=
  decidable_of_iff _ isTautology_iff

/-- The truth table of the negation decides satisfiability. -/
instance (f : Formula) : Decidable f.Satisfiable :=
  decidable_of_iff (¬(not f).Valid) <| by
    rw [valid_iff_not_satisfiable_not]
    constructor
    · intro h
      apply Classical.not_not.mp
      intro hf
      exact h fun ⟨σ, hσ⟩ => hf ⟨σ, by simpa [eval] using hσ⟩
    · rintro ⟨σ, hσ⟩ h
      exact h ⟨σ, by simpa [eval] using hσ⟩

end Formula

end Maithic.Oracle.Logic
