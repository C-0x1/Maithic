/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Propositional formulas

The datatype that propositional goals are reflected into, with its two meanings: `eval` computes a
truth value from an assignment of truth values to the atoms, and `denote` builds a proposition
from an assignment of propositions to the atoms. A formula that evaluates to `true` under every
assignment is `Valid`, and a valid formula denotes a true proposition whatever its atoms stand
for (`Formula.denote_of_valid`). That theorem is the bridge every checker in this directory uses:
a checker establishes validity by computation, and the theorem turns it into a proof of the goal.
-/

namespace Maithic.Oracle.Logic

/-- A propositional formula over atoms numbered from zero. -/
inductive Formula where
  /-- The atom with the given number. -/
  | atom (i : Nat)
  /-- The true formula. -/
  | tt
  /-- The false formula. -/
  | ff
  /-- Negation. -/
  | not (f : Formula)
  /-- Conjunction. -/
  | and (f g : Formula)
  /-- Disjunction. -/
  | or (f g : Formula)
  /-- Implication. -/
  | imp (f g : Formula)
  /-- Equivalence. -/
  | iff (f g : Formula)
  deriving Repr, DecidableEq, Inhabited

/-- An assignment gives every atom a truth value. -/
abbrev Assignment := Nat → Bool

namespace Formula

/-- The truth value of a formula under an assignment. -/
def eval (σ : Assignment) : Formula → Bool
  | atom i => σ i
  | tt => true
  | ff => false
  | not f => !f.eval σ
  | and f g => f.eval σ && g.eval σ
  | or f g => f.eval σ || g.eval σ
  | imp f g => !f.eval σ || g.eval σ
  | iff f g => f.eval σ == g.eval σ

/-- The proposition a formula stands for when atom `i` stands for `ρ i`. -/
def denote (ρ : Nat → Prop) : Formula → Prop
  | atom i => ρ i
  | tt => True
  | ff => False
  | not f => ¬f.denote ρ
  | and f g => f.denote ρ ∧ g.denote ρ
  | or f g => f.denote ρ ∨ g.denote ρ
  | imp f g => f.denote ρ → g.denote ρ
  | iff f g => (f.denote ρ ↔ g.denote ρ)

/-- A formula is valid when it is true under every assignment. -/
def Valid (f : Formula) : Prop := ∀ σ : Assignment, f.eval σ = true

/-- A formula is satisfiable when it is true under some assignment. -/
def Satisfiable (f : Formula) : Prop := ∃ σ : Assignment, f.eval σ = true

/-- One more than the largest atom of a formula, and zero when it has none. -/
def bound : Formula → Nat
  | atom i => i + 1
  | tt => 0
  | ff => 0
  | not f => f.bound
  | and f g => max f.bound g.bound
  | or f g => max f.bound g.bound
  | imp f g => max f.bound g.bound
  | iff f g => max f.bound g.bound

/-- The number of connectives and atoms of a formula. -/
def size : Formula → Nat
  | atom _ => 1
  | tt => 1
  | ff => 1
  | not f => f.size + 1
  | and f g => f.size + g.size + 1
  | or f g => f.size + g.size + 1
  | imp f g => f.size + g.size + 1
  | iff f g => f.size + g.size + 1

/-- The formula as text, with atom `i` written `a<i>` and every binary connective in
parentheses. -/
def toText : Formula → String
  | atom i => s!"a{i}"
  | tt => "True"
  | ff => "False"
  | not f => s!"¬{f.toText}"
  | and f g => s!"({f.toText} ∧ {g.toText})"
  | or f g => s!"({f.toText} ∨ {g.toText})"
  | imp f g => s!"({f.toText} → {g.toText})"
  | iff f g => s!"({f.toText} ↔ {g.toText})"

/-- The value of a formula depends only on the atoms below its bound. -/
theorem eval_congr {σ τ : Assignment} (f : Formula) (h : ∀ i, i < f.bound → σ i = τ i) :
    f.eval σ = f.eval τ := by
  induction f with
  | atom i => exact h i (Nat.lt_succ_self i)
  | tt => rfl
  | ff => rfl
  | not f ih => simp only [eval, ih h]
  | and f g ihf ihg | or f g ihf ihg | imp f g ihf ihg | iff f g ihf ihg =>
    simp only [eval,
      ihf fun i hi => h i (Nat.lt_of_lt_of_le hi (Nat.le_max_left ..)),
      ihg fun i hi => h i (Nat.lt_of_lt_of_le hi (Nat.le_max_right ..))]

/-- When the truth values agree with the propositions, evaluation agrees with denotation. -/
theorem denote_iff_eval {ρ : Nat → Prop} {σ : Assignment} (h : ∀ i, ρ i ↔ σ i = true)
    (f : Formula) : f.denote ρ ↔ f.eval σ = true := by
  induction f with
  | atom i => exact h i
  | tt => simp [denote, eval]
  | ff => simp [denote, eval]
  | not f ih => simp [denote, eval, ih]
  | and f g ihf ihg => simp [denote, eval, ihf, ihg]
  | or f g ihf ihg => simp [denote, eval, ihf, ihg]
  | imp f g ihf ihg =>
    simp only [denote, eval, ihf, ihg, Bool.or_eq_true, Bool.not_eq_true']
    cases f.eval σ <;> simp
  | iff f g ihf ihg =>
    simp only [denote, eval, ihf, ihg, beq_iff_eq]
    cases f.eval σ <;> cases g.eval σ <;> simp

/-- A valid formula denotes a true proposition, whatever its atoms stand for. The truth values
are read off the propositions classically; nothing else about them is used. -/
theorem denote_of_valid {f : Formula} (h : f.Valid) (ρ : Nat → Prop) : f.denote ρ := by
  classical
  exact (denote_iff_eval (σ := fun i => decide (ρ i)) (fun i => by simp) f).mpr (h _)

/-- A formula that is false under some assignment is not valid. -/
theorem not_valid_of_eval_false {f : Formula} {σ : Assignment} (h : f.eval σ = false) :
    ¬f.Valid := fun valid => by simp [valid σ] at h

/-- A formula is valid exactly when its negation is unsatisfiable. -/
theorem valid_iff_not_satisfiable_not (f : Formula) : f.Valid ↔ ¬(not f).Satisfiable := by
  constructor
  · rintro h ⟨σ, hσ⟩
    simp [eval, h σ] at hσ
  · intro h σ
    cases hf : f.eval σ
    · exact absurd ⟨σ, by simp [eval, hf]⟩ h
    · rfl

end Formula

end Maithic.Oracle.Logic
