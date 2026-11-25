/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Table
import Maithic.Oracle.Logic.Lrat

/-!
# From an accepted proof to the goal

The theorems the tactics apply. Each takes the evidence that a checker returned `true`, a fact
the kernel establishes by running the checker, and concludes the proposition that the formula
denotes. Their proofs chain the pieces proved elsewhere: the conversion to clauses keeps truth
values, an accepted proof means the clauses are unsatisfiable, an unsatisfiable negation means
the formula is valid, and a valid formula denotes a true proposition.

The other direction is here as well: an assignment under which a formula is false shows that the
formula is not valid, and that the statement "it holds for all propositions" is false.
-/

namespace Maithic.Oracle.Logic

/-- The propositions of a list by position. Positions past the end stand for `True`. -/
def atomsOf : List Prop → Nat → Prop
  | [], _ => True
  | p :: _, 0 => p
  | _ :: ps, i + 1 => atomsOf ps i

/-- The assignment under which exactly the listed atoms are true. -/
def Assignment.ofTrueAtoms (trueAtoms : List Nat) : Assignment := fun i => trueAtoms.contains i

namespace Formula

/-- A formula whose truth table is all true denotes a true proposition. -/
theorem denote_of_isTautology {f : Formula} (h : f.isTautology = true) (ρ : Nat → Prop) :
    f.denote ρ :=
  denote_of_valid (valid_of_isTautology h) ρ

/-- A formula denotes a true proposition when a clausal proof refutes its negation. -/
theorem denote_of_lrat {f : Formula} {proof : List Lrat.Step}
    (h : Lrat.refutes f.refutation proof = true) (ρ : Nat → Prop) : f.denote ρ :=
  denote_of_valid (valid_of_refutation_unsatisfiable (Lrat.unsatisfiable_of_refutes h)) ρ

/-- A formula denotes a true proposition when a resolution proof refutes its negation. -/
theorem denote_of_resolution {f : Formula} {proof : List Resolution.Step}
    (h : Resolution.refutes f.refutation proof = true) (ρ : Nat → Prop) : f.denote ρ :=
  denote_of_valid (valid_of_refutation_unsatisfiable (Resolution.unsatisfiable_of_refutes h)) ρ

/-- Whether the formula is false when exactly the listed atoms are true. -/
def falsifiedBy (f : Formula) (trueAtoms : List Nat) : Bool :=
  !f.eval (Assignment.ofTrueAtoms trueAtoms)

/-- A formula with a falsifying assignment is not valid. -/
theorem not_valid_of_falsifiedBy {f : Formula} {trueAtoms : List Nat}
    (h : f.falsifiedBy trueAtoms = true) : ¬f.Valid :=
  not_valid_of_eval_false (σ := Assignment.ofTrueAtoms trueAtoms) (by simpa [falsifiedBy] using h)

/-- A falsifying assignment refutes the proposition a formula denotes when its atoms are read as
the assignment says: `True` for the listed atoms and `False` for the others. -/
theorem not_denote_of_falsifiedBy {f : Formula} {trueAtoms : List Nat}
    (h : f.falsifiedBy trueAtoms = true) : ¬f.denote fun i => i ∈ trueAtoms := by
  intro hd
  have := (denote_iff_eval (σ := Assignment.ofTrueAtoms trueAtoms)
    (fun i => by simp [Assignment.ofTrueAtoms]) f).mp hd
  simp [falsifiedBy, this] at h

/-- A falsifying assignment refutes the claim that a formula holds for all propositions. -/
theorem not_forall_denote_of_falsifiedBy {f : Formula} {trueAtoms : List Nat}
    (h : f.falsifiedBy trueAtoms = true) : ¬∀ ρ : Nat → Prop, f.denote ρ :=
  fun all => not_denote_of_falsifiedBy h (all _)

end Formula

/-- A clause set that is true under some listed assignment is not unsatisfiable. -/
theorem Cnf.not_unsatisfiable_of_model {φ : Cnf} {trueAtoms : List Nat}
    (h : Cnf.eval (Assignment.ofTrueAtoms trueAtoms) φ = true) : ¬φ.Unsatisfiable :=
  fun unsat => by simp [unsat _] at h

end Maithic.Oracle.Logic
