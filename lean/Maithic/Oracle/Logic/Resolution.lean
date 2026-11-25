/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Clause

/-!
# Checking resolution proofs

A resolution proof starts from the clauses of a clause set, numbered from zero, and adds one
clause per step: the resolvent of two earlier clauses on a literal that occurs in the first and
whose negation occurs in the second. A proof that reaches the empty clause shows that the clause
set is unsatisfiable (`Resolution.unsatisfiable_of_refutes`).

`Resolution.check` reads a proof step by step and reports the first step it cannot accept, with
the reason. The checker is a plain function, so the kernel can run it: a refutation is accepted by
evaluating `Resolution.refutes` to `true`.
-/

namespace Maithic.Oracle.Logic

/-- Why a proof was not accepted: the step at fault, counted from zero, and the reason. A step
number equal to the length of the proof means that every step was accepted but the proof does
not end in the empty clause. -/
structure Failure where
  /-- The number of the step at fault. -/
  step : Nat
  /-- What is wrong with it. -/
  reason : String
  deriving Repr, DecidableEq, Inhabited

instance : ToString Failure where
  toString e := s!"step {e.step}: {e.reason}"

namespace Resolution

/-- The resolvent of two clauses on a literal: the first without the literal, followed by the
second without its negation, each remaining literal once. -/
def resolvent (c d : Clause) (l : Literal) : Clause :=
  Clause.dedup (c.filter (· != l) ++ d.filter (· != l.negate))

/-- An assignment that satisfies two clauses satisfies each of their resolvents. -/
theorem eval_resolvent {σ : Assignment} {c d : Clause} (l : Literal)
    (hc : Clause.eval σ c = true) (hd : Clause.eval σ d = true) :
    Clause.eval σ (resolvent c d l) = true := by
  rw [resolvent, Clause.eval_dedup, Clause.eval_append, Bool.or_eq_true]
  cases hl : l.eval σ
  · obtain ⟨m, hm, hv⟩ := Clause.eval_eq_true_iff.mp hc
    refine Or.inl (Clause.eval_eq_true_iff.mpr ⟨m, ?_, hv⟩)
    refine List.mem_filter.mpr ⟨hm, ?_⟩
    have : m ≠ l := fun e => by rw [e, hl] at hv; cases hv
    simpa using this
  · obtain ⟨m, hm, hv⟩ := Clause.eval_eq_true_iff.mp hd
    refine Or.inr (Clause.eval_eq_true_iff.mpr ⟨m, ?_, hv⟩)
    refine List.mem_filter.mpr ⟨hm, ?_⟩
    have : m ≠ l.negate := fun e => by rw [e, Literal.eval_negate, hl] at hv; cases hv
    simpa using this

/-- One step of a resolution proof: resolve clause `left`, which contains `pivot`, with clause
`right`, which contains its negation. Clauses are numbered from zero: first those of the clause
set, then the resolvents in the order of the steps. -/
structure Step where
  /-- The number of the clause that contains the pivot. -/
  left : Nat
  /-- The number of the clause that contains the negation of the pivot. -/
  right : Nat
  /-- The literal resolved on. -/
  pivot : Literal
  deriving Repr, DecidableEq, Inhabited

/-- Runs the steps from the given clauses, returning all clauses at the end, or the first step
that cannot be accepted. `index` is the number of the first step. -/
def run (db : Cnf) (index : Nat) : List Step → Except Failure Cnf
  | [] => .ok db
  | s :: rest =>
    match db[s.left]?, db[s.right]? with
    | some c, some d =>
      if c.contains s.pivot && d.contains s.pivot.negate then
        run (db ++ [resolvent c d s.pivot]) (index + 1) rest
      else
        .error ⟨index, "the pivot does not occur in the two clauses with opposite signs"⟩
    | none, _ => .error ⟨index, s!"there is no clause {s.left}"⟩
    | some _, none => .error ⟨index, s!"there is no clause {s.right}"⟩

/-- Checks that the steps derive the empty clause from the clause set. -/
def check (φ : Cnf) (proof : List Step) : Except Failure Unit :=
  match run φ 0 proof with
  | .error e => .error e
  | .ok db =>
    if db.contains [] then .ok ()
    else .error ⟨proof.length, "the empty clause is not derived"⟩

/-- Whether the steps are a refutation of the clause set. -/
def refutes (φ : Cnf) (proof : List Step) : Bool :=
  match check φ proof with
  | .ok _ => true
  | .error _ => false

/-- Running accepted steps keeps every satisfying assignment satisfying. -/
theorem eval_run {σ : Assignment} :
    ∀ {steps : List Step} {db db' : Cnf} {index : Nat},
      run db index steps = .ok db' → Cnf.eval σ db = true → Cnf.eval σ db' = true
  | [], db, db', index, h, hdb => by
    simp only [run, Except.ok.injEq] at h
    exact h ▸ hdb
  | s :: rest, db, db', index, h, hdb => by
    simp only [run] at h
    split at h
    · rename_i c d hc hd
      split at h
      · refine eval_run h ?_
        have hc' := Cnf.eval_eq_true_iff.mp hdb c (List.mem_of_getElem? hc)
        have hd' := Cnf.eval_eq_true_iff.mp hdb d (List.mem_of_getElem? hd)
        simp [Cnf.eval_append, hdb, eval_resolvent s.pivot hc' hd']
      · cases h
    · cases h
    · cases h

/-- A clause set with a resolution refutation is unsatisfiable. -/
theorem unsatisfiable_of_refutes {φ : Cnf} {proof : List Step} (h : refutes φ proof = true) :
    φ.Unsatisfiable := by
  intro σ
  cases hφ : Cnf.eval σ φ
  · rfl
  · simp only [refutes, check] at h
    split at h
    · rename_i hcheck
      split at hcheck
      · cases hcheck
      · rename_i db hrun
        split at hcheck
        · rename_i hnil
          have := Cnf.eval_of_mem_nil (List.contains_iff_mem.mp hnil) σ
          rw [eval_run hrun hφ] at this
          cases this
        · cases hcheck
    · cases h

end Resolution

end Maithic.Oracle.Logic
