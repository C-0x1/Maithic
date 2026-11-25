/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Resolution

/-!
# Checking clausal proofs

The proofs that satisfiability solvers write: a list of clauses, each of which follows from the
clauses before it by unit propagation. A step names the clauses to propagate with, in order (its
hints), so checking a step is a single pass over the hints and needs no search. This is the
`RUP` fragment of the LRAT format; steps that introduce a clause by the `RAT` rule are not
accepted.

To check that a clause follows, assume all of its literals false. Each hint must then be a clause
with exactly one literal that is not yet false, which becomes true, or with none, which is the
conflict that ends the step. A proof is accepted when it adds the empty clause.

`Lrat.unsatisfiable_of_refutes` is the soundness theorem: a clause set with an accepted proof is
unsatisfiable. `Lrat.check` reports the first step that fails and why, and `Lrat.parse` reads the
text form, in which the clauses of the problem are numbered from one, as solvers number them.
-/

namespace Maithic.Oracle.Logic.Lrat

/-- A step of a clausal proof. -/
inductive Step where
  /-- Adds `clause` under the number `id`. Unit propagation over the clauses numbered by `hints`,
  in that order, must refute the negation of the clause. -/
  | add (id : Nat) (clause : Clause) (hints : List Nat)
  /-- Removes the clauses with the given numbers. Later steps cannot use them. -/
  | delete (ids : List Nat)
  deriving Repr, DecidableEq, Inhabited

/-- The clauses in play, each with its number. -/
abbrev Database := List (Nat × Clause)

/-- The clause with the given number; the one added last, should a number be used twice. -/
def find : Database → Nat → Option Clause
  | [], _ => none
  | (i, c) :: rest, id => if i = id then some c else find rest id

theorem mem_of_find : ∀ {db : Database} {id : Nat} {c : Clause}, find db id = some c → (id, c) ∈ db
  | [], _, _, h => by cases h
  | (i, d) :: rest, id, c, h => by
    simp only [find] at h
    split at h
    · rename_i hid
      cases h
      exact hid ▸ List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ (mem_of_find h)

/-- The clauses of a clause set, numbered from `start`. -/
def number : Nat → Cnf → Database
  | _, [] => []
  | start, c :: φ => (start, c) :: number (start + 1) φ

theorem mem_of_mem_number : ∀ {φ : Cnf} {start i : Nat} {c : Clause}, (i, c) ∈ number start φ → c ∈ φ
  | [], _, _, _, h => by cases h
  | d :: φ, start, i, c, h => by
    simp only [number, List.mem_cons, Prod.mk.injEq] at h
    rcases h with ⟨_, rfl⟩ | h
    · exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ (mem_of_mem_number h)

/-- An assignment satisfies a database when it makes every clause in it true. -/
def Satisfies (σ : Assignment) (db : Database) : Prop :=
  ∀ i c, (i, c) ∈ db → Clause.eval σ c = true

/-- How unit propagation along a list of hints ended. -/
inductive Propagation where
  /-- A hint had all of its literals false: the assumption is refuted. -/
  | conflict
  /-- The hints ran out before a conflict. -/
  | stuck
  /-- A hint names a clause that does not exist. -/
  | missing (id : Nat)
  /-- A hint had two or more literals that were not yet false. -/
  | notUnit (id : Nat)
  deriving Repr, DecidableEq, Inhabited

/-- Unit propagation over the hinted clauses, in order. `trail` lists the literals taken to be
true so far; a literal counts as false when its negation is on the trail. -/
def propagate (db : Database) : List Literal → List Nat → Propagation
  | _, [] => .stuck
  | trail, h :: hints =>
    match find db h with
    | none => .missing h
    | some c =>
      match c.filter fun l => !trail.contains l.negate with
      | [] => .conflict
      | [l] => propagate db (l :: trail) hints
      | _ :: _ :: _ => .notUnit h

/-- Propagation from true literals over satisfied clauses never reaches a conflict. -/
theorem propagate_ne_conflict {σ : Assignment} {db : Database} (hdb : Satisfies σ db) :
    ∀ (hints : List Nat) (trail : List Literal), (∀ l, l ∈ trail → l.eval σ = true) →
      propagate db trail hints ≠ .conflict
  | [], _, _ => by simp [propagate]
  | h :: hints, trail, htrail => by
    simp only [propagate]
    split
    · simp
    · rename_i c hc
      obtain ⟨m, hm, hv⟩ := Clause.eval_eq_true_iff.mp (hdb h c (mem_of_find hc))
      have hkept : m ∈ c.filter fun l => !trail.contains l.negate := by
        refine List.mem_filter.mpr ⟨hm, ?_⟩
        cases hn : trail.contains m.negate
        · rfl
        · have := htrail _ (List.contains_iff_mem.mp hn)
          rw [Literal.eval_negate, hv] at this
          cases this
      split
      · rename_i hnil
        rw [hnil] at hkept
        cases hkept
      · rename_i l hl
        rw [hl] at hkept
        have hml : m = l := by simpa using hkept
        refine propagate_ne_conflict hdb hints (l :: trail) fun k hk => ?_
        rcases List.mem_cons.mp hk with rfl | hk
        · exact hml ▸ hv
        · exact htrail k hk
      · simp

/-- A clause whose negation is refuted by unit propagation over satisfied clauses is true. -/
theorem eval_of_propagate {σ : Assignment} {db : Database} (hdb : Satisfies σ db) {c : Clause}
    {hints : List Nat} (h : propagate db (c.map Literal.negate) hints = .conflict) :
    Clause.eval σ c = true := by
  cases hv : Clause.eval σ c
  · refine absurd h (propagate_ne_conflict hdb hints _ fun l hl => ?_)
    obtain ⟨m, hm, rfl⟩ := List.mem_map.mp hl
    have hfalse : m.eval σ = false := by
      cases hmv : m.eval σ
      · rfl
      · rw [Clause.eval_eq_true_iff.mpr ⟨m, hm, hmv⟩] at hv
        cases hv
    simp [hfalse]
  · rfl

/-- Runs the steps on a database. The result is `ok` when a step adds the empty clause; otherwise
it is the first step that cannot be accepted. `index` is the number of the first step. -/
def run (db : Database) (index : Nat) : List Step → Except Failure Unit
  | [] => .error ⟨index, "the empty clause is not derived"⟩
  | .delete ids :: rest => run (db.filter fun entry => !ids.contains entry.1) (index + 1) rest
  | .add id clause hints :: rest =>
    match propagate db (clause.map Literal.negate) hints with
    | .conflict => if clause.isEmpty then .ok () else run ((id, clause) :: db) (index + 1) rest
    | .stuck =>
      .error ⟨index, s!"clause {id}: unit propagation along the hints ends without a conflict"⟩
    | .missing h => .error ⟨index, s!"clause {id}: there is no clause {h} to use as a hint"⟩
    | .notUnit h =>
      .error ⟨index, s!"clause {id}: hint {h} has more than one literal that is not yet false"⟩

/-- Checks a clausal proof of the unsatisfiability of a clause set, whose clauses are numbered
from one. -/
def check (φ : Cnf) (proof : List Step) : Except Failure Unit :=
  run (number 1 φ) 0 proof

/-- Whether the steps are an accepted proof that the clause set is unsatisfiable. -/
def refutes (φ : Cnf) (proof : List Step) : Bool :=
  match check φ proof with
  | .ok _ => true
  | .error _ => false

/-- No assignment satisfies a database on which a proof is accepted. -/
theorem not_satisfies_of_run {σ : Assignment} :
    ∀ {steps : List Step} {db : Database} {index : Nat}, run db index steps = .ok () →
      ¬Satisfies σ db
  | [], _, _, h, _ => by cases h
  | .delete ids :: rest, db, index, h, hdb => by
    simp only [run] at h
    exact not_satisfies_of_run h fun i c hc => hdb i c (List.mem_filter.mp hc).1
  | .add id clause hints :: rest, db, index, h, hdb => by
    simp only [run] at h
    split at h
    · rename_i hprop
      have hclause := eval_of_propagate hdb hprop
      split at h
      · rename_i hempty
        rw [List.isEmpty_iff.mp hempty] at hclause
        cases hclause
      · refine not_satisfies_of_run (σ := σ) h fun i c hc => ?_
        rcases List.mem_cons.mp hc with hic | hc
        · cases hic
          exact hclause
        · exact hdb i c hc
    · cases h
    · cases h
    · cases h

/-- Soundness: a clause set with an accepted clausal proof is unsatisfiable. -/
theorem unsatisfiable_of_refutes {φ : Cnf} {proof : List Step} (h : refutes φ proof = true) :
    φ.Unsatisfiable := by
  intro σ
  cases hφ : Cnf.eval σ φ
  · rfl
  · simp only [refutes, check] at h
    split at h
    · rename_i hrun
      have hrun' : run (number 1 φ) 0 proof = .ok () := hrun
      exact absurd (fun i c hc => Cnf.eval_eq_true_iff.mp hφ c (mem_of_mem_number hc))
        (not_satisfies_of_run (σ := σ) hrun')
    · cases h

/-! ## The text form -/

/-- Reads one line of a proof: `id literals 0 hints 0` adds a clause, `id d ids 0` deletes
clauses, and an empty line or a line that starts with `c` says nothing. -/
def parseLine (line : String) : Except String (Option Step) := do
  match words line with
  | [] => return none
  | "c" :: _ => return none
  | first :: rest =>
    let some id := first.toNat? | throw s!"\"{first}\" is not a clause number"
    match rest with
    | "d" :: ids =>
      let numbers : List Nat ← ids.mapM fun w =>
        match w.toNat? with
        | some n => pure n
        | none => throw s!"\"{w}\" is not a clause number"
      match numbers.reverse with
      | 0 :: body => return some (.delete body.reverse)
      | _ => throw "a deletion ends with 0"
    | _ =>
      let numbers : List Int ← rest.mapM fun w =>
        match w.toInt? with
        | some n => pure n
        | none => throw s!"\"{w}\" is not a number"
      match numbers.dropWhile (· != 0) with
      | [] => throw "the literals end with 0"
      | _ :: tail =>
        match tail.reverse with
        | 0 :: hints =>
          if hints.any (· < 0) then
            throw "hints with a minus sign belong to RAT steps, which are not accepted"
          if hints.any (· == 0) then throw "a step has one list of hints"
          let literals := (numbers.takeWhile (· != 0)).filterMap Literal.ofDimacs
          return some (.add id literals (hints.reverse.map Int.toNat))
        | _ => throw "the hints end with 0"

/-- Reads a proof in the text form, one step per line. Errors name the line, counted from one. -/
def parse (text : String) : Except String (List Step) := do
  let mut steps : Array Step := #[]
  let mut number := 1
  for line in text.splitOn "\n" do
    match parseLine line with
    | .ok none => pure ()
    | .ok (some step) => steps := steps.push step
    | .error e => throw s!"line {number}: {e}"
    number := number + 1
  return steps.toList

/-- A step in the text form. -/
def Step.toText : Step → String
  | .add id clause hints =>
    String.intercalate " " ([toString id] ++ clause.map (fun l => toString l.toDimacs) ++ ["0"] ++
      hints.map toString ++ ["0"])
  | .delete ids => String.intercalate " " ([toString (ids.foldl max 0 + 1), "d"] ++ ids.map toString ++ ["0"])

/-- A proof in the text form, one step per line. -/
def toText (proof : List Step) : String :=
  String.join (proof.map fun step => step.toText ++ "\n")

end Maithic.Oracle.Logic.Lrat
