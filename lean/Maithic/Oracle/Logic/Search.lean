/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Lrat

/-!
# Finding clausal proofs

A small satisfiability search that writes down what it learns. It decides an atom, propagates
units, and when a clause becomes false it records the clause that rules the current decisions
out, with the propagation that shows it. When the search has ruled everything out, the recorded
clauses are a proof in the form `Lrat.check` reads; when it finds an assignment instead, the
assignment is the answer.

Nothing here is trusted and nothing here is proved: the proof is checked by `Lrat.check`, whose
soundness is a theorem, and a model is checked by evaluating the clauses under it
(`Search.Outcome.confirmed`). The search is meant for goals of a few dozen atoms; larger problems
belong to a dedicated solver, whose proofs the same checker reads.
-/

namespace Maithic.Oracle.Logic.Search

open Lrat (Database Step)

/-- What a pass over the clauses finds under the literals on the trail. -/
inductive Scan where
  /-- A clause all of whose literals are false. -/
  | conflict (id : Nat)
  /-- A clause with one literal left open and all others false. -/
  | unit (id : Nat) (l : Literal)
  /-- Neither. -/
  | nothing

/-- The first clause that is false or unit under the trail. Satisfied clauses are passed over. -/
def scan (trail : List Literal) : Database → Scan
  | [] => .nothing
  | (id, c) :: rest =>
    if c.any trail.contains then scan trail rest
    else
      match c.filter fun l => !trail.contains l.negate with
      | [] => .conflict id
      | [l] => .unit id l
      | _ => scan trail rest

/-- Unit propagation to the end: the clauses used, in order, and the final trail, or `none` in
its place when a clause became false. Each round but the last fixes one more atom, so one round
more than there are atoms is enough. -/
def saturate (db : Database) : Nat → List Literal → Array Nat → Array Nat × Option (List Literal)
  | 0, trail, used => (used, some trail)
  | fuel + 1, trail, used =>
    match scan trail db with
    | .conflict id => (used.push id, none)
    | .unit id l => saturate db fuel (l :: trail) (used.push id)
    | .nothing => (used, some trail)

/-- The clauses known to the search, the number for the next one, and the proof so far. -/
structure State where
  /-- The clauses of the problem and those learned since. -/
  db : Database
  /-- The number the next learned clause gets. -/
  nextId : Nat
  /-- The steps recorded so far, oldest first. -/
  proof : Array Step

/-- How a branch of the search ended. -/
inductive Branch where
  /-- The decisions of the branch are ruled out by the clause with this number. -/
  | closed (id : Nat)
  /-- Every clause is true under these literals. -/
  | model (trail : List Literal)
  /-- The budget ran out. -/
  | exhausted

/-- Searches below the given decisions. On a conflict the clause that forbids the decisions is
learned; the caller then propagates again, and the learned clause turns its last decision around.
`atoms` is the order in which atoms are decided, and `fuel` bounds the depth. -/
def search (atoms : List Nat) : Nat → List Literal → State → Branch × State
  | 0, _, state => (.exhausted, state)
  | fuel + 1, decisions, state =>
    match saturate state.db (atoms.length + 1) decisions #[] with
    | (used, none) =>
      let clause := decisions.map Literal.negate
      (.closed state.nextId,
        { db := (state.nextId, clause) :: state.db, nextId := state.nextId + 1,
          proof := state.proof.push (.add state.nextId clause used.toList) })
    | (_, some trail) =>
      match atoms.find? fun a => !trail.any (·.atom == a) with
      | none => (.model trail, state)
      | some a =>
        match search atoms fuel (⟨a, true⟩ :: decisions) state with
        | (.closed _, state) => search atoms fuel decisions state
        | other => other

/-- The answer of the search. -/
inductive Outcome where
  /-- The clause set is unsatisfiable, with a proof for `Lrat.check`. -/
  | refuted (proof : List Step)
  /-- The clause set is true when exactly these atoms are true. -/
  | satisfiable (trueAtoms : List Nat)
  /-- The search stopped at its budget. -/
  | unknown
  deriving Repr, Inhabited

/-- The atoms of a clause set, each once, in the order of their first occurrence. -/
def atomsOf (φ : Cnf) : List Nat :=
  (φ.foldl (fun seen c => c.foldl (fun seen l => if seen.contains l.atom then seen else seen.push l.atom) seen)
    (#[] : Array Nat)).toList

/-- Decides a clause set by search. `depth` bounds the depth of the search; the default is
enough for the search to finish. -/
def solve (φ : Cnf) (depth : Option Nat := none) : Outcome :=
  let atoms := atomsOf φ
  let start : State := { db := Lrat.number 1 φ, nextId := φ.length + 1, proof := #[] }
  match search atoms (depth.getD (2 * atoms.length + 2)) [] start with
  | (.closed _, state) => .refuted state.proof.toList
  | (.model trail, _) => .satisfiable ((trail.filter (·.positive)).map (·.atom))
  | (.exhausted, _) => .unknown

/-- Whether an outcome stands up to checking: a proof is accepted by `Lrat.check`, and a model
makes every clause true. An outcome of `unknown` claims nothing. -/
def Outcome.confirmed (φ : Cnf) : Outcome → Bool
  | .refuted proof => Lrat.refutes φ proof
  | .satisfiable trueAtoms => Cnf.eval (fun i => trueAtoms.contains i) φ
  | .unknown => true

end Maithic.Oracle.Logic.Search
