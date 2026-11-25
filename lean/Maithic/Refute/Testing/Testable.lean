/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Testing.Arbitrary

/-!
# Statements that can be tested

`Testable p` says how to run one test case of the proposition `p`. A decidable proposition is
tested by deciding it. A statement `∀ x, p x` is tested by generating an `x` and testing `p x`;
when that fails, `x` is shrunk for as long as the failure remains. An implication whose premise
is false proves nothing about the statement, so its case is discarded and does not count.

`check` runs the cases of a test and returns a `Report`: the statement passed the requested number
of cases, or it was falsified, with the values of its variables, or too many cases had to be
discarded. Passing is evidence, never a proof. A falsified statement comes with a counterexample
that can be checked on its own.

A candidate of shrinking replaces a value only if the statement still fails with the same draws
for the variables quantified inside it. The counterexample reported is therefore small, and the
same on every run, but it is not always the smallest one there is.
-/

namespace Maithic.Refute

/-- The proposition `p`, marked with the name of the variable it quantifies over, so that a
counterexample can name its values. -/
def NamedBinder (_name : String) (p : Prop) : Prop := p

/-- The settings of a test run. -/
structure Config where
  /-- The number of cases that must pass. -/
  cases : Nat := 100
  /-- The size of the last case; sizes grow from zero. -/
  size : Nat := 30
  /-- The seed of the random stream. -/
  seed : Nat := 0
  /-- The number of discarded cases after which the run gives up. -/
  discards : Nat := 1000
  /-- The number of shrinking steps allowed for one variable. -/
  shrinks : Nat := 1000
  deriving Repr, Inhabited, DecidableEq

/-- What one test case showed. -/
inductive CaseResult where
  /-- The statement held. -/
  | pass
  /-- A premise was false, so the case says nothing. -/
  | discard
  /-- The statement failed for these values of its variables, outermost first, after the given
  number of shrinking steps. -/
  | fail (bindings : List (String × String)) (shrinks : Nat)
  deriving Repr, Inhabited

/-- A proposition that can be tested by sampling. -/
class Testable (p : Prop) where
  /-- Runs one case. `budget` is the number of shrinking steps allowed for each variable. -/
  run (budget : Nat) : Gen CaseResult

/-- A decidable proposition is tested by deciding it. -/
instance (priority := low) decidableTestable {p : Prop} [Decidable p] : Testable p where
  run _ := pure (if p then .pass else .fail [] 0)

/-- An implication is tested when its premise holds, and discarded otherwise. -/
instance impTestable {p q : Prop} [Decidable p] [Testable q] : Testable (p → q) where
  run budget := if p then Testable.run q budget else pure .discard

/-- A conjunction fails when either side fails. -/
instance andTestable {p q : Prop} [Testable p] [Testable q] : Testable (p ∧ q) where
  run budget := do
    match ← Testable.run p budget with
    | .fail bindings shrinks => pure (.fail bindings shrinks)
    | first =>
      match ← Testable.run q budget with
      | .fail bindings shrinks => pure (.fail bindings shrinks)
      | .discard => pure .discard
      | .pass => pure first

/-- Shrinks a failing value. `fails` says whether a candidate still fails, and with which
bindings of the inner variables. The first failing candidate replaces the value, and the search
goes on from it until no candidate fails or the budget is spent. -/
def minimize [Shrinkable α] (fails : α → Option (List (String × String))) :
    Nat → α → List (String × String) → Nat → α × List (String × String) × Nat
  | 0, x, bindings, steps => (x, bindings, steps)
  | budget + 1, x, bindings, steps =>
    match (shrink x).findSome? fun candidate => (fails candidate).map (candidate, ·) with
    | some (candidate, bindings) => minimize fails budget candidate bindings (steps + 1)
    | none => (x, bindings, steps)

/-- One case of a quantified statement: generate a value, test the body, and shrink on failure.
The body is tested again on each candidate from the same point of the random stream, so that the
only thing that changes is the candidate. -/
def runForall {α : Type} {p : α → Prop} [Arbitrary α] [Shrinkable α] [Repr α]
    [∀ x, Testable (p x)] (name : String) (budget : Nat) : Gen CaseResult := do
  let x ← (arbitrary : Gen α)
  let size ← Gen.size
  let stream ← get
  match ← Testable.run (p x) budget with
  | .fail bindings shrinks =>
    let fails := fun candidate =>
      match (Gen.run (Testable.run (p candidate) budget) size stream).1 with
      | .fail bindings _ => some bindings
      | _ => none
    let (x, bindings, steps) := minimize fails budget x bindings 0
    pure (.fail ((name, reprStr x) :: bindings) (shrinks + steps))
  | other => pure other

/-- A quantified statement whose variable has a recorded name. -/
instance namedForallTestable {α : Type} {p : α → Prop} {name : String} [Arbitrary α] [Shrinkable α]
    [Repr α] [∀ x, Testable (p x)] : Testable (NamedBinder name (∀ x, p x)) where
  run := runForall (p := p) name

/-- A quantified statement whose variable has no recorded name; a counterexample calls it `_`. -/
instance (priority := low) forallTestable {α : Type} {p : α → Prop} [Arbitrary α] [Shrinkable α]
    [Repr α] [∀ x, Testable (p x)] : Testable (∀ x, p x) where
  run := runForall (p := p) "_"

/-- How a test run ended. -/
inductive Verdict where
  /-- The requested number of cases passed. -/
  | passed
  /-- A case failed. -/
  | falsified
  /-- Too many cases were discarded. -/
  | gaveUp
  deriving Repr, Inhabited, DecidableEq

/-- The result of a test run. -/
structure Report where
  /-- How the run ended. -/
  verdict : Verdict
  /-- The number of cases that were run to the end, the failing one included. -/
  cases : Nat
  /-- The number of cases discarded because a premise was false. -/
  discarded : Nat
  /-- The number of shrinking steps that led to the counterexample. -/
  shrinks : Nat
  /-- The seed of the run. -/
  seed : Nat
  /-- The values of the variables in the failing case, outermost first: name and value. -/
  counterexample : List (String × String)
  deriving Repr, Inhabited, DecidableEq

/-- The size of the case started after `index` others, growing from zero to the configured size
and staying there. Discarded cases count, so that a premise which small values cannot meet is
soon tried on larger ones. -/
def Config.sizeOf (cfg : Config) (index : Nat) : Nat :=
  if cfg.cases ≤ 1 then cfg.size else min cfg.size (index * cfg.size / (cfg.cases - 1))

/-- Runs cases until enough have passed, one fails, or too many are discarded. `fuel` bounds the
number of cases started. -/
def checkLoop (cfg : Config) (test : Gen CaseResult) : Nat → Nat → Nat → Rng → Report
  | 0, passed, discarded, _ =>
    { verdict := .gaveUp, cases := passed, discarded, shrinks := 0, seed := cfg.seed, counterexample := [] }
  | fuel + 1, passed, discarded, stream =>
    if passed ≥ cfg.cases then
      { verdict := .passed, cases := passed, discarded, shrinks := 0, seed := cfg.seed, counterexample := [] }
    else
      match Gen.run test (cfg.sizeOf (passed + discarded)) stream with
      | (.pass, stream) => checkLoop cfg test fuel (passed + 1) discarded stream
      | (.discard, stream) =>
        if discarded + 1 ≥ cfg.discards then
          { verdict := .gaveUp, cases := passed, discarded := discarded + 1, shrinks := 0, seed := cfg.seed,
            counterexample := [] }
        else checkLoop cfg test fuel passed (discarded + 1) stream
      | (.fail bindings shrinks, _) =>
        { verdict := .falsified, cases := passed + 1, discarded, shrinks, seed := cfg.seed,
          counterexample := bindings }

/-- Tests a proposition by sampling. -/
def check (p : Prop) [Testable p] (cfg : Config := {}) : Report :=
  checkLoop cfg (Testable.run p cfg.shrinks) (cfg.cases + cfg.discards + 2) 0 0 (Rng.ofSeed cfg.seed)

/-- `n` followed by the noun, in the singular for one. -/
private def count (n : Nat) (singular plural : String) : String :=
  s!"{n} {if n = 1 then singular else plural}"

/-- The report as text: one line, followed by the counterexample, one variable per line. -/
def Report.toString (r : Report) : String :=
  match r.verdict with
  | .passed =>
    s!"passed {count r.cases "case" "cases"}" ++ (if r.discarded = 0 then "" else s!", {r.discarded} discarded")
  | .gaveUp => s!"gave up: {count r.discarded "case" "cases"} discarded, {r.cases} passed"
  | .falsified =>
    let head := s!"falsified at case {r.cases}" ++
      (if r.shrinks = 0 then "" else s!", shrunk {count r.shrinks "time" "times"}")
    r.counterexample.foldl (fun text (name, value) => text ++ s!"\n  {name} := {value}") head

instance : ToString Report := ⟨Report.toString⟩

end Maithic.Refute
