/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean.Data.Json
import Maithic.Audit.Axioms
import Maithic.Oracle.Logic
import MaithicTest.Unit

/-!
# Tests of the logic checkers

Compile-time checks of the checkers on concrete inputs, of the messages of the tactics, and of
the axioms behind the soundness theorems; and run-time tests that read the proof fixtures in
`fixtures/oracle/logic/` and compare the verdict of the checker with the one the manifest
expects.

The theorems are proved in the library. What is tested here is behaviour: that accepted proofs
are accepted, and above all that proofs with a flaw are rejected at the right step.
-/

namespace MaithicTest.Logic

open Lean (Json FromJson)
open Maithic.Oracle.Logic

/-- The failure of a check, if it failed. -/
def failureOf : Except Failure Unit → Option Failure
  | .ok _ => none
  | .error e => some e

/-- The positive literal of an atom. -/
abbrev pos (i : Nat) : Literal := ⟨i, true⟩

/-- The negative literal of an atom. -/
abbrev neg (i : Nat) : Literal := ⟨i, false⟩

/-- A clause set that is satisfied by making atom 1 true. -/
def satisfiable : Cnf := [[pos 0, pos 1], [neg 0, pos 1]]

/-- Three pigeons in two holes: atom `2 * i + j` says that pigeon `i` sits in hole `j`. -/
def pigeons : Cnf :=
  [[pos 0, pos 1], [pos 2, pos 3], [pos 4, pos 5],
   [neg 0, neg 2], [neg 0, neg 4], [neg 2, neg 4],
   [neg 1, neg 3], [neg 1, neg 5], [neg 3, neg 5]]

/-- A clausal proof that the pigeons do not fit. -/
def pigeonProof : List Lrat.Step :=
  [.add 10 [neg 0] [4, 2, 5, 3, 7, 9], .add 11 [] [10, 1, 7, 2, 6, 3, 8]]

/-- The eight assignments of three atoms. -/
def assignments : List Assignment :=
  (List.range 8).map fun n i => (n >>> i) % 2 == 1

/-- Formulas that use every connective, over three atoms. -/
def samples : List Formula :=
  let a := Formula.atom 0
  let b := Formula.atom 1
  let c := Formula.atom 2
  [a, .tt, .ff, .not a, .and a b, .or a b, .imp a b, .iff a b,
   .imp (.and a (.imp a b)) b, .iff (.not (.and a b)) (.or (.not a) (.not b)),
   .or (.and a b) (.and (.not a) c), .iff (.iff a b) (.iff b c), .imp (.imp (.imp a b) a) a,
   .not (.iff a (.not a)), .and (.or a b) (.and (.or (.not a) c) (.not c)),
   .imp .ff a, .imp a .tt, .iff .tt (.or a (.not a)), .and (.iff a .ff) (.iff b .tt)]

/-! ## Reflection -/

section
variable (p q r : Prop)

-- cap: P13-04-01
/--
info: ((a0 ∧ a1) → (a1 ∨ ¬a2))
where a0 := p, a1 := q, a2 := r
-/
#guard_msgs in
#maithic_reflect p ∧ q → q ∨ ¬r

-- cap: P13-04-01
-- The same proposition is the same atom wherever it occurs.
/--
info: ((a0 ↔ a1) → (a1 → a0))
where a0 := p, a1 := q
-/
#guard_msgs in
#maithic_reflect (p ↔ q) → (q → p)

-- cap: P13-04-01
-- A quantified statement is one atom: its inside is not propositional structure.
/--
info: (a0 → a0)
where a0 := ∀ (x : Nat), x = x
-/
#guard_msgs in
#maithic_reflect (∀ x : Nat, x = x) → ∀ x : Nat, x = x

-- cap: P13-04-01
-- `≠` is read as the negation of an equation, so it shares its atom with the equation.
/--
info: ((¬a0 ∧ a0) → False)
where a0 := 1 = 2
-/
#guard_msgs in
#maithic_reflect (1 : Nat) ≠ 2 ∧ (1 : Nat) = 2 → False

-- cap: P13-04-01
/--
info: (True ∨ a0)
where a0 := p
-/
#guard_msgs in
#maithic_reflect True ∨ p

end

/-! ## Clause normal form -/

-- cap: P13-04-02
#guard (Formula.and (.atom 0) (.atom 1)).cnf true == [[pos 0], [pos 1]]
-- cap: P13-04-02
#guard (Formula.and (.atom 0) (.atom 1)).cnf false == [[neg 0, neg 1]]
-- cap: P13-04-02
#guard (Formula.imp (.atom 0) (.atom 1)).cnf true == [[neg 0, pos 1]]
-- cap: P13-04-02
#guard (Formula.imp (.atom 0) (.atom 1)).refutation == [[pos 0], [neg 1]]
-- cap: P13-04-02
#guard (Formula.iff (.atom 0) (.atom 1)).cnf true == [[neg 0, pos 1], [pos 0, neg 1]]
-- cap: P13-04-02
#guard Formula.tt.cnf true == [] && Formula.tt.cnf false == [[]]
-- cap: P13-04-02
#guard Formula.ff.cnf true == [[]] && Formula.ff.cnf false == []
-- cap: P13-04-02
-- Simplification drops clauses that are always true and literals that repeat.
#guard Cnf.simplify [[pos 0, neg 0], [pos 1, pos 1], [neg 2]] == [[pos 1], [neg 2]]
-- cap: P13-04-02
-- On every sample and every assignment the clauses agree with the formula, in both polarities.
#guard samples.all fun f => assignments.all fun σ =>
  Cnf.eval σ (f.cnf true) == f.eval σ && Cnf.eval σ (f.cnf false) == !f.eval σ &&
    Cnf.eval σ f.refutation == !f.eval σ
-- cap: P13-04-02
example (σ : Assignment) (f : Formula) : Cnf.eval σ f.refutation = !f.eval σ :=
  Formula.eval_refutation σ f
-- cap: P13-04-02
#guard Examples.twoAtoms.toDimacs == "p cnf 2 4\n1 2 0\n-1 2 0\n1 -2 0\n-1 -2 0\n"
-- cap: P13-04-02
#guard (Cnf.parseDimacs Examples.twoAtoms.toDimacs).toOption == some Examples.twoAtoms
-- cap: P13-04-02
#guard (Cnf.parseDimacs "c two clauses over three lines\n1 -2\n0 3 0\n").toOption ==
  some [[pos 0, neg 1], [pos 2]]
-- cap: P13-04-02
#guard (Cnf.parseDimacs "p cnf 2 2\n1 2 0\n").toOption == none
-- cap: P13-04-02
#guard (Cnf.parseDimacs "1 2\n").toOption == none
-- cap: P13-04-02
#guard Literal.ofDimacs (-3) == some (neg 2) && Literal.ofDimacs 0 == none
-- cap: P13-04-02
#guard (neg 2).toDimacs == -3 && (pos 0).toDimacs == 1

/-! ## The truth table -/

-- cap: P13-04-05
-- The truth table agrees with evaluation under all assignments.
#guard samples.all fun f => f.isTautology == assignments.all fun σ => f.eval σ
-- cap: P13-04-05
example : (Formula.imp (.and (.atom 0) (.imp (.atom 0) (.atom 1))) (.atom 1)).Valid := by decide
-- cap: P13-04-05
example : ¬(Formula.imp (.atom 0) (.atom 1)).Valid := by decide
-- cap: P13-04-05
example : (Formula.and (.atom 0) (.not (.atom 1))).Satisfiable := by decide

/-! ## Resolution proofs -/

-- cap: P13-04-03
#guard Resolution.resolvent [pos 0, pos 1] [neg 0, pos 1] (pos 0) == [pos 1]
-- cap: P13-04-03
#guard Resolution.resolvent [pos 0, pos 1] [neg 0, pos 2] (pos 0) == [pos 1, pos 2]
-- cap: P13-04-03
#guard Resolution.refutes Examples.twoAtoms [⟨0, 1, pos 0⟩, ⟨2, 3, pos 0⟩, ⟨4, 5, pos 1⟩]
-- cap: P13-04-03
-- A proof that stops before the empty clause is not a refutation.
#guard failureOf (Resolution.check Examples.twoAtoms [⟨0, 1, pos 0⟩, ⟨2, 3, pos 0⟩]) ==
  some ⟨2, "the empty clause is not derived"⟩
-- cap: P13-04-03
#guard failureOf (Resolution.check Examples.twoAtoms [⟨0, 1, pos 0⟩, ⟨2, 9, pos 0⟩]) ==
  some ⟨1, "there is no clause 9"⟩
-- cap: P13-04-03
-- The pivot must occur with opposite signs: clauses 0 and 2 both contain the positive literal.
#guard failureOf (Resolution.check Examples.twoAtoms [⟨0, 2, pos 0⟩]) ==
  some ⟨0, "the pivot does not occur in the two clauses with opposite signs"⟩
-- cap: P13-04-03
-- A step may only use clauses that exist when it runs: clause 4 is added by the first step of
-- the proof in its right order.
#guard failureOf (Resolution.check Examples.twoAtoms [⟨4, 5, pos 1⟩, ⟨2, 3, pos 0⟩, ⟨0, 1, pos 0⟩]) ==
  some ⟨0, "there is no clause 4"⟩
-- cap: P13-04-03
#guard failureOf (Resolution.check Examples.twoAtoms [⟨0, 1, pos 0⟩, ⟨4, 5, pos 1⟩]) ==
  some ⟨1, "there is no clause 5"⟩
-- cap: P13-04-03
#guard !Resolution.refutes satisfiable [⟨0, 1, pos 0⟩]
-- cap: P13-04-03
#guard Resolution.refutes [[]] []

/-! ## Clausal proofs -/

-- cap: P13-04-04
#guard Lrat.refutes Examples.twoAtoms [.add 5 [pos 1] [1, 2], .add 6 [] [5, 3, 4]]
-- cap: P13-04-04
#guard Lrat.refutes pigeons pigeonProof
-- cap: P13-04-04
-- A deleted clause is gone: the same proof fails once clause 3 is deleted before its use.
#guard failureOf (Lrat.check Examples.twoAtoms [.add 5 [pos 1] [1, 2], .delete [3], .add 6 [] [5, 3, 4]]) ==
  some ⟨2, "clause 6: there is no clause 3 to use as a hint"⟩
-- cap: P13-04-04
-- Deleting clauses that are no longer needed changes nothing.
#guard Lrat.refutes Examples.twoAtoms [.add 5 [pos 1] [1, 2], .delete [1, 2], .add 6 [] [5, 3, 4]]
-- cap: P13-04-04
#guard (Lrat.parse "c a comment\n5 2 0 1 2 0\n\n5 d 1 2 0\n6 0 5 3 4 0\n").toOption ==
  some [.add 5 [pos 1] [1, 2], .delete [1, 2], .add 6 [] [5, 3, 4]]
-- cap: P13-04-04
#guard (Lrat.parse (Lrat.toText pigeonProof)).toOption == some pigeonProof
-- cap: P13-04-04
#guard Lrat.toText pigeonProof == "10 -1 0 4 2 5 3 7 9 0\n11 0 10 1 7 2 6 3 8 0\n"
-- cap: P13-04-04
#guard (match Lrat.parse "5 2 0 1 2 0\n6 x 0 0\n" with | .error e => e | .ok _ => "") ==
  "line 2: \"x\" is not a number"
-- cap: P13-04-04
#guard (match Lrat.parse "5 2 0 1 2\n" with | .error e => e | .ok _ => "") ==
  "line 1: the hints end with 0"
-- cap: P13-04-04
#guard (match Lrat.parse "5 2 1\n" with | .error e => e | .ok _ => "") ==
  "line 1: the literals end with 0"

section
variable (p q r : Prop)

-- cap: P13-04-04
example (hp : p) (hpq : p → q) : q := by maithic_lrat "4 0 1 2 3 0"

-- cap: P13-04-04
-- `maithic_cnf` prints the problem a proof must refute, with the meaning of each variable.
/--
info: c 1 := p
c 2 := q
p cnf 2 3
1 0
-1 2 0
-2 0
-/
#guard_msgs in
example (hp : p) (hpq : p → q) : q := by
  maithic_cnf
  maithic_lrat "4 0 1 2 3 0"

-- cap: P13-04-04
/-- error: maithic_lrat: step 0: clause 4: unit propagation along the hints ends without a conflict -/
#guard_msgs in
example (hp : p) (hpq : p → q) : q := by maithic_lrat "4 0 1 2 0"

-- cap: P13-04-04
/-- error: maithic_lrat: line 1: "x" is not a number -/
#guard_msgs in
example (hp : p) (hpq : p → q) : q := by maithic_lrat "4 0 1 x 0"

-- cap: P13-04-04
example (hpq : p → q) (hqr : q → r) : p → r := by maithic_sat

-- cap: P13-04-04
example : (p ↔ q) ↔ (q ↔ p) := by maithic_sat

-- cap: P13-04-04
example (h : False) : p := by maithic_sat

-- cap: P13-04-04
example (x y : Nat) (h : x ≠ y) : ¬x = y := by maithic_sat

-- cap: P13-04-04
example : ((p → q) → p) → p := by maithic_taut

-- cap: P13-04-04
example (x : Nat) (h : x < 3 ∨ x = 5) (h' : ¬x < 3) : x = 5 := by maithic_taut

-- cap: P13-04-06
-- A goal that does not follow is answered with truth values under which it fails.
/--
error: maithic_sat: the goal does not follow by propositional reasoning; it fails when
  p := True
  q := False
-/
#guard_msgs in
example : p → q := by maithic_sat

-- cap: P13-04-06
/--
error: maithic_taut: the goal does not follow by propositional reasoning; it fails when
  p := False
  q := True
-/
#guard_msgs in
example (h : p ∨ q) : p := by maithic_taut

end

/-! ## Soundness -/

-- cap: P13-04-05
example {f : Formula} {proof : List Lrat.Step} (h : Lrat.refutes f.refutation proof = true)
    (ρ : Nat → Prop) : f.denote ρ := Formula.denote_of_lrat h ρ
-- cap: P13-04-05
example {f : Formula} {proof : List Resolution.Step}
    (h : Resolution.refutes f.refutation proof = true) (ρ : Nat → Prop) : f.denote ρ :=
  Formula.denote_of_resolution h ρ
-- cap: P13-04-05
example {f : Formula} (h : f.isTautology = true) (ρ : Nat → Prop) : f.denote ρ :=
  Formula.denote_of_isTautology h ρ
-- cap: P13-04-05
example {φ : Cnf} {proof : List Lrat.Step} (h : Lrat.refutes φ proof = true) : φ.Unsatisfiable :=
  Lrat.unsatisfiable_of_refutes h
-- cap: P13-04-05
-- The denotation of a formula is the proposition itself, by definition.
example (p q : Prop) :
    (Formula.imp (.and (.atom 0) (.atom 1)) (.atom 1)).denote (atomsOf [p, q]) = (p ∧ q → q) := rfl
-- cap: P13-04-05
-- The search finds a refutation for every sample that is valid, and the checker accepts it; for
-- the others it finds a falsifying assignment, which evaluation confirms.
#guard samples.all fun f =>
  match Search.solve f.refutation with
  | .refuted proof => f.isTautology && Lrat.refutes f.refutation proof
  | .satisfiable trueAtoms => !f.isTautology && f.falsifiedBy trueAtoms
  | .unknown => false

/-! ## Models -/

-- cap: P13-04-06
#guard (Formula.imp (.atom 0) (.atom 1)).falsifiedBy [0]
-- cap: P13-04-06
#guard !(Formula.imp (.atom 0) (.atom 1)).falsifiedBy [0, 1]
-- cap: P13-04-06
#guard !(Formula.imp (.atom 0) (.atom 1)).falsifiedBy []
-- cap: P13-04-06
example : ¬(Formula.imp (.atom 0) (.atom 1)).Valid :=
  Formula.not_valid_of_falsifiedBy (trueAtoms := [0]) (by decide)
-- cap: P13-04-06
example : ¬∀ a b : Prop, a ∨ b → a ∧ b := fun h =>
  Formula.not_forall_denote_of_falsifiedBy
    (f := .imp (.or (.atom 0) (.atom 1)) (.and (.atom 0) (.atom 1))) (trueAtoms := [1])
    (by decide) fun ρ => h (ρ 0) (ρ 1)
-- cap: P13-04-06
#guard (Search.solve satisfiable).confirmed satisfiable
-- cap: P13-04-06
#guard (match Search.solve satisfiable with | .satisfiable m => m | _ => []) == [1, 0]
-- cap: P13-04-06
-- A model that does not satisfy the clauses is not confirmed.
#guard !(Search.Outcome.satisfiable [0]).confirmed satisfiable
-- cap: P13-04-06
example : ¬satisfiable.Unsatisfiable :=
  Cnf.not_unsatisfiable_of_model (trueAtoms := [1]) (by decide)

/-! ## Examples -/

-- cap: P13-04-07
example (p q : Prop) : ((p → q) → p) → p := Examples.peirce
-- cap: P13-04-07
example (p q : Prop) (hp : p) (hpq : p → q) : q := Examples.modus_ponens hp hpq
-- cap: P13-04-07
example : Examples.twoAtoms.Unsatisfiable := Examples.twoAtoms_unsatisfiable
-- cap: P13-04-07
example (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False :=
  Examples.no_integer_point x y h₁ h₂
-- cap: P13-04-07
#guard Lrat.refutes Examples.twoAtoms [.add 5 [pos 1] [1, 2], .add 6 [] [5, 3, 4]]
-- cap: P13-04-07
example (a b c d e f g h i j k l : Prop) (ha : a ∨ b ∨ c) (hb : d ∨ e ∨ f) (hc : g ∨ h ∨ i)
    (hd : j ∨ k ∨ l)
    (h₁ : ¬(a ∧ d) ∧ ¬(a ∧ g) ∧ ¬(a ∧ j) ∧ ¬(d ∧ g) ∧ ¬(d ∧ j) ∧ ¬(g ∧ j))
    (h₂ : ¬(b ∧ e) ∧ ¬(b ∧ h) ∧ ¬(b ∧ k) ∧ ¬(e ∧ h) ∧ ¬(e ∧ k) ∧ ¬(h ∧ k))
    (h₃ : ¬(c ∧ f) ∧ ¬(c ∧ i) ∧ ¬(c ∧ l) ∧ ¬(f ∧ i) ∧ ¬(f ∧ l) ∧ ¬(i ∧ l)) : False :=
  Examples.pigeonhole_four_three ha hb hc hd h₁ h₂ h₃

/-! ## Reporting the first step that fails -/

-- cap: P13-04-10
#guard failureOf (Lrat.check Examples.twoAtoms [.add 5 [pos 1] [1], .add 6 [] [5, 3, 4]]) ==
  some ⟨0, "clause 5: unit propagation along the hints ends without a conflict"⟩
-- cap: P13-04-10
#guard failureOf (Lrat.check Examples.twoAtoms [.add 5 [pos 1] [1, 2], .add 6 [] [5, 9, 4]]) ==
  some ⟨1, "clause 6: there is no clause 9 to use as a hint"⟩
-- cap: P13-04-10
#guard failureOf (Lrat.check Examples.twoAtoms [.add 5 [] [1, 2, 3, 4]]) ==
  some ⟨0, "clause 5: hint 1 has more than one literal that is not yet false"⟩
-- cap: P13-04-10
#guard failureOf (Lrat.check Examples.twoAtoms [.add 5 [pos 1] [1, 2]]) ==
  some ⟨1, "the empty clause is not derived"⟩
-- cap: P13-04-10
#guard failureOf (Lrat.check Examples.twoAtoms []) == some ⟨0, "the empty clause is not derived"⟩
-- cap: P13-04-10
#guard toString (⟨3, "there is no clause 9"⟩ : Failure) == "step 3: there is no clause 9"
-- cap: P13-04-10
#guard failureOf (Lrat.check pigeons pigeonProof) == none

/-! ## Proofs with a flaw are rejected -/

/-- Every proof made by removing the last hint of one step. -/
def withoutLastHint : List Lrat.Step → List (List Lrat.Step)
  | [] => []
  | .add id c hints :: rest =>
    (Lrat.Step.add id c hints.dropLast :: rest) ::
      (withoutLastHint rest).map (Lrat.Step.add id c hints :: ·)
  | step :: rest => (withoutLastHint rest).map (step :: ·)

/-- Every proof made by pointing one hint of one step at a clause that does not exist. -/
def withUnknownHint : List Lrat.Step → List (List Lrat.Step)
  | [] => []
  | .add id c hints :: rest =>
    ((List.range hints.length).map fun i => Lrat.Step.add id c (hints.set i 999) :: rest) ++
      (withUnknownHint rest).map (Lrat.Step.add id c hints :: ·)
  | step :: rest => (withUnknownHint rest).map (step :: ·)

/-- Every proof made by turning one literal of one added clause around. -/
def withFlippedLiteral : List Lrat.Step → List (List Lrat.Step)
  | [] => []
  | .add id c hints :: rest =>
    ((List.range c.length).map fun i => Lrat.Step.add id (c.modify i Literal.negate) hints :: rest) ++
      (withFlippedLiteral rest).map (Lrat.Step.add id c hints :: ·)
  | step :: rest => (withFlippedLiteral rest).map (step :: ·)

-- cap: P13-04-11
-- The last hint of a step is its conflict; without it no step of the proof goes through.
#guard (withoutLastHint pigeonProof).length == 2 &&
  (withoutLastHint pigeonProof).all fun proof => !Lrat.refutes pigeons proof
-- cap: P13-04-11
#guard (withUnknownHint pigeonProof).length == 13 &&
  (withUnknownHint pigeonProof).all fun proof => !Lrat.refutes pigeons proof
-- cap: P13-04-11
#guard (withFlippedLiteral pigeonProof).length == 1 &&
  (withFlippedLiteral pigeonProof).all fun proof => !Lrat.refutes pigeons proof
-- cap: P13-04-11
-- The steps in the other order: the second needs the clause the first one adds.
#guard !Lrat.refutes pigeons pigeonProof.reverse

/-- Every clause over two atoms with at most two literals. -/
def smallClauses : List Clause :=
  let literals := [pos 0, neg 0, pos 1, neg 1]
  [[]] ++ literals.map (fun l => [l]) ++ literals.flatMap fun l => literals.map fun m => [l, m]

/-- Every list of hints of length at most three over the clause numbers one to four. -/
def smallHints : List (List Nat) :=
  let ids := [1, 2, 3, 4]
  [[]] ++ ids.map (fun i => [i]) ++ (ids.flatMap fun i => ids.map fun j => [i, j]) ++
    ids.flatMap fun i => ids.flatMap fun j => ids.map fun k => [i, j, k]

-- cap: P13-04-11
-- No proof whatever of a satisfiable clause set is accepted. Here: all proofs of one step that
-- adds the empty clause, and all proofs of two steps, over a pool of clauses and hints.
#guard smallHints.all fun hints => !Lrat.refutes satisfiable [.add 3 [] hints]
-- cap: P13-04-11
#guard smallClauses.all fun clause => smallHints.all fun first => smallHints.all fun second =>
  !Lrat.refutes satisfiable [.add 3 clause first, .add 4 [] second]

/-! ## Certificates of linear integer arithmetic -/

section
open LinearInt

-- cap: P13-04-09
#guard refutes [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)]
-- cap: P13-04-09
-- The left sides must cancel.
#guard !refutes [(1, ⟨[1, 2], .le, 3⟩), (2, ⟨[-1, -2], .le, -4⟩)]
-- cap: P13-04-09
-- A multiplier of an inequality must not be negative, even if the sums come out.
#guard !refutes [(-1, ⟨[1], .le, 0⟩), (-1, ⟨[-1], .le, -1⟩)]
-- cap: P13-04-09
-- A zero bound refutes only when a strict row takes part with a positive multiplier.
#guard refutes [(1, ⟨[1], .lt, 0⟩), (1, ⟨[-1], .le, 0⟩)]
-- cap: P13-04-09
#guard !refutes [(1, ⟨[1], .le, 0⟩), (1, ⟨[-1], .le, 0⟩)]
-- cap: P13-04-09
#guard !refutes [(0, ⟨[1], .lt, 0⟩), (1, ⟨[0], .le, 0⟩)]
-- cap: P13-04-09
-- An equation may have a negative multiplier.
#guard refutes [(-3, ⟨[1], .eq, 2⟩), (1, ⟨[3], .le, 5⟩)]
-- cap: P13-04-09
-- Rows of different lengths: missing coefficients are zero.
#guard refutes [(1, ⟨[1], .le, 3⟩), (1, ⟨[0, 1], .le, 6⟩), (-1, ⟨[1, 1], .eq, 10⟩)]
-- cap: P13-04-09
#guard !refutes []
-- cap: P13-04-09
#guard dot [2, 3] (valuation [5, 7]) 0 == 31 && valuation [5, 7] 2 == 0
-- cap: P13-04-09
example {c : Certificate} (h : refutes c = true) (x : Nat → Int) : ¬AllHold x c :=
  not_allHold_of_refutes h x

-- cap: P13-04-09
example (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h₁, h₂]

-- cap: P13-04-09
example (x : Int) (h : x < x) : False := by
  maithic_linear_int [x] [(1, ⟨[0], .lt, 0⟩)] using [h]

-- cap: P13-04-09
/--
error: maithic_linear_int: the checker does not accept the certificate: with these multipliers the rows do not add up to a contradiction
-/
#guard_msgs in
example (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (2, ⟨[-1, -2], .le, -4⟩)] using [h₁, h₂]

-- cap: P13-04-09
-- A row must follow from the hypothesis named for it, by itself: `x + 2 * y ≤ 2` is more than
-- `2 * x + 4 * y < 7` gives, although the two hypotheses together are contradictory.
/--
error: maithic_linear_int: row 0 does not follow from
  h₁
by integer reasoning
-/
#guard_msgs in
example (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 2⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h₁, h₂]

-- cap: P13-04-09
/-- error: maithic_linear_int: the certificate has 2 row(s) and 1 hypothesis name(s) -/
#guard_msgs in
example (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h₁]

end

/-! ## Axioms -/

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.Lrat.unsatisfiable_of_refutes: clean (standard axioms: propext, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Lrat.unsatisfiable_of_refutes

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.Resolution.unsatisfiable_of_refutes: clean (standard axioms: propext, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Resolution.unsatisfiable_of_refutes

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.Formula.valid_of_isTautology: clean (standard axioms: propext, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Formula.valid_of_isTautology

-- cap: P13-04-12
-- Reading truth values off arbitrary propositions is classical; nothing else is.
/-- info: Maithic.Oracle.Logic.Formula.denote_of_lrat: clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Formula.denote_of_lrat

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.LinearInt.not_allHold_of_refutes: clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.LinearInt.not_allHold_of_refutes

-- cap: P13-04-12
-- Theorems closed by the tactics inherit nothing else: no compiled evaluation, no new axiom.
/-- info: Maithic.Oracle.Logic.Examples.pigeonhole: clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Examples.pigeonhole

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.Examples.pigeonhole': clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Examples.pigeonhole'

-- cap: P13-04-12
-- A theorem closed from a solver's proof rests on the same axioms: the solver adds none.
/-- info: Maithic.Oracle.Logic.Examples.pigeonhole_four_three: clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Examples.pigeonhole_four_three

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.Examples.peirce: clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Examples.peirce

-- cap: P13-04-12
/-- info: Maithic.Oracle.Logic.Examples.no_integer_point: clean (standard axioms: propext, Classical.choice, Quot.sound) -/
#guard_msgs in
#maithic_audit Maithic.Oracle.Logic.Examples.no_integer_point

/-! ## Proof fixtures -/

/-- What the manifest expects of one proof. -/
structure Expectation where
  /-- The name of the entry. -/
  name : String
  /-- The file of the clause set. -/
  problem : String
  /-- The file of the proof. -/
  proof : String
  /-- `accepted`, `rejected` or `unreadable`. -/
  expect : String
  /-- The step a rejected proof fails at. -/
  step : Option Nat := none
  /-- Text that the reason of a failure contains. -/
  reason : Option String := none
  deriving FromJson

/-- Whether `needle` occurs in `text`. -/
def containsText (text needle : String) : Bool := (text.splitOn needle).length > 1

/-- Compares the verdict of the checker on one fixture with the expectation. -/
def judge (dir : System.FilePath) (e : Expectation) : IO (Option String) := do
  let problem ← match Cnf.parseDimacs (← IO.FS.readFile (dir / e.problem)) with
    | .ok φ => pure φ
    | .error err => return some s!"{e.problem}: {err}"
  let reason := e.reason.getD ""
  match Lrat.parse (← IO.FS.readFile (dir / e.proof)) with
  | .error err =>
    if e.expect == "unreadable" && containsText err reason then return none
    else return some s!"{e.proof} cannot be read ({err}) but is expected to be {e.expect}"
  | .ok proof =>
    match Lrat.check problem proof, e.expect with
    | .ok _, "accepted" => return none
    | .error failure, "rejected" =>
      if some failure.step == e.step && containsText failure.reason reason then return none
      else return some s!"rejected at {failure}, expected step {e.step.getD 0} with \"{reason}\""
    | .ok _, expected => return some s!"accepted, expected {expected}"
    | .error failure, expected => return some s!"rejected at {failure}, expected {expected}"

-- cap: P13-04-08
/-- The tests of the proof fixtures: one per entry of the manifest, and one that every proof and
problem file in the directory is listed. -/
def fixtureTests (fixtures : System.FilePath) : IO (List MaithicTest.UnitTest) := do
  let dir := fixtures / "oracle" / "logic"
  let manifest ← IO.FS.readFile (dir / "manifest.json")
  let entries ← match Json.parse manifest >>= fun j => j.getObjValAs? (List Expectation) "proofs" with
    | .ok entries => pure entries
    | .error e => return [{ name := "P13-04-08 the manifest of proof fixtures is readable", run := pure (some e) }]
  let listed := entries.flatMap fun e => [e.problem, e.proof]
  let onDisk := (← dir.readDir).toList.map (·.fileName) |>.filter fun n => n.endsWith ".cnf" || n.endsWith ".lrat"
  let unlisted := onDisk.filter fun n => !listed.contains n
  let coverage : MaithicTest.UnitTest :=
    { name := "P13-04-08 every proof fixture on disk is listed in the manifest",
      run := pure (if unlisted.isEmpty then none else some s!"not listed: {unlisted}") }
  return coverage :: entries.map fun e =>
    { name := s!"P13-04-08 proof fixture {e.name} is {e.expect}", run := judge dir e }

end MaithicTest.Logic
