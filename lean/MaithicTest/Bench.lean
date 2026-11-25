/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBench

/-!
# Tests of the benchmark conventions

The attribute, the identifier rules, the manifest and the link between problems and their
reference solutions.
-/

namespace MaithicTest.Bench

open Lean MaithicBench

-- cap: P15-01-02
#guard validId "basics-001"
-- cap: P15-01-02
#guard validId "nat-arith-042"
-- cap: P15-01-02
#guard !validId "basics-1"
-- cap: P15-01-02
#guard !validId "Basics-001"
-- cap: P15-01-02
#guard !validId "001"
-- cap: P15-01-02
#guard suiteOf "nat-arith-042" == "nat-arith"

-- cap: P15-01-01
-- A problem is a proposition, and a solution is a term of that proposition.
example : MaithicBench.Basics.add_comm' := MaithicBench.Basics.Solutions.add_comm'

-- cap: P15-01-05
example : MaithicBench.Basics.even_add_even ∧ MaithicBench.Basics.two_pow_ten :=
  ⟨MaithicBench.Basics.Solutions.even_add_even, MaithicBench.Basics.Solutions.two_pow_ten⟩

/-- The manifest, parsed. -/
def manifestJson? : Option Json := (Json.parse MaithicBench.manifest).toOption

/-- The problems of the manifest. -/
def manifestProblems : Array Json :=
  match manifestJson? with
  | some j => (j.getObjValAs? (Array Json) "problems").toOption.getD #[]
  | none => #[]

/-- Reads a string field of a problem. -/
def field (p : Json) (key : String) : String := (p.getObjValAs? String key).toOption.getD ""

-- cap: P15-01-04
#guard manifestJson?.isSome
-- cap: P15-01-04
#guard manifestProblems.size == 24
-- cap: P15-01-03
#guard manifestProblems.all fun p => field p "suite" == "basics"
-- cap: P15-01-04
#guard (manifestProblems.map (field · "id")).toList ==
  (List.range 24).map fun i =>
    let n := toString (i + 1)
    "basics-" ++ String.ofList (List.replicate (3 - n.length) '0') ++ n
-- cap: P15-01-04
#guard field manifestProblems[4]! "statement" == "∀ (a b : Nat), a % 2 = 0 → b % 2 = 0 → (a + b) % 2 = 0"
-- cap: P15-01-04
#guard field manifestProblems[4]! "decl" == "MaithicBench.Basics.even_add_even"

-- cap: P15-01-06
-- The attribute refuses a duplicate identifier.
/-- error: maithic_problem: identifier 'basics-001' is already in use -/
#guard_msgs in
@[maithic_problem "basics-001"]
def duplicate : Prop := True

-- cap: P15-01-06
-- The attribute refuses a malformed identifier.
/-- error: maithic_problem: identifier 'Bad_Id' must have the form name-NNN -/
#guard_msgs in
@[maithic_problem "Bad_Id"]
def malformed : Prop := True

-- cap: P15-01-06
-- The attribute refuses a definition that is not a proposition.
/-- error: maithic_problem: 'MaithicTest.Bench.notAProp' must be a definition of type Prop -/
#guard_msgs in
@[maithic_problem "tests-001"]
def notAProp : Nat := 3

end MaithicTest.Bench
