/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Testing
import Maithic.Refute.Vacuity

/-!
# Tests of testing by sampling

The random stream is compared with reference values that the Julia packages produce from the same
seeds. Generators are checked for staying within the size and for reaching the ends of their
ranges, shrinking for its exact candidates, and the runner, the command and the tactic for their
exact reports: a run is determined by its settings, so every message here is the same on every
machine.
-/

namespace MaithicTest.Refute

open Maithic.Refute

set_option linter.unusedVariables false

/-- The first `n` values of the stream of a seed. -/
def stream (seed n : Nat) : List Nat :=
  ((List.range n).foldl (fun (state : List Nat × Rng) _ =>
    let (x, g) := state.2.next
    (state.1 ++ [x.toNat], g)) ([], Rng.ofSeed seed)).1

/-- The first `n` values of a generator at a size, from a seed. -/
def draws (seed n size : Nat) (x : Gen α) : List α :=
  (Gen.run (Gen.listOfLength x n) size (Rng.ofSeed seed)).1

/-! ## The random stream -/

-- cap: P14-01-06
-- The reference values of SplitMix64 for seed 0, and the values `MaithicCore.SplitMix` gives.
#guard stream 0 4 == [0xe220a8397b1dcdaf, 0x6e789e6aa1b965f4, 0x06c45d188009454f, 0xf88bb8a8724c81ec]
-- cap: P14-01-06
#guard stream 42 4 == [0xbdd732262feb6e95, 0x28efe333b266f103, 0x47526757130f9f52, 0x581ce1ff0e4ae394]
-- cap: P14-01-06
-- Ranges are sampled as in Julia: `integer!(SplitMix(42), 0:9)` gives the same twelve numbers.
#guard draws 42 12 0 (Gen.chooseNat 0 9) == [3, 1, 8, 4, 0, 2, 5, 8, 5, 4, 7, 6]
-- cap: P14-01-06
#guard draws 7 12 0 (Gen.chooseInt (-5) 5) == [-3, -5, -5, -5, 2, 2, -4, 4, -3, 3, -5, 4]
-- cap: P14-01-06
#guard draws 7 20 0 (Gen.below 3) == [0, 0, 0, 0, 1, 0, 1, 0, 2, 2, 1, 1, 0, 1, 0, 0, 1, 2, 2, 1]
-- cap: P14-01-06
-- A seed is taken modulo 2 ^ 64, and a span of zero draws nothing.
#guard Rng.ofSeed (2 ^ 64 + 5) == Rng.ofSeed 5 && (Rng.below 0 (Rng.ofSeed 1)) == (0, Rng.ofSeed 1)
-- cap: P14-01-06
-- The same settings give the same report, and the seed is part of the report.
#guard check (NamedBinder "x" (∀ x : Nat, x < 10)) { seed := 5 } ==
  { verdict := .falsified, cases := 42, discarded := 0, shrinks := 0, seed := 5, counterexample := [("x", "10")] }
-- cap: P14-01-06
#guard check (NamedBinder "x" (∀ x : Nat, x < 10)) ==
  { verdict := .falsified, cases := 35, discarded := 0, shrinks := 0, seed := 0, counterexample := [("x", "10")] }

/-! ## Generators -/

-- cap: P14-01-02
-- Values stay within the size, and reach both ends of the range.
#guard (draws 1 200 5 (arbitrary : Gen Nat)).all (· ≤ 5)
-- cap: P14-01-02
#guard (draws 1 200 5 (arbitrary : Gen Nat)).contains 0 && (draws 1 200 5 (arbitrary : Gen Nat)).contains 5
-- cap: P14-01-02
#guard (draws 1 200 0 (arbitrary : Gen Nat)).all (· == 0)
-- cap: P14-01-02
#guard (draws 1 200 4 (arbitrary : Gen (List Nat))).all fun l => l.length ≤ 4 && l.all (· ≤ 4)
-- cap: P14-01-02
#guard (draws 1 50 9 (Gen.resize (fun _ => 2) (arbitrary : Gen Nat))).all (· ≤ 2)
-- cap: P14-01-02
#guard (draws 1 50 3 (do let size ← Gen.size; pure (size + 1))) == List.replicate 50 4
-- cap: P14-01-02
#guard (draws 3 100 0 (Gen.elements 0 [10, 20, 30])).all [10, 20, 30].contains &&
  draws 3 5 0 (Gen.elements 7 ([] : List Nat)) == [7, 7, 7, 7, 7]
-- cap: P14-01-02
#guard (draws 3 60 0 (Gen.oneOf (pure 0) [pure 1, pure 2])).all [1, 2].contains
-- cap: P14-01-02
-- An empty range gives its lower end.
#guard draws 3 4 0 (Gen.chooseNat 5 2) == [5, 5, 5, 5] && draws 3 4 0 (Gen.chooseInt 2 (-2)) == [2, 2, 2, 2]

-- cap: P14-01-05
#guard draws 1 10 5 (arbitrary : Gen Nat) == [5, 1, 0, 5, 3, 2, 3, 3, 0, 4]
-- cap: P14-01-05
#guard draws 1 10 5 (arbitrary : Gen Int) == [4, 3, -5, 2, 2, -4, -5, -2, -5, -3]
-- cap: P14-01-05
#guard draws 1 8 5 (arbitrary : Gen Bool) == [true, true, false, true, true, false, true, true]
-- cap: P14-01-05
#guard draws 1 6 3 (arbitrary : Gen (List Nat)) == [[3], [3, 1], [], [1], [], [1, 2]]
-- cap: P14-01-05
#guard draws 1 6 3 (arbitrary : Gen (Option Nat)) == [some 3, some 3, some 0, some 1, none, some 1]
-- cap: P14-01-05
#guard draws 1 6 6 (arbitrary : Gen Rat) == [0, -5, mkRat (-1) 3, mkRat (-1) 2, 1, mkRat 1 3]
-- cap: P14-01-05
#guard draws 1 6 3 (arbitrary : Gen (Nat × Bool)) ==
  [(1, true), (2, true), (1, false), (1, true), (0, false), (1, false)]
-- cap: P14-01-05
#guard draws 1 4 3 (arbitrary : Gen (Array Int)) == [#[-3], #[-3, 2], #[], #[0]]
-- cap: P14-01-05
#guard draws 1 6 3 (arbitrary : Gen String) == ["1", "RJ", "", "l", "", "1 "]
-- cap: P14-01-05
#guard (draws 1 6 9 (arbitrary : Gen (Fin 3))).map (·.val) == [2, 1, 0, 2, 0, 2]
-- cap: P14-01-05
-- Integers come out negative and positive, rationals whole and not, options empty and full.
#guard (draws 2 100 8 (arbitrary : Gen Int)).any (· < 0) && (draws 2 100 8 (arbitrary : Gen Int)).any (· > 0)
-- cap: P14-01-05
#guard (draws 2 100 8 (arbitrary : Gen Rat)).any (·.den != 1) && (draws 2 100 8 (arbitrary : Gen Rat)).any (·.den == 1)
-- cap: P14-01-05
#guard (draws 2 100 8 (arbitrary : Gen (Option Bool))).contains none &&
  (draws 2 100 8 (arbitrary : Gen (Option Bool))).contains (some true)

/-! ## Shrinking -/

-- cap: P14-01-03
#guard shrink (10 : Nat) == [0, 5, 9] && shrink (1 : Nat) == [0] && shrink (0 : Nat) == []
-- cap: P14-01-03
-- A negative integer is first replaced by zero, then by its positive counterpart.
#guard shrink (-6 : Int) == [0, 6, -3, -5] && shrink (7 : Int) == [0, 3, 6] && shrink (-1 : Int) == [0, 1]
-- cap: P14-01-03
#guard shrink (0 : Int) == [] && shrink true == [false] && shrink false == []
-- cap: P14-01-03
#guard shrink (mkRat 7 2) == [0, 3] && shrink (3 : Rat) == [0, 1, 2] && shrink (0 : Rat) == []
-- cap: P14-01-03
-- A list: empty it, halve it, remove one element, make one element simpler.
#guard shrink [3, 4] == [[], [3], [4], [4], [3], [0, 4], [1, 4], [2, 4], [3, 0], [3, 2], [3, 3]]
-- cap: P14-01-03
#guard shrink ([] : List Nat) == [] && shrink [0] == [[]] && shrink [2] == [[], [0], [1]]
-- cap: P14-01-03
#guard shrink (some 2 : Option Nat) == [none, some 0, some 1] && shrink (none : Option Nat) == []
-- cap: P14-01-03
#guard shrink ((2 : Nat), true) == [(0, true), (1, true), (2, false)]
-- cap: P14-01-03
#guard shrink "ab" == ["", "a", "b", "b", "a", "aa"] && shrink #[1, 0] == [#[], #[1], #[0], #[0], #[1], #[0, 0]]
-- cap: P14-01-03
#guard (shrink (3 : Fin 5)).map (·.val) == [0, 1, 2]
-- cap: P14-01-03
-- A value is never among its own candidates, so shrinking always moves.
#guard (List.range 50).all (fun n => !(shrink n).contains n) &&
  ((List.range 41).map fun (n : Nat) => (n : Int) - 20).all fun i => !(shrink i).contains i
-- cap: P14-01-03
-- The smallest number that is at least 17, from 500, and the smallest list with two elements.
#guard minimize (fun n : Nat => if n ≥ 17 then some [] else none) 1000 500 [] 0 == (17, [], 18)
-- cap: P14-01-03
#guard minimize (fun l : List Nat => if l.length ≥ 2 then some [] else none) 1000 [9, 8, 7, 6, 5] [] 0 ==
  ([0, 0], [], 3)
-- cap: P14-01-03
-- The budget bounds the number of steps.
#guard minimize (fun n : Nat => if n ≥ 17 then some [] else none) 3 500 [] 0 == (62, [], 3)

/-! ## Testable statements -/

-- cap: P14-01-01
#guard (check (2 + 2 = 4)).verdict == .passed && (check (2 + 2 = 5)).verdict == .falsified
-- cap: P14-01-01
#guard (check (NamedBinder "x" (∀ x : Nat, NamedBinder "y" (∀ y : Nat, x + y = y + x)))).verdict == .passed
-- cap: P14-01-01
-- A quantifier with no recorded name calls its variable `_`.
#guard (check (∀ q : Rat, q * q ≠ 2 * q)).counterexample == [("_", "0")]
-- cap: P14-01-01
-- A conjunction fails when either side does.
#guard check (NamedBinder "a" (∀ a : Nat, NamedBinder "b" (∀ b : Nat, a ≤ b ∧ b ≤ a + 100))) ==
  { verdict := .falsified, cases := 5, discarded := 0, shrinks := 0, seed := 0, counterexample := [("a", "1"), ("b", "0")] }
-- cap: P14-01-01
#guard (check (NamedBinder "a" (∀ a : Nat, a + 0 = a ∧ 0 + a = a))).verdict == .passed
-- cap: P14-01-01
#guard (check (NamedBinder "l" (∀ l : List Nat, l.reverse = l))).counterexample == [("l", "[0, 1]")]
-- cap: P14-01-01
#guard (check (NamedBinder "x" (∀ x : Int, NamedBinder "y" (∀ y : Int, x * y ≥ 0)))).counterexample ==
  [("x", "1"), ("y", "-1")]
-- cap: P14-01-01
-- Sizes grow from zero to the configured size and stay there.
#guard (Config.sizeOf {} 0, Config.sizeOf {} 50, Config.sizeOf {} 99, Config.sizeOf {} 500,
  Config.sizeOf { cases := 1 } 0) == (0, 15, 30, 30, 30)

-- cap: P14-01-08
-- Cases whose premise fails are discarded and do not count as passed.
#guard check (NamedBinder "x" (∀ x : Nat, x > 5 → x * x > 30)) ==
  { verdict := .passed, cases := 100, discarded := 57, shrinks := 0, seed := 0, counterexample := [] }
-- cap: P14-01-08
-- A premise that no generated value meets ends the run: nothing was tested.
#guard check (NamedBinder "x" (∀ x : Nat, x > 1000 → x = 0)) ==
  { verdict := .gaveUp, cases := 0, discarded := 1000, shrinks := 0, seed := 0, counterexample := [] }
-- cap: P14-01-08
#guard (check (NamedBinder "x" (∀ x : Nat, x > 1000 → x = 0)) { discards := 50 }).discarded == 50
-- cap: P14-01-08
-- With larger values the same statement is tested, and falsified at the smallest value there is.
#guard (check (NamedBinder "x" (∀ x : Nat, x > 1000 → x = 0)) { size := 2000 }).counterexample == [("x", "1001")]

/-! ## Reports -/

-- cap: P14-01-07
#guard (check (NamedBinder "l" (∀ l : List Nat, l.reverse = l))).toJson.compress ==
  "{\"cases\":15,\"counterexample\":[{\"name\":\"l\",\"value\":\"[0, 1]\"}],\"discarded\":0,\"seed\":0,\"shrinks\":3,\"verdict\":\"falsified\"}"
-- cap: P14-01-07
#guard (check (2 + 2 = 4)).toJson.compress ==
  "{\"cases\":100,\"counterexample\":[],\"discarded\":0,\"seed\":0,\"shrinks\":0,\"verdict\":\"passed\"}"
-- cap: P14-01-07
#guard toString (check (NamedBinder "x" (∀ x : Int, NamedBinder "y" (∀ y : Int, x * y ≥ 0)))) ==
  "falsified at case 12, shrunk 2 times\n  x := 1\n  y := -1"
-- cap: P14-01-07
#guard toString (check (NamedBinder "x" (∀ x : Nat, x > 5 → x * x > 30))) == "passed 100 cases, 57 discarded"
-- cap: P14-01-07
#guard toString (check (NamedBinder "x" (∀ x : Nat, x > 1000 → x = 0))) == "gave up: 1000 cases discarded, 0 passed"
-- cap: P14-01-07
#guard toString (check (0 = 0) { cases := 1 }) == "passed 1 case"

/-! ## The command -/

-- cap: P14-01-04
/-- info: passed 100 cases -/
#guard_msgs in
#maithic_test ∀ l : List Nat, l.reverse.reverse = l

-- cap: P14-01-04
/--
error: falsified at case 12, shrunk 2 times
  x := 1
  y := -1
-/
#guard_msgs in
#maithic_test ∀ x y : Int, x * y ≥ 0

-- cap: P14-01-04
-- Both variables of a counterexample are reported, each under its own name.
/--
error: falsified at case 4, shrunk 2 times
  l := [-2, 0, 0]
  k := 0
-/
#guard_msgs in
#maithic_test ∀ (l : List Int) (k : Int), k ∈ l → l.length < 3

-- cap: P14-01-04
/-- info: passed 100 cases -/
#guard_msgs in
#maithic_test 2 + 2 = 4

set_option warn.sorry false in
/-- A conjecture that is false, stated before anyone has tried to prove it. -/
theorem sumLeProduct (a b : Nat) : a + b ≤ a * b := sorry

-- cap: P14-01-04
-- The name of a theorem stands for its statement.
/--
error: falsified at case 5
  a := 1
  b := 0
-/
#guard_msgs in
#maithic_test sumLeProduct

-- cap: P14-01-07
-- As JSON the report is a message whatever the verdict, so that a program can read it.
/--
info: {"verdict": "falsified",
 "shrinks": 0,
 "seed": 0,
 "discarded": 0,
 "counterexample": [{"value": "1", "name": "a"}, {"value": "0", "name": "b"}],
 "cases": 5}
-/
#guard_msgs in
#maithic_test json sumLeProduct

-- cap: P14-01-07
/--
info: {"verdict": "passed",
 "shrinks": 0,
 "seed": 3,
 "discarded": 0,
 "counterexample": [],
 "cases": 20}
-/
#guard_msgs in
#maithic_test json (cases := 20) (seed := 3) ∀ n : Nat, n + 0 = n

-- cap: P14-01-04
-- Variables of the section are generated like those of the statement.
section
variable (n : Nat)

/-- info: passed 100 cases -/
#guard_msgs in
#maithic_test n + 0 = n

end

-- cap: P14-01-04
-- Six variables of six kinds in one statement.
/-- info: passed 100 cases -/
#guard_msgs in
#maithic_test ∀ (p : Nat × Bool) (o : Option Nat) (a : Array Int) (q : Rat) (c : Char) (i : Fin 5), True

-- cap: P14-01-04
/--
error: this statement cannot be tested by sampling: every type it quantifies over needs instances of `Arbitrary`, `Shrinkable` and `Repr`, and its conditions must be decidable.
No instance was found for
  Testable (NamedBinder "f" (∀ (f : Nat → Nat), f 0 = 0))
-/
#guard_msgs in
#maithic_test ∀ f : Nat → Nat, f 0 = 0

-- cap: P14-01-08
/-- error: gave up: 1000 cases discarded, 0 passed -/
#guard_msgs in
#maithic_test ∀ x : Nat, x > 1000 → x = 0

-- cap: P14-01-10
/-- info: passed 500 cases, 308 discarded -/
#guard_msgs in
#maithic_test (cases := 500) (seed := 7) ∀ x : Nat, x > 5 → x * x > 30

-- cap: P14-01-10
/--
error: falsified at case 1, shrunk 52 times
  x := 1001
-/
#guard_msgs in
#maithic_test (size := 2000) ∀ x : Nat, x > 1000 → x = 0

-- cap: P14-01-10
/-- error: gave up: 20 cases discarded, 0 passed -/
#guard_msgs in
#maithic_test (discards := 20) ∀ x : Nat, x > 1000 → x = 0

-- cap: P14-01-10
-- A shrinking budget of zero reports the value as it was drawn.
/--
error: falsified at case 15
  s := "F2y!"
-/
#guard_msgs in
#maithic_test (shrinks := 0) ∀ s : String, s.length ≤ 3

-- cap: P14-01-10
/-- error: unknown test option `case`; the options are cases, size, seed, discards and shrinks -/
#guard_msgs in
#maithic_test (case := 5) ∀ x : Nat, x = x

/-! ## The tactic -/

-- cap: P14-01-09
-- The hypotheses are premises; a goal that passes is left for the proof.
/-- info: passed 100 cases, 102 discarded -/
#guard_msgs in
example (x y : Nat) (h : x < y) : x + 1 ≤ y := by
  maithic_test
  omega

-- cap: P14-01-09
/--
error: falsified at case 1
  x := 0
  y := 0
-/
#guard_msgs in
example (x y : Nat) (h : x ≤ y) : x + 1 ≤ y := by
  maithic_test
  omega

/-! ## Vacuity -/

-- cap: P14-03-01
/-- warning: vacuous: the hypotheses are contradictory (found by omega, from h₁ and h₂) -/
#guard_msgs in
#maithic_vacuous ∀ (x : Nat) (h₁ : x < 3) (h₂ : x > 5), x = 4

-- cap: P14-03-01
-- Only the hypotheses that the derivation uses are named: `h₃` is not part of the contradiction.
/-- warning: vacuous: the hypotheses are contradictory (found by omega, from h₁ and h₂) -/
#guard_msgs in
#maithic_vacuous ∀ (x y : Int) (h₁ : x ≤ y) (h₂ : y < x) (h₃ : x = 5), x + y = 0

-- cap: P14-03-01
-- A hypothesis with no name is called by its statement.
/-- warning: vacuous: the hypotheses are contradictory (found by omega, from x < 3 and x > 5) -/
#guard_msgs in
#maithic_vacuous ∀ x : Nat, x < 3 → x > 5 → x = 4

-- cap: P14-03-01
/-- warning: vacuous: the hypotheses are contradictory (found by contradiction, from hp and hnp) -/
#guard_msgs in
#maithic_vacuous ∀ (p q : Prop) (hp : p) (hq : q) (hnp : ¬p), q

-- cap: P14-03-01
/-- warning: vacuous: the hypotheses are contradictory (found by simp_all, from h₁, h₂ and h₃) -/
#guard_msgs in
#maithic_vacuous ∀ (p q : Prop) (h₁ : p ∨ q) (h₂ : ¬p) (h₃ : ¬q), p ∧ q

-- cap: P14-03-01
-- A contradiction that takes a search: three pigeons, two holes. The clausal search finds it and
-- the kernel checks its proof.
/--
warning: vacuous: the hypotheses are contradictory (found by maithic_sat, from ha, hb, hc, h₁, h₂, h₃, h₄, h₅ and h₆)
-/
#guard_msgs in
#maithic_vacuous ∀ (a₁ a₂ b₁ b₂ c₁ c₂ : Prop) (ha : a₁ ∨ a₂) (hb : b₁ ∨ b₂) (hc : c₁ ∨ c₂)
  (h₁ : ¬(a₁ ∧ b₁)) (h₂ : ¬(a₁ ∧ c₁)) (h₃ : ¬(b₁ ∧ c₁))
  (h₄ : ¬(a₂ ∧ b₂)) (h₅ : ¬(a₂ ∧ c₂)) (h₆ : ¬(b₂ ∧ c₂)), a₁

-- cap: P14-03-01
-- A statement whose conclusion is `False` claims the contradiction itself; it is not vacuous.
/-- info: the conclusion is False: the statement says itself that its hypotheses are contradictory -/
#guard_msgs in
#maithic_vacuous ∀ (x : Nat) (h : x < 3) (h' : x > 5), False

-- cap: P14-03-08
-- Sound statements are not flagged.
/--
info: nothing found: no contradiction was derived from the hypotheses, no variable was shown to range over an empty type, and the conclusion was not proved without the hypotheses
-/
#guard_msgs in
#maithic_vacuous ∀ (x : Nat) (h₁ : x > 0) (h₂ : x < 10), x * x < 100

-- cap: P14-03-08
/--
info: nothing found: no contradiction was derived from the hypotheses, no variable was shown to range over an empty type, and the conclusion was not proved without the hypotheses
-/
#guard_msgs in
#maithic_vacuous ∀ (a b : Int) (h : a < b), a + 1 ≤ b

-- cap: P14-03-08
/-- info: nothing found: the statement has no hypotheses, and no variable was shown to range over an empty type -/
#guard_msgs in
#maithic_vacuous ∀ x : Nat, x + 0 = x

-- cap: P14-03-08
-- The automation is bounded: this statement is vacuous, since no `i : Fin 0` exists once `n = 0`,
-- and it is not flagged. Finding nothing is not a verdict.
/--
info: nothing found: no contradiction was derived from the hypotheses, no variable was shown to range over an empty type, and the conclusion was not proved without the hypotheses
-/
#guard_msgs in
#maithic_vacuous ∀ (n : Nat) (h : n = 0) (i : Fin n), i.val = 3

-- cap: P14-03-02
-- The command takes the name of a theorem as well as a statement.
theorem vacuousFixture (x : Nat) (h₁ : x < 3) (h₂ : x > 5) : x = 4 := by omega

/-- warning: vacuous: the hypotheses are contradictory (found by omega, from h₁ and h₂) -/
#guard_msgs in
#maithic_vacuous vacuousFixture

-- cap: P14-03-02
theorem soundFixture (x : Nat) (h₁ : x > 0) (h₂ : x < 10) : x ≥ 1 := h₁

/--
info: nothing found: no contradiction was derived from the hypotheses, no variable was shown to range over an empty type, and the conclusion was not proved without the hypotheses
-/
#guard_msgs in
#maithic_vacuous soundFixture

-- cap: P14-03-02
-- Variables of the section are part of the statement.
section
variable (m : Nat) (hm : m < 0)

/-- warning: vacuous: the hypotheses are contradictory (found by contradiction, from hm) -/
#guard_msgs in
#maithic_vacuous hm = hm → m = 7

end

-- cap: P14-03-03
/-- warning: the conclusion holds without the hypotheses (found by rfl) -/
#guard_msgs in
#maithic_vacuous ∀ (x : Nat) (h : x > 3), x + 0 = x

-- cap: P14-03-03
/-- warning: the conclusion holds without the hypotheses (found by simp) -/
#guard_msgs in
#maithic_vacuous ∀ (x y : Nat) (h : x < y) (h' : y < 100), x ≤ x + y

-- cap: P14-03-03
-- A conclusion that mentions a hypothesis cannot be separated from it and is not examined.
/--
info: nothing found: no contradiction was derived from the hypotheses, no variable was shown to range over an empty type, and the conclusion was not proved without the hypotheses
-/
#guard_msgs in
#maithic_vacuous ∀ (n : Nat) (h : 0 < n), (⟨0, h⟩ : Fin n).val = 0

-- cap: P14-03-06
/-- warning: vacuous: the variable i ranges over an empty type, Fin 0 (found by cases and omega) -/
#guard_msgs in
#maithic_vacuous ∀ (i : Fin 0), i.val = 7

-- cap: P14-03-06
/-- warning: vacuous: the variable e ranges over an empty type, Empty (found by cases) -/
#guard_msgs in
#maithic_vacuous ∀ (e : Empty) (n : Nat), n = 0

-- cap: P14-03-06
/-- warning: vacuous: the variable x ranges over an empty type, { n // n < 0 } (found by cases and omega) -/
#guard_msgs in
#maithic_vacuous ∀ (x : {n : Nat // n < 0}), x.val = 1

-- cap: P14-03-06
-- Types with elements are not flagged, whatever the hypotheses say about them.
/-- warning: vacuous: the hypotheses are contradictory (found by contradiction, from h) -/
#guard_msgs in
#maithic_vacuous ∀ (x : Nat) (h : x < 0), x = 1

-- cap: P14-03-07
/--
info: {"variables": ["x"],
 "vacuous": true,
 "trivial_conclusion": null,
 "refutation": false,
 "hypotheses": ["h₁", "h₂"],
 "empty_domains": [],
 "contradiction": {"hypotheses": ["h₁", "h₂"], "found_by": "omega"}}
-/
#guard_msgs in
#maithic_vacuous json ∀ (x : Nat) (h₁ : x < 3) (h₂ : x > 5), x = 4

-- cap: P14-03-07
/--
info: {"variables": ["i", "x"],
 "vacuous": true,
 "trivial_conclusion": {"found_by": "rfl"},
 "refutation": false,
 "hypotheses": ["h"],
 "empty_domains":
 [{"variable": "i", "type": "Fin 0", "found_by": "cases and omega"}],
 "contradiction": null}
-/
#guard_msgs in
#maithic_vacuous json ∀ (i : Fin 0) (x : Nat) (h : x > 3), x + 0 = x

-- cap: P14-03-04
/-- warning: MaithicTest.Refute.soundFixture: the proof does not mention h₂ -/
#guard_msgs in
#maithic_unused soundFixture

-- cap: P14-03-04
theorem usesBoth (x : Nat) (h₁ : x > 0) (h₂ : x < 10) : 0 < x ∧ x < 10 := ⟨h₁, h₂⟩

/-- info: MaithicTest.Refute.usesBoth: the proof mentions every hypothesis -/
#guard_msgs in
#maithic_unused usesBoth

-- cap: P14-03-04
theorem usesNeither (x : Nat) (h₁ : x > 0) (h₂ : x < 10) : x = x := rfl

/-- warning: MaithicTest.Refute.usesNeither: the proof does not mention h₁ and h₂ -/
#guard_msgs in
#maithic_unused usesNeither

-- cap: P14-03-05
-- Each hypothesis is dropped in turn. A counterexample shows that one is needed; a proof of the
-- statement without the other shows that it is not.
/--
info: h : a ≤ b is needed: without it the statement fails at
  a := 3
  b := 0
h' : b < 1000 is not needed: the statement holds without it (found by omega)
-/
#guard_msgs in
#maithic_necessity ∀ (a b : Nat) (h : a ≤ b) (h' : b < 1000), a ≤ b + 1

-- cap: P14-03-05
/--
info: h : a ≤ b is needed: without it the statement fails at
  a := 1
  b := 0
-/
#guard_msgs in
#maithic_necessity ∀ (a b : Nat) (h : a ≤ b), b - a + a = b

-- cap: P14-03-05
/--
info: h₁ : x > 0 is needed: without it the statement fails at
  x := 0
h₂ : x < 10 is not needed: the statement holds without it (found by trivial)
-/
#guard_msgs in
#maithic_necessity soundFixture

-- cap: P14-03-05
-- A hypothesis that the rest of the statement refers to cannot be dropped alone.
/-- info: h : 0 < n is undecided: the rest of the statement depends on it -/
#guard_msgs in
#maithic_necessity ∀ (n : Nat) (h : 0 < n), (⟨0, h⟩ : Fin n).val = 0

-- cap: P14-03-05
/-- info: the statement has no hypotheses -/
#guard_msgs in
#maithic_necessity ∀ x : Nat, x = x

-- cap: P14-03-05
-- Neither a proof nor a counterexample: the test cases passed, and that is all that is said.
/-- info: h : n > 0 was not needed in 100 test cases, and no proof without it was found -/
#guard_msgs in
#maithic_necessity ∀ (n : Nat) (h : n > 0), 2 * (List.range (n + 1)).sum = n * (n + 1)

-- cap: P14-03-05
-- The statements themselves, with one hypothesis removed from each.
theorem squareFixture (x : Nat) (h₁ : x > 0) (h₂ : x < 10) : x * x < 100 :=
  Nat.lt_of_le_of_lt (Nat.mul_le_mul (by omega) (by omega)) (by decide : 9 * 9 < 100)

/--
info: without h₁: ∀ (x : Nat), x < 10 → x * x < 100
without h₂: ∀ (x : Nat), x > 0 → x * x < 100
-/
#guard_msgs in
run_meta do
  let variants ← dropVariants (← Lean.getConstInfo ``squareFixture).type
  let lines := variants.map fun v => m!"without {v.hypothesis}: {v.statement.get!}"
  Lean.logInfo (Lean.MessageData.joinSep lines.toList "\n")

/-! ## The kernel behind every finding -/

-- cap: P14-03-01
-- A derivation counts when the kernel accepts it. The kernel is really asked: it refuses a term
-- that proves something else, and a term with a hole is not sent at all.
run_meta do
  let two := Lean.mkNatLit 2
  let claim ← Lean.Meta.mkEq two two
  unless ← kernelAccepts claim (← Lean.Meta.mkEqRefl two) do
    throwError "the proof of 2 = 2 by reflexivity was refused"
  if ← kernelAccepts claim (← Lean.Meta.mkEqRefl (Lean.mkNatLit 3)) then
    throwError "a proof of 3 = 3 was accepted as a proof of 2 = 2"
  if ← kernelAccepts claim (← Lean.Meta.mkFreshExprMVar claim) then
    throwError "a hole was accepted as a proof"
  -- In a context, the hypotheses are abstracted before the kernel sees the theorem.
  Lean.Meta.withLocalDeclD `x (Lean.mkConst ``Nat) fun x => do
    let same ← Lean.Meta.mkEq x x
    unless ← kernelAccepts same (← Lean.Meta.mkEqRefl x) do
      throwError "the proof of x = x in the context of x was refused"
    if ← kernelAccepts same (← Lean.Meta.mkEqRefl two) then
      throwError "a proof of 2 = 2 was accepted as a proof of x = x"

/-! ## With the linter for unused variables switched on

The hypotheses of a statement under test are named so that the reports can name them, and nothing
has to refer to them. The commands say so to the linter; a warning here would fail the build. -/

section LinterOn
set_option linter.unusedVariables true

-- cap: P14-03-01
/-- warning: vacuous: the hypotheses are contradictory (found by omega, from h₁ and h₂) -/
#guard_msgs in
#maithic_vacuous ∀ (x : Nat) (h₁ : x < 3) (h₂ : x > 5), x = 4

-- cap: P14-03-05
/--
info: h : a ≤ b is needed: without it the statement fails at
  a := 3
  b := 0
h' : b < 1000 is not needed: the statement holds without it (found by omega)
-/
#guard_msgs in
#maithic_necessity ∀ (a b : Nat) (h : a ≤ b) (h' : b < 1000), a ≤ b + 1

-- cap: P14-01-04
/-- info: passed 100 cases, 42 discarded -/
#guard_msgs in
#maithic_test ∀ (x : Nat) (h : x > 3), x + 0 = x

end LinterOn

end MaithicTest.Refute
