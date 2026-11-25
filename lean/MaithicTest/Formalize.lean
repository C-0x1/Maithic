/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Formalize

/-!
# Tests of the statement utilities

Statements are taken apart, printed with nothing left implicit and compared, and every report is
checked word for word: what these commands say is read by a program on the other side, and by a
person who has to decide whether a formal statement means what was meant.
-/

namespace MaithicTest.Formalize

open Lean Meta Elab Command Maithic.Formalize

set_option linter.unusedVariables false

/-- Fails the build with a message when a check does not hold. -/
def expect (what : String) (holds : Bool) : CommandElabM Unit :=
  unless holds do throwError "check failed: {what}"

/-- The statement a text stands for, closed. The text is parsed as a person would have typed it,
so that the names of its binders are the names in the text. -/
def statementOf (text : String) : CommandElabM Expr := do
  let stx ← match Parser.runParserCategory (← getEnv) `term text with
    | .ok stx => pure stx
    | .error e => throwError "the statement does not parse: {e}"
  runTermElabM fun variables => Maithic.Refute.elabStatement stx variables

/-! ## Taking a statement apart -/

-- cap: P02-05-01
-- Variables, a hypothesis and the conclusion, each as Lean prints it. A report with nothing to
-- point out is information.
/--
info: ∀ (a b : Nat), a ≤ b → a < b + 1
  (a : Nat)  variable
  (b : Nat)  variable
  (h : a ≤ b)  hypothesis
  ⊢ a < b + 1
-/
#guard_msgs in
#maithic_stmt ∀ (a b : Nat) (h : a ≤ b), a < b + 1

-- cap: P02-05-01
-- Types, instances and the ways of binding; a variable nothing mentions makes the report a warning.
/--
warning: ∀ {α : Type} [inst : Inhabited α] (xs : List α) (n spare : Nat), xs.length = n → xs ≠ [] → xs.head! = xs.head!
  {α : Type}  type
  [inst : Inhabited α]  instance
  (xs : List α)  variable
  (n : Nat)  variable
  (spare : Nat)  variable, which nothing mentions
  (xs.length = n)  hypothesis
  (xs ≠ [])  hypothesis
  ⊢ xs.head! = xs.head!
-/
#guard_msgs in
#maithic_stmt ∀ {α : Type} [inst : Inhabited α] (xs : List α) (n spare : Nat), xs.length = n → xs ≠ [] → xs.head! = xs.head!

-- cap: P02-05-01
-- A statement without binders is its conclusion.
/--
info: 2 + 2 = 4
  ⊢ 2 + 2 = 4
-/
#guard_msgs in
#maithic_stmt 2 + 2 = 4

set_option warn.sorry false in
/-- A statement somebody means to prove, with the proof still open. -/
theorem pred_lt (n : Nat) (h : 0 < n) : n - 1 < n := sorry

-- cap: P02-05-01
-- The name of a theorem stands for its statement, and an operation that does not mean what its
-- symbol suggests is pointed out.
/--
warning: ∀ (n : Nat), 0 < n → n - 1 < n
  (n : Nat)  variable
  (h : 0 < n)  hypothesis
  ⊢ n - 1 < n
  trap  n - 1: subtraction of natural numbers stops at zero: it is 0 whenever the second number is the larger
-/
#guard_msgs in
#maithic_stmt pred_lt

-- cap: P02-05-01
-- As JSON, for a program: binders with their roles, the conclusion and its head, the constants
-- the statement mentions, and both printed forms.
/--
info: {"unused": [],
 "traps":
 [{"term": "k / 2",
   "note": "division of integers rounds, and division by zero is 0",
   "kind": "int-division"}],
 "statement": "∀ (n : Nat) (k : Int), ↑n + k / 2 > 0",
 "head": "GT.gt",
 "explicit":
 "∀ (n : Nat) (k : Int),\n  @GT.gt Int Int.instLTInt\n    (@HAdd.hAdd Int Int Int (@instHAdd Int Int.instAdd) (@Nat.cast Int instNatCastInt n)\n      (@HDiv.hDiv Int Int Int (@instHDiv Int Int.instDiv) k\n        (@OfNat.ofNat Int (nat_lit 2) (@instOfNat (nat_lit 2)))))\n    (@OfNat.ofNat Int (nat_lit 0) (@instOfNat (nat_lit 0)))",
 "constants": ["GT.gt", "HAdd.hAdd", "HDiv.hDiv", "Int", "Nat", "Nat.cast"],
 "conclusion": "↑n + k / 2 > 0",
 "coercions":
 [{"to": "Int", "term": "n", "function": "Nat.cast", "from": "Nat"}],
 "binders":
 [{"used": true,
   "type": "Nat",
   "role": "variable",
   "name": "n",
   "binder": "explicit"},
  {"used": true,
   "type": "Int",
   "role": "variable",
   "name": "k",
   "binder": "explicit"}],
 "annotated": "∀ (n : Nat) (k : Int), (↑n : Int) + k / (2 : Int) > (0 : Int)"}
-/
#guard_msgs in
#maithic_stmt json ∀ (n : Nat) (k : Int), (n : Int) + k / 2 > 0

-- cap: P02-05-01
-- What is not a proposition is not a statement.
/-- error: a proposition or the name of a theorem is expected -/
#guard_msgs in
#maithic_stmt Nat

-- cap: P02-05-01
run_cmd do
  let s ← liftTermElabM <| inspect (← statementOf "∀ {α : Type} [inst : Inhabited α] (xs : List α) (n spare : Nat), xs.length = n → xs ≠ [] → xs.head! = xs.head!")
  expect "the binders, in order" (s.binders.map (·.name) == #["α", "inst", "xs", "n", "spare", "", ""])
  expect "their roles" (s.binders.map (·.role) == #["type", "instance", "variable", "variable", "variable", "hypothesis", "hypothesis"])
  expect "how they are bound" (s.binders.map (·.binder) == #["implicit", "instance", "explicit", "explicit", "explicit", "explicit", "explicit"])
  expect "their types" (s.binders.map (·.type) == #["Type", "Inhabited α", "List α", "Nat", "Nat", "xs.length = n", "xs ≠ []"])
  expect "the variables" (s.variables.map (·.name) == #["xs", "n", "spare"])
  expect "the hypotheses" (s.hypotheses.map (·.type) == #["xs.length = n", "xs ≠ []"])
  -- A type is mentioned by the variables of that type, an instance by the terms that need it.
  expect "what nothing mentions" (s.unused.map (·.name) == #["spare"])
  expect "the conclusion" (s.conclusion == "xs.head! = xs.head!" && s.head == "Eq")
  expect "the constants, without instances and numerals" (s.constants == #["Eq", "Inhabited", "List", "List.head!", "List.length", "List.nil", "Nat", "Ne"])
  -- A premise that is stated as an implication is a hypothesis without a name.
  let arrow ← liftTermElabM <| inspect (← statementOf "∀ p q : Prop, p → q → p ∧ q")
  expect "premises of implications" (arrow.hypotheses.map (·.type) == #["p", "q"] && arrow.hypotheses.all (·.name == ""))
  expect "a conclusion that is a conjunction" (arrow.head == "And")
  -- A definition that hides a quantifier is not unfolded: the statement is read as written.
  let hidden ← liftTermElabM <| inspect (← statementOf "∀ xs : List Nat, xs ⊆ xs")
  expect "one binder, and the subset as it is" (hidden.binders.size == 1 && hidden.conclusion == "xs ⊆ xs")
  expect "a binder in a line" ((s.binders[0]!).toString == "{α : Type}  type" && (s.binders[4]!).toString == "(spare : Nat)  variable, which nothing mentions")

/-! ## Nothing left implicit -/

-- cap: P02-05-02
-- Every implicit argument, every instance and the coercion as the function it is; then the usual
-- notation with the type of each numeral and of the coerced term.
/--
info: ∀ (n : Nat),
  @GT.gt Int Int.instLTInt
    (@HAdd.hAdd Int Int Int (@instHAdd Int Int.instAdd) (@Nat.cast Int instNatCastInt n)
      (@OfNat.ofNat Int (nat_lit 1) (@instOfNat (nat_lit 1))))
    (@OfNat.ofNat Int (nat_lit 0) (@instOfNat (nat_lit 0)))

with the types of numerals and coercions:
∀ (n : Nat), (↑n : Int) + (1 : Int) > (0 : Int)
-/
#guard_msgs in
#maithic_explicit ∀ n : Nat, (n : Int) + 1 > 0

-- cap: P02-05-02
-- The same numeral is a natural number here and an integer there, and the printed form says which.
/--
info: ∀ (a : Nat) (b : Int),
  And (@Eq Nat a (@OfNat.ofNat Nat (nat_lit 2) (instOfNatNat (nat_lit 2))))
    (@Eq Int b (@OfNat.ofNat Int (nat_lit 2) (@instOfNat (nat_lit 2))))

with the types of numerals and coercions:
∀ (a : Nat) (b : Int), a = (2 : Nat) ∧ b = (2 : Int)
-/
#guard_msgs in
#maithic_explicit ∀ (a : Nat) (b : Int), a = 2 ∧ b = 2

-- cap: P02-05-02
run_cmd do
  let statement ← statementOf "∀ (n m : Nat) (k : Int) (i : Fin 5), (n : Int) + k / 2 > 0 ∧ (i : Nat) - m / 3 = 0"
  let (coerced, trapped) ← liftTermElabM <| coercionsAndTraps statement
  -- Each coercion with the types it goes between.
  expect "the coercions" (coerced.map (·.function) == #["Nat.cast", "Fin.val"])
  expect "a natural number taken to the integers" (coerced[0]! == { function := "Nat.cast", term := "n", source := "Nat", target := "Int" })
  expect "an element of Fin taken to the natural numbers" (coerced[1]! == { function := "Fin.val", term := "i", source := "Fin 5", target := "Nat" })
  expect "in a line" (coerced[0]!.toString == "n : Nat is taken to Int by Nat.cast")
  -- The operations that do not mean what their symbols mean in ordinary mathematics.
  expect "the traps" ((trapped.map (·.kind)).qsort (· < ·) == #["int-division", "nat-division", "nat-subtraction"])
  expect "integer division" (trapped.any fun t => t.kind == "int-division" && t.term == "k / 2")
  expect "natural division" (trapped.any fun t => t.kind == "nat-division" && t.term == "m / 3")
  expect "natural subtraction" (trapped.any fun t => t.kind == "nat-subtraction" && t.term == "↑i - m / 3")
  -- Subtraction of integers is the subtraction everybody means.
  let integers ← liftTermElabM <| traps (← statementOf "∀ a b : Int, a - b = a - b")
  expect "integer subtraction is no trap" integers.isEmpty
  let predecessor ← liftTermElabM <| traps (← statementOf "∀ n : Nat, n.pred ≤ n")
  expect "the predecessor" (predecessor.map (·.kind) == #["nat-predecessor"] && predecessor[0]!.toString == "n.pred: the predecessor of 0 is 0")
  -- A term under a quantifier inside the statement is printed with the name it has there.
  let inner ← liftTermElabM <| traps (← statementOf "∀ x : Nat, ∃ y : Nat, y - x = 0")
  expect "under an inner binder" (inner.map (·.term) == #["y - x"])
  -- The same term twice is one finding.
  let twice ← liftTermElabM <| traps (← statementOf "∀ a b : Nat, a - b = a - b")
  expect "each once" (twice.size == 1)
  let none ← liftTermElabM <| coercions (← statementOf "∀ a b : Nat, a + b = b + a")
  expect "a statement without coercions" none.isEmpty
  let explicit ← liftTermElabM <| explicitForm (← statementOf "∀ a b : Nat, a - b = 0") 200
  expect "the explicit form names the subtraction and its type"
    (explicit == "∀ (a b : Nat), @Eq Nat (@HSub.hSub Nat Nat Nat (@instHSub Nat instSubNat) a b) (@OfNat.ofNat Nat (nat_lit 0) (instOfNatNat (nat_lit 0)))")
  let annotated ← liftTermElabM <| annotatedForm (← statementOf "∀ a b : Nat, a - b = 0")
  expect "the annotated form keeps the notation" (annotated == "∀ (a b : Nat), a - b = (0 : Nat)")

/-! ## Two statements side by side -/

/-- Two properties of numbers that nothing is known about. -/
opaque Wide : Nat → Prop
@[inherit_doc Wide] opaque Tall : Nat → Prop

-- cap: P02-05-01
-- The same statement with other names for its variables.
/-- info: identical: the same statement -/
#guard_msgs in
#maithic_compare (∀ a b : Nat, a ≤ b → a < b + 1) with (∀ x y : Nat, x ≤ y → x < y + 1)

-- cap: P02-05-01
-- The same once a definition is unfolded: `a < b` is `a + 1 ≤ b`.
/-- info: definitional: the same statement once definitions are unfolded -/
#guard_msgs in
#maithic_compare (∀ a b : Nat, a < b → Wide a) with (∀ a b : Nat, a + 1 ≤ b → Wide a)

-- cap: P02-05-01
-- The hypotheses in another order: each statement proves the other by being applied.
/-- info: equivalent: each follows from the other (proved by its own hypotheses) -/
#guard_msgs in
#maithic_compare (∀ n : Nat, Wide n → 0 < n → Tall n) with (∀ n : Nat, 0 < n → Wide n → Tall n)

-- cap: P02-05-01
-- One hypothesis more makes the weaker claim.
/-- info: stronger: the first implies the second (proved by its own hypotheses); the converse was not proved -/
#guard_msgs in
#maithic_compare (∀ n : Nat, Wide n → Tall n) with (∀ n : Nat, Wide n → 0 < n → Tall n)

-- cap: P02-05-01
/-- info: weaker: the second implies the first (proved by its own hypotheses); the converse was not proved -/
#guard_msgs in
#maithic_compare (∀ n : Nat, Wide n → 0 < n → Tall n) with (∀ n : Nat, Wide n → Tall n)

-- cap: P02-05-01
-- The converse of a statement is another statement.
/-- info: unrelated: nothing was proved either way, which does not show that the statements differ -/
#guard_msgs in
#maithic_compare (∀ n : Nat, Wide n → Tall n) with (∀ n : Nat, Tall n → Wide n)

-- cap: P02-05-01
-- Two theorems that automation proves outright imply each other, as any two true statements do.
-- That is no reason to call them one statement: a proof has to use what it starts from.
/-- info: unrelated: nothing was proved either way, which does not show that the statements differ -/
#guard_msgs in
#maithic_compare (∀ a b : Nat, a ≤ b → a < b + 1) with (∀ a b : Nat, a < b + 1 → a ≤ b)

-- cap: P02-05-01
/--
info: {"same": true, "relation": "equivalent", "by": "its own hypotheses"}
-/
#guard_msgs in
#maithic_compare json (∀ n : Nat, Wide n → 0 < n → Tall n) with (∀ n : Nat, 0 < n → Wide n → Tall n)

-- cap: P02-05-01
/--
info: {"same": false, "relation": "unrelated"}
-/
#guard_msgs in
#maithic_compare json (∀ n : Nat, Wide n → Tall n) with (∀ n : Nat, Tall n → Wide n)

set_option warn.sorry false in
/-- The statement of `pred_lt` once more, under another name. -/
theorem pred_lt_again (m : Nat) (positive : 0 < m) : m - 1 < m := sorry

-- cap: P02-05-01
-- Theorems are compared by name.
/-- info: identical: the same statement -/
#guard_msgs in
#maithic_compare pred_lt with pred_lt_again

-- cap: P02-05-01
run_cmd do
  let wide ← statementOf "∀ n : Nat, Wide n → Tall n"
  let narrow ← statementOf "∀ n : Nat, Wide n → 0 < n → Tall n"
  let forth ← liftTermElabM <| compare wide narrow
  let back ← liftTermElabM <| compare narrow wide
  expect "stronger one way" (forth == .stronger "its own hypotheses" && !forth.same)
  expect "weaker the other" (back == .weaker "its own hypotheses" && !back.same)
  expect "the same statement is one reading" ((← liftTermElabM <| compare wide wide).same)
  expect "in words" (forth.word == "stronger" && back.word == "weaker" && forth.provedBy == "its own hypotheses")
  expect "nothing proved, nothing named" (Relation.unrelated.provedBy == "" && !Relation.unrelated.same)
  expect "an equivalence is one reading" ((Relation.equivalent "omega").same && (Relation.definitional).same)

end MaithicTest.Formalize
