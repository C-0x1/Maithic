/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Formalize.Inspect
import Maithic.Formalize.Elaborated
import Maithic.Formalize.Compare
import Maithic.Refute.Testing.Command

/-!
# `#maithic_stmt`, `#maithic_explicit` and `#maithic_compare`

Three commands for looking at a statement before anyone tries to prove it.

```
#maithic_stmt ∀ (a b : Nat) (h : b ≤ a), a - b + b = a
-- ∀ (a b : Nat), b ≤ a → a - b + b = a
--   (a : Nat)  variable
--   (b : Nat)  variable
--   (h : b ≤ a)  hypothesis
--   ⊢ a - b + b = a
--   a - b: subtraction of natural numbers stops at zero: ...

#maithic_explicit ∀ n : Nat, (n : Int) + 1 > 0
-- ∀ (n : Nat), @GT.gt Int Int.instLTInt (@HAdd.hAdd Int Int Int ... (@Nat.cast Int ... n) ...) ...

#maithic_compare (∀ a b : Nat, a ≤ b → a < b + 1) with (∀ a b : Nat, a < b + 1 → a ≤ b)
-- unrelated: nothing was proved either way, ...
```

Each command takes a proposition written out, or the name of a declared theorem, whose statement
is then examined. `#maithic_stmt json` and `#maithic_compare json` print their findings as JSON,
for a program to read.
-/

namespace Maithic.Formalize

open Lean Meta Elab Command
open Maithic.Refute (elabStatement)

/-- Everything `#maithic_stmt` reports about a statement. -/
structure Report where
  statement : Statement
  explicit : String
  annotated : String
  coercions : Array Coercion
  traps : Array Trap

/-- Examines a closed statement. -/
def report (statement : Expr) : MetaM Report := do
  let (coercions, traps) ← coercionsAndTraps statement
  return {
    statement := ← inspect statement
    explicit := ← explicitForm statement
    annotated := ← annotatedForm statement
    coercions, traps }

/-- The report as JSON: the statement as Lean prints it, its binders with their roles, its
conclusion with the constant at its head, the constants it mentions, its explicit and its
annotated form, its coercions and its traps. -/
def Report.toJson (r : Report) : Json :=
  Json.mkObj [
    ("statement", r.statement.pretty),
    ("binders", Json.arr (r.statement.binders.map Binder.toJson)),
    ("conclusion", r.statement.conclusion),
    ("head", r.statement.head),
    ("constants", Json.arr (r.statement.constants.map Json.str)),
    ("unused", Json.arr (r.statement.unused.map fun b => Json.str (if b.name.isEmpty then b.type else b.name))),
    ("explicit", r.explicit),
    ("annotated", r.annotated),
    ("coercions", Json.arr (r.coercions.map Coercion.toJson)),
    ("traps", Json.arr (r.traps.map Trap.toJson))]

/-- The report for a person: the statement, a line for each binder, the conclusion, and then
what deserves a second look. -/
def Report.toString (r : Report) : String :=
  let binders := r.statement.binders.toList.map fun b => s!"  {b.toString}"
  let coercions := r.coercions.toList.map fun c => s!"  coercion  {c.toString}"
  let traps := r.traps.toList.map fun t => s!"  trap  {t.toString}"
  String.intercalate "\n" ([r.statement.pretty] ++ binders ++ [s!"  ⊢ {r.statement.conclusion}"] ++ coercions ++ traps)

/-- Whether the report holds something that deserves a second look: a binder nothing mentions, or
an operation that does not mean what its symbol suggests. -/
def Report.suspicious (r : Report) : Bool := !r.statement.unused.isEmpty || !r.traps.isEmpty

/--
`#maithic_stmt p` takes the statement `p` apart: its binders, each with its type, its role and
whether anything mentions it; its conclusion; the coercions Lean inserted; and the operations that
do not mean on their type what the symbol means in ordinary mathematics, such as subtraction of
natural numbers. `p` is a proposition or the name of a theorem. A binder that nothing mentions,
and a trap, make the report a warning. `#maithic_stmt json p` prints the report as JSON.
-/
syntax (name := maithicStmt) "#maithic_stmt " (&"json ")? term : command

@[command_elab maithicStmt] def elabMaithicStmt : CommandElab := fun stx =>
  runTermElabM fun variables => do
    let r ← report (← elabStatement stx[2] variables)
    if !stx[1].isNone then logInfo r.toJson.pretty
    else if r.suspicious then logWarning r.toString
    else logInfo r.toString

/--
`#maithic_explicit p` prints the statement `p` with nothing left implicit: every implicit
argument, every instance and every coercion written out as the function it is; and then once
more in its usual notation, with the type of every numeral and of every coerced term beside it.
`p` is a proposition or the name of a theorem.
-/
syntax (name := maithicExplicit) "#maithic_explicit " term : command

@[command_elab maithicExplicit] def elabMaithicExplicit : CommandElab := fun stx =>
  runTermElabM fun variables => do
    let statement ← elabStatement stx[1] variables
    logInfo s!"{← explicitForm statement}\n\nwith the types of numerals and coercions:\n{← annotatedForm statement}"

/--
`#maithic_compare p with q` says how the statement `p` stands to the statement `q`: identical,
the same by definition, equivalent, stronger, weaker, or unrelated as far as the automation can
tell. Every answer but the last rests on a proof Lean's kernel accepted. `p` and `q` are
propositions or names of theorems. `#maithic_compare json p with q` prints the answer as JSON.
-/
syntax (name := maithicCompare) "#maithic_compare " (&"json ")? term " with " term : command

@[command_elab maithicCompare] def elabMaithicCompare : CommandElab := fun stx =>
  runTermElabM fun variables => do
    let relation ← compare (← elabStatement stx[2] variables) (← elabStatement stx[4] variables)
    logInfo (if stx[1].isNone then relation.toString else relation.toJson.pretty)

/-- The variables of a statement given to these commands are named so that the report can name
them. A hypothesis that nothing refers to is what the report is there to point out. -/
@[unused_variables_ignore_fn]
def ignoreInspectedStatement : Lean.Linter.IgnoreFunction := fun _ stack _ =>
  stack.any fun (stx, _) =>
    stx.isOfKind ``maithicStmt || stx.isOfKind ``maithicExplicit || stx.isOfKind ``maithicCompare

end Maithic.Formalize
