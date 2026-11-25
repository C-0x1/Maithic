/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# What a statement says once nothing is left implicit

Lean's notation hides what matters most when a formal statement is compared with an informal
one. `a - b` on natural numbers is not the subtraction a mathematician means: it is zero when
`b` is larger. `a / b` on natural numbers rounds down. A numeral is a natural number in one place
and an integer in another, and between the two stands a cast that the notation shows as an arrow
or not at all.

This file prints a statement so that these things can be seen. `explicitForm` writes every
implicit argument and every coercion as the function it is. `annotatedForm` keeps the notation
and adds the type of every numeral and of every coerced term. `coercions` lists the coercions of
a statement, each with the types it goes between. `traps` lists the operations that do not mean
on their type what the same symbol means in ordinary mathematics.
-/

namespace Maithic.Formalize

open Lean Meta

/-- The statement with every implicit argument written out, coercions as the functions they are,
and every name in full. -/
def explicitForm (statement : Expr) (width : Nat := 100) : MetaM String := do
  let options := (← getOptions)
    |>.setBool `pp.explicit true |>.setBool `pp.coercions false |>.setBool `pp.fullNames true
    |>.setBool `pp.universes false |>.setBool `pp.proofs true
  return (← withOptions (fun _ => options) (ppExpr statement)).pretty width

/-- The statement in its usual notation, with the type of every numeral and of every coerced
term written beside it. -/
def annotatedForm (statement : Expr) (width : Nat := 100) : MetaM String := do
  let options := (← getOptions) |>.setBool `pp.numericTypes true |>.setBool `pp.coercions.types true
  return (← withOptions (fun _ => options) (ppExpr statement)).pretty width

/-- One coercion in a statement. -/
structure Coercion where
  /-- The function that coerces, such as `Nat.cast`. -/
  function : String
  /-- The term that is coerced, as Lean prints it. -/
  term : String
  /-- The type it has. -/
  source : String
  /-- The type it is taken to. -/
  target : String
  deriving Repr, BEq, Inhabited

/-- One place where an operation does not mean what its symbol means in ordinary mathematics. -/
structure Trap where
  /-- `nat-subtraction`, `nat-division`, `int-division` or `nat-predecessor`. -/
  kind : String
  /-- The term, as Lean prints it. -/
  term : String
  /-- What the operation does there. -/
  note : String
  deriving Repr, BEq, Inhabited

/-- Whether an expression is the type of natural numbers, or of integers. -/
def isNat (e : Expr) : Bool := e.isConstOf ``Nat
@[inherit_doc isNat] def isInt (e : Expr) : Bool := e.isConstOf ``Int

/-- The trap an application is, if it is one. -/
def trapOf (e : Expr) : MetaM (Option Trap) := do
  let fn := e.getAppFn
  let args := e.getAppArgs
  let shown ← do pure (toString (← ppExpr e))
  if fn.isConstOf ``HSub.hSub && args.size == 6 && isNat args[0]! && isNat args[1]! then
    return some { kind := "nat-subtraction", term := shown,
                  note := "subtraction of natural numbers stops at zero: it is 0 whenever the second number is the larger" }
  if fn.isConstOf ``Nat.sub && args.size == 2 then
    return some { kind := "nat-subtraction", term := shown,
                  note := "subtraction of natural numbers stops at zero: it is 0 whenever the second number is the larger" }
  if fn.isConstOf ``HDiv.hDiv && args.size == 6 && isNat args[0]! && isNat args[1]! then
    return some { kind := "nat-division", term := shown,
                  note := "division of natural numbers rounds down, and division by zero is 0" }
  if fn.isConstOf ``HDiv.hDiv && args.size == 6 && isInt args[0]! && isInt args[1]! then
    return some { kind := "int-division", term := shown,
                  note := "division of integers rounds, and division by zero is 0" }
  if fn.isConstOf ``Nat.pred && args.size == 1 then
    return some { kind := "nat-predecessor", term := shown, note := "the predecessor of 0 is 0" }
  return none

/-- The coercion an application is, if it is one. -/
def coercionOf (e : Expr) : MetaM (Option Coercion) := do
  let .const name _ := e.getAppFn | return none
  let some info ← getCoeFnInfo? name | return none
  let args := e.getAppArgs
  if args.size != info.numArgs then return none
  let some coerced := args[info.coercee]? | return none
  return some {
    function := name.toString
    term := toString (← ppExpr coerced)
    source := toString (← ppExpr (← inferType coerced))
    target := toString (← ppExpr (← inferType e)) }

/-- The coercions and the traps of a statement, each once, in the order they are met. The
statement is closed; what stands under its binders is visited with the binders in scope, so that
every term is printed with the names it has in the statement. -/
def coercionsAndTraps (statement : Expr) : MetaM (Array Coercion × Array Trap) :=
  withLCtx {} {} do
    let found ← IO.mkRef ((#[], #[]) : Array Coercion × Array Trap)
    Meta.forEachExpr statement fun e => do
      if e.isApp then
        if let some c ← coercionOf e then found.modify fun (cs, ts) => (if cs.contains c then cs else cs.push c, ts)
        if let some t ← trapOf e then found.modify fun (cs, ts) => (cs, if ts.contains t then ts else ts.push t)
    found.get

/-- The coercions of a statement. -/
def coercions (statement : Expr) : MetaM (Array Coercion) := return (← coercionsAndTraps statement).1

/-- The traps of a statement. -/
def traps (statement : Expr) : MetaM (Array Trap) := return (← coercionsAndTraps statement).2

/-- A coercion as JSON. -/
def Coercion.toJson (c : Coercion) : Json :=
  Json.mkObj [("function", c.function), ("term", c.term), ("from", c.source), ("to", c.target)]

/-- A trap as JSON. -/
def Trap.toJson (t : Trap) : Json := Json.mkObj [("kind", t.kind), ("term", t.term), ("note", t.note)]

/-- A coercion in a line. -/
def Coercion.toString (c : Coercion) : String := s!"{c.term} : {c.source} is taken to {c.target} by {c.function}"

/-- A trap in a line. -/
def Trap.toString (t : Trap) : String := s!"{t.term}: {t.note}"

end Maithic.Formalize
