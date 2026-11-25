/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# The parts of a statement

A theorem statement, once Lean has elaborated it, is a chain of binders and a conclusion. Some
binders are the things the statement is about, some are the assumptions it makes, some are types
and some are instances that Lean found. A person who checks whether a formal statement says what
an informal one meant looks at exactly these parts: what is quantified, over which type, under
which assumptions, and what is claimed.

`inspect` takes a statement apart into those parts. Each binder is reported with its type as
Lean prints it, with the way it is bound, with its role, and with whether anything after it
mentions it. A variable that nothing mentions is a sign that the statement does not say what was
meant: the informal text spoke of it, and the formal one forgot it.
-/

namespace Maithic.Formalize

open Lean Meta

/-- One binder of a statement. -/
structure Binder where
  /-- The name, empty for a binder that was given none, as the premise of an implication. -/
  name : String
  /-- The type, as Lean prints it. -/
  type : String
  /-- How the binder is bound: `explicit`, `implicit`, `strict-implicit` or `instance`. -/
  binder : String
  /-- What the binder is: a `type`, an `instance`, a `hypothesis` (its type is a proposition) or
  a `variable`. -/
  role : String
  /-- Whether a later binder or the conclusion mentions it. A hypothesis counts as used: it is
  there to be assumed, not to be mentioned. -/
  used : Bool
  deriving Repr, BEq, Inhabited

/-- A statement taken apart. -/
structure Statement where
  /-- The binders, in order. -/
  binders : Array Binder
  /-- What is claimed, as Lean prints it. -/
  conclusion : String
  /-- The constant at the head of the conclusion, such as `Eq`, `LE.le`, `Exists` or `And`;
  empty when the conclusion is not an application of a constant. -/
  head : String
  /-- The constants the statement mentions, without instances and without names Lean made up,
  in the order of their names. -/
  constants : Array String
  /-- The whole statement, as Lean prints it. -/
  pretty : String
  deriving Repr, BEq, Inhabited

/-- The variables of a statement: the binders that are neither hypotheses, nor types, nor instances. -/
def Statement.variables (s : Statement) : Array Binder := s.binders.filter (·.role == "variable")

/-- The hypotheses of a statement. -/
def Statement.hypotheses (s : Statement) : Array Binder := s.binders.filter (·.role == "hypothesis")

/-- The variables, types and instances that nothing after them mentions. The informal text spoke
of them, or the person who wrote the statement thought it did, and the formal statement says
nothing about them. -/
def Statement.unused (s : Statement) : Array Binder := s.binders.filter fun b => !b.used

/-- How a binder is bound, in a word. -/
def binderWord : BinderInfo → String
  | .default => "explicit"
  | .implicit => "implicit"
  | .strictImplicit => "strict-implicit"
  | .instImplicit => "instance"

/-- The name of a binder as a person wrote it: nothing for a name Lean made up. -/
def writtenName (name : Name) : String :=
  if name.isAnonymous || name.hasMacroScopes || name.isInternal then "" else name.toString

/-- The constants a person would recognise in a statement: no instances, no names Lean made up,
and not the wrapper around numerals. -/
def recognisable (constants : Array Name) : MetaM (Array String) := do
  let mut kept : Array String := #[]
  for name in constants do
    if name.isInternal || name.hasMacroScopes then continue
    if name == ``OfNat.ofNat then continue
    if ← isInstance name then continue
    kept := kept.push name.toString
  return (kept.qsort (· < ·)).foldl (fun acc n => if acc.back? == some n then acc else acc.push n) #[]

/-- Takes a statement apart. The statement is closed, and it is read as it is written: a
definition that hides a quantifier is not unfolded. -/
def inspect (statement : Expr) : MetaM Statement :=
  withLCtx {} {} <| forallTelescope statement fun binders conclusion => do
    let types ← binders.mapM inferType
    let mut parts : Array Binder := #[]
    for h : i in [0:binders.size] do
      let binder := binders[i]
      let decl ← binder.fvarId!.getDecl
      let type := types[i]!
      let role ←
        if ← isProp type then pure "hypothesis"
        else if decl.binderInfo.isInstImplicit then pure "instance"
        else if (← whnf type).isSort then pure "type"
        else pure "variable"
      let later := (types.extract (i + 1) types.size).any (·.containsFVar binder.fvarId!)
      parts := parts.push {
        name := writtenName decl.userName
        type := toString (← ppExpr type)
        binder := binderWord decl.binderInfo
        role
        used := role == "hypothesis" || later || conclusion.containsFVar binder.fvarId! }
    return {
      binders := parts
      conclusion := toString (← ppExpr conclusion)
      head := match conclusion.getAppFn with | .const name _ => name.toString | _ => ""
      constants := ← recognisable statement.getUsedConstants
      pretty := toString (← ppExpr statement) }

/-- A binder as JSON. -/
def Binder.toJson (b : Binder) : Json :=
  Json.mkObj [("name", b.name), ("type", b.type), ("binder", b.binder), ("role", b.role), ("used", b.used)]

/-- A binder in a line: `(n : Nat)`, `{α : Type}`, `[inst : Add α]`, with its role. -/
def Binder.toString (b : Binder) : String :=
  let inside := if b.name.isEmpty then b.type else s!"{b.name} : {b.type}"
  let bracketed := match b.binder with
    | "implicit" => "{" ++ inside ++ "}"
    | "strict-implicit" => "⦃" ++ inside ++ "⦄"
    | "instance" => "[" ++ inside ++ "]"
    | _ => "(" ++ inside ++ ")"
  s!"{bracketed}  {b.role}" ++ (if b.used then "" else ", which nothing mentions")

end Maithic.Formalize
