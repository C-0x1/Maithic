/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Extract.Declarations

/-!
# The lint framework

A rule looks at one declaration of a file, with the other declarations of the file in view, and
says what is wrong with it, if anything. The framework runs a set of rules over the declarations
and returns findings: which rule, how severe, which declaration, where, and a sentence.

Rules work on the records of `Maithic.Extract`, so they see what the kernel saw: the axioms a
declaration really rests on and the constants it really uses, not what its text suggests.
-/

namespace Maithic.Lint

open Lean (ToJson FromJson)
open Maithic.Extract

/-- One thing a rule found. -/
structure Finding where
  rule : String
  /-- `info`, `warning` or `error`. -/
  severity : String
  declaration : String
  /-- The line on which the name of the declaration stands. -/
  line : Nat
  message : String
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- The ways of writing a name that the rule for names can judge; `any` judges nothing. -/
def conventions : List String := ["snake_case", "lowerCamelCase", "UpperCamelCase", "any"]

/-- How names are to be written, for each kind of declaration: one of `conventions`. -/
structure Convention where
  theorems : String := "snake_case"
  definitions : String := "lowerCamelCase"
  types : String := "UpperCamelCase"
  deriving ToJson, FromJson, Repr, Inhabited

/-- What a rule may look at besides the declaration in front of it. -/
structure Context where
  declarations : Array Declaration
  convention : Convention := {}

/-- A check over declarations. `check` returns one sentence for each thing it finds. -/
structure Rule where
  name : String
  description : String
  severity : String
  check : Context → Declaration → Array String

/-- Runs rules over the declarations of a context. Findings come in the order of the file, and for
one declaration in the order of the rules. -/
def run (rules : Array Rule) (ctx : Context) : Array Finding :=
  ctx.declarations.flatMap fun d =>
    rules.flatMap fun rule =>
      (rule.check ctx d).map fun message =>
        { rule := rule.name, severity := rule.severity, declaration := d.name, line := d.nameLine, message }

/-- The rules among `rules` with the given names, in the order of the names. An unknown name is an
error that lists the names there are. -/
def select (rules : Array Rule) (names : Array String) : Except String (Array Rule) :=
  names.mapM fun name =>
    match rules.find? (·.name == name) with
    | some rule => .ok rule
    | none => .error s!"there is no rule '{name}'; the rules are {", ".intercalate (rules.map (·.name)).toList}"

end Maithic.Lint
