/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Lint.Framework

/-!
# The rules

- `sorry`: a declaration rests on `sorry`, in its own text or through something it uses.
- `axiom`: a declaration is an axiom, or rests on an axiom beyond the three of Lean's logic.
- `docstring`: a public definition, or a theorem that nothing else in the file uses, has no
  documentation.
- `naming`: a name does not follow the convention for its kind.
-/

namespace Maithic.Lint

open Maithic.Extract

/-- The axioms of Lean's own logic. A proof that rests on these alone rests on nothing unusual. -/
def standardAxioms : Array String := #["propext", "Classical.choice", "Quot.sound"]

/-- Whether a declaration of the file uses the declaration with the given name. -/
def usedBy (name : String) (d : Declaration) : Bool :=
  d.name != name && (d.valueDeps.contains name || d.typeDeps.contains name)

/-- The rule for unfinished work. The kernel's own account is used: a declaration rests on `sorry`
when `sorryAx` is among its axioms, wherever in its dependencies the `sorry` stands. -/
def sorryRule : Rule where
  name := "sorry"
  description := "A declaration rests on sorry, in its own text or through a declaration it uses."
  severity := "error"
  check ctx d :=
    if !d.usesSorry then #[]
    else if d.valueDeps.contains "sorryAx" || d.typeDeps.contains "sorryAx" then
      #[s!"the {d.kind} contains sorry"]
    else
      let through := (d.valueDeps ++ d.typeDeps).filter fun dep =>
        ctx.declarations.any fun other => other.name == dep && other.usesSorry
      match through[0]? with
      | some first => #[s!"the {d.kind} depends on sorry through {first}"]
      | none => #[s!"the {d.kind} depends on sorry through a declaration of another file"]

/-- The rule for axioms. `sorryAx` is left to the rule for unfinished work. -/
def axiomRule : Rule where
  name := "axiom"
  description := "A declaration is an axiom, or rests on an axiom other than propext, Classical.choice and Quot.sound."
  severity := "warning"
  check _ d :=
    if d.kind == "axiom" then #["declares an axiom: nothing checks that it is true"]
    else
      let other := d.axioms.filter fun a => !standardAxioms.contains a && a != "sorryAx"
      if other.isEmpty then #[]
      else #[s!"rests on {", ".intercalate other.toList}, beyond the axioms of Lean's logic"]

/-- The rule for documentation. Definitions and types are read by people who did not write them. A
theorem that nothing else in the file uses is a result of the file and not a step on the way. -/
def docstringRule : Rule where
  name := "docstring"
  description := "A public definition, or a theorem that nothing else in the file uses, has no documentation."
  severity := "info"
  check ctx d :=
    if d.doc.isSome || d.isPrivate then #[]
    else if #["definition", "structure", "class", "inductive"].contains d.kind then
      #[s!"the {d.kind} has no documentation"]
    else if d.kind == "theorem" && !(ctx.declarations.any (usedBy d.name)) then
      #["the theorem is a result of this file, since nothing else in it uses it, and has no documentation"]
    else #[]

/-- Whether a name is written in a convention. Names with letters outside the Latin alphabet are
not judged. `snake_case` is the convention of Lean's libraries for theorems: small words joined by
underscores, where a word that is the name of a definition keeps its capitals. -/
def conforms (convention name : String) : Bool :=
  let ascii := name.all fun c => c.isAlphanum || c == '_' || c == '\''
  if !ascii || name.isEmpty then true
  else match convention with
    | "snake_case" =>
      -- Words joined by underscores, each beginning with a small letter or a digit. A word may
      -- have capitals further in: the name of a definition keeps its form inside the name of a
      -- theorem about it, as in `insertionSort_idem`.
      (name.splitOn "_").all fun word => !word.isEmpty && (word.front.isLower || word.front.isDigit)
    | "lowerCamelCase" => (name.front.isLower || name.front == '_') && !(name.contains '_' && name.front != '_')
    | "UpperCamelCase" => name.front.isUpper && !name.contains '_'
    | _ => true

/-- Whether the printed type of a declaration ends in a universe: the declaration is a type or a
proposition, or a family of them. -/
def definesType (type : String) : Bool :=
  let result := ((type.splitOn "→").getLast?.getD type).trimAscii.toString
  result == "Prop" || result.startsWith "Type" || result.startsWith "Sort"

/-- The rule for names. Theorems, definitions and types each have a convention; a definition that
defines a type or a proposition is named like a type. Instances are not judged, since most are
named by Lean. -/
def namingRule : Rule where
  name := "naming"
  description := "A name does not follow the convention for its kind."
  severity := "info"
  check ctx d :=
    let last := (d.name.splitOn ".").getLast?.getD d.name
    let expected :=
      if d.kind == "theorem" then some ctx.convention.theorems
      else if #["structure", "class", "inductive"].contains d.kind then some ctx.convention.types
      else if d.kind == "definition" then
        some (if definesType d.type then ctx.convention.types else ctx.convention.definitions)
      else none
    match expected with
    | some convention =>
      if conforms convention last then #[] else #[s!"the name {last} is not in {convention}, the convention for a {d.kind}"]
    | none => #[]

/-- The rules that ship. -/
def builtin : Array Rule := #[sorryRule, axiomRule, docstringRule, namingRule]

end Maithic.Lint
