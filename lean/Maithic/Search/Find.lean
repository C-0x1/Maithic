/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# `#maithic_find`: declarations by the parts of their names

A proof is often one lemma away, and the lemma has a name that can nearly be guessed. This
command looks through everything that is in scope for the declarations whose names contain all of
the given parts, and prints each with its kind and its statement.

```
#maithic_find succ mul
-- Nat.succ_mul : ∀ (n m : Nat), n.succ * m = n * m + m
-- Nat.mul_succ : ∀ (n m : Nat), n * m.succ = n * m + n
-- …
```

Parts are compared without regard to case. A part that is a whole component of a name counts for
more than one that is a piece of a component, and a shorter name comes before a longer one, so the
basic lemmas come first. Declarations that Lean generates for its own use are left out.
`#maithic_find json` prints the findings as JSON, for a program to read, and `(limit := n)` sets
how many are shown.
-/

namespace Maithic.Search

open Lean Meta Elab Command

/-- A declaration that was found. -/
structure Hit where
  name : Name
  /-- `theorem`, `definition`, `inductive`, `constructor`, `axiom`, `opaque` or `recursor`. -/
  kind : String
  /-- The type of the declaration, as Lean prints it. -/
  type : String
  /-- How many of the parts are whole components of the name, or whole words of one. -/
  whole : Nat
  deriving Inhabited

/-- The kind of a declaration, in a word. -/
def kindOf : ConstantInfo → String
  | .thmInfo _ => "theorem"
  | .defnInfo _ => "definition"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "constructor"
  | .axiomInfo _ => "axiom"
  | .opaqueInfo _ => "opaque"
  | .recInfo _ => "recursor"
  | .quotInfo _ => "axiom"

/-- The components of a name as lower-case strings; a numeric component is its digits. -/
def components (name : Name) : List String :=
  name.components.map fun
    | .str _ s => s.toLower
    | .num _ n => toString n
    | .anonymous => ""

/-- Whether a declaration is one a person would look for: it is not private, and it is not one of
the declarations Lean generates for its own use, such as the equations of a definition or the
auxiliary definitions of a `match`. -/
def userFacing (env : Environment) (name : Name) : Bool :=
  !name.isInternalDetail && !isPrivateName name && !isAuxRecursor env name && !isNoConfusion env name
    && !name.isAnonymous

/-- How well a name matches the parts: nothing when a part does not occur in it; otherwise the
number of parts that are whole components of the name, or whole words of a component where words
are set off by underscores. -/
def matchParts (parts : Array String) (name : Name) : Option Nat :=
  let comps := components name
  let full := ".".intercalate comps
  let words := comps.flatMap fun c => c :: c.splitOn "_"
  if parts.all fun p => (full.splitOn p).length > 1 then
    some (parts.foldl (fun n p => if words.contains p then n + 1 else n) 0)
  else none

/-- The declarations in scope whose names contain all the parts, best first: more parts that are
whole components or words, then shorter names, then by name. At most `limit` are returned. -/
def find (parts : Array String) (limit : Nat := 20) : MetaM (Array Hit) := do
  let parts := parts.map String.toLower |>.filter (· ≠ "")
  if parts.isEmpty then return #[]
  let env ← getEnv
  let found := env.constants.fold (init := (#[] : Array (Name × ConstantInfo × Nat))) fun acc name info =>
    if userFacing env name then
      match matchParts parts name with
      | some whole => acc.push (name, info, whole)
      | none => acc
    else acc
  let length (n : Name) := n.toString.length
  let sorted := found.qsort fun (a, _, wa) (b, _, wb) =>
    wa > wb || (wa == wb && (length a < length b || (length a == length b && a.toString < b.toString)))
  let mut hits := #[]
  for (name, info, whole) in sorted.extract 0 limit do
    let type ← ppExpr info.type
    let line := " ".intercalate ((toString type).splitOn "\n" |>.map fun part => part.trimAscii.toString)
    hits := hits.push { name, kind := kindOf info, type := line, whole }
  return hits

/-- A hit in a line: the name and the statement. -/
def Hit.toString (h : Hit) : String := s!"{h.name} : {h.type}"

/-- A hit as JSON. -/
def Hit.toJson (h : Hit) : Json :=
  Json.mkObj [("name", h.name.toString), ("kind", h.kind), ("type", h.type), ("whole", h.whole)]

/--
`#maithic_find part…` lists the declarations in scope whose names contain every part, with their
statements: the basic ones first. A part is a word, or a text in quotes. `#maithic_find json`
prints the list as JSON, and `(limit := n)` sets how many are shown; twenty are shown otherwise.
-/
syntax (name := maithicFind) "#maithic_find " (&"json ")? ("(" &"limit" " := " num ")")? (str <|> ident)+ : command

@[command_elab maithicFind] def elabMaithicFind : CommandElab := fun stx => do
  let asJson := !stx[1].isNone
  let limit := if stx[2].isNone then 20 else stx[2][3].toNat
  let parts := stx[3].getArgs.map fun part =>
    match part.isStrLit? with
    | some text => text
    | none => part.getId.toString
  let hits ← liftTermElabM (find parts limit)
  if asJson then
    logInfo (Json.mkObj [("parts", Json.arr (parts.map Json.str)), ("hits", Json.arr (hits.map Hit.toJson))]).pretty
  else if hits.isEmpty then
    logInfo s!"no declaration has a name with all of: {", ".intercalate parts.toList}"
  else
    logInfo ("\n".intercalate (hits.toList.map Hit.toString))

end Maithic.Search
