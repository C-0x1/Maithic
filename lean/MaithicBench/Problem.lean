/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Benchmark problems

A benchmark problem is a definition of type `Prop`. Stating problems as propositions rather than
as theorems with missing proofs means a suite compiles as it is, with no `sorry` anywhere, and a
solution is simply a term of the stated type.

The attribute `@[maithic_problem "suite-NNN" (tags := ["nat"]) (difficulty := 2)]` registers a
definition as a problem. Identifiers are permanent: the attribute rejects a duplicate, and a
retired identifier is never given to another problem.
-/

namespace MaithicBench

open Lean Meta

/-- One registered problem. -/
structure Problem where
  /-- Permanent identifier, of the form `suite-NNN`. -/
  id : String
  /-- The suite the problem belongs to: the part of the identifier before the last dash. -/
  suite : String
  /-- The definition that states the problem. -/
  decl : Name
  /-- The statement, pretty-printed. -/
  statement : String
  tags : Array String
  /-- From 1 (one automation step) to 5. -/
  difficulty : Nat
  deriving Inhabited, Repr, BEq

/-- The suite of an identifier: everything before the last dash. -/
def suiteOf (id : String) : String :=
  match (id.splitOn "-").reverse with
  | _ :: rest@(_ :: _) => "-".intercalate rest.reverse
  | _ => id

/-- `true` for identifiers of the form `name-NNN` with a lowercase name and three digits. -/
def validId (id : String) : Bool :=
  let namePart (part : String) : Bool :=
    !part.isEmpty && part.toList.all fun c => c.isLower || c.isDigit
  match (id.splitOn "-").reverse with
  | [] => false
  | [_] => false
  | num :: rest => num.length == 3 && num.toList.all Char.isDigit && rest.all namePart

/-- The environment extension that holds the registered problems. -/
initialize problemExt : SimplePersistentEnvExtension Problem (Array Problem) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := Array.push
    addImportedFn := fun arrays => arrays.foldl (· ++ ·) #[]
  }

/-- Every problem registered in the environment, ordered by identifier. -/
def problems (env : Environment) : Array Problem :=
  (problemExt.getState env).qsort fun a b => a.id < b.id

/-- `@[maithic_problem "suite-NNN" (tags := [...]) (difficulty := n)]` registers a `Prop`-valued
definition as a benchmark problem. -/
syntax (name := maithicProblem) "maithic_problem " str
  (" (" &"tags" " := " "[" str,* "]" ")")? (" (" &"difficulty" " := " num ")")? : attr

initialize registerBuiltinAttribute {
  name := `maithicProblem
  descr := "Registers a Prop-valued definition as a Maithic benchmark problem."
  add := fun decl stx kind => do
    unless kind == AttributeKind.global do
      throwError "maithic_problem: the attribute must be global"
    let (id, tags, difficulty) ← match stx with
      | `(attr| maithic_problem $id:str $[(tags := [$tags:str,*])]? $[(difficulty := $d:num)]?) =>
        let tags := match tags with
          | some ts => ts.getElems.map (·.getString)
          | none => #[]
        let difficulty := match d with
          | some d => d.getNat
          | none => 1
        pure (id.getString, tags, difficulty)
      | _ => Elab.throwUnsupportedSyntax
    unless validId id do
      throwError "maithic_problem: identifier '{id}' must have the form name-NNN"
    unless 1 ≤ difficulty && difficulty ≤ 5 do
      throwError "maithic_problem: difficulty must be between 1 and 5"
    let info ← getConstInfo decl
    unless info.type.isProp do
      throwError "maithic_problem: '{decl}' must be a definition of type Prop"
    let some value := info.value?
      | throwError "maithic_problem: '{decl}' must be a definition with a body"
    if (problemExt.getState (← getEnv)).any (·.id == id) then
      throwError "maithic_problem: identifier '{id}' is already in use"
    let statement := (← (ppExpr value).run').pretty 100
    modifyEnv (problemExt.addEntry · { id, suite := suiteOf id, decl, statement, tags, difficulty })
}

/-- One problem as JSON, in the shape of `schemas/bench/problem.schema.json`. -/
def Problem.toJson (p : Problem) : Json :=
  Json.mkObj [
    ("id", Json.str p.id),
    ("suite", Json.str p.suite),
    ("decl", Json.str p.decl.toString),
    ("statement", Json.str p.statement),
    ("tags", Json.arr (p.tags.map Json.str)),
    ("difficulty", Json.num (JsonNumber.fromNat p.difficulty))]

/-- The manifest of a list of problems, in the shape of `schemas/bench/manifest.schema.json`. -/
def manifestJson (ps : Array Problem) : Json :=
  Json.mkObj [
    ("schema", Json.str "maithic.bench.manifest/1"),
    ("count", Json.num (JsonNumber.fromNat ps.size)),
    ("problems", Json.arr (ps.map Problem.toJson))]

open Elab Command in
/-- `maithic_manifest_def name` defines `name : String` as the JSON manifest of every problem
registered so far. The manifest is computed while the file is compiled, so reading it at run time
needs no access to the environment. -/
elab "maithic_manifest_def " id:ident : command => do
  let text := (manifestJson (problems (← getEnv))).pretty 100
  elabCommand (← `(def $id : String := $(quote text)))

end MaithicBench
