/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Extract
import Maithic.Trace
import Maithic.Lint

/-!
# Tests of extraction, traces and lint rules

A small file is elaborated in memory, with information trees switched on, and everything the
libraries say about it is compared with what the file contains. The file has an unfinished proof
and an axiom on purpose; they live in a string, so this module itself has neither.
-/

namespace MaithicTest.Extract

open Lean Elab Command Maithic

/-- The file under test. Line 1 is empty, so the module documentation stands on line 2. -/
def sample : String := "
/-! Notes about doubling. -/

/-- Doubles a number. -/
def double : Nat → Nat
  | 0 => 0
  | n + 1 => double n + 2

theorem double_eq (n : Nat) : double n = 2 * n := by
  induction n with
  | zero => rfl
  | succ n ih =>
    simp [double]
    omega

theorem Bad_Name (a b : Nat) : a + b = b + a := Nat.add_comm a b

theorem open_one (n : Nat) : n + 0 = n := by
  sorry

theorem uses_open (n : Nat) : n + 0 = n := open_one n

structure Point where
  x : Nat
  y : Nat

instance : Inhabited Point := ⟨⟨0, 0⟩⟩

axiom shortcut : ∀ n : Nat, n = n + 0

theorem by_shortcut (n : Nat) : n = n + 0 := shortcut n
"

/-- What an elaboration of the sample leaves behind. -/
structure Elaborated where
  base : Environment
  env : Environment
  fileMap : FileMap
  trees : PersistentArray InfoTree
  decls : Array Extract.Declaration

/-- Elaborates a source, the sample unless another is given, on top of the environment of this
module. -/
def elaborate (duplicates : Bool := false) (source : String := sample) : CommandElabM Elaborated := do
  let base ← getEnv
  let inputCtx := Parser.mkInputContext source "<sample>"
  let options := ({} : Options).set `Elab.async false
  let start : Command.State := { Command.mkState base {} options with infoState := { enabled := true } }
  let done ← IO.processCommands inputCtx {} start
  let state := done.commandState
  let decls ← Extract.runMeta state.env (Extract.declarationsSince base "Demo" duplicates)
  let decls := Extract.attachTexts decls (Extract.declarationTexts inputCtx.fileMap state.infoState.trees)
  return { base, env := state.env, fileMap := inputCtx.fileMap, trees := state.infoState.trees, decls }

/-- Fails the build with a message when a check does not hold. -/
def expect (what : String) (holds : Bool) : CommandElabM Unit :=
  unless holds do throwError "check failed: {what}"

/-- The declaration of the sample with the given name. -/
def Elaborated.get (e : Elaborated) (name : String) : CommandElabM Extract.Declaration :=
  match e.decls.find? (·.name == name) with
  | some d => pure d
  | none => throwError "the sample has no declaration {name}"

-- cap: P07-01-01
-- cap: P07-01-02
-- cap: P07-01-03
-- cap: P07-01-05
run_cmd do
  let e ← elaborate
  -- Every declaration somebody wrote, in the order of the file, and nothing Lean derived.
  expect "names in order" (e.decls.map (·.name) ==
    #["double", "double_eq", "Bad_Name", "open_one", "uses_open", "Point", "instInhabitedPoint", "shortcut", "by_shortcut"])
  expect "kinds" (e.decls.map (·.kind) ==
    #["definition", "theorem", "theorem", "theorem", "theorem", "structure", "instance", "axiom", "theorem"])
  let double ← e.get "double"
  let doubleEq ← e.get "double_eq"
  expect "type of a definition" (double.type == "Nat → Nat")
  expect "statement of a theorem" (doubleEq.type == "∀ (n : Nat), double n = 2 * n")
  expect "module" (e.decls.all (·.module == "Demo"))
  -- The range of a declaration starts at its documentation and ends with its last line.
  expect "range of double" ((double.line, double.column, double.endLine) == (4, 0, 7))
  expect "range of double_eq" ((doubleEq.line, doubleEq.endLine) == (9, 14))
  -- The name stands after the documentation and the keyword: that is where a reader is sent.
  expect "name of double" ((double.nameLine, double.nameColumn) == (5, 4))
  expect "name of double_eq" ((doubleEq.nameLine, doubleEq.nameColumn) == (9, 8))
  expect "a name stands inside the range of its declaration" (e.decls.all fun d => d.line ≤ d.nameLine && d.nameLine ≤ d.endLine)
  -- One record per line, each a JSON object that reads back as the record.
  let lines := (Trace.jsonLines e.decls).splitOn "\n" |>.filter (· != "")
  expect "one line per declaration" (lines.length == e.decls.size)
  for (line, d) in lines.zip e.decls.toList do
    match Json.parse line >>= fromJson? (α := Extract.Declaration) with
    | .ok back => expect s!"line of {d.name} reads back" (back.name == d.name && back.type == d.type && back.valueDeps == d.valueDeps)
    | .error err => throwError "a line is not a record: {err}"
  -- Dependencies: what the type and the value refer to, without the declaration itself and
  -- without the auxiliary constants Lean made up on the way.
  expect "the statement refers to double" (doubleEq.typeDeps.contains "double")
  expect "a proof refers to the lemma it applies" ((← e.get "Bad_Name").valueDeps == #["Nat", "Nat.add_comm"])
  expect "a proof refers to the theorem it uses" ((← e.get "uses_open").valueDeps.contains "open_one")
  expect "a recursive definition does not list itself" (!double.valueDeps.contains "double")
  expect "no auxiliary constants" (e.decls.all fun d =>
    (d.typeDeps ++ d.valueDeps).all fun dep => !dep.toName.isInternalDetail)
  expect "an instance names its class" ((← e.get "instInhabitedPoint").instanceOf == some "Inhabited")
  -- Documentation of declarations and of the module.
  expect "documentation of double" (double.doc == some "Doubles a number.")
  expect "no documentation where there is none" (doubleEq.doc.isNone && (← e.get "Point").doc.isNone)
  -- The module documentation of the sample comes after that of the module the test runs in.
  expect "module documentation" ((Extract.moduleDocs e.env).back? == some "Notes about doubling." &&
    (Extract.moduleDocs e.env).size == (Extract.moduleDocs e.base).size + 1)
  -- Texts: the whole declaration and what follows `:=`.
  expect "proof text" ((← e.get "Bad_Name").proof == some "Nat.add_comm a b")
  expect "source text" ((← e.get "Bad_Name").source == some "theorem Bad_Name (a b : Nat) : a + b = b + a := Nat.add_comm a b")
  expect "equations of a definition" (double.proof == some "| 0 => 0\n  | n + 1 => double n + 2")
  -- Unfinished work is flagged from the kernel's account of axioms, wherever the sorry stands.
  expect "sorry in the proof" (← e.get "open_one").usesSorry
  expect "sorry through a dependency" (← e.get "uses_open").usesSorry
  expect "finished proofs" (!doubleEq.usesSorry && !(← e.get "by_shortcut").usesSorry)
  expect "axioms are listed" ((← e.get "by_shortcut").axioms == #["shortcut"])

-- cap: P07-01-04
run_cmd do
  let imports ← Extract.importsOf "import Lean.Data.Json\nimport Init.Data.Nat.Basic\nimport Lean.Data.Json\n\ndef x := 1\n"
  expect "imports in order, each once, after Init" (imports == #["Init", "Lean.Data.Json", "Init.Data.Nat.Basic"])
  expect "a file with no import imports Init" ((← Extract.importsOf "def x := 1\n") == #["Init"])
  expect "a prelude imports nothing" ((← Extract.importsOf "prelude\ndef x := 1\n") == #[])

-- cap: P03-02-01
run_cmd do
  let e ← elaborate (duplicates := true)
  -- Statements are compared up to the names of bound variables, across the whole environment.
  expect "a theorem that restates a library lemma" ((← e.get "Bad_Name").sameStatement == #["Nat.add_comm"])
  expect "two theorems of the file with one statement" ((← e.get "open_one").sameStatement == #["Nat.add_zero", "uses_open"])
  expect "a statement of its own" ((← e.get "double_eq").sameStatement.isEmpty)
  expect "hashes agree for equal statements"
    ((← e.get "open_one").statementHash == (← e.get "uses_open").statementHash &&
     (← e.get "open_one").statementHash != (← e.get "double_eq").statementHash)
  let plain ← elaborate
  expect "not computed unless asked for" (plain.decls.all (·.sameStatement.isEmpty))

-- cap: P11-01-01
-- cap: P11-01-02
-- cap: P11-01-03
-- cap: P11-01-04
-- cap: P11-01-05
-- cap: P11-02-01
run_cmd do
  let e ← elaborate
  let steps ← Trace.steps e.fileMap e.trees
  let ofDoubleEq := steps.filter (·.decl == "double_eq")
  -- Every tactic that stands in the source, in order, with its place.
  expect "tactics" (ofDoubleEq.map (·.tactic) ==
    #["induction n with\n  | zero => rfl\n  | succ n ih =>\n    simp [double]\n    omega", "rfl", "simp [double]", "omega"])
  expect "places" (ofDoubleEq.map (fun s => (s.line, s.column)) == #[(10, 2), (11, 12), (13, 4), (14, 4)])
  expect "indices count the steps of the file" (steps.map (·.index) == (Array.range steps.size))
  -- The goals before and after, as Lean prints them.
  let rfl := ofDoubleEq[1]!
  expect "goal before rfl" (rfl.goalsBefore == #["case zero\n⊢ double 0 = 2 * 0"])
  expect "no goal after rfl" (rfl.goalsAfter.isEmpty)
  let simp := ofDoubleEq[2]!
  expect "the hypothesis of the induction is in the goal before simp"
    (simp.goalsBefore.size == 1 && (simp.goalsBefore[0]!.splitOn "ih : double n = 2 * n").length == 2)
  expect "simp leaves one goal, which omega closes"
    (simp.goalsAfter.size == 1 && ofDoubleEq[3]!.goalsBefore == simp.goalsAfter && ofDoubleEq[3]!.goalsAfter.isEmpty)
  -- The outline of a proof, and its smallest moves.
  expect "depths" (ofDoubleEq.map (·.depth) == #[0, 1, 1, 1])
  expect "steps inside" (ofDoubleEq.map (·.inner) == #[3, 0, 0, 0])
  expect "top-level steps of the file" ((steps.filter (·.topLevel)).map (·.decl) == #["double_eq", "open_one"])
  expect "smallest moves" ((steps.filter (·.atomic)).map (·.tactic) == #["rfl", "simp [double]", "omega", "sorry"])
  expect "a term proof has no steps" (!(steps.any (·.decl == "Bad_Name")))
  -- One record per line; a line reads back as the step.
  let lines := (Trace.jsonLines steps).splitOn "\n" |>.filter (· != "")
  expect "one line per step" (lines.length == steps.size)
  match Json.parse lines[1]! >>= fromJson? (α := Trace.Step) with
  | .ok back => expect "a line reads back" (back.tactic == "rfl" && back.goalsBefore == rfl.goalsBefore && back.depth == 1)
  | .error err => throwError "a line is not a step: {err}"
  -- Theorems with their proofs and premises.
  let theorems := Trace.theorems e.decls
  expect "the theorems of the file" (theorems.map (·.name) == #["double_eq", "Bad_Name", "open_one", "uses_open", "by_shortcut"])
  let restated := theorems[1]!
  expect "a theorem record" (restated.statement == "∀ (a b : Nat), a + b = b + a" && restated.module == "Demo" &&
    restated.proof == "Nat.add_comm a b" && restated.premises == #["Nat", "Nat.add_comm"])
  expect "the proof of a tactic proof is its text" (theorems[0]!.proof.startsWith "by\n  induction n with")
  expect "premises from the term the kernel checked" (Trace.Premise.ofProof e.env `Bad_Name == #[`Nat, `Nat.add_comm])
  expect "the lemmas among them" (Trace.Premise.lemmasOf e.env `Bad_Name == #[`Nat.add_comm])
  expect "the premises of a tactic proof" ((Trace.Premise.ofProof e.env `uses_open).contains `open_one)
  expect "nothing for a name that is not there" (Trace.Premise.ofProof e.env `no_such_theorem == #[])

/-- A proof with bullets and with an `intro` that takes two names. -/
def bulleted : String := "
theorem both (p q : Prop) : p → q → p ∧ q := by
  intro hp hq
  constructor
  · exact hp
  · exact hq
"

-- cap: P11-01-02
-- cap: P11-01-05
run_cmd do
  let e ← elaborate (source := bulleted)
  let steps ← Trace.steps e.fileMap e.trees
  -- Only tactics are steps: neither the bullet in front of a step nor the second name of `intro`.
  expect "steps of a bulleted proof" (steps.map (·.tactic) ==
    #["intro hp hq", "constructor", "· exact hp", "exact hp", "· exact hq", "exact hq"])
  expect "a bullet holds the step behind it" (steps.map (·.inner) == #[0, 0, 1, 0, 1, 0])
  expect "depths under bullets" (steps.map (·.depth) == #[0, 0, 0, 1, 0, 1])
  expect "intro with two names is one step that leaves one goal"
    (steps[0]!.goalsBefore.size == 1 && steps[0]!.goalsAfter.size == 1 &&
      (steps[0]!.goalsAfter[0]!.splitOn "hq : q").length == 2)
  expect "constructor opens two goals, and each bullet closes one"
    (steps[1]!.goalsAfter.size == 2 && steps[3]!.goalsAfter.isEmpty && steps[5]!.goalsAfter.isEmpty)
  expect "smallest moves of a bulleted proof" ((steps.filter (·.atomic)).map (·.tactic) ==
    #["intro hp hq", "constructor", "exact hp", "exact hq"])

-- cap: P03-01-01
-- cap: P03-01-02
-- cap: P03-01-03
-- cap: P03-01-04
-- cap: P03-01-05
run_cmd do
  let e ← elaborate
  let ctx : Lint.Context := { declarations := e.decls }
  let found := Lint.run Lint.builtin ctx
  let by_ (rule : String) := (found.filter (·.rule == rule)).map fun f => (f.declaration, f.message)
  -- Unfinished work, in a proof and through a dependency.
  expect "sorry" (by_ "sorry" == #[("open_one", "the theorem contains sorry"),
    ("uses_open", "the theorem depends on sorry through open_one")])
  -- Axioms: declaring one, and resting on one.
  expect "axiom" (by_ "axiom" == #[("shortcut", "declares an axiom: nothing checks that it is true"),
    ("by_shortcut", "rests on shortcut, beyond the axioms of Lean's logic")])
  -- Documentation: definitions and types, and theorems that nothing in the file uses.
  expect "docstring" ((by_ "docstring").map (·.1) == #["double_eq", "Bad_Name", "uses_open", "Point", "by_shortcut"])
  -- Names.
  expect "naming" (by_ "naming" == #[("Bad_Name", "the name Bad_Name is not in snake_case, the convention for a theorem")])
  let relaxed := Lint.run Lint.builtin { ctx with convention := { theorems := "any" } }
  expect "a convention can be switched off" (!(relaxed.any (·.rule == "naming")))
  let camel := Lint.run #[Lint.namingRule] { ctx with convention := { theorems := "lowerCamelCase" } }
  expect "or changed" (camel.map (·.declaration) == #["double_eq", "Bad_Name", "open_one", "uses_open", "by_shortcut"])
  let upper := Lint.run #[Lint.namingRule] { ctx with convention := { types := "snake_case" } }
  expect "for types as well" (upper.map (·.declaration) == #["Bad_Name", "Point"])
  -- The framework: severities and places come with the findings, in the order of the file.
  expect "severities" ((found.filter (·.rule == "sorry")).all (·.severity == "error") &&
    (found.filter (·.rule == "axiom")).all (·.severity == "warning"))
  expect "findings carry the line of the declaration" ((found.find? (·.rule == "naming")).map (·.line) == some 16)
  expect "in the order of the file" (found.map (·.line) == (found.map (·.line)).qsort (· < ·))
  -- Rules are values: a rule of one's own runs like the ones that ship.
  let long : Lint.Rule := {
    name := "long-name", description := "A name is longer than nine characters.", severity := "info",
    check := fun _ d => if d.name.length > 9 then #[s!"{d.name.length} characters"] else #[] }
  expect "a rule of one's own" ((Lint.run #[long] ctx).map (·.declaration) == #["instInhabitedPoint", "by_shortcut"])
  match Lint.select Lint.builtin #["naming", "sorry"] with
  | .ok rules => expect "rules are selected by name, in the order asked" (rules.map (·.name) == #["naming", "sorry"])
  | .error err => throwError err
  match Lint.select Lint.builtin #["speling"] with
  | .ok _ => throwError "an unknown rule was accepted"
  | .error err =>
    expect "an unknown rule is an error that lists the rules"
      (err == "there is no rule 'speling'; the rules are sorry, axiom, docstring, naming")

-- cap: P03-01-05
#guard Lint.conforms "snake_case" "add_comm'" && Lint.conforms "snake_case" "insertionSort_idem" &&
  !Lint.conforms "snake_case" "Add_comm" && !Lint.conforms "snake_case" "add__comm" && !Lint.conforms "snake_case" "add_Comm"
#guard Lint.conforms "lowerCamelCase" "addComm" && !Lint.conforms "lowerCamelCase" "add_comm" &&
  !Lint.conforms "lowerCamelCase" "AddComm"
#guard Lint.conforms "UpperCamelCase" "Point" && !Lint.conforms "UpperCamelCase" "point" &&
  !Lint.conforms "UpperCamelCase" "My_Point"
-- Names outside the Latin alphabet are not judged, and neither is anything under `any`.
#guard Lint.conforms "snake_case" "αβγ" && Lint.conforms "any" "Whatever_this_is"
#guard Lint.definesType "Nat → Prop" && Lint.definesType "Type" && Lint.definesType "Sort u" &&
  !Lint.definesType "Nat → Nat"

end MaithicTest.Extract
