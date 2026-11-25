/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# The declarations of a file, as data

After Lean has elaborated a file, its environment knows everything about the declarations of the
file: their kinds and types, where they stand, what they refer to, which axioms they rest on. This
module reads those facts out as plain records, one per declaration that somebody wrote. The
records are what the code graph, the corpus and the linters are built from.

Nothing here is trusted or needs to be: the records describe an environment that the kernel has
already checked.
-/

namespace Maithic.Extract

open Lean Meta

/-- What is recorded about one declaration. Positions are one-based lines and zero-based columns,
as Lean reports them. -/
structure Declaration where
  /-- The name as it is written and used; a private name without its mangling. -/
  name : String
  /-- `theorem`, `definition`, `instance`, `structure`, `class`, `inductive`, `axiom` or `opaque`. -/
  kind : String
  /-- The type, as Lean prints it. For a theorem this is its statement. -/
  type : String
  module : String := ""
  /-- Where the declaration begins, its documentation and attributes included. -/
  line : Nat := 0
  column : Nat := 0
  endLine : Nat := 0
  endColumn : Nat := 0
  /-- Where its name stands: the place to send a reader to. -/
  nameLine : Nat := 0
  nameColumn : Nat := 0
  doc : Option String := none
  isPrivate : Bool := false
  /-- Whether the declaration rests on `sorryAx`, directly or through what it uses. -/
  usesSorry : Bool := false
  axioms : Array String := #[]
  /-- The constants its type refers to. -/
  typeDeps : Array String := #[]
  /-- The constants its value refers to: for a theorem, the premises of its proof. -/
  valueDeps : Array String := #[]
  /-- For an instance, the class it is an instance of. -/
  instanceOf : Option String := none
  /-- For a structure, the structures it extends. -/
  parents : Array String := #[]
  /-- Equal for statements that differ only in the names of bound variables. -/
  statementHash : String := ""
  /-- Other theorems with the same statement up to the names of bound variables, when asked for. -/
  sameStatement : Array String := #[]
  /-- The text of the declaration in the file. -/
  source : Option String := none
  /-- The text after `:=`. -/
  proof : Option String := none
  deriving ToJson, FromJson, Repr, Inhabited

/-- A text without the blank space around it. -/
def trimmed (s : String) : String := s.trimAscii.toString

/-- The value of a constant, when it has one. -/
def valueOf? : ConstantInfo → Option Expr
  | .defnInfo v => some v.value
  | .thmInfo v => some v.value
  | .opaqueInfo v => some v.value
  | _ => none

/-- The worker of `constantsOf`. -/
partial def constantsOf.go (env : Environment) (todo : List Expr) (seen : NameSet) (out : Array Name) :
    Array Name :=
  match todo with
  | [] => out
  | e :: rest =>
    let (todo, seen, out) := e.getUsedConstants.foldl (init := (rest, seen, out)) fun (todo, seen, out) c =>
      if seen.contains c then (todo, seen, out)
      else if c.isInternalDetail then
        match env.find? c with
        | some ci => ((valueOf? ci).toList ++ ci.type :: todo, seen.insert c, out)
        | none => (todo, seen.insert c, out)
      else (todo, seen.insert c, out.push c)
    constantsOf.go env todo seen out

/-- The constants an expression refers to, sorted by name. Auxiliary constants that Lean made up
while compiling a declaration, a nested proof or a matcher, are looked through: what they refer
to is listed in their place. -/
def constantsOf (env : Environment) (e : Expr) : Array Name :=
  (constantsOf.go env [e] {} #[]).qsort Name.lt

/-- The kind of a constant, as a reader of the source would name it. -/
def kindOf (n : Name) (ci : ConstantInfo) : CoreM String := do
  let env ← getEnv
  match ci with
  | .thmInfo _ => return "theorem"
  | .axiomInfo _ => return "axiom"
  | .opaqueInfo _ => return "opaque"
  | .inductInfo _ =>
    if isClass env n then return "class"
    else if isStructure env n then return "structure"
    else return "inductive"
  | .defnInfo _ => if ← Meta.isInstance n then return "instance" else return "definition"
  | .ctorInfo _ => return "constructor"
  | .recInfo _ => return "recursor"
  | .quotInfo _ => return "quot"

/-- Whether a constant is a declaration that somebody wrote: it has a place in a source file and
is not a constructor, a projection or one of the constants Lean derives from a declaration. -/
def isWritten (n : Name) (ci : ConstantInfo) : CoreM Bool := do
  let env ← getEnv
  if n.isInternalDetail || isAuxRecursor env n || isNoConfusion env n || env.isProjectionFn n then
    return false
  match ci with
  | .ctorInfo _ | .recInfo _ | .quotInfo _ => return false
  | _ => return (← findDeclarationRangesCore? n).isSome


/-- The type of a constant with its universe parameters renamed in order. -/
def canonicalType (ci : ConstantInfo) : Expr :=
  let canonical := (List.range ci.levelParams.length).map fun i => Level.param (Name.mkSimple s!"u{i}")
  ci.type.instantiateLevelParams ci.levelParams canonical

/-- For each of the given theorems, the other theorems of the environment that state the same
thing: their types are equal up to the names of bound variables and of universe parameters. The
whole environment is searched, imports included. -/
def sameStatements (env : Environment) (names : Array Name) : Std.HashMap Name (Array Name) :=
  let targets := names.filterMap fun n => (env.find? n).map fun ci => (n, canonicalType ci)
  let table : Std.HashMap UInt64 (Array (Name × Expr)) :=
    targets.foldl (init := {}) fun table (n, type) =>
      table.insert (hash type) ((table.getD (hash type) #[]).push (n, type))
  env.constants.fold (init := {}) fun found c ci =>
    match ci with
    | .thmInfo _ =>
      if c.isInternalDetail then found
      else
        let type := canonicalType ci
        match table[hash type]? with
        | some hits => hits.foldl (init := found) fun found (n, other) =>
            if n != c && other == type then found.insert n ((found.getD n #[]).push c) else found
        | none => found
    | _ => found

/-- A text that is equal for two types exactly when their hashes agree, with the universe
parameters renamed in order. Lean's hash of an expression does not look at the names of bound
variables, so statements that differ only in those get the same text. -/
def statementHash (ci : ConstantInfo) : String :=
  toString (hash (canonicalType ci))

/-- The record of one declaration of the current environment. -/
def describe (module : String) (n : Name) (ci : ConstantInfo) : MetaM Declaration := do
  let env ← getEnv
  let kind ← kindOf n ci
  let type := (← ppExpr ci.type).pretty 100
  let ranges ← findDeclarationRangesCore? n
  let axioms := (← collectAxioms n).qsort Name.lt
  let typeDeps := (constantsOf env ci.type).filter (· != n)
  let valueDeps := match valueOf? ci with
    | some v => (constantsOf env v).filter (· != n)
    | none => #[]
  let instanceOf ← if kind == "instance" then
      forallTelescope ci.type fun _ body => return body.getAppFn.constName?.map toString
    else pure none
  let parents := if isStructure env n then
      (getStructureParentInfo env n).map (toString ·.structName)
    else #[]
  let doc ← findDocString? env n
  let range := ranges.map (·.range)
  let selection := ranges.map (·.selectionRange)
  return {
    name := toString ((privateToUserName? n).getD n), kind, type, module,
    line := (range.map (·.pos.line)).getD 0, column := (range.map (·.pos.column)).getD 0,
    endLine := (range.map (·.endPos.line)).getD 0, endColumn := (range.map (·.endPos.column)).getD 0,
    nameLine := (selection.map (·.pos.line)).getD 0, nameColumn := (selection.map (·.pos.column)).getD 0,
    doc := doc.map trimmed, isPrivate := isPrivateName n,
    usesSorry := axioms.contains ``sorryAx, axioms := axioms.map toString,
    typeDeps := typeDeps.map toString, valueDeps := valueDeps.map toString,
    instanceOf, parents, statementHash := statementHash ci }

/-- The declarations that the current environment has and `base` has not, in the order of the
file. With `duplicates`, every theorem also lists the other theorems that state the same thing. -/
def declarationsSince (base : Environment) (module : String := "") (duplicates : Bool := false) :
    MetaM (Array Declaration) := do
  let env ← getEnv
  let added := env.constants.map₂.foldl (init := #[]) fun acc n ci =>
    if base.contains n then acc else acc.push (n, ci)
  let mut written : Array (Name × ConstantInfo) := #[]
  for (n, ci) in added do
    if ← isWritten n ci then written := written.push (n, ci)
  let same := if duplicates then
      sameStatements env (written.filterMap fun (n, ci) => match ci with | .thmInfo _ => some n | _ => none)
    else {}
  let mut out : Array Declaration := #[]
  for (n, ci) in written do
    let d ← describe module n ci
    let twins := ((same.getD n #[]).map fun t => toString ((privateToUserName? t).getD t)).qsort (· < ·)
    out := out.push { d with sameStatement := twins }
  return out.qsort fun a b =>
    a.line < b.line || (a.line == b.line && (a.column < b.column || (a.column == b.column && a.name < b.name)))

/-- Runs a `MetaM` action against an environment, with no limit on steps: the action reads an
environment that has already been checked. -/
def runMeta (env : Environment) (x : MetaM α) : IO α := do
  let ctx : Core.Context := { fileName := "<extract>", fileMap := default, maxHeartbeats := 0 }
  let ((a, _), _) ← (x.run {} {}).toIO ctx { env }
  return a

/-- The module documentation of the file being elaborated. -/
def moduleDocs (env : Environment) : Array String :=
  (getMainModuleDoc env).toArray.map (trimmed ·.doc)

/-- The modules a source text imports, in the order they are read, each once. `Init` comes first
unless the text says `prelude`. Only the header is read; the rest of the text may be anything. -/
def importsOf (source : String) : IO (Array String) := do
  let (header, _, _) ← Parser.parseHeader (Parser.mkInputContext source "<source>")
  return ((Elab.headerToImports header).map (toString ·.module)).toList.eraseDups.toArray

/-! ## The text of declarations -/

/-- The first node of one of the given kinds, searching the arguments from the left. -/
partial def findKind? (kinds : List SyntaxNodeKind) (stx : Syntax) : Option Syntax :=
  if kinds.contains stx.getKind then some stx else stx.getArgs.findSome? (findKind? kinds)

/-- The text of a piece of syntax as it stands in the file, without the blank space and comments
that follow it. -/
def textOf (stx : Syntax) : Option String :=
  stx.unsetTrailing.reprint.map trimmed

/-- The text of a declaration and of its value: what follows `:=`, or the equations. -/
def declarationText (stx : Syntax) : Option (String × Option String) := do
  let whole ← textOf stx
  let value := findKind? [``Lean.Parser.Command.declValSimple, ``Lean.Parser.Command.declValEqns,
    ``Lean.Parser.Command.whereStructInst] stx
  let proof := value.bind fun v => (textOf v).map fun text =>
    if text.startsWith ":=" then trimmed (text.drop 2).toString else text
  return (whole, proof)

/-- The declaration commands in the information trees of an elaboration: where each starts, its
text, and the text of its value. -/
partial def declarationTexts (fileMap : FileMap) (trees : PersistentArray Elab.InfoTree) :
    Array (Position × String × Option String) :=
  let rec visit (tree : Elab.InfoTree) (acc : Array (Position × String × Option String)) :=
    match tree with
    | .context _ tree => visit tree acc
    | .node (.ofCommandInfo ci) children =>
      let acc := if ci.stx.getKind == ``Lean.Parser.Command.declaration then
          match ci.stx.getPos?, declarationText ci.stx with
          | some pos, some (text, proof) => acc.push (fileMap.toPosition pos, text, proof)
          | _, _ => acc
        else acc
      children.foldl (init := acc) fun acc child => visit child acc
    | .node _ children => children.foldl (init := acc) fun acc child => visit child acc
    | .hole _ => acc
  trees.foldl (init := #[]) fun acc tree => visit tree acc

/-- Adds the texts to the records of the declarations that start where the texts start. -/
def attachTexts (decls : Array Declaration) (texts : Array (Position × String × Option String)) :
    Array Declaration :=
  decls.map fun d =>
    match texts.find? fun (pos, _, _) => pos.line == d.line && pos.column == d.column with
    | some (_, text, proof) => { d with source := some text, proof }
    | none => d

end Maithic.Extract
