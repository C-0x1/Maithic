/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Extract.Declarations

/-!
# Tactic steps from information trees

While Lean elaborates a proof it writes down, for every tactic, the goals before and after. This
module reads those notes out of the information trees of an elaboration: one record per tactic
that stands in the source, with its text, its place, the goals it met and the goals it left.

A tactic can contain tactics: `induction n with | zero => rfl | succ n ih => simp; omega` is one
step that contains three. Every step says how deep it stands and how many steps stand inside it,
so that a reader can keep the outline of a proof (depth zero), its smallest moves (nothing
inside), or both.
-/

namespace Maithic.Trace

open Lean Elab

/-- One tactic of a proof, as it ran. -/
structure Step where
  /-- The declaration whose proof the tactic belongs to. -/
  decl : String
  /-- The position of the step among the steps of the file, from zero. -/
  index : Nat
  /-- The text of the tactic as it stands in the file. -/
  tactic : String
  line : Nat
  column : Nat
  endLine : Nat
  endColumn : Nat
  /-- Every goal before the tactic ran, as Lean prints it. -/
  goalsBefore : Array String
  goalsAfter : Array String
  /-- How many steps this one stands inside; zero for a step of the proof's own sequence. -/
  depth : Nat
  /-- How many steps stand inside this one; zero for a tactic with no tactic inside it. -/
  inner : Nat
  deriving ToJson, FromJson, Repr, Inhabited

/-- A step of the outline of a proof: it stands inside no other step. -/
def Step.topLevel (s : Step) : Bool := s.depth == 0

/-- A smallest move: no step stands inside it. -/
def Step.atomic (s : Step) : Bool := s.inner == 0

/-- Syntax that groups tactics and is not a step itself. -/
def isGrouping (k : SyntaxNodeKind) : Bool :=
  k == ``Lean.Parser.Tactic.tacticSeq || k == ``Lean.Parser.Tactic.tacticSeq1Indented ||
  k == ``Lean.Parser.Tactic.tacticSeqBracketed || k == ``Lean.Parser.Term.byTactic ||
  k == ``Lean.Parser.Tactic.paren || k == nullKind

/-- Whether a kind of syntax is a tactic: one of the parsers of the category `tactic`. Lean also
notes goals at syntax that is not a tactic, such as the bullet in front of `· simp` and the second
name of `intro a b`; those are places inside a step, not steps. -/
def isTactic (env : Environment) (k : SyntaxNodeKind) : Bool :=
  match (Parser.parserExtension.getState env).categories.find? `tactic with
  | some category => category.kinds.contains k
  | none => false

/-- A tactic application found in a tree, before its goals are printed. -/
structure Found where
  ctx : ContextInfo
  info : TacticInfo
  depth : Nat
  first : Position
  last : Position

/-- The tactic applications in the information trees, in the order of the source. A node counts
when its syntax stands in the file, when it is a tactic and not a mere grouping, when it had a
goal to work on, and when it does not cover exactly the text of the step around it, which is how
the pieces a macro unfolds into show up. -/
partial def collect (fileMap : FileMap) (trees : PersistentArray InfoTree) : Array Found :=
  let rec visit (ctx? : Option ContextInfo) (depth : Nat) (around : Option (Position × Position))
      (tree : InfoTree) (acc : Array Found) : Array Found :=
    match tree with
    | .context inner tree => visit (inner.mergeIntoOuter? ctx?) depth around tree acc
    | .node info children =>
      let below := info.updateContext? ctx?
      let skip := children.foldl (init := acc) fun acc child => visit below depth around child acc
      match ctx?, info with
      | some ctx, .ofTacticInfo ti =>
        match ti.stx.getPos? true, ti.stx.getTailPos? true with
        | some first, some last =>
          let range := (fileMap.toPosition first, fileMap.toPosition last)
          if ti.stx.isAtom || isGrouping ti.stx.getKind || !isTactic ctx.env ti.stx.getKind ||
              ti.goalsBefore.isEmpty || around == some range then skip
          else
            let acc := acc.push { ctx, info := ti, depth, first := range.1, last := range.2 }
            children.foldl (init := acc) fun acc child => visit below (depth + 1) (some range) child acc
        | _, _ => skip
      | _, _ => skip
    | .hole _ => acc
  trees.foldl (init := #[]) fun acc tree => visit none 0 none tree acc

/-- The goals of a tactic application as Lean prints them, in the metavariable context given. -/
def goalTexts (ctx : ContextInfo) (mctx : MetavarContext) (goals : List MVarId) : IO (Array String) :=
  { ctx with mctx }.runMetaM {} do
    goals.toArray.mapM fun goal => do return (← Meta.ppGoal goal).pretty 100

/-- The steps of an elaboration. -/
def steps (fileMap : FileMap) (trees : PersistentArray InfoTree) : IO (Array Step) := do
  let found := collect fileMap trees
  let mut out : Array Step := #[]
  for h : i in [0:found.size] do
    let f := found[i]
    -- The steps inside this one follow it directly, and stand deeper.
    let mut inner := 0
    for g in found.extract (i + 1) found.size do
      if g.depth > f.depth then inner := inner + 1 else break
    let name := match f.ctx.parentDecl? with
      | some n => toString ((privateToUserName? n).getD n)
      | none => ""
    out := out.push {
      decl := name, index := i,
      tactic := (Maithic.Extract.textOf f.info.stx).getD (toString f.info.stx),
      line := f.first.line, column := f.first.column, endLine := f.last.line, endColumn := f.last.column,
      goalsBefore := ← goalTexts f.ctx f.info.mctxBefore f.info.goalsBefore,
      goalsAfter := ← goalTexts f.ctx f.info.mctxAfter f.info.goalsAfter,
      depth := f.depth, inner }
  return out

/-- Records as JSON lines: one record on each line. -/
def jsonLines [ToJson α] (records : Array α) : String :=
  String.join (records.toList.map fun r => (toJson r).compress ++ "\n")

/-- A theorem with its proof, as a record of its own. -/
structure TheoremRecord where
  name : String
  /-- The statement, as Lean prints it. -/
  statement : String
  module : String
  /-- The proof as it stands in the file, after `:=`. -/
  proof : String
  line : Nat
  /-- The constants the proof term refers to. -/
  premises : Array String
  deriving ToJson, FromJson, Repr, Inhabited

/-- The theorems among the declarations of a file, each with its proof and its premises. -/
def theorems (decls : Array Maithic.Extract.Declaration) : Array TheoremRecord :=
  decls.filterMap fun d =>
    if d.kind == "theorem" then
      some { name := d.name, statement := d.type, module := d.module, proof := d.proof.getD "", line := d.line,
             premises := d.valueDeps }
    else none

end Maithic.Trace
