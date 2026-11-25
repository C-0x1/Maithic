/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Env.Elab

/-!
# Proof snapshots

A proof snapshot is everything Lean needs to continue a tactic proof from one point: the
environment, the metavariable context with the open goals, and the settings of the elaborator at
that point. Snapshots are values. Running a tactic on a snapshot returns a new snapshot and leaves
the old one as it was, so a search can come back to any state it has seen without redoing the work
that led there.

Snapshots are taken where a declaration says `sorry` in tactic position: the declaration is
elaborated by Lean's own frontend, binders, universe variables, open namespaces and all, and the
state just before each `sorry` is read from the information Lean records for editors.

A snapshot is an aid to search, not a proof. Closing every goal of a snapshot shows that a tactic
script works; the theorem is verified only when the finished declaration is elaborated as a whole
and accepted by the kernel, which is what `runCommand`, `checkFile` and `tryCandidates` do.
-/

namespace Maithic.Bridge

open Lean Elab Meta

namespace ProofSnapshot

/-- The snapshot just before a tactic recorded in an information tree. -/
def ofTacticInfo (ci : ContextInfo) (ti : TacticInfo) : IO ProofSnapshot :=
  { ci with mctx := ti.mctxBefore }.runMetaM {} do
    return {
      coreState := ← getThe Core.State
      coreContext := ← readThe Core.Context
      metaState := ← getThe Meta.State
      metaContext := ← readThe Meta.Context
      termState := {}
      termContext := {}
      goals := ti.goalsBefore }

/-- Runs a `MetaM` action that only reads the snapshot. -/
def inspect (p : ProofSnapshot) (x : MetaM α) : IO α := do
  let ((a, _), _) ← (x.run p.metaContext p.metaState).toIO p.coreContext p.coreState
  return a

/-- Runs a tactic action from the snapshot. On success the result comes with the snapshot after
the action; on failure the error is returned as text, with the goal it refers to printed in full.
Messages logged by the action are in the new snapshot alone; the step budget of `limits` applies
to this run and starts counting now, and running out of it is a failure like any other. Errors
are not recovered from: a tactic that would log an error and carry on fails instead. -/
def step (p : ProofSnapshot) (limits : Limits) (x : Tactic.TacticM α) :
    IO (Except String (α × ProofSnapshot)) := do
  let budget :=
    if limits.maxHeartbeats == 0 then p.coreContext.maxHeartbeats else limits.maxHeartbeats * 1000
  let coreContext := { p.coreContext with
    maxHeartbeats := budget
    initHeartbeats := ← IO.getNumHeartbeats
    maxRecDepth := if limits.maxRecDepth == 0 then p.coreContext.maxRecDepth else limits.maxRecDepth }
  let run : Tactic.TacticM α := do
    let a ← x
    Tactic.pruneSolvedGoals
    return a
  let action : Term.TermElabM (Except String (α × List MVarId)) :=
    tryCatchRuntimeEx
      (do
        let (a, s) ← (run { elaborator := .anonymous, recover := false }).run { goals := p.goals }
        Term.synthesizeSyntheticMVarsNoPostponing
        return .ok (a, s.goals))
      (fun e => do
        return .error (← (← addMessageContext e.toMessageData).toString))
  let (((outcome, termState), metaState), coreState) ←
    ((action.run p.termContext p.termState).run p.metaContext p.metaState).toIO coreContext
      { p.coreState with messages := {} }
  match outcome with
  | .error message => return .error message
  | .ok (a, goals) => return .ok (a, { p with coreState, metaState, termState, goals })

/-- One goal as text. Names are the ones Lean shows: a hypothesis that the proof cannot refer to
by name appears with a dagger, as in `n✝`. -/
def goalView (g : MVarId) : MetaM Goal := do
  let decl ← g.getDecl
  let lctx : LocalContext := decl.lctx.sanitizeNames.run' { options := ← getOptions }
  withLCtx lctx decl.localInstances do
    let mut hypotheses : Array Hypothesis := #[]
    for ldecl in lctx do
      if ldecl.isImplementationDetail then
        continue
      let type := (← ppExpr (← instantiateMVars ldecl.type)).pretty 100
      let value ← match ldecl.value? with
        | some v => pure (some ((← ppExpr (← instantiateMVars v)).pretty 100))
        | none => pure none
      hypotheses := hypotheses.push { name := ldecl.userName.toString, type, value }
    let target := (← ppExpr (← instantiateMVars decl.type)).pretty 100
    let pretty := (← Meta.ppGoal g).pretty 100
    let tag := if decl.userName.isAnonymous then "" else decl.userName.toString
    return { id := toString g.name, tag, hypotheses, target, pretty }

/-- The open goals of a snapshot as text. -/
def goalViews (p : ProofSnapshot) : IO (Array Goal) :=
  p.inspect (p.goals.toArray.mapM goalView)

end ProofSnapshot

/-- Parses a tactic sequence: one tactic, or several separated by line breaks or semicolons. -/
def parseTactics (env : Environment) (input : String) : Except String Syntax :=
  let p := Parser.andthenFn Parser.whitespace Parser.Tactic.tacticSeq.fn
  let ictx := Parser.mkInputContext input virtualFile
  let s := p.run ictx { env, options := {} } (Parser.getTokenTable env) (Parser.mkParserState input)
  if !s.allErrors.isEmpty then
    .error (s.toErrorMsg ictx)
  else if ictx.atEnd s.pos then
    .ok s.stxStack.back
  else
    .error ((s.mkError "end of input").toErrorMsg ictx)

/-- The tactic positions marked `sorry` in the information trees of an elaboration, in source
order, each with the context it was elaborated in. A `sorry` inside a term is not a tactic
position and is not listed. -/
partial def sorryHoles (trees : PersistentArray InfoTree) : Array (ContextInfo × TacticInfo) :=
  let rec visit (ctx? : Option ContextInfo) (tree : InfoTree)
      (acc : Array (ContextInfo × TacticInfo)) : Array (ContextInfo × TacticInfo) :=
    match tree with
    | .context inner tree => visit (inner.mergeIntoOuter? ctx?) tree acc
    | .node info children =>
      match ctx?, info with
      | some ctx, .ofTacticInfo ti =>
        if ti.stx.getKind == ``Lean.Parser.Tactic.tacticSorry then
          acc.push (ctx, ti)
        else
          children.foldl (init := acc) fun acc child => visit (info.updateContext? ctx?) child acc
      | _, _ => children.foldl (init := acc) fun acc child => visit (info.updateContext? ctx?) child acc
    | .hole _ => acc
  let holes := trees.foldl (init := #[]) fun acc tree => visit none tree acc
  holes.qsort fun a b => (a.2.stx.getPos?.getD 0) < (b.2.stx.getPos?.getD 0)

end Maithic.Bridge
