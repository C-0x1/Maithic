/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Oracle.Logic.Reflect
import Maithic.Refute.Testing.Command

/-!
# Statements that are true for the wrong reasons

A theorem can be proved and still say nothing. Its hypotheses may contradict each other, so that
no case is covered. A variable may range over an empty type. The conclusion may hold whatever the
hypotheses say, or a hypothesis may play no part. This file looks for each of these.

Every positive finding is certain, because it comes with a proof that Lean checked: a derivation
of `False` from the hypotheses, a proof that a type is empty, a proof of the conclusion from no
hypotheses. A negative finding is only the absence of a proof within a budget: "no contradiction
found" does not mean that the hypotheses are consistent.

Whether a hypothesis is *needed* is asked differently, by testing: the statement without the
hypothesis is run through the sampling of `Maithic.Refute.Testing`, and a counterexample shows
that the hypothesis cannot be dropped.
-/

namespace Maithic.Refute

open Lean Meta Elab

/-- The budget of one attempt of the automation, in thousands of heartbeats. -/
def vacuityHeartbeats : Nat := 20000

/-- A named piece of automation. -/
structure Attempt where
  /-- The name shown in reports. -/
  name : String
  /-- The tactic. -/
  tactic : Syntax

/-- Runs a tactic on a goal under a budget. The goal is closed and `true` is returned, or nothing
changes. Errors are errors here: a tactic that fails does not leave an admitted goal behind, a
proof that contains `sorry` does not count, and neither does a run that logged an error. The
messages of the run are dropped. -/
def closes (goal : MVarId) (tactic : Syntax) (thousands : Nat := vacuityHeartbeats) : MetaM Bool := do
  let saved ← saveState
  let messages := (← getThe Core.State).messages
  modifyThe Core.State fun s => { s with messages := {} }
  let start ← IO.getNumHeartbeats
  let outcome ← tryCatchRuntimeEx
    (do
      let remaining ← withTheReader Core.Context
        (fun ctx => { ctx with maxHeartbeats := thousands * 1000, initHeartbeats := start })
        (Term.TermElabM.run' <| Term.withoutErrToSorry <|
          Tactic.run goal (Tactic.withoutRecover (Tactic.evalTactic tactic)))
      if !remaining.isEmpty || (← getThe Core.State).messages.hasErrors then pure false
      else pure !(← instantiateMVars (mkMVar goal)).hasSorry)
    (fun _ => pure false)
  unless outcome do saved.restore
  modifyThe Core.State fun s => { s with messages }
  return outcome

/-- Has the kernel check a proof of a statement, both given in the current context. The context is
abstracted first, so that what the kernel sees is a closed theorem; the theorem is checked against
the environment and not added to it. A proof with anything left open is not sent, and counts as
not accepted. -/
def kernelAccepts (type proof : Expr) : MetaM Bool := do
  let fvars := (← getLCtx).getFVars
  let type ← instantiateMVars (← mkForallFVars fvars type)
  let proof ← instantiateMVars (← mkLambdaFVars fvars proof)
  if type.hasMVar || proof.hasMVar || type.hasFVar || proof.hasFVar then return false
  let levels := (collectLevelParams (collectLevelParams {} type) proof).params.toList
  let options ← getOptions
  let theorem_ := Declaration.thmDecl
    { name := `Maithic.Refute.kernelCheck, levelParams := levels, type := type, value := proof }
  match (← getEnv).toKernelEnv.addDeclCore (Core.getMaxHeartbeats options).toUSize
      (maxRecDepth.get options).toUSize theorem_ none with
  | .ok _ => return true
  | .error _ => return false

/-- Tries the attempts in order on a fresh goal of the given type, in the current context. Returns
the name of the first that proves it, with the proof. A proof counts when the tactic closed the
goal and the kernel accepted the term it built. -/
def proveBy (type : Expr) (attempts : List Attempt) : MetaM (Option (String × Expr)) := do
  for attempt in attempts do
    let goal ← mkFreshExprSyntheticOpaqueMVar type
    if ← closes goal.mvarId! attempt.tactic then
      let proof ← instantiateMVars goal
      if ← kernelAccepts type proof then
        return some (attempt.name, proof)
  return none

/-- The automation that looks for a contradiction among hypotheses. -/
def contradictionAttempts : MetaM (List Attempt) := do
  return [
    ⟨"contradiction", ← `(tactic| contradiction)⟩,
    ⟨"omega", ← `(tactic| omega)⟩,
    ⟨"simp_all", ← `(tactic| simp_all)⟩,
    ⟨"maithic_sat", ← `(tactic| maithic_sat)⟩,
    ⟨"grind", ← `(tactic| grind)⟩]

/-- The automation that tries to prove a conclusion. -/
def conclusionAttempts : MetaM (List Attempt) := do
  return [
    ⟨"rfl", ← `(tactic| rfl)⟩,
    ⟨"trivial", ← `(tactic| trivial)⟩,
    ⟨"simp", ← `(tactic| simp)⟩,
    ⟨"omega", ← `(tactic| omega)⟩,
    ⟨"decide", ← `(tactic| decide)⟩,
    ⟨"grind", ← `(tactic| grind)⟩]

/-- The automation that tries to show that a type has no elements, as a proof of `α → False`. -/
def emptinessAttempts : MetaM (List Attempt) := do
  return [
    ⟨"cases", ← `(tactic| (intro x; cases x))⟩,
    ⟨"cases and omega", ← `(tactic| (intro x; cases x <;> omega))⟩,
    ⟨"cases and simp_all", ← `(tactic| (intro x; cases x <;> simp_all))⟩,
    ⟨"nomatch", ← `(tactic| (intro x; exact nomatch x))⟩]

/-- A contradiction among hypotheses. -/
structure Contradiction where
  /-- The automation that found it. -/
  foundBy : String
  /-- The hypotheses that the derivation of `False` uses: their names, or their statements where
  they have no name. -/
  hypotheses : Array String
  deriving Repr, Inhabited, BEq

/-- A variable that ranges over an empty type. -/
structure EmptyDomain where
  /-- The name of the variable. -/
  name : String
  /-- Its type, as text. -/
  type : String
  /-- The automation that proved the type empty. -/
  foundBy : String
  deriving Repr, Inhabited, BEq

/-- What the analysis found about a statement. -/
structure VacuityResult where
  /-- The hypotheses of the statement, in order: their names, or their statements where they have
  no name. -/
  hypotheses : Array String
  /-- The names of its variables, in order. -/
  variables : Array String
  /-- Whether the conclusion is `False`: such a statement says itself that its hypotheses are
  contradictory, and is not examined further. -/
  refutation : Bool := false
  /-- A contradiction among the hypotheses, if one was found. -/
  contradiction : Option Contradiction := none
  /-- The variables found to range over empty types. -/
  emptyDomains : Array EmptyDomain := #[]
  /-- The automation that proved the conclusion with no hypothesis, if one did. -/
  trivialBy : Option String := none
  deriving Repr, Inhabited, BEq

/-- Whether the statement is vacuous: no case satisfies its hypotheses. -/
def VacuityResult.vacuous (r : VacuityResult) : Bool :=
  r.contradiction.isSome || !r.emptyDomains.isEmpty

/-- Runs `k` on the variables, the hypotheses and the conclusion of a statement. A binder is a
hypothesis when its type is a proposition. -/
def withStatement (statement : Expr) (k : Array Expr → Array Expr → Expr → MetaM α) : MetaM α :=
  forallTelescope statement fun binders conclusion => do
    let mut variables := #[]
    let mut hypotheses := #[]
    for binder in binders do
      if ← isProp (← inferType binder) then hypotheses := hypotheses.push binder
      else variables := variables.push binder
    k variables hypotheses conclusion

/-- How a report refers to a local variable: by its name, or, for a hypothesis that was given
none, by its statement. -/
def binderLabel (e : Expr) : MetaM String := do
  let name ← e.fvarId!.getUserName
  if name.hasMacroScopes || name.isAnonymous then return toString (← ppExpr (← inferType e))
  return name.toString

/-- Whether automation proves a closed statement, with nothing else in sight: its binders are
introduced, all of them or the first `binders`, and the attempts are tried on what remains.
Returns the name of the attempt that succeeded. -/
def provesStatement (statement : Expr) (attempts : List Attempt) (binders : Option Nat := none) :
    MetaM (Option String) :=
  withLCtx {} {} <| forallBoundedTelescope statement binders fun _ conclusion => do
    return (← proveBy conclusion attempts).map (·.1)

/-- Looks for the ways in which a statement can be true for the wrong reasons. The statement is
closed, and it is examined on its own: nothing of the context of the caller is in sight. -/
def analyzeVacuity (statement : Expr) : MetaM VacuityResult :=
  withLCtx {} {} <| withStatement statement fun variables hypotheses conclusion => do
    let mut result : VacuityResult :=
      { hypotheses := ← hypotheses.mapM binderLabel, variables := ← variables.mapM binderLabel }
    if conclusion.isConstOf ``False then return { result with refutation := true }
    -- Variables over empty types. The claim is about the type alone: it is stated over the
    -- variables that come before, with no hypothesis, and skipped if the type mentions one.
    let mut before : Array Expr := #[]
    for x in variables do
      let type ← inferType x
      let claim ← mkForallFVars before (← mkArrow type (mkConst ``False))
      let introduced := before.size
      before := before.push x
      if claim.hasFVar then continue
      if let some foundBy ← provesStatement claim (← emptinessAttempts) (some introduced) then
        let domain : EmptyDomain := { name := ← binderLabel x, type := toString (← ppExpr type), foundBy }
        result := { result with emptyDomains := result.emptyDomains.push domain }
    -- A contradiction among the hypotheses.
    unless hypotheses.isEmpty do
      if let some (foundBy, proof) ← proveBy (mkConst ``False) (← contradictionAttempts) then
        let used ← hypotheses.filterMapM fun h => do
          if proof.containsFVar h.fvarId! then return some (← binderLabel h) else return none
        result := { result with contradiction := some { foundBy, hypotheses := used } }
    -- A conclusion that holds with no hypothesis: stated over the variables alone, and skipped
    -- if the conclusion mentions a hypothesis.
    unless hypotheses.isEmpty || result.contradiction.isSome do
      let claim ← mkForallFVars variables conclusion
      unless claim.hasFVar do
        result := { result with trivialBy := ← provesStatement claim (← conclusionAttempts) }
    return result

/-- The items in a sentence: `a`, `a and b`, `a, b and c`. -/
def nameList (names : Array String) : String :=
  match names.toList with
  | [] => ""
  | [a] => a
  | many => String.intercalate ", " many.dropLast ++ " and " ++ many.getLast!

/-- The findings as text, one per line. -/
def VacuityResult.toString (r : VacuityResult) : String :=
  if r.refutation then
    "the conclusion is False: the statement says itself that its hypotheses are contradictory"
  else
    let lines := #[]
    let lines := match r.contradiction with
      | some c =>
        lines.push (s!"vacuous: the hypotheses are contradictory (found by {c.foundBy}" ++
          (if c.hypotheses.isEmpty then ")" else s!", from {nameList c.hypotheses})"))
      | none => lines
    let lines := r.emptyDomains.foldl (fun lines d =>
      lines.push s!"vacuous: the variable {d.name} ranges over an empty type, {d.type} (found by {d.foundBy})") lines
    let lines := match r.trivialBy with
      | some by_ => lines.push s!"the conclusion holds without the hypotheses (found by {by_})"
      | none => lines
    if lines.isEmpty then
      if r.hypotheses.isEmpty then
        "nothing found: the statement has no hypotheses, and no variable was shown to range over an empty type"
      else
        "nothing found: no contradiction was derived from the hypotheses, no variable was shown to range \
          over an empty type, and the conclusion was not proved without the hypotheses"
    else String.intercalate "\n" lines.toList

/-- The findings as JSON. -/
def VacuityResult.toJson (r : VacuityResult) : Json :=
  Json.mkObj [
    ("vacuous", r.vacuous),
    ("refutation", r.refutation),
    ("variables", Json.arr (r.variables.map fun n => Json.str n)),
    ("hypotheses", Json.arr (r.hypotheses.map fun n => Json.str n)),
    ("contradiction", match r.contradiction with
      | some c => Json.mkObj [("found_by", c.foundBy),
          ("hypotheses", Json.arr (c.hypotheses.map fun n => Json.str n))]
      | none => Json.null),
    ("empty_domains", Json.arr (r.emptyDomains.map fun d =>
      Json.mkObj [("variable", d.name), ("type", d.type), ("found_by", d.foundBy)])),
    ("trivial_conclusion", match r.trivialBy with
      | some by_ => Json.mkObj [("found_by", by_)]
      | none => Json.null)]

/-! ## Dropping hypotheses -/

/-- A statement with one hypothesis removed. -/
structure DropVariant where
  /-- The hypothesis that was removed: its name, or its statement where it has no name. -/
  hypothesis : String
  /-- What the hypothesis says, as text. -/
  text : String
  /-- The statement without it, or `none` when something later in the statement depends on the
  hypothesis, so that it cannot be removed alone. -/
  statement : Option Expr

/-- The statement with each of its hypotheses removed in turn. -/
def dropVariants (statement : Expr) : MetaM (Array DropVariant) :=
  forallTelescope statement fun binders conclusion => do
    let mut variants := #[]
    for h in binders do
      let type ← inferType h
      unless ← isProp type do continue
      let others := binders.filter (· != h)
      let dependent ← (others.push conclusion).anyM fun e => do
        let e ← if e.isFVar then inferType e else pure e
        return (← instantiateMVars e).containsFVar h.fvarId!
      let variant ← if dependent then pure none else some <$> mkForallFVars others conclusion
      variants := variants.push { hypothesis := ← binderLabel h, text := toString (← ppExpr type), statement := variant }
    return variants

/-- What was found out about a statement with one hypothesis removed. -/
inductive Necessity where
  /-- Without the hypothesis the statement fails, at this counterexample. -/
  | needed (counterexample : List (String × String))
  /-- Without the hypothesis the statement still holds: the named automation proved it. -/
  | notNeeded (foundBy : String)
  /-- Without the hypothesis the statement passed this many cases, and no proof was found. -/
  | notShown (cases : Nat)
  /-- Nothing could be found out, for the stated reason. -/
  | undecided (reason : String)
  deriving Repr, Inhabited, BEq

/-- Asks of each hypothesis whether the statement needs it. The statement without the hypothesis
is first tried by automation, which settles that the hypothesis is not needed, and then tested by
sampling, where a counterexample settles that it is. -/
def analyzeNecessity (statement : Expr) (cfg : Config := {}) : MetaM (Array (DropVariant × Necessity)) :=
  withLCtx {} {} do
  (← dropVariants statement).mapM fun variant => do
    match variant.statement with
    | none => return (variant, .undecided "the rest of the statement depends on it")
    | some dropped =>
      if let some foundBy ← provesStatement dropped (← conclusionAttempts) then
        return (variant, .notNeeded foundBy)
      let report ← try some <$> testProposition dropped cfg catch _ => pure none
      match report with
      | none => return (variant, .undecided "the statement without it cannot be tested by sampling")
      | some report =>
        match report.verdict with
        | .falsified => return (variant, .needed report.counterexample)
        | .passed => return (variant, .notShown report.cases)
        | .gaveUp => return (variant, .undecided "too many test cases were discarded")

/-- The label of a hypothesis in a report: its name with its statement, or the statement alone. -/
def DropVariant.label (variant : DropVariant) : String :=
  if variant.hypothesis == variant.text then variant.text else s!"{variant.hypothesis} : {variant.text}"

/-- The finding about one hypothesis as text. -/
def Necessity.describe (variant : DropVariant) : Necessity → String
  | .needed counterexample =>
    counterexample.foldl (fun text (name, value) => text ++ s!"\n  {name} := {value}")
      s!"{variant.label} is needed: without it the statement fails at"
  | .notNeeded foundBy => s!"{variant.label} is not needed: the statement holds without it (found by {foundBy})"
  | .notShown cases =>
    s!"{variant.label} was not needed in {cases} test cases, and no proof without it was found"
  | .undecided reason => s!"{variant.label} is undecided: {reason}"

/-! ## Hypotheses that a proof does not use -/

/-- The hypotheses of a theorem that its proof term does not mention. This is a fact about the
proof as it stands: automation that gathers every hypothesis mentions them all, whether or not
the conclusion depends on them, and `analyzeNecessity` is the tool for that question. -/
def unusedHypotheses (declaration : Name) : MetaM (Array String) := do
  let info ← getConstInfo declaration
  let some value := info.value? (allowOpaque := true)
    | throwError "{declaration} has no proof to examine"
  lambdaTelescope value fun binders body => do
    let body ← instantiateMVars body
    let mut unused := #[]
    for binder in binders do
      if ← isProp (← inferType binder) then
        unless body.containsFVar binder.fvarId! do unused := unused.push (← binderLabel binder)
    return unused

end Maithic.Refute
