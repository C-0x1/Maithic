/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Oracle.Logic.Soundness
import Maithic.Oracle.Logic.Search

/-!
# Propositional goals by reflection

The tactics that connect goals to the checkers of this directory. A goal is read as a formula:
`True`, `False`, `¬`, `∧`, `∨`, `→`, `↔` and `≠` are connectives, and every other proposition is
an atom, the same proposition always the same atom. The hypotheses that are propositions are read
the same way and become the premises of an implication. What remains is a statement about a
`Formula`, and the kernel settles it by running a checker:

* `maithic_taut` evaluates the truth table (`Formula.isTautology`);
* `maithic_sat` searches for a clausal proof and has `Lrat.check` check it;
* `maithic_lrat "…"` has `Lrat.check` check a clausal proof given as text, for instance one
  written by a satisfiability solver for the problem that `maithic_cnf` prints.

None of these adds an axiom or trusts the search: the proof term applies a soundness theorem to
the fact that the checker returns `true`, which the kernel confirms by computation. When a goal
does not follow by propositional reasoning, the tactics say under which truth values it fails.
Atoms are independent for these tactics: `x < 3` and `x < 5` are two unrelated atoms.
-/

namespace Maithic.Oracle.Logic

open Lean Meta Elab Tactic

deriving instance ToExpr for Formula
deriving instance ToExpr for Literal
deriving instance ToExpr for Lrat.Step

/-- A goal read as a formula, with the propositions its atoms stand for. -/
structure Reflected where
  /-- The formula. -/
  formula : Formula
  /-- The proposition of each atom, by number. -/
  atoms : Array Expr

/-- Reads a proposition as a formula. New atoms are added to the state. -/
partial def reify (e : Expr) : StateRefT (Array Expr) MetaM Formula := do
  let e := (← instantiateMVars e).consumeMData
  match_expr e with
  | True => return .tt
  | False => return .ff
  | Not p => return .not (← reify p)
  | And p q => return .and (← reify p) (← reify q)
  | Or p q => return .or (← reify p) (← reify q)
  | Iff p q => return .iff (← reify p) (← reify q)
  | Ne _ a b => return .not (← atom (← mkEq a b))
  | _ =>
    if let .forallE _ domain body _ := e then
      if !body.hasLooseBVars then
        if ← isProp domain then
          return .imp (← reify domain) (← reify body)
    atom e
where
  /-- The atom of a proposition: its position among the atoms seen so far. -/
  atom (e : Expr) : StateRefT (Array Expr) MetaM Formula := do
    let atoms ← get
    match atoms.findIdx? (· == e) with
    | some i => return .atom i
    | none =>
      set (atoms.push e)
      return .atom atoms.size

/-- Reads a proposition as a formula. -/
def reflect (e : Expr) : MetaM Reflected := do
  let (formula, atoms) ← (reify e).run #[]
  return { formula, atoms }

/-- The expression `atomsOf [p₀, p₁, …]`: the meaning of the atoms inside Lean. -/
def Reflected.valuation (r : Reflected) : MetaM Expr := do
  return mkApp (mkConst ``atomsOf) (← mkListLit (mkSort Level.zero) r.atoms.toList)

/-- The truth values of a model, one line per atom, for an error message. -/
def Reflected.describe (r : Reflected) (trueAtoms : List Nat) : MessageData :=
  MessageData.joinSep
    (r.atoms.toList.zipIdx.map fun (e, i) => m!"  {e} := {if trueAtoms.contains i then "True" else "False"}")
    "\n"

/-- The main goal together with its hypotheses, as one implication. -/
structure Problem where
  /-- The goal. -/
  goal : MVarId
  /-- The hypotheses that are propositions, in the order of the context. -/
  hypotheses : Array Expr
  /-- The implication from the hypotheses to the target. -/
  statement : Expr
  /-- The statement as a formula. -/
  reflected : Reflected

/-- Reads the main goal and its propositional hypotheses. -/
def readProblem : TacticM Problem := do
  let goal ← getMainGoal
  goal.withContext do
    let mut hypotheses : Array Expr := #[]
    for decl in ← getLCtx do
      if decl.isImplementationDetail then continue
      if ← isProp decl.type then hypotheses := hypotheses.push decl.toExpr
    let target ← instantiateMVars (← goal.getType)
    let mut statement := target
    for h in hypotheses.reverse do
      statement ← mkArrow (← instantiateMVars (← inferType h)) statement
    return { goal, hypotheses, statement, reflected := ← reflect statement }

/-- Closes the goal of a problem with a proof of the denotation of its formula. -/
def Problem.close (p : Problem) (proof : Expr) : TacticM Unit := do
  p.goal.withContext do
    unless ← isDefEq (← inferType proof) p.statement do
      throwError "the reflected formula does not denote the goal:{indentExpr p.statement}"
    p.goal.assign (mkAppN (← mkExpectedTypeHint proof p.statement) p.hypotheses)
    replaceMainGoal []

/-- The message for a goal that does not follow, with the truth values under which it fails. -/
def Problem.failure (p : Problem) : MessageData :=
  match Search.solve p.reflected.formula.refutation with
  | .satisfiable trueAtoms =>
    m!"the goal does not follow by propositional reasoning; it fails when\n{p.reflected.describe trueAtoms}"
  | _ => m!"the goal does not follow by propositional reasoning"

/-- The proof `Eq.refl true`, which the kernel accepts for `check = true` by running `check`. -/
private def reflTrue : Expr :=
  mkApp2 (mkConst ``Eq.refl [Level.one]) (mkConst ``Bool) (mkConst ``Bool.true)

/-- The largest number of atoms `maithic_taut` takes on: a truth table of 2^16 rows. -/
def tautAtomLimit : Nat := 16

/--
`maithic_taut` closes a goal that follows from the hypotheses by propositional reasoning, by
evaluating a truth table in the kernel. It is meant for goals with few atoms; `maithic_sat`
handles more. When the goal does not follow, the error shows truth values under which it fails.
-/
syntax (name := maithicTaut) "maithic_taut" : tactic

@[tactic maithicTaut] def evalMaithicTaut : Tactic := fun _ => do
  let p ← readProblem
  let f := p.reflected.formula
  if p.reflected.atoms.size > tautAtomLimit then
    throwError "maithic_taut: {p.reflected.atoms.size} atoms are too many for a truth table \
      (the limit is {tautAtomLimit}); use maithic_sat"
  unless f.isTautology do throwError "maithic_taut: {p.failure}"
  p.close (mkApp3 (mkConst ``Formula.denote_of_isTautology) (toExpr f) reflTrue (← p.reflected.valuation))

/-- Closes the goal of a problem from a clausal proof that `Lrat.check` accepts. -/
def Problem.closeByLrat (p : Problem) (proof : List Lrat.Step) : TacticM Unit := do
  p.close (mkApp4 (mkConst ``Formula.denote_of_lrat) (toExpr p.reflected.formula) (toExpr proof)
    reflTrue (← p.reflected.valuation))

/--
`maithic_sat` closes a goal that follows from the hypotheses by propositional reasoning. It
searches for a clausal proof and the kernel checks the proof with the verified checker
`Lrat.check`, so the search itself is not trusted. When the goal does not follow, the error
shows truth values under which it fails.
-/
syntax (name := maithicSat) "maithic_sat" : tactic

@[tactic maithicSat] def evalMaithicSat : Tactic := fun _ => do
  let p ← readProblem
  match Search.solve p.reflected.formula.refutation with
  | .refuted proof =>
    unless Lrat.refutes p.reflected.formula.refutation proof do
      throwError "maithic_sat: the search returned a proof that the checker does not accept"
    p.closeByLrat proof
  | .satisfiable trueAtoms =>
    throwError "maithic_sat: the goal does not follow by propositional reasoning; it fails when\n\
      {p.reflected.describe trueAtoms}"
  | .unknown => throwError "maithic_sat: the search stopped at its budget"

/--
`maithic_lrat "proof"` closes a goal that follows from the hypotheses by propositional reasoning,
from a clausal proof in the LRAT text form: one step per line, `id literals 0 hints 0`. The proof
refutes the problem that `maithic_cnf` prints, whose clauses are numbered from one. Only steps
justified by unit propagation are accepted. A proof that fails is reported with its first failing
step.
-/
syntax (name := maithicLrat) "maithic_lrat " str : tactic

@[tactic maithicLrat] def evalMaithicLrat : Tactic := fun stx => do
  let p ← readProblem
  let proof ← match Lrat.parse stx[1].isStrLit?.get! with
    | .ok proof => pure proof
    | .error e => throwError "maithic_lrat: {e}"
  match Lrat.check p.reflected.formula.refutation proof with
  | .ok _ => p.closeByLrat proof
  | .error e => throwError "maithic_lrat: {e}"

/--
`maithic_cnf` prints the clause set whose unsatisfiability proves the goal, in the DIMACS format,
with the proposition each variable stands for. The goal is left as it is. A clausal proof for
this problem closes the goal with `maithic_lrat`.
-/
syntax (name := maithicCnf) "maithic_cnf" : tactic

@[tactic maithicCnf] def evalMaithicCnf : Tactic := fun _ => do
  let p ← readProblem
  let legend := MessageData.joinSep
    (p.reflected.atoms.toList.zipIdx.map fun (e, i) => m!"c {i + 1} := {e}") "\n"
  logInfo m!"{legend}\n{p.reflected.formula.refutation.toDimacs}"

/--
`#maithic_reflect p` shows how the proposition `p` is read: the formula, and the proposition each
atom stands for.
-/
syntax (name := maithicReflect) "#maithic_reflect " term : command

@[command_elab maithicReflect] def elabMaithicReflect : Command.CommandElab := fun stx =>
  Command.runTermElabM fun _ => do
    let e ← Term.elabType stx[1]
    Term.synthesizeSyntheticMVarsNoPostponing
    let r ← reflect (← instantiateMVars e)
    let legend := MessageData.joinSep
      (r.atoms.toList.zipIdx.map fun (e, i) => m!"a{i} := {e}") ", "
    logInfo m!"{r.formula.toText}\nwhere {legend}"

end Maithic.Oracle.Logic
