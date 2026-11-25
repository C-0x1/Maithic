/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Maithic.Refute.Testing.Testable

/-!
# `#maithic_test` and `maithic_test`

The command tests a statement by sampling and reports the result; the tactic does the same for
the goal in front of it.

```
#maithic_test ∀ l : List Nat, l.reverse.reverse = l
-- passed 100 cases

#maithic_test ∀ x y : Int, x * y ≥ 0
-- error: falsified at case 12, shrunk 2 times
--   x := 1
--   y := -1
```

Options go before the statement: `(cases := 500)`, `(size := 100)`, `(seed := 7)`,
`(discards := 5000)` and `(shrinks := 200)`. A run is determined by its options, so a failure is
found again by running the same line.

The statement may also be the name of a declared theorem, whose statement is then tested:
`#maithic_test my_conjecture`. And `#maithic_test json …` prints the report as JSON whatever the
verdict, for a program that reads it.

A statement that is falsified, or that could not be tested because too many cases were discarded,
is an error: a file with a failing test does not compile. A statement that passes is not proved;
the message says how many cases it passed and nothing more.
-/

namespace Maithic.Refute

open Lean Meta Elab

deriving instance ToExpr for Config

/-- Marks every quantifier over a type with the name of its variable, so that the instances of
`Testable` can name the values of a counterexample. Premises and conjunctions are entered;
everything else is left as it is. -/
partial def decorate (e : Expr) : MetaM Expr := do
  let e := e.consumeMData
  if let .forallE name domain body info := e then
    withLocalDecl name info domain fun x => do
      let body ← decorate (body.instantiate1 x)
      let statement ← mkForallFVars #[x] body
      if ← isProp domain then return statement
      return mkApp2 (mkConst ``NamedBinder) (toExpr name.eraseMacroScopes.toString) statement
  else
    match_expr e with
    | And p q => return mkApp2 (mkConst ``And) (← decorate p) (← decorate q)
    | _ => return e

private unsafe def evalReportUnsafe (e : Expr) : MetaM Report :=
  evalExpr Report (mkConst ``Report) e

/-- Runs a closed expression of type `Report`. -/
@[implemented_by evalReportUnsafe]
private opaque evalReport (e : Expr) : MetaM Report

/-- Tests a closed proposition with the given settings. -/
def testProposition (statement : Expr) (cfg : Config) : MetaM Report := withLCtx {} {} do
  let statement ← decorate (← instantiateMVars statement)
  let goal := mkApp (mkConst ``Testable) statement
  -- A statement with several variables of compound types needs more room than the default.
  let instance_ ← try
      withOptions (fun o => synthInstance.maxHeartbeats.set (synthInstance.maxSize.set o 8192) 200000)
        (synthInstance goal)
    catch _ =>
    throwError "this statement cannot be tested by sampling: every type it quantifies over needs \
      instances of `Arbitrary`, `Shrinkable` and `Repr`, and its conditions must be decidable.\n\
      No instance was found for{indentExpr goal}"
  evalReport (mkApp3 (mkConst ``check) statement instance_ (toExpr cfg))

/-- The report as JSON: the verdict, the counts, the seed, and the counterexample as a list of
variables with their values. -/
def Report.toJson (r : Report) : Json :=
  Json.mkObj [
    ("verdict", match r.verdict with
      | .passed => "passed"
      | .falsified => "falsified"
      | .gaveUp => "gave-up"),
    ("cases", r.cases), ("discarded", r.discarded), ("shrinks", r.shrinks), ("seed", r.seed),
    ("counterexample", Json.arr (r.counterexample.toArray.map fun (name, value) =>
      Json.mkObj [("name", name), ("value", value)]))]

/-- One setting of a test, written before the statement. -/
syntax testOption := atomic("(" ident " := ") num ")"

/-- Reads the settings of a test. -/
def readConfig (options : Array Syntax) : CoreM Config := do
  let mut cfg : Config := {}
  for option in options do
    let value := option[3].toNat
    match option[1].getId with
    | `cases => cfg := { cfg with cases := value }
    | `size => cfg := { cfg with size := value }
    | `seed => cfg := { cfg with seed := value }
    | `discards => cfg := { cfg with discards := value }
    | `shrinks => cfg := { cfg with shrinks := value }
    | other => throwErrorAt option[1] "unknown test option `{other}`; the options are cases, size, seed, discards and shrinks"
  return cfg

/-- The statement a command was given: the type of a declared theorem, or a proposition written
out. Variables of the section that the proposition mentions are quantified. -/
def elabStatement (stx : Syntax) (variables : Array Expr) : TermElabM Expr := do
  if stx.isIdent then
    if let some name ← try some <$> realizeGlobalConstNoOverloadWithInfo stx catch _ => pure none then
      let info ← getConstInfo name
      if ← isProp info.type then return info.type
  let statement ← Term.withoutErrToSorry (Term.elabType stx)
  Term.synthesizeSyntheticMVarsNoPostponing
  let statement ← instantiateMVars statement
  unless ← isProp statement do throwErrorAt stx "a proposition or the name of a theorem is expected"
  mkForallFVars variables statement (usedOnly := true)

/--
`#maithic_test p` tests the statement `p` by sampling. `p` is a proposition or the name of a
theorem. Variables that the statement quantifies over are generated; premises that fail discard a
case. The command reports the number of cases passed, or fails with a counterexample.
`#maithic_test json p` prints the report as JSON and does not fail on a counterexample.
-/
syntax (name := maithicTestCommand) "#maithic_test " (&"json ")? (testOption)* term : command

@[command_elab maithicTestCommand] def elabMaithicTestCommand : Command.CommandElab := fun stx => do
  let cfg ← Command.liftCoreM (readConfig stx[2].getArgs)
  Command.runTermElabM fun variables => do
    let report ← testProposition (← elabStatement stx[3] variables) cfg
    if !stx[1].isNone then logInfo report.toJson.pretty
    else match report.verdict with
      | .passed => logInfo report.toString
      | _ => throwError report.toString

/-- The variables of a statement given to `#maithic_test` are named so that a counterexample can
be reported by name. A hypothesis that nothing refers to is as it should be. -/
@[unused_variables_ignore_fn]
def ignoreTestedStatement : Lean.Linter.IgnoreFunction := fun _ stack _ =>
  stack.any fun (stx, _) => stx.isOfKind ``maithicTestCommand

/--
`maithic_test` tests the goal by sampling, with the hypotheses as premises and the variables of
the context generated. It fails with a counterexample when it finds one, and otherwise reports
the number of cases passed and leaves the goal as it is: passing a test proves nothing.
-/
syntax (name := maithicTestTactic) "maithic_test" (testOption)* : tactic

@[tactic maithicTestTactic] def evalMaithicTestTactic : Tactic.Tactic := fun stx => do
  let cfg ← readConfig stx[1].getArgs
  let goal ← Tactic.getMainGoal
  goal.withContext do
    let locals := (← getLCtx).foldl (init := #[]) fun acc decl =>
      if decl.isImplementationDetail then acc else acc.push decl.toExpr
    let statement ← mkForallFVars locals (← instantiateMVars (← goal.getType))
    let report ← testProposition statement cfg
    match report.verdict with
    | .passed => logInfo report.toString
    | _ => throwError report.toString

end Maithic.Refute
