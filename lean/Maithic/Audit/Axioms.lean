/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Axiom and trust audit

Lean's kernel accepts a proof relative to the axioms it uses. The audit makes that dependence
explicit. It collects every axiom a declaration rests on, transitively, and sorts them into four
classes:

* **standard**: propositional extensionality, quotient soundness and choice, the axioms that
  ordinary mathematics in Lean is built on;
* **incomplete**: the axiom that stands in for a missing proof, which means the proof is unfinished;
* **native**: axioms that trust compiled evaluation, as used by `native_decide`;
* **other**: everything else, in particular axioms declared by the file under audit.

A declaration is *clean* when all its axioms are standard. `#maithic_audit foo` prints the audit
of `foo`.
-/

namespace Maithic.Audit

open Lean

/-- The trust class of an axiom. -/
inductive AxiomClass where
  | standard
  | incomplete
  | native
  | other
  deriving Repr, BEq, DecidableEq, Inhabited

/-- The axioms of ordinary mathematics in Lean. -/
def standardAxioms : List Name := [``propext, ``Quot.sound, ``Classical.choice]

/-- Axioms through which earlier toolchains trusted the compiler and the native evaluator. The
names are kept so that audits of results produced by those toolchains classify correctly. -/
def nativeAxioms : List Name := [`Lean.ofReduceBool, `Lean.ofReduceNat, `Lean.trustCompiler]

/-- Classifies one axiom. The pinned toolchain records each native evaluation as an auxiliary
axiom whose name contains a `_native` component, for example `foo._native.native_decide.ax_1_1`;
those are classified as native. -/
def classify (n : Name) : AxiomClass :=
  if standardAxioms.contains n then .standard
  else if n == ``sorryAx then .incomplete
  else if nativeAxioms.contains n || n.components.any (fun c => c.toString.startsWith "_native") then
    .native
  else .other

/-- The overall verdict of an audit, from best to worst. -/
inductive Verdict where
  | clean
  | usesNative
  | nonStandard
  | usesSorry
  deriving Repr, BEq, DecidableEq, Inhabited

/-- The stable textual form of a verdict. -/
def Verdict.toString : Verdict → String
  | .clean => "clean"
  | .usesNative => "uses-native"
  | .nonStandard => "non-standard-axioms"
  | .usesSorry => "uses-sorry"

instance : ToString Verdict := ⟨Verdict.toString⟩

/-- The audit of one declaration. -/
structure Result where
  name : Name
  axioms : Array Name
  standard : Array Name
  native : Array Name
  other : Array Name
  usesSorry : Bool
  deriving Repr, Inhabited

/-- Builds the audit from a list of axioms. -/
def Result.ofAxioms (name : Name) (axioms : Array Name) : Result :=
  let axioms := axioms.qsort Name.lt
  { name, axioms,
    standard := axioms.filter (classify · == .standard),
    native := axioms.filter (classify · == .native),
    other := axioms.filter (classify · == .other),
    usesSorry := axioms.any (classify · == .incomplete) }

/-- The verdict: the worst class present decides. -/
def Result.verdict (r : Result) : Verdict :=
  if r.usesSorry then .usesSorry
  else if !r.other.isEmpty then .nonStandard
  else if !r.native.isEmpty then .usesNative
  else .clean

/-- `true` when every axiom is standard. -/
def Result.isClean (r : Result) : Bool := r.verdict == .clean

/-- The audit as JSON, in the shape of `schemas/ledger/audit.schema.json`. -/
def Result.toJson (r : Result) : Json :=
  let names (xs : Array Name) : Json := Json.arr (xs.map fun n => Json.str n.toString)
  Json.mkObj [
    ("name", Json.str r.name.toString),
    ("verdict", Json.str r.verdict.toString),
    ("axioms", names r.axioms),
    ("standard", names r.standard),
    ("native", names r.native),
    ("other", names r.other),
    ("usesSorry", Json.bool r.usesSorry)]

/-- One line for people. -/
def Result.render (r : Result) : String :=
  let list (xs : Array Name) : String := ", ".intercalate (xs.map (·.toString)).toList
  match r.verdict with
  | .clean =>
    if r.axioms.isEmpty then s!"{r.name}: clean (no axioms)"
    else s!"{r.name}: clean (standard axioms: {list r.standard})"
  | .usesNative => s!"{r.name}: trusts native evaluation ({list r.native})"
  | .nonStandard => s!"{r.name}: depends on non-standard axioms ({list r.other})"
  | .usesSorry => s!"{r.name}: unfinished, depends on sorry"

/-- Audits a declaration: collects its axioms transitively and classifies them. -/
def audit [Monad m] [MonadEnv m] (name : Name) : m Result := do
  return Result.ofAxioms name (← collectAxioms name)

open Elab Command in
/-- `#maithic_audit foo` prints the trust audit of the declaration `foo`. -/
elab "#maithic_audit " id:ident : command => do
  let name ← liftCoreM <| realizeGlobalConstNoOverloadWithInfo id
  let result ← liftCoreM <| audit name
  logInfo result.render

end Maithic.Audit
