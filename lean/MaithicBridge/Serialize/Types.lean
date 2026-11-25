/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Wire types

The data the Bridge reports. Every type here has `ToJson` and `FromJson` instances and a matching
schema under `schemas/bridge/`. The OCaml client in `ocaml/lib/bridge/client` decodes the same
shapes.

The Bridge reports facts only. Whether a declaration counts as *verified* is decided by the trust
policy on the orchestrator side, from the facts reported here: the absence of error diagnostics,
the kind of the declaration, and the set of axioms it depends on.
-/

namespace Maithic.Bridge

open Lean

/-- Version of the wire protocol. Bumped on any incompatible change. -/
def protocolVersion : Nat := 1

/-- Version of the Bridge executable. Kept equal to the repository version. -/
def bridgeVersion : String := "0.1.0"

/-- One diagnostic reported by the elaborator or by the kernel. Positions are one-based lines and
zero-based code-point columns, as Lean reports them. -/
structure Diagnostic where
  severity : String
  line : Nat
  column : Nat
  endLine : Option Nat := none
  endColumn : Option Nat := none
  text : String
  deriving FromJson, Repr, BEq, Inhabited

/-- End positions are left out when they are unknown, rather than sent as `null`. -/
instance : ToJson Diagnostic where
  toJson d :=
    let fields : List (String × Json) := [
      ("severity", Json.str d.severity), ("line", toJson d.line), ("column", toJson d.column)]
    let fields := match d.endLine with
      | some l => fields ++ [("endLine", toJson l)]
      | none => fields
    let fields := match d.endColumn with
      | some c => fields ++ [("endColumn", toJson c)]
      | none => fields
    Json.mkObj (fields ++ [("text", Json.str d.text)])

/-- Facts about one declaration after elaboration. -/
structure DeclReport where
  name : String
  found : Bool
  kind : String := ""
  type : String := ""
  axioms : Array String := #[]
  usesSorry : Bool := false
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Outcome of elaborating one piece of source text. -/
structure CheckReport where
  ok : Bool
  errors : Nat
  warnings : Nat
  diagnostics : Array Diagnostic
  decls : Array DeclReport
  elapsedMs : Nat
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- A report with no diagnostics and no declarations. -/
def CheckReport.empty : CheckReport :=
  { ok := true, errors := 0, warnings := 0, diagnostics := #[], decls := #[], elapsedMs := 0 }

/-- One candidate of a `tryCandidates` request: the full text to elaborate, and the part of it
that came from an untrusted proposer and must pass the guard. -/
structure Candidate where
  source : String
  untrusted : String := ""
  deriving ToJson, Repr, BEq, Inhabited

/-- `untrusted` may be left out; the whole source is then treated as untrusted. -/
instance : FromJson Candidate where
  fromJson? j := do
    let source ← j.getObjValAs? String "source"
    let untrusted := (j.getObjValAs? String "untrusted").toOption.getD ""
    return { source, untrusted }

/-- Outcome of one candidate. A candidate rejected by the guard is never elaborated. -/
structure CandidateReport where
  index : Nat
  admitted : Bool
  rejection : String := ""
  report : Option CheckReport := none
  deriving FromJson, Repr, BEq, Inhabited

/-- The report is left out for a candidate that was not elaborated. -/
instance : ToJson CandidateReport where
  toJson c :=
    let fields : List (String × Json) := [
      ("index", toJson c.index), ("admitted", toJson c.admitted), ("rejection", toJson c.rejection)]
    match c.report with
    | some r => Json.mkObj (fields ++ [("report", toJson r)])
    | none => Json.mkObj fields

/-- One hypothesis of a goal, as text. `value` is present for a `let` binding. -/
structure Hypothesis where
  name : String
  type : String
  value : Option String := none
  deriving FromJson, Repr, BEq, Inhabited

/-- The value is left out for an ordinary hypothesis. -/
instance : ToJson Hypothesis where
  toJson h :=
    let fields : List (String × Json) := [("name", Json.str h.name), ("type", Json.str h.type)]
    match h.value with
    | some v => Json.mkObj (fields ++ [("value", Json.str v)])
    | none => Json.mkObj fields

/-- One goal of a proof state, as text: an identifier that stays the same for as long as the goal
is open, the case tag (empty when the goal has none), the hypotheses in order, the target, and the
whole goal as Lean prints it. -/
structure Goal where
  id : String
  tag : String := ""
  hypotheses : Array Hypothesis := #[]
  target : String
  pretty : String
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- The outcome of running a tactic on a proof state. On success `state` is the handle of the new
state and `goals` are its goals. On failure `kind` says where the tactic was stopped: `guard`
(refused before parsing), `parse`, or `tactic` (Lean raised an error or ran out of steps). -/
structure TacticOutcome where
  ok : Bool
  state : Option Nat := none
  goals : Array Goal := #[]
  solved : Bool := false
  kind : String := ""
  error : String := ""
  diagnostics : Array Diagnostic := #[]
  deriving FromJson, Repr, BEq, Inhabited

/-- A success carries the new state and its goals; a failure carries its kind and the error. -/
instance : ToJson TacticOutcome where
  toJson o :=
    let common : List (String × Json) := [("diagnostics", toJson o.diagnostics)]
    match o.ok, o.state with
    | true, some id =>
      Json.mkObj ([("ok", Json.bool true), ("state", toJson id), ("goals", toJson o.goals),
        ("solved", Json.bool o.solved)] ++ common)
    | _, _ =>
      Json.mkObj ([("ok", Json.bool false), ("kind", Json.str o.kind), ("error", Json.str o.error)]
        ++ common)

/-- Limits applied to one elaboration. Zero means the Lean default. -/
structure Limits where
  /-- Deterministic step budget, in thousands of heartbeats. -/
  maxHeartbeats : Nat := 0
  /-- Maximum recursion depth. -/
  maxRecDepth : Nat := 0
  deriving ToJson, FromJson, Repr, BEq, Inhabited

/-- Reads an optional field, falling back to a default when it is absent or has the wrong type. -/
def optField [FromJson α] (j : Json) (key : String) (default : α) : α :=
  match j.getObjValAs? α key with
  | .ok v => v
  | .error _ => default

/-- Reads limits from the parameters of a request. -/
def Limits.ofParams (j : Json) : Limits :=
  { maxHeartbeats := optField j "maxHeartbeats" 0, maxRecDepth := optField j "maxRecDepth" 0 }

/-- Applies limits to a set of options. -/
def Limits.apply (l : Limits) (opts : Options) : Options :=
  let opts := if l.maxHeartbeats == 0 then opts else opts.set `maxHeartbeats l.maxHeartbeats
  if l.maxRecDepth == 0 then opts else opts.set `maxRecDepth l.maxRecDepth

end Maithic.Bridge
