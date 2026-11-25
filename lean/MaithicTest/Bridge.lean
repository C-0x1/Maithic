/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicBridge
import MaithicTest.Unit

/-!
# Run-time tests of the Bridge

Two kinds of test run inside the test executable, against the same code the server runs:

* *scripted sessions* from `fixtures/bridge/sessions/`: a list of requests, each with the part
  of the response it must produce;
* *unit tests* of the wire types and of the environment store.

A session file is JSON: `{"name", "title", "steps": [{"request" | "raw", "expect"}]}`. The
`expect` value is matched as a subset of the response: every member it names must be present and
match, arrays must have the same length, and `{"$contains": "text"}` matches any string that
contains the text.
-/

namespace MaithicTest.Bridge

open Lean Maithic.Bridge

/-- `true` when `needle` occurs in `haystack`. -/
def containsText (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Subset match of an expected value against an actual one. Returns the path of the first
mismatch, or `none` when the values match. -/
partial def mismatch (path : String) (expected actual : Json) : Option String :=
  match expected, actual with
  | .obj e, _ =>
    match e.get? "$contains", actual with
    | some (.str needle), .str text =>
      if containsText text needle then none else some s!"{path}: \"{text}\" does not contain \"{needle}\""
    | some _, _ => some s!"{path}: $contains needs a string on both sides"
    | none, .obj a =>
      e.foldl (init := none) fun acc key value =>
        match acc with
        | some m => some m
        | none =>
          match a.get? key with
          | some av => mismatch s!"{path}.{key}" value av
          | none => some s!"{path}.{key}: missing"
    | none, _ => some s!"{path}: expected an object, got {actual.compress}"
  | .arr e, .arr a =>
    if e.size != a.size then
      some s!"{path}: expected {e.size} elements, got {a.size}"
    else
      (List.range e.size).foldl (init := none) fun acc i =>
        match acc with
        | some m => some m
        | none => mismatch s!"{path}[{i}]" e[i]! a[i]!
  | e, a => if e == a then none else some s!"{path}: expected {e.compress}, got {a.compress}"

/-- One step of a scripted session. -/
structure Step where
  line : String
  expect : Json

/-- Parses a session file. -/
def parseSession (text : String) : Except String (Array Step) := do
  let j ← Json.parse text
  let steps ← j.getObjValAs? (Array Json) "steps"
  steps.mapM fun s => do
    let expect ← s.getObjVal? "expect"
    match s.getObjVal? "raw" with
    | .ok (.str raw) => pure { line := raw, expect }
    | _ =>
      let request ← s.getObjVal? "request"
      pure { line := request.compress, expect }

/-- Runs a session in a fresh server state and returns the mismatches. -/
def runSession (steps : Array Step) : IO (Array String) := do
  let run : SessionM (Array String) := do
    let mut problems := #[]
    for h : i in [0:steps.size] do
      let step := steps[i]
      let (response, _) ← handleLine step.line
      match Json.parse response with
      | .error e => problems := problems.push s!"step {i + 1}: response is not JSON: {e}"
      | .ok actual =>
        if containsText response "\n" then
          problems := problems.push s!"step {i + 1}: response spans several lines"
        match mismatch "$" step.expect actual with
        | some m => problems := problems.push s!"step {i + 1}: {m}"
        | none => pure ()
    return problems
  run.run' {}

/-- The scripted sessions, with the capabilities each one covers. -/
def sessions : List String := [
  -- cap: P09-01-01
  -- cap: P09-01-02
  -- cap: P09-01-08
  -- cap: P09-01-14
  -- cap: P09-01-15
  -- cap: P09-01-17
  "handshake",
  -- cap: P09-01-03
  -- cap: P09-01-17
  "prove-even",
  -- cap: P09-01-04
  -- cap: P09-01-13
  -- cap: P09-03-02
  -- cap: P09-03-03
  -- cap: P09-03-05
  -- cap: P09-03-06
  -- cap: P09-03-07
  -- cap: P09-03-08
  "environments",
  -- cap: P09-01-10
  -- cap: P09-02-03
  -- cap: P09-03-01
  "whole-file",
  -- cap: P09-01-17
  "limits",
  -- cap: P09-02-07
  "escaping",
  -- cap: P09-01-16
  "self-test",
  -- cap: P09-01-05
  -- cap: P09-01-06
  -- cap: P09-01-07
  -- cap: P09-01-09
  -- cap: P09-01-11
  -- cap: P09-01-12
  -- cap: P09-02-01
  -- cap: P09-02-02
  -- cap: P09-02-06
  -- cap: P09-03-04
  "proof-states"]

/-- Checks that a value survives encoding and decoding. -/
def roundTrip [ToJson α] [FromJson α] [BEq α] (what : String) (value : α) : Option String :=
  match Json.parse (toJson value).compress with
  | .error e => some s!"{what}: encoded form is not JSON: {e}"
  | .ok j =>
    match fromJson? (α := α) j with
    | .error e => some s!"{what}: cannot decode: {e}"
    | .ok back => if back == value then none else some s!"{what}: value changed in the round trip"

/-- Runs a handler in a given server state. -/
def runHandler (s : Session) (x : HandlerM α) : IO (Except BridgeError α × Session) :=
  x.run.run s

def sampleDiagnostic : Diagnostic :=
  { severity := "error", line := 3, column := 14, endLine := some 3, endColumn := some 20,
    text := "quote \" backslash \\ newline \n tab \t unicode ∀ε>0 ✓ 数学" }

def sampleReport : CheckReport :=
  { ok := false, errors := 1, warnings := 0, diagnostics := #[sampleDiagnostic],
    decls := #[{ name := "t", found := true, kind := "theorem", type := "1 = 1",
                 axioms := #["propext"], usesSorry := false },
               { name := "absent", found := false }],
    elapsedMs := 12 }

def unitTests : List UnitTest := [
  -- cap: P09-02-04
  { name := "P09-02-04 diagnostics have JSON instances",
    run := pure (roundTrip "Diagnostic" sampleDiagnostic) },
  -- cap: P09-02-08
  { name := "P09-02-08 reports survive a round trip",
    run := pure (roundTrip "CheckReport" sampleReport) },
  -- cap: P09-02-08
  { name := "P09-02-08 candidates survive a round trip",
    run := pure <| (roundTrip "Candidate" ({ source := "theorem t : True := trivial", untrusted := "trivial" } : Candidate))
      <|> (roundTrip "CandidateReport" ({ index := 2, admitted := true, report := some sampleReport } : CandidateReport))
      <|> (roundTrip "CandidateReport" ({ index := 0, admitted := false, rejection := "forbidden keyword 'sorry'" } : CandidateReport))
      <|> (roundTrip "Limits" ({ maxHeartbeats := 400, maxRecDepth := 1024 } : Limits)) },
  -- cap: P09-02-07
  { name := "P09-02-07 a diagnostic with every awkward character stays on one line",
    run := pure <|
      if containsText (toJson sampleDiagnostic).compress "\n" then some "newline in compressed JSON" else none },
  -- cap: P09-02-03
  { name := "P09-02-03 optional end positions are omitted when unknown",
    run := pure <|
      let j := (toJson ({ severity := "info", line := 1, column := 0, text := "x" } : Diagnostic)).compress
      if containsText j "endLine" then some s!"unexpected endLine in {j}" else none },
  -- cap: P09-02-01
  { name := "P09-02-01 goals keep their text through JSON and omit values that are absent",
    run := pure <|
      let plain : Hypothesis := { name := "n", type := "Nat" }
      let bound : Hypothesis := { name := "m", type := "Nat", value := some "n + 1" }
      let goal : Goal := { id := "_uniq.7", tag := "succ", hypotheses := #[plain, bound], target := "m = n + 1",
                           pretty := "case succ\nn : Nat\nm : Nat := n + 1\n⊢ m = n + 1" }
      let text := (toJson goal).compress
      if containsText (toJson plain).compress "value" then some "a plain hypothesis has a value member"
      else if !containsText (toJson bound).compress "\"value\":\"n + 1\"" then some "the value of a let is missing"
      else if containsText text "\n" then some "newline in compressed JSON"
      else match fromJson? (toJson goal) with
        | .ok (back : Goal) => if back == goal then none else some "the goal changed in a round trip"
        | .error e => some e },
  -- cap: P09-01-06
  { name := "P09-01-06 a tactic outcome is a new state with goals, or a failure with its kind",
    run := pure <|
      let success : TacticOutcome := { ok := true, state := some 9, solved := true }
      let failure : TacticOutcome := { ok := false, kind := "parse", error := "unexpected token" }
      let good := (toJson success).compress
      let bad := (toJson failure).compress
      if !(containsText good "\"state\":9" && containsText good "\"solved\":true") then some s!"unexpected success: {good}"
      else if containsText good "kind" || containsText good "error" then some s!"a success mentions a failure: {good}"
      else if !(containsText bad "\"kind\":\"parse\"" && containsText bad "\"ok\":false") then some s!"unexpected failure: {bad}"
      else if containsText bad "state" || containsText bad "goals" then some s!"a failure carries a state: {bad}"
      else none },
  -- cap: P09-01-09
  { name := "P09-01-09 a full store refuses new proof states, and handles are never reused",
    run := do
      let (_, s) ← runHandler { maxProofs := 1 } (loadHeader #[])
      let (first, s) ← runHandler s (startProof 1 "theorem a : 1 = 1 := by\n  sorry" {})
      let (second, s) ← runHandler s (startProof 1 "theorem b : 2 = 2 := by\n  sorry" {})
      let (released, s) ← (releaseProof 2).run s
      let (third, _) ← runHandler s (startProof 1 "theorem c : 3 = 3 := by\n  sorry" {})
      match first, second, third with
      | .ok one, .error (.storeFull 1), .ok three =>
        if (one.states.map (·.state)) == #[2] && released && (three.states.map (·.state)) == #[3] then return none
        else return some s!"unexpected handles {one.states.map (·.state)} and {three.states.map (·.state)}"
      | _, _, _ => return some "the store did not refuse and recover as expected" },
  -- cap: P09-03-04
  { name := "P09-03-04 stepping a proof state leaves the state it started from unchanged",
    run := do
      let (_, s) ← runHandler {} (loadHeader #[])
      let (opened, s) ← runHandler s (startProof 1 "theorem t (p q : Prop) (hp : p) (hq : q) : p ∧ q := by\n  sorry" {})
      let (left, s) ← runHandler s (runTactic 2 "constructor" none {})
      let (direct, s) ← runHandler s (runTactic 2 "exact ⟨hp, hq⟩" none {})
      let (again, _) ← runHandler s (runTactic 2 "constructor" none {})
      match opened, left, direct, again with
      | .ok _, .ok a, .ok b, .ok c =>
        if a.ok && a.goals.size == 2 && b.ok && b.solved && c.ok && c.goals.map (·.target) == a.goals.map (·.target)
            && a.state != c.state then return none
        else return some "the branches of one state disagree"
      | _, _, _, _ => return some "a request failed" },
  -- cap: P09-03-07
  { name := "P09-03-07 import lists are keyed without order or duplicates",
    run := pure <|
      if importsKey #["Std", "Init", "Std"] == "Init,Std" && importsKey #["Init", "Std"] == "Init,Std"
          && importsKey #[] == "" then none
      else some s!"unexpected keys: {importsKey #["Std", "Init", "Std"]}" },
  -- cap: P09-03-06
  { name := "P09-03-06 a full store refuses new environments with a typed error",
    run := do
      let (r1, s) ← runHandler { maxEnvs := 1 } (loadHeader #[])
      let (r2, _) ← runHandler s (runCommand 1 "def one : Nat := 1" #[] true {})
      match r1, r2 with
      | .ok (1, false), .error (.storeFull 1) => return none
      | .ok _, .error e => return some s!"unexpected error {e.tag}"
      | _, _ => return some "the second environment was accepted" },
  -- cap: P09-03-03
  { name := "P09-03-03 deriving an environment leaves its parent unchanged",
    run := do
      let (_, s) ← runHandler {} (loadHeader #[])
      let (r1, s) ← runHandler s (runCommand 1 "def x : Nat := 1" #["x"] true {})
      let (r2, _) ← runHandler s (runCommand 1 "example : Nat := x" #[] false {})
      match r1, r2 with
      | .ok a, .ok b =>
        if a.report.ok && a.env == some 2 && !b.report.ok then return none
        else return some "the parent environment saw the derived definition"
      | _, _ => return some "handler failed" },
  -- cap: P09-01-14
  { name := "P09-01-14 every error carries a distinct code",
    run := pure <|
      let all : List BridgeError := [.parseError "", .invalidRequest "", .methodNotFound "", .invalidParams "",
        .internal "", .importFailed "", .unknownEnv 0, .storeFull 0]
      let tags := all.map (·.tag)
      let codes := all.map (·.rpcCode)
      if tags.eraseDups.length == all.length && codes.eraseDups.length == all.length
          && tags.all (·.startsWith "MTH-BRG-") then none
      else some "error codes are not distinct" }]

end MaithicTest.Bridge
