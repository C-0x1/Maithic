/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import MaithicTest.Bridge
import MaithicTest

/-!
# `maithic_test`

The test driver of the Lean package, run by `lake test`. Compile-time tests are checked when this
executable is built, because it imports them. At run time it replays the scripted Bridge sessions,
runs the unit tests of the Bridge, and checks the proof fixtures of the logic checkers.

Fixtures are read from `../fixtures`, or from the directory named by `MAITHIC_FIXTURES`.
-/

open Lean MaithicTest.Bridge

def main : IO UInt32 := do
  initSearchPath (← findSysroot)
  let fixtures : System.FilePath := (← IO.getEnv "MAITHIC_FIXTURES").getD "../fixtures"
  let dir := fixtures / "bridge" / "sessions"
  let mut passed := 0
  let mut failed := 0
  for name in sessions do
    let path := dir / s!"{name}.json"
    let outcome ← try
        match parseSession (← IO.FS.readFile path) with
        | .error e => pure #[s!"cannot parse {path}: {e}"]
        | .ok steps => runSession steps
      catch e => pure #[s!"cannot run {path}: {e}"]
    if outcome.isEmpty then
      passed := passed + 1
      IO.println s!"ok    session {name}"
    else
      failed := failed + 1
      IO.println s!"FAIL  session {name}"
      for line in outcome do IO.println s!"        {line}"
  let onDisk := (← dir.readDir).filterMap fun entry =>
    if entry.fileName.endsWith ".json" then some ((entry.fileName.dropEnd 5).toString) else none
  for name in onDisk do
    unless sessions.contains name do
      failed := failed + 1
      IO.println s!"FAIL  session {name} is on disk but not listed in MaithicTest.Bridge.sessions"
  for test in unitTests ++ (← MaithicTest.Logic.fixtureTests fixtures) do
    let outcome ← try test.run catch e => pure (some s!"exception: {e}")
    match outcome with
    | none =>
      passed := passed + 1
      IO.println s!"ok    {test.name}"
    | some reason =>
      failed := failed + 1
      IO.println s!"FAIL  {test.name}: {reason}"
  IO.println s!"maithic_test: {passed} passed, {failed} failed"
  return if failed == 0 then 0 else 1
