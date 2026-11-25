/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Server.Loop

/-!
# `maithic-bridge`

The executable entry point of the Bridge.

```
maithic-bridge            serve requests on standard input and output
maithic-bridge --version  print the Bridge, protocol and Lean versions
maithic-bridge --help     print usage
```
-/

open Lean Maithic.Bridge

/-- The compiled modules of the package this executable was built in: `lib/lean` next to the
`bin` directory that holds it. With them on the search path, a file may import the Maithic
libraries, the statement tests among them, whatever project the Bridge runs in. `none` when the
executable was copied away from its build. -/
def ownLibrary : IO (Option System.FilePath) := do
  let some bin := (← IO.appPath).parent | return none
  let some build := bin.parent | return none
  let library := build / "lib" / "lean"
  return if ← library.isDir then some library else none

def usage : String :=
  "maithic-bridge " ++ bridgeVersion ++ "\n" ++
  "Usage: maithic-bridge [--version | --help]\n" ++
  "Without arguments, serves newline-delimited JSON requests on standard input and output.\n" ++
  "Methods: handshake, loadHeader, runCommand, checkFile, extract, tryCandidates, releaseEnv, guard,\n" ++
  "startProof, runTactic, tryTactics, getState, releaseState, stats, selfTest, shutdown.\n" ++
  "See docs/reference/bridge-protocol.md."

def main (args : List String) : IO UInt32 := do
  match args with
  | ["--version"] =>
    IO.println s!"maithic-bridge {bridgeVersion} (protocol {protocolVersion}, Lean {Lean.versionString})"
    return 0
  | ["--help"] =>
    IO.println usage
    return 0
  | [] =>
    initSearchPath (← findSysroot)
    -- The modules of a project the Bridge runs in come first; the Bridge's own come after them.
    if let some library ← ownLibrary then
      searchPathRef.modify fun path => if path.contains library then path else path ++ [library]
    let _ ← serve.run {}
    return 0
  | _ =>
    IO.eprintln usage
    return 2
