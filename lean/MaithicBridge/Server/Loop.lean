/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Server.Handlers

/-!
# Request loop

Reads one request per line from standard input, answers with one response per line on standard
output and writes nothing else there. The loop ends at end of input or after a `shutdown`
request. A failure inside a handler becomes an error response; it never ends the loop.
-/

namespace Maithic.Bridge

open Lean

/-- Handles one line of input. Returns the response line and whether the loop continues. -/
def handleLine (line : String) : SessionM (String × Bool) := do
  modify fun s => { s with requests := s.requests + 1 }
  match Json.parse line with
  | .error e =>
    return ((errorResponse Json.null (.parseError e)).compress, true)
  | .ok j =>
    match Request.ofJson j with
    | .error e =>
      let id := (j.getObjVal? "id").toOption.getD Json.null
      return ((errorResponse id e).compress, true)
    | .ok req =>
      let outcome ← try
          (handle req.method req.params).run
        catch e =>
          pure (.error (.internal (toString e)))
      match outcome with
      | .ok result => return ((okResponse req.id result).compress, req.method != "shutdown")
      | .error e => return ((errorResponse req.id e).compress, true)

/-- Answers every line of a scripted session and returns the response lines. Used by tests. -/
def runScript (lines : List String) : SessionM (List String) := do
  let mut out := []
  for line in lines do
    if line.trimAscii.toString.isEmpty then continue
    let (resp, continue?) ← handleLine line
    out := resp :: out
    if !continue? then break
  return out.reverse

/-- Runs the request loop until end of input or `shutdown`. -/
partial def serve : SessionM Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let rec loop : SessionM Unit := do
    let line ← stdin.getLine
    if line.isEmpty then
      return
    let trimmed := line.trimAscii.toString
    if trimmed.isEmpty then
      loop
    else
      let (resp, continue?) ← handleLine trimmed
      stdout.putStrLn resp
      stdout.flush
      if continue? then loop
  loop

end Maithic.Bridge
