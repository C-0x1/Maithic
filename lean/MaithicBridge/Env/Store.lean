/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Serialize.Rpc

/-!
# Environment store

The Bridge keeps every environment it has built in an in-memory table and hands out numeric
handles. Environments are immutable values, so a handle can be shared freely: running commands on
an environment never changes it, it produces a *derived* environment with its own handle.

Import lists are deduplicated: asking for the same set of imports twice returns the same handle.
-/

namespace Maithic.Bridge

open Lean Elab

/-- Name used for the virtual file that requests are elaborated in. -/
def virtualFile : String := "<maithic>"

/-- One stored environment. -/
structure EnvEntry where
  /-- The command state: environment, scopes and options. -/
  state : Command.State
  /-- The environment right after the imports, used to compute what was added since. -/
  base : Environment
  /-- The handle this environment was derived from, if any. -/
  parent : Option Nat
  /-- The key of the import list at the root of the derivation. -/
  headerKey : String
  /-- Number of derivation steps from the header. -/
  depth : Nat

/-- The state of a tactic proof at one point: the environment, the metavariable context with the
open goals, and the settings of the elaborator. `MaithicBridge.Proof.Snapshot` explains how
snapshots are taken and used. -/
structure ProofSnapshot where
  coreState : Core.State
  coreContext : Core.Context
  metaState : Meta.State
  metaContext : Meta.Context
  termState : Term.State
  termContext : Term.Context
  /-- The open goals, the main goal first. -/
  goals : List MVarId

/-- One stored proof state. -/
structure ProofEntry where
  snapshot : ProofSnapshot
  /-- The environment the proof was opened on. -/
  env : Nat
  /-- The state this one was derived from by a tactic, if any. -/
  parent : Option Nat
  /-- Number of tactic steps from the opening of the proof. -/
  depth : Nat

/-- State kept between requests. Environments and proof states draw their handles from one
counter, so a handle names one thing for the life of the session. -/
structure Session where
  envs : Std.HashMap Nat EnvEntry := {}
  proofs : Std.HashMap Nat ProofEntry := {}
  headers : Std.HashMap String Nat := {}
  nextId : Nat := 1
  maxEnvs : Nat := 512
  maxProofs : Nat := 8192
  requests : Nat := 0
  headerHits : Nat := 0
  headerMisses : Nat := 0
  elaborations : Nat := 0
  tacticRuns : Nat := 0

abbrev SessionM := StateT Session IO

/-- Handlers run in the session monad and may fail with a typed error. -/
abbrev HandlerM := ExceptT BridgeError SessionM

/-- A stable key for an import list: sorted, deduplicated module names. -/
def importsKey (imports : Array String) : String :=
  let sorted := imports.qsort (· < ·)
  let dedup := sorted.foldl (init := (#[] : Array String)) fun acc m =>
    if acc.back? == some m then acc else acc.push m
  String.intercalate "," dedup.toList

/-- The header text for an import list. An empty list means the default imports. -/
def headerText (imports : Array String) : String :=
  String.join (imports.toList.map fun m => s!"import {m}\n")

/-- Stores an environment and returns its handle. -/
def Session.insert (s : Session) (e : EnvEntry) : Session × Nat :=
  let id := s.nextId
  ({ s with envs := s.envs.insert id e, nextId := id + 1 }, id)

/-- Looks up a handle. -/
def getEnv (id : Nat) : HandlerM EnvEntry := do
  match (← get).envs[id]? with
  | some e => return e
  | none => throw (.unknownEnv id)

/-- Stores an environment, failing when the store is full. -/
def putEnv (e : EnvEntry) : HandlerM Nat := do
  let s ← get
  if s.envs.size ≥ s.maxEnvs then
    throw (.storeFull s.maxEnvs)
  let (s, id) := s.insert e
  set s
  return id

/-- Looks up a proof state. -/
def getProof (id : Nat) : HandlerM ProofEntry := do
  match (← get).proofs[id]? with
  | some p => return p
  | none => throw (.unknownState id)

/-- Stores a proof state, failing when the store is full. -/
def putProof (p : ProofEntry) : HandlerM Nat := do
  let s ← get
  if s.proofs.size ≥ s.maxProofs then
    throw (.storeFull s.maxProofs)
  let id := s.nextId
  set { s with proofs := s.proofs.insert id p, nextId := id + 1 }
  return id

/-- Releases a proof state. Returns `false` when the handle was not in the store. States derived
from it stay valid: every state is complete in itself. -/
def releaseProof (id : Nat) : SessionM Bool := do
  let s ← get
  if s.proofs.contains id then
    set { s with proofs := s.proofs.erase id }
    return true
  else
    return false

/-- Releases a handle. Returns `false` when the handle was not in the store. Releasing a header
environment also forgets its import-list entry, so that the next request reloads it. -/
def releaseEnv (id : Nat) : SessionM Bool := do
  let s ← get
  match s.envs[id]? with
  | none => return false
  | some e =>
    let headers := if e.parent.isNone then s.headers.erase e.headerKey else s.headers
    set { s with envs := s.envs.erase id, headers }
    return true

private unsafe def enableInitializersUnsafe : BaseIO Unit := enableInitializersExecution

/-- Allows the `initialize` declarations of imported modules to run during the next import. Lean
switches this permission off again after every import, so it is requested before each one. -/
@[implemented_by enableInitializersUnsafe]
private opaque enableInitializers : BaseIO Unit

/-- Imports modules through the search path and returns the environment right after the header. -/
def importHeader (imports : Array String) (opts : Options) : HandlerM Environment := do
  let inputCtx := Parser.mkInputContext (headerText imports) virtualFile
  let (header, _, messages) ← Parser.parseHeader inputCtx
  if messages.hasErrors then
    let texts ← messages.toList.mapM (·.data.toString)
    throw (.invalidParams ("malformed import list: " ++ String.intercalate "; " texts))
  enableInitializers
  let result ← (processHeader header opts messages inputCtx).toBaseIO
  match result with
  | .error e => throw (.importFailed (toString e))
  | .ok (env, messages) =>
    if messages.hasErrors then
      let texts ← messages.toList.mapM (·.data.toString)
      throw (.importFailed (String.intercalate "; " texts))
    return env

/-- Returns the handle for an import list, loading it on first use. The Boolean is `true` when the
environment was already in the store. -/
def loadHeader (imports : Array String) : HandlerM (Nat × Bool) := do
  let imports := if imports.contains "Init" then imports else #["Init"] ++ imports
  let key := importsKey imports
  match (← get).headers[key]? with
  | some id =>
    modify fun s => { s with headerHits := s.headerHits + 1 }
    return (id, true)
  | none =>
    let env ← importHeader imports {}
    let entry : EnvEntry :=
      { state := Command.mkState env {} {}, base := env, parent := none, headerKey := key, depth := 0 }
    let id ← putEnv entry
    modify fun s => { s with headers := s.headers.insert key id, headerMisses := s.headerMisses + 1 }
    return (id, false)

/-- The header of a source text: its imports, and where the commands after it start. -/
structure SourceHeader where
  imports : Array String
  inputCtx : Parser.InputContext
  parserState : Parser.ModuleParserState

/-- Parses the header of a source text. Positions in later diagnostics refer to the whole text. -/
def sourceHeader (source : String) : HandlerM SourceHeader := do
  let inputCtx := Parser.mkInputContext source virtualFile
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  if messages.hasErrors then
    let texts ← messages.toList.mapM (·.data.toString)
    throw (.importFailed ("malformed header: " ++ String.intercalate "; " texts))
  let imports := (headerToImports header).map fun i => toString i.module
  return { imports, inputCtx, parserState }

end Maithic.Bridge
