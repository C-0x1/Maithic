/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import MaithicBridge.Serialize.Types

/-!
# JSON-RPC 2.0 envelopes

The Bridge speaks JSON-RPC 2.0 with one message per line. A request is
`{"jsonrpc":"2.0","id":7,"method":"runCommand","params":{...}}`; the response carries the same
`id` and either a `result` or an `error` object. Errors are typed: besides the numeric JSON-RPC
code and a message they carry a stable Maithic error code in `error.data.code`.
-/

namespace Maithic.Bridge

open Lean

/-- Everything that can go wrong while serving a request. -/
inductive BridgeError where
  | parseError (detail : String)
  | invalidRequest (detail : String)
  | methodNotFound (method : String)
  | invalidParams (detail : String)
  | internal (detail : String)
  | importFailed (detail : String)
  | unknownEnv (env : Nat)
  | storeFull (limit : Nat)
  | unknownState (state : Nat)
  deriving Repr, BEq, Inhabited

namespace BridgeError

/-- The JSON-RPC error code. -/
def rpcCode : BridgeError → Int
  | parseError _ => -32700
  | invalidRequest _ => -32600
  | methodNotFound _ => -32601
  | invalidParams _ => -32602
  | internal _ => -32603
  | importFailed _ => -32000
  | unknownEnv _ => -32001
  | storeFull _ => -32002
  | unknownState _ => -32003

/-- The stable Maithic error code, in the `MTH-BRG` area. -/
def tag : BridgeError → String
  | parseError _ => "MTH-BRG-0001"
  | invalidRequest _ => "MTH-BRG-0002"
  | methodNotFound _ => "MTH-BRG-0003"
  | invalidParams _ => "MTH-BRG-0004"
  | internal _ => "MTH-BRG-0005"
  | importFailed _ => "MTH-BRG-0101"
  | unknownEnv _ => "MTH-BRG-0102"
  | storeFull _ => "MTH-BRG-0103"
  | unknownState _ => "MTH-BRG-0104"

/-- A message for people. -/
def message : BridgeError → String
  | parseError d => s!"request is not valid JSON: {d}"
  | invalidRequest d => s!"invalid request: {d}"
  | methodNotFound m => s!"unknown method '{m}'"
  | invalidParams d => s!"invalid parameters: {d}"
  | internal d => s!"internal error: {d}"
  | importFailed d => s!"imports could not be loaded: {d}"
  | unknownEnv e => s!"unknown environment {e}"
  | storeFull n => s!"the store is full ({n} entries); release some first"
  | unknownState p => s!"unknown proof state {p}"

/-- The `error` member of a response. -/
def toJson (e : BridgeError) : Json :=
  Json.mkObj [
    ("code", Json.num (JsonNumber.fromInt e.rpcCode)),
    ("message", Json.str e.message),
    ("data", Json.mkObj [("code", Json.str e.tag)])]

end BridgeError

/-- A successful response. -/
def okResponse (id : Json) (result : Json) : Json :=
  Json.mkObj [("jsonrpc", Json.str "2.0"), ("id", id), ("result", result)]

/-- A failed response. -/
def errorResponse (id : Json) (e : BridgeError) : Json :=
  Json.mkObj [("jsonrpc", Json.str "2.0"), ("id", id), ("error", e.toJson)]

/-- A parsed request. -/
structure Request where
  id : Json
  method : String
  params : Json

/-- Validates the envelope of a request. -/
def Request.ofJson (j : Json) : Except BridgeError Request := do
  let id := (j.getObjVal? "id").toOption.getD Json.null
  match j.getObjValAs? String "jsonrpc" with
  | .ok "2.0" => pure ()
  | _ => throw (.invalidRequest "member 'jsonrpc' must be \"2.0\"")
  match id with
  | .num _ | .str _ => pure ()
  | _ => throw (.invalidRequest "member 'id' must be a number or a string")
  let method ← match j.getObjValAs? String "method" with
    | .ok m => pure m
    | .error _ => throw (.invalidRequest "member 'method' must be a string")
  let params := (j.getObjVal? "params").toOption.getD (Json.mkObj [])
  match params with
  | .obj _ => pure { id, method, params }
  | _ => throw (.invalidRequest "member 'params' must be an object")

/-- Reads a required parameter. -/
def param [FromJson α] (params : Json) (key : String) : Except BridgeError α :=
  match params.getObjValAs? α key with
  | .ok v => .ok v
  | .error _ => .error (.invalidParams s!"missing or malformed parameter '{key}'")

end Maithic.Bridge
