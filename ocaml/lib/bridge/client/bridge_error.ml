(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Everything that can go wrong when talking to the Bridge. *)

open Maithic_schema

type t =
  | Not_found of string  (** no Bridge executable could be located *)
  | Process of string  (** the Bridge could not be started, or it died *)
  | Protocol of string  (** a response that does not follow the protocol *)
  | Remote of { code : int; tag : string; message : string }  (** a typed error from the server *)
  | Timeout of float  (** no response within the deadline, in seconds *)
  | Incompatible of string  (** the server speaks another protocol version *)

let to_string = function
  | Not_found detail -> "the Bridge executable was not found: " ^ detail
  | Process detail -> "Bridge process error: " ^ detail
  | Protocol detail -> "Bridge protocol error: " ^ detail
  | Remote { tag; message; _ } -> Printf.sprintf "%s: %s" tag message
  | Timeout seconds -> Printf.sprintf "the Bridge did not answer within %.1f seconds" seconds
  | Incompatible detail -> "incompatible Bridge: " ^ detail

(** The user-facing form, with a stable code and a remedy. *)
let to_fault = function
  | Not_found detail ->
    Fault.make ~area:"BRIDGE" ~number:1 ~exit:Exit_code.Toolchain
      ~hint:"Build it with `make build-lean`, or set MAITHIC_BRIDGE to the path of maithic-bridge." ("the Bridge executable was not found: " ^ detail)
  | Process detail ->
    Fault.make ~area:"BRIDGE" ~number:2 ~exit:Exit_code.Toolchain
      ~hint:"Run `maithic doctor` to check the Lean toolchain." ("the Bridge process failed: " ^ detail)
  | Protocol detail -> Fault.make ~area:"BRIDGE" ~number:3 ~exit:Exit_code.Failure ("the Bridge sent an unexpected response: " ^ detail)
  | Remote { tag; message; _ } -> Fault.make ~area:"BRIDGE" ~number:4 ~exit:Exit_code.Failure (Printf.sprintf "%s: %s" tag message)
  | Timeout seconds ->
    Fault.make ~area:"BRIDGE" ~number:5 ~exit:Exit_code.Timeout
      ~hint:"Raise [bridge] timeout_s in maithic.toml, or lower the heartbeat budget so Lean gives up earlier."
      (Printf.sprintf "the Bridge did not answer within %.1f seconds" seconds)
  | Incompatible detail ->
    Fault.make ~area:"BRIDGE" ~number:6 ~exit:Exit_code.Toolchain
      ~hint:"Rebuild the Bridge from the same release as the maithic command." ("incompatible Bridge: " ^ detail)
