(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Errors with stable codes.

    Every error a user can see carries a code of the form [MTH-AREA-NNNN], a
    message that says what happened, an optional hint that says what to do
    next, and the exit code a command ends with. The codes are listed in
    [docs/reference/errors.md]. *)

open Maithic_base

type t = { code : string; message : string; hint : string option; exit : Exit_code.t }

let make ?hint ~area ~number ~exit message =
  { code = Printf.sprintf "MTH-%s-%04d" (String.uppercase_ascii area) number; message; hint; exit }

let to_string f =
  match f.hint with
  | Some hint -> Printf.sprintf "%s: %s\n  hint: %s" f.code f.message hint
  | None -> Printf.sprintf "%s: %s" f.code f.message

let to_json f =
  Json.Obj
    ([ ("code", Json.String f.code); ("message", Json.String f.message) ]
     @ (match f.hint with Some h -> [ ("hint", Json.String h) ] | None -> [])
     @ [ ("exit", Json.Int (Exit_code.to_int f.exit)) ])

(** [true] when [code] has the shape of a Maithic error code. *)
let is_code code =
  match String.split_on_char '-' code with
  | [ "MTH"; area; number ] ->
    area <> ""
    && String.for_all (function 'A' .. 'Z' -> true | _ -> false) area
    && String.length number = 4
    && String.for_all (function '0' .. '9' -> true | _ -> false) number
  | _ -> false
