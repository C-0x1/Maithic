(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_ledger
open Maithic_refute

type lean = { accepted : bool; statement : string; axioms : string list; uses_sorry : bool; rechecked : bool; error : string }

let rejected error = { accepted = false; statement = ""; axioms = []; uses_sorry = false; rechecked = false; error }

type kind = Elaborated | Proved | Disproved
type t = { kind : kind; statement : string; axioms : string list; policy : string; detail : string }

let kind e = e.kind
let kind_name = function Elaborated -> "elaborated" | Proved -> "proved" | Disproved -> "disproved"
let statement e = e.statement
let axioms e = e.axioms
let policy e = e.policy

let elaborated (lean : lean) =
  if not lean.accepted then Error (match String.trim lean.error with "" -> "Lean did not accept the statement" | said -> said)
  else if lean.statement = "" then Error "Lean accepted the text and produced no statement"
  else Ok { kind = Elaborated; statement = lean.statement; axioms = []; policy = ""; detail = "" }

let proved ~(policy : Trust_policy.t) ~statement (lean : lean) =
  if not lean.accepted then Error (match String.trim lean.error with "" -> "Lean did not accept the proof" | said -> said)
  else if lean.uses_sorry then Error "the proof rests on sorry"
  else if lean.statement <> statement then Error (Printf.sprintf "the theorem Lean accepted states something else: %s" lean.statement)
  else
    let decision =
      Trust_policy.decide policy
        { Trust_policy.declaration = ""; found = true; kind = "theorem"; axioms = lean.axioms; errors = 0; rechecked = lean.rechecked }
    in
    match decision.Trust_policy.status with
    | Status.Verified -> Ok { kind = Proved; statement; axioms = List.sort_uniq compare lean.axioms; policy = policy.Trust_policy.name; detail = "" }
    | _ ->
      Error
        (match decision.Trust_policy.violations, decision.Trust_policy.reasons with
         | (_ :: _ as axioms), _ -> Printf.sprintf "the proof rests on what the trust policy does not allow: %s" (String.concat ", " axioms)
         | [], reason :: _ -> "the trust policy is not met: " ^ reason
         | [], [] -> "the trust policy is not met")

let disproved ~statement (confirmation : Refute_witness.confirmation) =
  { kind = Disproved; statement; axioms = List.sort_uniq compare confirmation.Refute_witness.axioms; policy = ""; detail = confirmation.Refute_witness.lean }

let to_json e =
  Json.Obj
    ([ ("kind", Json.String (kind_name e.kind)); ("statement", Json.String e.statement) ]
     @ (if e.kind = Elaborated then [] else [ ("axioms", Json.List (List.map (fun a -> Json.String a) e.axioms)) ])
     @ (if e.policy = "" then [] else [ ("policy", Json.String e.policy) ])
     @ if e.detail = "" then [] else [ ("lean", Json.String e.detail) ])
