(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_refute

type diagnostic = { severity : string; line : int; text : string }

type raw = {
  compile : untrusted:string -> string -> (diagnostic list, string) result;
  statement : unit -> Council_evidence.lean;
  proof : string -> Council_evidence.lean;
  lookup : string list -> (string * string) list;
  prove : (unit -> (string * string, string) result * int) option;
  refute : (unit -> ((string * string) list * Refute_witness.confirmation) option) option;
}

type t = { raw : raw; budget : Council_budget.t; counts : (string, int) Hashtbl.t }

let names = [ "lean_check"; "lookup"; "elaborate"; "check_proof"; "prove"; "refute" ]
let make ?(budget = Council_budget.create Council_budget.unlimited) raw = { raw; budget; counts = Hashtbl.create 8 }
let budget t = t.budget
let uses t = List.map (fun name -> (name, Option.value (Hashtbl.find_opt t.counts name) ~default:0)) names
let can_prove t = t.raw.prove <> None
let can_refute t = t.raw.refute <> None

(* One use of a tool: refused when the budget is used up, counted and charged otherwise. *)
let using t name ?(calls = 1) () =
  match Council_budget.blocked t.budget Council_budget.Lean with
  | Some why -> Error ("the budget of the session is used up: " ^ why)
  | None ->
    Hashtbl.replace t.counts name (Option.value (Hashtbl.find_opt t.counts name) ~default:0 + 1);
    Council_budget.charge_lean ~calls t.budget;
    Ok ()

let lean_check t ?(untrusted = "") text = Result.bind (using t "lean_check" ()) (fun () -> t.raw.compile ~untrusted text)
let errors diagnostics = List.filter_map (fun d -> if d.severity = "error" then Some (String.trim d.text) else None) diagnostics
let lookup t names = if names = [] then [] else match using t "lookup" () with Ok () -> t.raw.lookup names | Error _ -> []
let elaborate t = Result.bind (using t "elaborate" ()) (fun () -> Council_evidence.elaborated (t.raw.statement ()))

type refusal = Rejected of string | Unavailable of string

let check_proof t ~policy ~statement proof =
  match using t "check_proof" () with
  | Error why -> Error (Unavailable why)
  | Ok () -> Result.map_error (fun why -> Rejected why) (Council_evidence.proved ~policy ~statement (t.raw.proof proof))

let prove t =
  match t.raw.prove with
  | None -> Error "the session has no prover"
  | Some attempt ->
    Result.bind (using t "prove" ~calls:0 ()) (fun () ->
        let outcome, tried = attempt () in
        Council_budget.charge_lean ~calls:(max 1 tried) t.budget;
        outcome)

let refute t ~statement =
  match t.raw.refute with
  | None -> None
  | Some search ->
    (match using t "refute" () with
     | Error _ -> None
     | Ok () -> Option.map (fun (point, confirmation) -> (point, Council_evidence.disproved ~statement confirmation)) (search ()))
