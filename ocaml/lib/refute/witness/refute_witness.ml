(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type assignment = (string * string) list

let closers = [ "decide"; "decide +kernel"; "omega"; "grind"; "simp" ]

let name_of theorem =
  let plain c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  "maithic_refutation_of_" ^ String.map (fun c -> if plain c then c else '_') theorem

let arguments assignment = String.concat "" (List.map (fun (name, value) -> Printf.sprintf " (%s := %s)" name value) assignment)
let values assignment = String.concat "\n" (List.map (fun (name, value) -> Printf.sprintf "%s := %s" name value) assignment)

let text ~theorem ~closer assignment =
  Printf.sprintf "theorem _root_.%s : \xC2\xAC type_of%% @%s := by\n  intro h\n  have h' := h%s\n  revert h'\n  %s" (name_of theorem) theorem
    (arguments assignment) closer

type checked = { accepted : bool; uses_sorry : bool; axioms : string list; error : string }
type confirmation = { closer : string; lean : string; axioms : string list }

let first_line text = match String.split_on_char '\n' (String.trim text) with line :: _ -> line | [] -> ""

let confirm ~check ~theorem assignment =
  let rec go last = function
    | [] -> Error last
    | closer :: rest ->
      let lean = text ~theorem ~closer assignment in
      let c = check ~untrusted:(values assignment) lean in
      if c.accepted && not c.uses_sorry then Ok { closer; lean; axioms = List.sort_uniq compare c.axioms }
      else
        let why =
          if c.accepted then Printf.sprintf "with %s the proof rests on sorry" closer
          else Printf.sprintf "%s: %s" closer (if c.error = "" then "not accepted" else first_line c.error)
        in
        go why rest
  in
  go "no tactic to try" closers

let to_json c =
  Json.Obj [ ("lean", Json.String c.lean); ("closedBy", Json.String c.closer); ("axioms", Json.List (List.map (fun a -> Json.String a) c.axioms)) ]
