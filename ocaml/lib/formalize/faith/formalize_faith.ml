(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type inspection = { unused : string list; traps : (string * string * string) list; annotated : string }

type refutation = Refuted of (string * string) list | Survived of string

type lean = {
  inspect : Formalize_candidate.t -> (inspection, string) result;
  vacuous : Formalize_candidate.t -> (string option, string) result;
  refute : Formalize_candidate.t -> (refutation, string) result;
  closes : Formalize_candidate.t -> string list -> string option;
}

type model = {
  back : statement:string -> (string, string) result;
  judge : problem:string -> english:string -> (int * string list, string) result;
}

type check = { name : string; outcome : string; note : string }
type t = { checks : check list; english : string; agreement : int option; differences : string list; rejected : string option; score : float }

let check_names = [ "vacuity"; "counterexample"; "triviality"; "numbers"; "variables"; "traps"; "back-translation"; "round-trip" ]
let trivial_proofs = [ "by trivial"; "by rfl"; "by simp" ]
let agreement_threshold = 8

(* Whether a numeral stands in a text as a whole number: not as part of a longer one, or of a name. *)
let mentions text number =
  let n = String.length text and len = String.length number in
  let digit c = c >= '0' && c <= '9' in
  let word c = digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let rec go from =
    match Text.find_sub ~from text number with
    | None -> false
    | Some at -> ((at = 0 || not (word text.[at - 1])) && (at + len >= n || not (digit text.[at + len]))) || go (at + 1)
  in
  len > 0 && go 0

let missing_numbers intake signature = List.filter (fun number -> not (mentions signature number)) (Formalize_intake.numbers intake)

let score ~agreement checks =
  let outcome name = match List.find_opt (fun c -> c.name = name) checks with Some c -> c.outcome | None -> "skipped" in
  if outcome "vacuity" = "failed" || outcome "counterexample" = "failed" then 0.0
  else
    (* What the round trip says, or the middle when nobody was asked; less for each thing that was flagged. *)
    let base = match agreement with Some points -> float_of_int points /. 10.0 | None -> 0.6 in
    let penalty name amount = if outcome name = "flagged" then amount else 0.0 in
    Float.max 0.05 (base -. penalty "triviality" 0.3 -. penalty "numbers" 0.2 -. penalty "variables" 0.2 -. penalty "traps" 0.05)

let flagged t = List.filter_map (fun c -> if c.outcome = "flagged" then Some c.name else None) t.checks

let validate ?model ~lean (intake : Formalize_intake.t) (candidate : Formalize_candidate.t) ~statement =
  let check name outcome note = { name; outcome; note } in
  let skipped name why = check name "skipped" why in
  (* The checks that rest on proofs. *)
  let vacuity =
    match lean.vacuous candidate with
    | Ok (Some why) -> check "vacuity" "failed" why
    | Ok None -> check "vacuity" "passed" ""
    | Error why -> skipped "vacuity" why
  in
  let counterexample =
    if vacuity.outcome = "failed" then skipped "counterexample" "the statement is vacuous"
    else
      match lean.refute candidate with
      | Ok (Refuted point) ->
        check "counterexample" "failed"
          (if point = [] then "the statement is false as it stands" else "it fails at " ^ String.concat ", " (List.map (fun (name, value) -> name ^ " = " ^ value) point))
      | Ok (Survived tried) -> check "counterexample" "passed" (if tried = "" then "" else "no counterexample in " ^ tried)
      | Error why -> skipped "counterexample" why
  in
  let rejected =
    if vacuity.outcome = "failed" then Some ("vacuous: " ^ vacuity.note)
    else if counterexample.outcome = "failed" then Some ("refuted: " ^ counterexample.note)
    else None
  in
  match rejected with
  | Some _ ->
    let checks = [ vacuity; counterexample ] @ List.map (fun name -> skipped name "the statement was rejected") [ "triviality"; "numbers"; "variables"; "traps"; "back-translation"; "round-trip" ] in
    { checks; english = ""; agreement = None; differences = []; rejected; score = 0.0 }
  | None ->
    (* The checks that are evidence. *)
    let triviality =
      match lean.closes candidate trivial_proofs with
      | Some proof -> check "triviality" "flagged" (Printf.sprintf "Lean accepts `%s` for a proof" proof)
      | None -> check "triviality" "passed" ""
    in
    let numbers =
      match missing_numbers intake candidate.Formalize_candidate.signature with
      | [] -> check "numbers" "passed" ""
      | missing -> check "numbers" "flagged" (Printf.sprintf "the problem mentions %s, and the statement does not" (String.concat ", " missing))
    in
    let variables, traps =
      match lean.inspect candidate with
      | Error why -> (skipped "variables" why, skipped "traps" why)
      | Ok inspection ->
        ( (match inspection.unused with
           | [] -> check "variables" "passed" ""
           | unused -> check "variables" "flagged" (Printf.sprintf "nothing in the statement mentions %s" (String.concat ", " unused))),
          match inspection.traps with
          | [] -> check "traps" "passed" ""
          | found -> check "traps" "flagged" (String.concat "; " (List.map (fun (_, term, note) -> Printf.sprintf "%s: %s" term note) found)) )
    in
    let back, round_trip, english, agreement, differences =
      match model with
      | None -> (skipped "back-translation" "no model was asked", skipped "round-trip" "no model was asked", "", None, [])
      | Some model ->
        (match model.back ~statement with
         | Error why -> (skipped "back-translation" why, skipped "round-trip" "there is no back-translation to compare", "", None, [])
         | Ok english when Text.is_blank english ->
           (skipped "back-translation" "the model returned nothing", skipped "round-trip" "there is no back-translation to compare", "", None, [])
         | Ok english ->
           let back = check "back-translation" "passed" "" in
           (match model.judge ~problem:intake.Formalize_intake.text ~english with
            | Error why -> (back, skipped "round-trip" why, english, None, [])
            | Ok (points, differences) ->
              let points = max 0 (min 10 points) in
              let outcome = if points >= agreement_threshold then "passed" else "flagged" in
              (back, check "round-trip" outcome (Printf.sprintf "agreement %d of 10%s" points (if differences = [] then "" else ": " ^ String.concat "; " differences)), english,
               Some points, differences)))
    in
    let checks = [ vacuity; counterexample; triviality; numbers; variables; traps; back; round_trip ] in
    { checks; english; agreement; differences; rejected = None; score = score ~agreement checks }

let to_json t =
  Json.Obj
    ([ ( "checks",
         Json.List
           (List.map
              (fun c -> Json.Obj ([ ("name", Json.String c.name); ("outcome", Json.String c.outcome) ] @ if c.note = "" then [] else [ ("note", Json.String c.note) ]))
              t.checks) ) ]
     @ (if t.english = "" then [] else [ ("english", Json.String t.english) ])
     @ (match t.agreement with Some points -> [ ("agreement", Json.Int points) ] | None -> [])
     @ (if t.differences = [] then [] else [ ("differences", Json.List (List.map (fun d -> Json.String d) t.differences)) ])
     @ [ ("score", Json.Float t.score) ])
