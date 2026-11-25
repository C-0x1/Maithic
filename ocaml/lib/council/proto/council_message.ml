(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type performative = Request | Inform | Propose | Critique | Verdict | Decline

let performatives = [ Request; Inform; Propose; Critique; Verdict; Decline ]

let performative_name = function
  | Request -> "request"
  | Inform -> "inform"
  | Propose -> "propose"
  | Critique -> "critique"
  | Verdict -> "verdict"
  | Decline -> "decline"

let performative_of_string name = List.find_opt (fun p -> performative_name p = name) performatives

type t = {
  seq : int;
  at : float;
  round : int;
  sender : string;
  recipients : string list;
  performative : performative;
  subject : string;
  content : string;
  refers : int list;
}

let check m =
  if m.seq < 1 then Error "a message needs a place of one or more"
  else if String.trim m.sender = "" then Error "a message needs a sender"
  else if m.recipients = [] || List.exists (fun r -> String.trim r = "") m.recipients then Error "a message needs at least one recipient, and each needs a name"
  else if String.trim m.subject = "" then Error "a message needs a subject"
  else Ok ()

let to_json m =
  Json.Obj
    ([ ("seq", Json.Int m.seq); ("at", Json.Float (Float.round (m.at *. 1000.0) /. 1000.0)); ("round", Json.Int m.round); ("sender", Json.String m.sender);
       ("recipients", Json.List (List.map (fun r -> Json.String r) m.recipients)); ("performative", Json.String (performative_name m.performative));
       ("subject", Json.String m.subject); ("content", Json.String m.content) ]
     @ if m.refers = [] then [] else [ ("refers", Json.List (List.map (fun i -> Json.Int i) m.refers)) ])

let ( let* ) = Result.bind

let of_json j =
  let* seq = Json.get_int "seq" j in
  let* sender = Json.get_string "sender" j in
  let* subject = Json.get_string "subject" j in
  let* performative =
    match performative_of_string (Json.string_or "" "performative" j) with
    | Some p -> Ok p
    | None -> Error (Printf.sprintf "a message with the unknown performative '%s'" (Json.string_or "" "performative" j))
  in
  let at = match Json.member "at" j with Some (Json.Float f) -> f | Some (Json.Int i) -> float_of_int i | _ -> 0.0 in
  let message =
    { seq; at; round = Json.int_or 0 "round" j; sender; recipients = Json.strings (Json.List (Json.list_or_empty "recipients" j)); performative; subject;
      content = Json.string_or "" "content" j; refers = List.filter_map Json.int_opt (Json.list_or_empty "refers" j) }
  in
  let* () = check message in
  Ok message
