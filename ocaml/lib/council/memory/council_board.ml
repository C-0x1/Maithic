(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type kind = Definition | Statement | Claim | Proof | Counterexample | Note | Question

let kinds = [ Definition; Statement; Claim; Proof; Counterexample; Note; Question ]

let kind_name = function
  | Definition -> "definition"
  | Statement -> "statement"
  | Claim -> "claim"
  | Proof -> "proof"
  | Counterexample -> "counterexample"
  | Note -> "note"
  | Question -> "question"

let kind_of_string name = List.find_opt (fun k -> kind_name k = name) kinds

type status = Conjectured | Formalized | Refuted | Verified

let status_name = function Conjectured -> "conjectured" | Formalized -> "formalized" | Refuted -> "refuted" | Verified -> "verified"

type examined = Unexamined | Accepted | Rejected of string
type item = { id : int; kind : kind; author : string; round : int; at : float; title : string; body : string; refers : int list; checked : bool }

type t = {
  clock : unit -> float;
  started : float;
  mutable round : int;
  mutable posted : item list;  (* the last item first *)
  statuses : (int, status) Hashtbl.t;
  grounds : (int, Council_evidence.t list) Hashtbl.t;
  examinations : (int, examined) Hashtbl.t;
  proofs : (int, int) Hashtbl.t;  (* for a verified claim, the proof it was verified by *)
}

let create ?(clock = Unix.gettimeofday) () =
  { clock; started = clock (); round = 0; posted = []; statuses = Hashtbl.create 8; grounds = Hashtbl.create 8; examinations = Hashtbl.create 8;
    proofs = Hashtbl.create 8 }

let set_round t round = t.round <- round
let items t = List.rev t.posted
let find t id = List.find_opt (fun item -> item.id = id) t.posted

let post t ~author kind ~title ?(refers = []) ?(checked = false) body =
  (match List.find_opt (fun id -> find t id = None) refers with
   | Some missing -> invalid_arg (Printf.sprintf "Council_board.post: item %d is not on the board" missing)
   | None -> ());
  let item = { id = List.length t.posted + 1; kind; author; round = t.round; at = Float.max 0.0 (t.clock () -. t.started); title; body; refers; checked } in
  t.posted <- item :: t.posted;
  if kind = Claim then Hashtbl.replace t.statuses item.id Conjectured;
  item

let of_kind t kind = List.filter (fun item -> item.kind = kind) (items t)
let by t author = List.filter (fun item -> item.author = author) (items t)
let about t id = List.filter (fun item -> List.mem id item.refers) (items t)
let status t id = Hashtbl.find_opt t.statuses id
let evidence t id = Option.value (Hashtbl.find_opt t.grounds id) ~default:[]
let examined t id = Option.value (Hashtbl.find_opt t.examinations id) ~default:Unexamined
let accepted_proof t claim = Option.bind (Hashtbl.find_opt t.proofs claim) (find t)

module Verifier = struct
  let claim t id =
    match (find t id, status t id) with
    | Some { kind = Claim; _ }, Some status -> Ok status
    | _ -> Error (Printf.sprintf "item %d is not a claim" id)

  let ground t id evidence = Hashtbl.replace t.grounds id (Option.value (Hashtbl.find_opt t.grounds id) ~default:[] @ [ evidence ])
  let ( let* ) = Result.bind

  let formalized t ~claim:id evidence =
    let* status = claim t id in
    if Council_evidence.kind evidence <> Council_evidence.Elaborated then Error "a claim is formalized on evidence that its statement elaborates, and on no other"
    else if status <> Conjectured then Error (Printf.sprintf "claim %d is already %s" id (status_name status))
    else begin
      Hashtbl.replace t.statuses id Formalized;
      ground t id evidence;
      Ok ()
    end

  let formal_statement t id = List.find_map (fun e -> if Council_evidence.kind e = Council_evidence.Elaborated then Some (Council_evidence.statement e) else None) (evidence t id)

  let verified t ~claim:id ~proof evidence =
    let* status = claim t id in
    match (find t proof, formal_statement t id) with
    | _ when Council_evidence.kind evidence <> Council_evidence.Proved -> Error "a claim is verified on evidence of a proof, and on no other"
    | (None | Some { kind = Definition | Statement | Claim | Counterexample | Note | Question; _ }), _ -> Error (Printf.sprintf "item %d is not a proof" proof)
    | Some p, _ when not (List.mem id p.refers) -> Error (Printf.sprintf "proof %d is not a proof of claim %d" proof id)
    | _, None -> Error (Printf.sprintf "claim %d was never formalized: there is no statement for a proof to be a proof of" id)
    | Some _, Some statement ->
      if status = Refuted then Error (Printf.sprintf "claim %d is refuted" id)
      else if Council_evidence.statement evidence <> statement then
        Error (Printf.sprintf "the proof is of another statement: %s" (Council_evidence.statement evidence))
      else begin
        Hashtbl.replace t.statuses id Verified;
        Hashtbl.replace t.examinations proof Accepted;
        if not (Hashtbl.mem t.proofs id) then Hashtbl.replace t.proofs id proof;
        ground t id evidence;
        Ok ()
      end

  let refuted t ~claim:id ~counterexample evidence =
    let* status = claim t id in
    match find t counterexample with
    | _ when Council_evidence.kind evidence <> Council_evidence.Disproved -> Error "a claim is refuted on evidence of a refutation, and on no other"
    | Some { kind = Counterexample; refers; _ } when List.mem id refers ->
      if status = Verified then Error (Printf.sprintf "claim %d is verified" id)
      else begin
        Hashtbl.replace t.statuses id Refuted;
        ground t id evidence;
        Ok ()
      end
    | _ -> Error (Printf.sprintf "item %d is not a counterexample to claim %d" counterexample id)

  let rejected t ~proof said =
    match find t proof with
    | Some { kind = Proof; _ } ->
      if examined t proof = Accepted then Error (Printf.sprintf "proof %d was accepted" proof)
      else begin
        Hashtbl.replace t.examinations proof (Rejected said);
        Ok ()
      end
    | _ -> Error (Printf.sprintf "item %d is not a proof" proof)
end

let item_to_json t item =
  Json.Obj
    ([ ("id", Json.Int item.id); ("kind", Json.String (kind_name item.kind)); ("author", Json.String item.author); ("round", Json.Int item.round);
       ("at", Json.Float (Float.round (item.at *. 1000.0) /. 1000.0)); ("title", Json.String item.title); ("body", Json.String item.body) ]
     @ (if item.refers = [] then [] else [ ("refers", Json.List (List.map (fun i -> Json.Int i) item.refers)) ])
     @ (if item.checked then [ ("checked", Json.Bool true) ] else [])
     @ (match status t item.id with
        | Some status -> [ ("status", Json.String (status_name status)); ("evidence", Json.List (List.map Council_evidence.to_json (evidence t item.id))) ]
        | None -> [])
     @
     if item.kind <> Proof then []
     else
       match examined t item.id with
       | Unexamined -> [ ("examined", Json.String "unexamined") ]
       | Accepted -> [ ("examined", Json.String "accepted") ]
       | Rejected said -> [ ("examined", Json.String "rejected"); ("lean", Json.String said) ])

let to_json t = Json.List (List.map (item_to_json t) (items t))
