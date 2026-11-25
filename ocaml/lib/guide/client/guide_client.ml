(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type transport = { exchange : Json.t -> (Json.t, string) result; close : unit -> unit }
type info = { dim : int; max_length : int; pooling : string; mixer : string; vocabulary : int; classes : int }
type t = { transport : transport; info : info; mutable sent : int; mutable closed : bool }

let protocol = "maithic.guide.service/1"
let ( let* ) = Result.bind

(* One request with a number of its own, and the answer that carries the same number. An answer
   to another request means the two sides have lost step, and nothing after it can be trusted. *)
let call transport ~id meth members =
  let request = Json.Obj ([ ("id", Json.Int id); ("method", Json.String meth) ] @ members) in
  let* answer = transport.exchange request in
  match Json.member "error" answer with
  | Some (Json.String why) -> Error (Printf.sprintf "the guidance service refused the request: %s" why)
  | Some _ -> Error "the guidance service refused the request"
  | None ->
    (match Json.member "id" answer with
     | Some (Json.Int n) when n = id -> Ok answer
     | _ -> Error "the guidance service answered another request than the one that was sent")

let info_of answer =
  match Json.member "model" answer with
  | None -> Error "the guidance service did not describe its model"
  | Some m ->
    let* dim = Json.get_int "dim" m in
    let* max_length = Json.get_int "max_length" m in
    let* pooling = Json.get_string "pooling" m in
    let* mixer = Json.get_string "mixer" m in
    let* vocabulary = Json.get_int "vocabulary" m in
    let* classes = Json.get_int "classes" m in
    Ok { dim; max_length; pooling; mixer; vocabulary; classes }

let connect transport =
  let greeted =
    let* answer = call transport ~id:1 "handshake" [] in
    match Json.member "service" answer with
    | Some (Json.String s) when s = protocol -> Result.map_error (fun e -> "the greeting of the guidance service is incomplete: " ^ e) (info_of answer)
    | Some (Json.String s) -> Error (Printf.sprintf "the guidance service speaks %s, and this client speaks %s" s protocol)
    | _ -> Error "what answered is not a guidance service: its greeting names no protocol"
  in
  match greeted with
  | Ok info -> Ok { transport; info; sent = 1; closed = false }
  | Error e ->
    transport.close ();
    Error e

let info t = t.info
let requests t = t.sent

let ask t meth members =
  if t.closed then Error "the guidance service was closed"
  else begin
    t.sent <- t.sent + 1;
    call t.transport ~id:t.sent meth members
  end

let numbers ~what ~count answer key =
  match Option.bind (Json.member key answer) Json.list_opt with
  | None -> Error (Printf.sprintf "the guidance service answered without %s" what)
  | Some items ->
    let values = List.filter_map Json.float_opt items in
    if List.length values <> List.length items || List.exists (fun x -> not (Float.is_finite x)) values
    then Error (Printf.sprintf "the %s of the guidance service are not numbers" what)
    else if List.length values <> count then
      Error (Printf.sprintf "the guidance service gave %d %s for %d candidates" (List.length values) what count)
    else Ok values

type ranking = { priors : float list; templates : string list }

let strings items = Json.List (List.map (fun s -> Json.String s) items)

let tactic t ~state candidates =
  if candidates = [] then Ok { priors = []; templates = [] }
  else
    let* answer = ask t "tactic" [ ("state", Json.String state); ("candidates", strings candidates) ] in
    let* priors = numbers ~what:"priors" ~count:(List.length candidates) answer "priors" in
    let total = List.fold_left ( +. ) 0.0 priors in
    if List.exists (fun p -> p < 0.0 || p > 1.0 +. 1e-9) priors then Error "the priors of the guidance service are not probabilities"
    else if Float.abs (total -. 1.0) > 1e-6 then Error (Printf.sprintf "the priors of the guidance service add up to %g, not to 1" total)
    else
      let templates = match Json.member "templates" answer with Some l -> Json.strings l | None -> [] in
      if List.length templates <> List.length candidates then Error "the guidance service did not say how it read each candidate"
      else Ok { priors; templates }

let value t ~state =
  let* answer = ask t "value" [ ("state", Json.String state) ] in
  match Option.bind (Json.member "value" answer) Json.float_opt with
  | Some v when v >= 0.0 && v <= 1.0 -> Ok v
  | Some v -> Error (Printf.sprintf "the guidance service gave the value %g, which is not between 0 and 1" v)
  | None -> Error "the guidance service answered without a value"

let premise t ~state candidates =
  if candidates = [] then Ok []
  else
    let* answer = ask t "premise" [ ("state", Json.String state); ("candidates", strings candidates) ] in
    let* scores = numbers ~what:"scores" ~count:(List.length candidates) answer "scores" in
    if List.exists (fun s -> s < 0.0 || s > 1.0 +. 1e-9) scores then Error "the scores of the guidance service are not between 0 and 1" else Ok scores

let close t =
  if not t.closed then begin
    t.closed <- true;
    t.sent <- t.sent + 1;
    ignore (t.transport.exchange (Json.Obj [ ("id", Json.Int t.sent); ("method", Json.String "shutdown") ]));
    t.transport.close ()
  end
