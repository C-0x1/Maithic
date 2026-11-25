(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type status = Up | Down of string
type counts = { asked : int; answered : int }
type t = { mutable client : Guide_client.t option; mutable down : string option; mutable asked : int; mutable answered : int }

let of_client client = { client = Some client; down = None; asked = 0; answered = 0 }
let unavailable reason = { client = None; down = Some reason; asked = 0; answered = 0 }
let start connect = match connect () with Ok client -> of_client client | Error reason -> unavailable reason
let uniform n = if n <= 0 then [] else List.init n (fun _ -> 1.0 /. float_of_int n)
let status t = match t.down with Some reason -> Down reason | None -> Up
let counts t = { asked = t.asked; answered = t.answered }
let info t = Option.map Guide_client.info t.client

(* The first failure takes the guide down for the rest of the run. *)
let fail t reason =
  Option.iter Guide_client.close t.client;
  t.client <- None;
  t.down <- Some reason

let ask t ~fallback question =
  t.asked <- t.asked + 1;
  match t.client with
  | None -> fallback
  | Some client ->
    (match question client with
     | Ok answer ->
       t.answered <- t.answered + 1;
       answer
     | Error reason ->
       fail t reason;
       fallback)

let priors t ~state candidates =
  if candidates = [] then []
  else ask t ~fallback:(uniform (List.length candidates)) (fun c -> Result.map (fun r -> r.Guide_client.priors) (Guide_client.tactic c ~state candidates))

let value t ~state = ask t ~fallback:0.0 (fun c -> Guide_client.value c ~state)

let premise_scores t ~state candidates =
  if candidates = [] then [] else ask t ~fallback:(List.map (fun _ -> 0.0) candidates) (fun c -> Guide_client.premise c ~state candidates)

let close t =
  Option.iter Guide_client.close t.client;
  t.client <- None

let to_json ?model t =
  Json.Obj
    ((match model with Some m -> [ ("model", Json.String m) ] | None -> [])
     @ [ ("status", Json.String (match t.down with None -> "up" | Some _ -> "down")) ]
     @ (match t.down with Some reason -> [ ("reason", Json.String reason) ] | None -> [])
     @ [ ("asked", Json.Int t.asked); ("answered", Json.Int t.answered) ])

let describe ?model t =
  let named = match model with Some m -> " " ^ m | None -> "" in
  match t.down with
  | None -> Printf.sprintf "the guide%s answered %d of %d question(s)" named t.answered t.asked
  | Some reason ->
    Printf.sprintf "the guide%s was not used for %d of %d question(s), which were answered with equal priors: %s" named (t.asked - t.answered)
      t.asked reason
