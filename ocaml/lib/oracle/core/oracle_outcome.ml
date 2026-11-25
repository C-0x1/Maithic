(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type lean = { check : string option; replay : string option; scripts : string list }

type t =
  | Proved of { kind : string; claim : string; certificate : Json.t; lean : lean option }
  | Refuted of (string * string) list
  | Unknown of string

let schema = "maithic.oracle.outcome/1"
let label = function Proved _ -> "proved" | Refuted _ -> "refuted" | Unknown _ -> "unknown"
let text_member name j = match Json.member name j with Some (Json.String s) -> Some s | _ -> None

let of_json j =
  if Json.string_or "" "schema" j <> schema then Error ("the answer does not name the schema " ^ schema)
  else
    match Json.string_or "" "outcome" j with
    | "proved" ->
      (match Json.member "certificate" j with
       | Some (Json.Obj _ as certificate) ->
         let lean =
           Option.map
             (fun l ->
               { check = text_member "check" l; replay = text_member "replay" l;
                 scripts = List.filter_map (function Json.String s -> Some s | _ -> None) (Json.list_or_empty "scripts" l) })
             (Json.member "lean" j)
         in
         Ok (Proved { kind = Json.string_or "" "kind" certificate; claim = Json.string_or "" "claim" certificate; certificate; lean })
       | _ -> Error "a proved outcome has no certificate")
    | "refuted" ->
      (match Json.member "model" j with
       | Some (Json.Obj members) ->
         Ok (Refuted (List.filter_map (fun (name, value) -> match value with Json.String v -> Some (name, v) | _ -> None) members))
       | _ -> Error "a refuted outcome has no model")
    | "unknown" -> Ok (Unknown (Json.string_or "no reason was given" "reason" j))
    | other -> Error (Printf.sprintf "\"%s\" is not an outcome" other)

let certificate_id certificate =
  let part name = Option.value ~default:Json.Null (Json.member name certificate) in
  Sha256.hex (Json.to_string ~canonical:true (Json.Obj [ ("kind", part "kind"); ("claim", part "claim"); ("data", part "data") ]))

type result = { oracle : string; goal : Oracle_goal.t; outcome : t; seconds : float }

let result_schema = "maithic.oracle.result/1"

let result_to_json r =
  let outcome =
    match r.outcome with
    | Proved { kind; claim; certificate; lean } ->
      [ ("kind", Json.String kind); ("claim", Json.String claim); ("certificate_id", Json.String (certificate_id certificate));
        ("certificate", certificate) ]
      @ (match lean with
         | Some l -> [ ("scripts", Json.List (List.map (fun s -> Json.String s) l.scripts)) ]
         | None -> [])
    | Refuted model -> [ ("model", Json.Obj (List.map (fun (name, value) -> (name, Json.String value)) model)) ]
    | Unknown reason -> [ ("reason", Json.String reason) ]
  in
  Json.Obj
    ([ ("schema", Json.String result_schema); ("oracle", Json.String r.oracle); ("goal", Oracle_goal.to_json r.goal);
       ("outcome", Json.String (label r.outcome)) ]
     @ outcome
     @ [ ("elapsed_ms", Json.Int (int_of_float (r.seconds *. 1000.0))) ])
