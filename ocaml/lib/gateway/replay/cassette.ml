(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Gateway_types

let schema_tag = "maithic.gateway.cassette/1"

let canonical ~provider ~model (req : request) =
  Json.Obj
    ([ ("provider", Json.String provider); ("model", Json.String model); ("purpose", Json.String req.purpose);
       ("system", Json.String req.system);
       ("messages", Json.List (List.map message_to_json req.messages));
       ("max_tokens", Json.Int req.max_tokens) ]
     @ (match req.temperature with Some t -> [ ("temperature", Json.Float t) ] | None -> [])
     @ (match req.schema with
        | Some (name, schema) -> [ ("schema_name", Json.String name); ("schema", schema) ]
        | None -> []))

let hash_of_canonical j = Sha256.hex (Json.to_string ~canonical:true j)
let hash ~provider ~model req = hash_of_canonical (canonical ~provider ~model req)

type origin = Recorded | Authored

type t = { hash : string; origin : origin; request : Json.t; response : response; note : string }

let make ?(note = "") ~origin ~provider ~model req response =
  let request = canonical ~provider ~model req in
  { hash = hash_of_canonical request; origin; request; response; note }

let path ~dir hash = Fs.(dir / (hash ^ ".json"))
let origin_to_string = function Recorded -> "recorded" | Authored -> "authored"

let to_json c =
  Json.Obj
    ([ ("schema", Json.String schema_tag); ("hash", Json.String c.hash); ("origin", Json.String (origin_to_string c.origin)) ]
     @ (if c.note = "" then [] else [ ("note", Json.String c.note) ])
     @ [ ("request", c.request); ("response", response_to_json c.response) ])

let ( let* ) = Result.bind

let of_json j =
  let* tag = Json.get_string "schema" j in
  if tag <> schema_tag then Error (Printf.sprintf "not a cassette (schema '%s')" tag)
  else
    let* hash = Json.get_string "hash" j in
    let* origin =
      match Json.string_or "" "origin" j with
      | "recorded" -> Ok Recorded
      | "authored" -> Ok Authored
      | other -> Error (Printf.sprintf "unknown cassette origin '%s'" other)
    in
    let* request = match Json.member "request" j with Some r -> Ok r | None -> Error "cassette has no request" in
    let* response =
      match Json.member "response" j with Some r -> response_of_json r | None -> Error "cassette has no response"
    in
    Ok { hash; origin; request; response; note = Json.string_or "" "note" j }

let load ~dir hash =
  let file = path ~dir hash in
  if not (Fs.exists file) then Ok None
  else
    let* text = Fs.read file in
    let* j = Json.parse text in
    let* c = of_json j in
    Ok (Some c)

let rec scrub_json = function
  | Json.String s -> Json.String (Keys.scrub s)
  | Json.List items -> Json.List (List.map scrub_json items)
  | Json.Obj members -> Json.Obj (List.map (fun (k, v) -> (k, scrub_json v)) members)
  | other -> other

let save ~dir c = Fs.write (path ~dir c.hash) (Json.pretty (scrub_json (to_json c)) ^ "\n")

let lint c =
  (if hash_of_canonical c.request <> c.hash then [ "the hash does not match the request" ] else [])
  @ (if c.response.text = "" then [ "the response has no text" ] else [])
  @ (match Keys.find_key_shaped (Json.to_string (to_json c)) with
     | [] -> []
     | found -> [ Printf.sprintf "%d key-shaped token(s) in the cassette" (List.length found) ])

let lint_dir dir =
  Fs.walk dir
  |> List.filter (fun rel -> Fs.extension rel = ".json")
  |> List.filter_map (fun rel ->
         let file = Fs.(dir / rel) in
         let problems =
           match Result.bind (Result.bind (Fs.read file) Json.parse) of_json with
           | Error e -> [ e ]
           | Ok c ->
             lint c @ if Filename.basename rel <> c.hash ^ ".json" then [ "the file name is not the hash of the request" ] else []
         in
         if problems = [] then None else Some (rel, problems))
