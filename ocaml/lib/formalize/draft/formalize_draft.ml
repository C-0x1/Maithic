(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_gateway

let prompt = "formalize.draft"
let default_libraries = "the Lean core library, without Mathlib"

let libraries = function
  | [] -> default_libraries
  | imports -> Printf.sprintf "the Lean core library and the modules %s" (String.concat ", " imports)

let bindings ~count ~imports (intake : Formalize_intake.t) =
  [ ("count", string_of_int count); ("libraries", libraries imports); ("problem", intake.Formalize_intake.text); ("frame", Formalize_intake.frame intake) ]

(* The schema of the answer: statements, each with the reading it makes. *)
let schema =
  Json.Obj
    [ ("type", Json.String "object");
      ( "properties",
        Json.Obj
          [ ( "candidates",
              Json.Obj
                [ ("type", Json.String "array");
                  ( "items",
                    Json.Obj
                      [ ("type", Json.String "object");
                        ("properties", Json.Obj [ ("statement", Json.Obj [ ("type", Json.String "string") ]); ("reading", Json.Obj [ ("type", Json.String "string") ]) ]);
                        ("required", Json.List [ Json.String "statement"; Json.String "reading" ]); ("additionalProperties", Json.Bool false) ] ) ] ) ] );
      ("required", Json.List [ Json.String "candidates" ]); ("additionalProperties", Json.Bool false) ]

let ask gateway ~max_tokens ~name ~count ~imports intake =
  match Gateway.prompt gateway prompt with
  | Error e -> Error e
  | Ok loaded ->
    (match Prompt.render loaded (bindings ~count ~imports intake) with
     | Error e -> Error e
     | Ok (system, user) ->
       let request = Gateway_types.request ~purpose:prompt ~system ~max_tokens ~schema:("statement_candidates", schema) [ Gateway_types.user user ] in
       (match Gateway.complete gateway request with
        | Error e -> Error (Gateway_types.error_to_string e)
        | Ok response ->
          let provenance =
            { Formalize_candidate.origin = "draft"; provider = response.Gateway_types.provider; model = response.Gateway_types.model; prompt; round = 0 }
          in
          Ok (Formalize_candidate.of_answer ~name ~imports ~provenance ~count response.Gateway_types.text)))
