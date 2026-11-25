(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_gateway

let back_prompt = "formalize.back"
let judge_prompt = "formalize.judge"

let object_schema properties required =
  Json.Obj
    [ ("type", Json.String "object"); ("properties", Json.Obj properties); ("required", Json.List (List.map (fun name -> Json.String name) required));
      ("additionalProperties", Json.Bool false) ]

let back_schema = object_schema [ ("english", Json.Obj [ ("type", Json.String "string") ]) ] [ "english" ]

let judge_schema =
  object_schema
    [ ("score", Json.Obj [ ("type", Json.String "integer") ]);
      ("differences", Json.Obj [ ("type", Json.String "array"); ("items", Json.Obj [ ("type", Json.String "string") ]) ]) ]
    [ "score"; "differences" ]

let english_of_answer text =
  match Option.bind (Formalize_candidate.answer text) (Json.member "english") with Some (Json.String english) -> String.trim english | _ -> ""

let judgement_of_answer text =
  match Formalize_candidate.answer text with
  | None -> Error "the model's answer is not a JSON object"
  | Some answer ->
    let differences = List.filter (fun d -> d <> "") (List.filter_map (function Json.String d -> Some (String.trim d) | _ -> None) (Json.list_or_empty "differences" answer)) in
    (match Json.member "score" answer with
     | Some (Json.Int score) when score >= 0 && score <= 10 -> Ok (score, differences)
     | Some (Json.Float score) when score >= 0.0 && score <= 10.0 -> Ok (int_of_float (Float.round score), differences)
     | Some _ -> Error "the model's answer has a score that is not a number from 0 to 10"
     | None -> Error "the model's answer has no score")

let call gateway ~max_tokens ~prompt ~schema bindings =
  match Gateway.prompt gateway prompt with
  | Error e -> Error e
  | Ok loaded ->
    (match Prompt.render loaded bindings with
     | Error e -> Error e
     | Ok (system, user) ->
       (match Gateway.complete gateway (Gateway_types.request ~purpose:prompt ~system ~max_tokens ~schema [ Gateway_types.user user ]) with
        | Error e -> Error (Gateway_types.error_to_string e)
        | Ok response -> Ok response.Gateway_types.text))

let model gateway ~max_tokens : Formalize_faith.model =
  { Formalize_faith.back =
      (fun ~statement -> Result.map english_of_answer (call gateway ~max_tokens ~prompt:back_prompt ~schema:("statement_in_english", back_schema) [ ("statement", statement) ]));
    judge =
      (fun ~problem ~english ->
        Result.bind (call gateway ~max_tokens ~prompt:judge_prompt ~schema:("agreement", judge_schema) [ ("problem", problem); ("english", english) ]) judgement_of_answer) }
