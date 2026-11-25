(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Asking a model for an implementation, and for a better one. *)

open Maithic_base
open Maithic_gateway

let draft_prompt = "algolab.draft"
let repair_prompt = "algolab.repair"
let libraries = "the Lean core library, without Mathlib"

(** Who wrote a draft. *)
type provenance = { origin : string;  (** [draft], [repair] or [given] *) provider : string; model : string; prompt : string }

let given = { origin = "given"; provider = ""; model = ""; prompt = "" }

let listed = function [] -> "(none)" | items -> String.concat "\n" (List.map (fun item -> "- " ^ item) items)

let examples_text (task : Algolab_task.t) =
  match task.Algolab_task.examples with
  | [] -> "(none)"
  | examples -> String.concat "\n" (List.map (fun (e : Algolab_task.example) -> Printf.sprintf "%s  should give  %s" e.Algolab_task.call e.Algolab_task.expect) examples)

let draft_bindings (task : Algolab_task.t) =
  [ ("libraries", libraries); ("description", task.Algolab_task.description); ("signature", Algolab_task.header task);
    ("constraints", listed task.Algolab_task.constraints); ("examples", examples_text task) ]

let repair_bindings (task : Algolab_task.t) ~implementation ~report ~earlier =
  [ ("libraries", libraries); ("description", task.Algolab_task.description); ("signature", Algolab_task.header task); ("implementation", implementation);
    ("report", report); ("earlier", match earlier with [] -> "(none)" | texts -> String.concat "\n\n" texts) ]

let schema =
  Json.Obj
    [ ("type", Json.String "object");
      ("properties", Json.Obj [ ("implementation", Json.Obj [ ("type", Json.String "string") ]); ("notes", Json.Obj [ ("type", Json.String "string") ]) ]);
      ("required", Json.List [ Json.String "implementation" ]) ]

(** The implementation in a model's answer: the member of a JSON object, with
    a code fence around it taken off. *)
let implementation_of_answer text =
  match Json.parse (Text.trim text) with
  | Error e -> Error ("the answer is not JSON: " ^ e)
  | Ok json ->
    let raw = Text.trim (Json.string_or "" "implementation" json) in
    let raw =
      if Text.starts_with ~prefix:"```" raw then
        let lines = String.split_on_char '\n' raw in
        let body = List.filter (fun line -> not (Text.starts_with ~prefix:"```" (Text.trim line))) lines in
        Text.trim (String.concat "\n" body)
      else raw
    in
    if raw = "" then Error "the answer has no implementation" else Ok raw

let ask gateway ~max_tokens ~prompt ~origin bindings =
  match Gateway.prompt gateway prompt with
  | Error e -> Error e
  | Ok loaded ->
    (match Prompt.render loaded bindings with
     | Error e -> Error e
     | Ok (system, user) ->
       let request = Gateway_types.request ~purpose:prompt ~system ~max_tokens ~schema:("implementation", schema) [ Gateway_types.user user ] in
       (match Gateway.complete gateway request with
        | Error e -> Error (Gateway_types.error_to_string e)
        | Ok response ->
          Result.map
            (fun implementation -> (implementation, { origin; provider = response.Gateway_types.provider; model = response.Gateway_types.model; prompt }))
            (implementation_of_answer response.Gateway_types.text)))

let draft gateway ~max_tokens task = ask gateway ~max_tokens ~prompt:draft_prompt ~origin:"draft" (draft_bindings task)

let repair gateway ~max_tokens task ~implementation ~report ~earlier =
  ask gateway ~max_tokens ~prompt:repair_prompt ~origin:"repair" (repair_bindings task ~implementation ~report ~earlier)
