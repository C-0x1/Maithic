(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The model proposer: asks the configured model for complete proofs.

    The first round uses the [prove.propose] prompt. Later rounds use
    [prove.repair], which shows the model the proofs that failed and the
    exact error Lean reported for each. The answer is requested as a JSON
    object matching {!schema}; whatever comes back is treated as untrusted
    text and goes through the guard and the kernel like any other candidate. *)

open Maithic_base
open Maithic_gateway

let name = "model"

(** The JSON schema of the answer: an array of candidate proofs. *)
let schema =
  Json.Obj
    [ ("type", Json.String "object");
      ( "properties",
        Json.Obj [ ("candidates", Json.Obj [ ("type", Json.String "array"); ("items", Json.Obj [ ("type", Json.String "string") ]) ]) ] );
      ("required", Json.List [ Json.String "candidates" ]);
      ("additionalProperties", Json.Bool false) ]

(* The text between the first pair of code fences, if any. *)
let fenced text =
  match Text.find_sub text "```" with
  | None -> None
  | Some start ->
    let body_start = match String.index_from_opt text start '\n' with Some i -> i + 1 | None -> start + 3 in
    (match Text.find_sub ~from:body_start text "```" with
     | Some stop when stop > body_start -> Some (String.sub text body_start (stop - body_start))
     | _ -> None)

(* The outermost JSON object in a text that may carry prose around it. *)
let embedded_object text =
  match String.index_opt text '{', String.rindex_opt text '}' with
  | Some first, Some last when last > first -> Some (String.sub text first (last - first + 1))
  | _ -> None

(** Reads the candidates out of a model answer. Accepts the requested JSON
    object, the same object inside a code fence or surrounded by prose, and
    as a last resort a single fenced Lean proof. *)
let parse_candidates text =
  let of_json j =
    match Json.member "candidates" j with
    | Some (Json.List items) -> Some (List.filter_map Json.string_opt items)
    | _ -> None
  in
  let attempts =
    [ (fun () -> Option.bind (Result.to_option (Json.parse (String.trim text))) of_json);
      (fun () -> Option.bind (fenced text) (fun body -> Option.bind (Result.to_option (Json.parse (String.trim body))) of_json));
      (fun () -> Option.bind (embedded_object text) (fun body -> Option.bind (Result.to_option (Json.parse body)) of_json));
      (fun () ->
        Option.bind (fenced text) (fun body ->
            let body = String.trim body in
            if body = "" then None else Some [ body ])) ]
  in
  let found = List.find_map (fun attempt -> attempt ()) attempts in
  Option.value found ~default:[] |> List.map String.trim |> List.filter (fun c -> c <> "")

(* The end of the context, which is what the theorem most likely depends on. *)
let context_excerpt ?(max_chars = 6000) prelude =
  let n = String.length prelude in
  if n <= max_chars then prelude
  else
    let cut = n - max_chars in
    let cut = match String.index_from_opt prelude cut '\n' with Some i -> i + 1 | None -> cut in
    "-- (earlier part of the file omitted)\n" ^ String.sub prelude cut (n - cut)

(** Asks for candidates. [failures] is the rendered memory of failed
    attempts; when it is empty the first-round prompt is used. *)
let propose gateway ~max_tokens ~count ~statement ~prelude ~failures =
  let prompt_name = if failures = "" then "prove.propose" else "prove.repair" in
  match Gateway.prompt gateway prompt_name with
  | Error e -> Error (Gateway_types.Invalid_request e)
  | Ok prompt ->
    let context = match String.trim (context_excerpt prelude) with "" -> "(nothing)" | c -> c in
    let bindings =
      [ ("count", string_of_int count); ("theorem", statement); ("context", context) ]
      @ if failures = "" then [] else [ ("attempts", failures) ]
    in
    (match Prompt.render prompt bindings with
     | Error e -> Error (Gateway_types.Invalid_request e)
     | Ok (system, user) ->
       let request =
         Gateway_types.request ~purpose:prompt_name ~system ~max_tokens ~schema:("proof_candidates", schema)
           [ Gateway_types.user user ]
       in
       Result.map
         (fun (response : Gateway_types.response) ->
           let candidates = parse_candidates response.Gateway_types.text in
           List.filteri (fun i _ -> i < count) candidates)
         (Gateway.complete gateway request))
