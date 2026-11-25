(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type api = Messages | Responses | Chat_completions
type auth = Bearer | X_api_key | No_auth

type provider = {
  name : string;
  api : api;
  endpoint : string;
  key_env : string;
  auth : auth;
  version : string;
  headers : (string * string) list;
  max_tokens_field : string;
  tiers : (string * string) list;
}

type model = { id : string; provider : string; temperature : bool; structured : bool }
type t = { providers : provider list; models : model list }

let builtin_toml =
  {toml|# Maithic model registry.
#
# Which providers exist, how each is reached, and which model each quality tier
# means. Model identifiers change often: refresh this file from each provider's
# model list before relying on it, or point `gateway.models` in maithic.toml at
# your own copy. Nothing else in Maithic names a model.
#
# Keys are never stored here. Each provider names the environment variable that
# holds its key.

[providers.anthropic]
api = "messages"
endpoint = "https://api.anthropic.com/v1/messages"
key_env = "ANTHROPIC_API_KEY"
auth = "bearer"                 # "bearer" or "x-api-key"
version = "2026-10-21"          # sent as the anthropic-version header

[providers.anthropic.tiers]
fast = "claude-haiku-5-5"
balanced = "claude-sonnet-5-5"
best = "claude-opus-5-5"

[providers.openai]
api = "responses"
endpoint = "https://api.openai.com/v1/responses"
key_env = "OPENAI_API_KEY"
auth = "bearer"

[providers.openai.tiers]
fast = "gpt-6-luna"
balanced = "gpt-6.2-sol"
best = "gpt-6.2-astra"

[providers.xai]
api = "responses"               # "chat_completions" selects the older endpoint shape
endpoint = "https://api.x.ai/v1/responses"
key_env = "XAI_API_KEY"
auth = "bearer"

[providers.xai.tiers]
fast = "grok-4.3"
balanced = "grok-4.7"
best = "grok-4.7"

# A server on this machine that speaks the chat-completions shape.
[providers.local]
api = "chat_completions"
endpoint = "http://127.0.0.1:11434/v1/chat/completions"
key_env = ""
auth = "none"
max_tokens_field = "max_tokens"

[providers.local.tiers]
fast = "local-model"
balanced = "local-model"
best = "local-model"

# Per-model capabilities. A model that is not listed is assumed to accept a
# JSON schema for its output and no sampling temperature.
[[models]]
id = "local-model"
provider = "local"
temperature = true
structured = false
|toml}

let api_to_string = function Messages -> "messages" | Responses -> "responses" | Chat_completions -> "chat_completions"

let ( let* ) = Result.bind

let provider_of_json name j =
  let* api =
    match Json.string_or "" "api" j with
    | "messages" -> Ok Messages
    | "responses" -> Ok Responses
    | "chat_completions" -> Ok Chat_completions
    | other -> Error (Printf.sprintf "provider '%s': unknown api '%s'" name other)
  in
  let* endpoint =
    match Json.string_or "" "endpoint" j with
    | "" -> Error (Printf.sprintf "provider '%s': missing endpoint" name)
    | url when Text.starts_with ~prefix:"https://" url || Text.starts_with ~prefix:"http://127.0.0.1" url
               || Text.starts_with ~prefix:"http://localhost" url || Text.starts_with ~prefix:"http://[::1]" url -> Ok url
    | url -> Error (Printf.sprintf "provider '%s': endpoint '%s' must use https (plain http is allowed for this machine only)" name url)
  in
  let key_env = Json.string_or "" "key_env" j in
  let* auth =
    match Json.string_or (if key_env = "" then "none" else "bearer") "auth" j with
    | "bearer" -> Ok Bearer
    | "x-api-key" -> Ok X_api_key
    | "none" -> Ok No_auth
    | other -> Error (Printf.sprintf "provider '%s': unknown auth '%s'" name other)
  in
  let pairs key =
    match Json.member key j with
    | Some (Json.Obj members) -> List.filter_map (fun (k, v) -> Option.map (fun s -> (k, s)) (Json.string_opt v)) members
    | _ -> []
  in
  let headers = pairs "headers" in
  let* () =
    match List.find_opt (fun (k, _) -> List.mem (String.lowercase_ascii k) [ "authorization"; "x-api-key" ]) headers with
    | Some (k, _) -> Error (Printf.sprintf "provider '%s': header '%s' is a credential and cannot be set in the registry" name k)
    | None -> Ok ()
  in
  Ok
    { name; api; endpoint; key_env; auth;
      version = Json.string_or "" "version" j;
      headers;
      max_tokens_field =
        Json.string_or
          (match api with Messages -> "max_tokens" | Responses -> "max_output_tokens" | Chat_completions -> "max_completion_tokens")
          "max_tokens_field" j;
      tiers = pairs "tiers" }

let of_toml doc =
  let* providers =
    match Json.member "providers" doc with
    | Some (Json.Obj members) ->
      List.fold_left
        (fun acc (name, j) ->
          let* done_ = acc in
          let* p = provider_of_json name j in
          Ok (p :: done_))
        (Ok []) members
      |> Result.map List.rev
    | _ -> Error "the model registry has no [providers] table"
  in
  let models =
    List.filter_map
      (fun j ->
        match Json.get_string "id" j with
        | Error _ -> None
        | Ok id ->
          Some
            { id;
              provider = Json.string_or "" "provider" j;
              temperature = Json.bool_or false "temperature" j;
              structured = Json.bool_or true "structured" j })
      (Json.list_or_empty "models" doc)
  in
  Ok { providers; models }

let load path = Result.bind (Toml.parse_file path) of_toml

let builtin =
  match Result.bind (Toml.parse builtin_toml) of_toml with
  | Ok registry -> registry
  | Error e -> failwith ("built-in model registry: " ^ e)

let provider t name = List.find_opt (fun p -> p.name = name) t.providers

let resolve t ~provider:name ~tier ~model =
  match provider t name with
  | None ->
    Error (Printf.sprintf "unknown provider '%s' (known: %s)" name (String.concat ", " (List.map (fun p -> p.name) t.providers)))
  | Some p ->
    let* id =
      match model with
      | Some id when id <> "" -> Ok id
      | _ ->
        (match List.assoc_opt tier p.tiers with
         | Some id -> Ok id
         | None -> Error (Printf.sprintf "provider '%s' has no model for tier '%s'" name tier))
    in
    let caps =
      match List.find_opt (fun (m : model) -> m.id = id) t.models with
      | Some m -> m
      | None -> { id; provider = name; temperature = false; structured = true }
    in
    Ok (p, caps)

let to_json t =
  Json.Obj
    [ ( "providers",
        Json.List
          (List.map
             (fun p ->
               Json.Obj
                 [ ("name", Json.String p.name); ("api", Json.String (api_to_string p.api)); ("endpoint", Json.String p.endpoint);
                   ("key_env", Json.String p.key_env);
                   ("tiers", Json.Obj (List.map (fun (k, v) -> (k, Json.String v)) p.tiers)) ])
             t.providers) ) ]
