(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The chat-completions shape: [POST .../chat/completions] with a list of
    messages, the system prompt as the first message. It is spoken by local
    model servers and by the older xAI endpoint. *)

open Maithic_base
open Gateway_types

let name = "chat_completions"

let body ~(provider : Model_registry.provider) ~(model : Model_registry.model) (req : request) =
  let system = if req.system = "" then [] else [ Json.Obj [ ("role", Json.String "system"); ("content", Json.String req.system) ] ] in
  Json.Obj
    ([ ("model", Json.String model.Model_registry.id);
       ("messages", Json.List (system @ List.map message_to_json req.messages));
       (provider.Model_registry.max_tokens_field, Json.Int req.max_tokens) ]
     @ (match req.temperature with
        | Some t when model.Model_registry.temperature -> [ ("temperature", Json.Float t) ]
        | _ -> [])
     @ (match req.schema with
        | Some (schema_name, schema) when model.Model_registry.structured ->
          [ ( "response_format",
              Json.Obj
                [ ("type", Json.String "json_schema");
                  ("json_schema", Json.Obj [ ("name", Json.String schema_name); ("strict", Json.Bool true); ("schema", schema) ]) ] ) ]
        | _ -> []))

let build ~(provider : Model_registry.provider) ~model ~key (req : request) : Transport.request =
  { Transport.meth = "POST";
    url = provider.Model_registry.endpoint;
    headers = Provider.json_headers @ Provider.auth_headers provider key @ provider.Model_registry.headers;
    body = Json.to_string (body ~provider ~model req) }

let parse ~(provider : Model_registry.provider) ~(model : Model_registry.model) (resp : Transport.response) =
  match Json.parse resp.Transport.body with
  | Error e ->
    if resp.Transport.status >= 200 && resp.Transport.status < 300 then Error (Malformed e)
    else Error (Provider.http_error resp ~kind:"" ~message:"")
  | Ok j ->
    let failed = match Json.member "error" j with None | Some Json.Null -> false | Some _ -> true in
    if resp.Transport.status < 200 || resp.Transport.status >= 300 || failed then Error (Openai.error_of_body resp j)
    else
      match Json.list_or_empty "choices" j with
      | [] -> Error (Malformed "the response has no choices")
      | choice :: _ ->
        let message = Option.value (Json.member "message" choice) ~default:(Json.Obj []) in
        let usage = Option.value (Json.member "usage" j) ~default:(Json.Obj []) in
        (match Json.member "content" message, Json.member "refusal" message with
         | (None | Some Json.Null), Some (Json.String refusal) when refusal <> "" -> Error (Refused refusal)
         | Some (Json.String text), _ ->
           Ok
             { text;
               stop =
                 (match Json.string_or "stop" "finish_reason" choice with
                  | "stop" | "end_turn" -> End_turn
                  | "length" -> Max_tokens
                  | other -> Other other);
               usage =
                 { input_tokens = Json.int_or 0 "prompt_tokens" usage; output_tokens = Json.int_or 0 "completion_tokens" usage };
               model = Json.string_or model.Model_registry.id "model" j;
               provider = provider.Model_registry.name;
               id = Json.string_or "" "id" j }
         | _ -> Error (Malformed "the first choice has no text content"))
