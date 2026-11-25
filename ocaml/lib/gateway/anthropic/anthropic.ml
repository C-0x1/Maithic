(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The Anthropic adapter: the Claude Messages API.

    Request: [POST /v1/messages] with [model], [max_tokens], an optional
    top-level [system] prompt and [messages]. The key travels as a bearer
    token, or in [x-api-key] when the registry says so, next to the
    [anthropic-version] header. Structured output is requested through
    [output_config.format].

    From API version [2026-10-21] on, a request that carries a sampling
    parameter is rejected, so [temperature] is only sent with an older
    version and a model that accepts it. *)

open Maithic_base
open Gateway_types

let name = "anthropic"

(** The first API version that rejects sampling parameters. *)
let no_sampling_from = "2026-10-21"

let sampling_allowed (provider : Model_registry.provider) (model : Model_registry.model) =
  model.Model_registry.temperature && provider.Model_registry.version <> "" && provider.Model_registry.version < no_sampling_from

let body ~(provider : Model_registry.provider) ~(model : Model_registry.model) (req : request) =
  Json.Obj
    ([ ("model", Json.String model.Model_registry.id); (provider.Model_registry.max_tokens_field, Json.Int req.max_tokens) ]
     @ (if req.system = "" then [] else [ ("system", Json.String req.system) ])
     @ [ ("messages", Json.List (List.map message_to_json req.messages)) ]
     @ (match req.temperature with
        | Some t when sampling_allowed provider model -> [ ("temperature", Json.Float t) ]
        | _ -> [])
     @ (match req.schema with
        | Some (_, schema) when model.Model_registry.structured ->
          [ ("output_config", Json.Obj [ ("format", Json.Obj [ ("type", Json.String "json_schema"); ("schema", schema) ]) ]) ]
        | _ -> []))

let build ~(provider : Model_registry.provider) ~model ~key (req : request) : Transport.request =
  { Transport.meth = "POST";
    url = provider.Model_registry.endpoint;
    headers =
      Provider.json_headers
      @ Provider.auth_headers provider key
      @ (if provider.Model_registry.version = "" then [] else [ ("anthropic-version", provider.Model_registry.version) ])
      @ provider.Model_registry.headers;
    body = Json.to_string (body ~provider ~model req) }

let parse ~(provider : Model_registry.provider) ~(model : Model_registry.model) (resp : Transport.response) =
  match Json.parse resp.Transport.body with
  | Error e ->
    if resp.Transport.status >= 200 && resp.Transport.status < 300 then Error (Malformed e)
    else Error (Provider.http_error resp ~kind:"" ~message:"")
  | Ok j ->
    if resp.Transport.status >= 200 && resp.Transport.status < 300 && Json.member "type" j <> Some (Json.String "error") then begin
      let blocks = Json.list_or_empty "content" j in
      let text =
        blocks
        |> List.filter (fun b -> Json.member "type" b = Some (Json.String "text"))
        |> List.map (Json.string_or "" "text")
        |> String.concat ""
      in
      let usage = Option.value (Json.member "usage" j) ~default:(Json.Obj []) in
      let stop_reason = Json.string_or "end_turn" "stop_reason" j in
      if stop_reason = "refusal" then Error (Refused (if text = "" then "the model refused the request" else text))
      else if blocks = [] && Json.member "content" j = None then Error (Malformed "the response has no content")
      else
        Ok
          { text;
            stop = stop_of_string stop_reason;
            usage = { input_tokens = Json.int_or 0 "input_tokens" usage; output_tokens = Json.int_or 0 "output_tokens" usage };
            model = Json.string_or model.Model_registry.id "model" j;
            provider = provider.Model_registry.name;
            id = Json.string_or "" "id" j }
    end
    else begin
      let err = Option.value (Json.member "error" j) ~default:(Json.Obj []) in
      let kind = Json.string_or "" "type" err and message = Json.string_or "" "message" err in
      match kind with
      | "overloaded_error" -> Error (Overloaded message)
      | "rate_limit_error" -> Error (Rate_limited { retry_after = Provider.retry_after resp; detail = message })
      | "authentication_error" | "permission_error" -> Error (Authentication message)
      | "billing_error" -> Error (Authentication ("billing: " ^ message))
      | "not_found_error" -> Error (Not_found message)
      | "timeout_error" -> Error (Timeout message)
      | "invalid_request_error" | "request_too_large" -> Error (Invalid_request message)
      | _ -> Error (Provider.http_error resp ~kind ~message)
    end
