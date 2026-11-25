(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The OpenAI adapter: the Responses API.

    Request: [POST /v1/responses] with [model], [instructions] and [input]
    items; the key travels as a bearer token. Structured output is requested
    with a strict JSON schema in [text.format]. Responses are not stored at
    the provider ([store] is false). The same shapes are used by the xAI
    adapter, which is why the functions take the provider entry. *)

open Maithic_base
open Gateway_types

let name = "openai"

let body ~(provider : Model_registry.provider) ~(model : Model_registry.model) (req : request) =
  Json.Obj
    ([ ("model", Json.String model.Model_registry.id) ]
     @ (if req.system = "" then [] else [ ("instructions", Json.String req.system) ])
     @ [ ("input", Json.List (List.map message_to_json req.messages));
         (provider.Model_registry.max_tokens_field, Json.Int req.max_tokens);
         ("store", Json.Bool false) ]
     @ (match req.temperature with
        | Some t when model.Model_registry.temperature -> [ ("temperature", Json.Float t) ]
        | _ -> [])
     @ (match req.schema with
        | Some (schema_name, schema) when model.Model_registry.structured ->
          [ ( "text",
              Json.Obj
                [ ( "format",
                    Json.Obj
                      [ ("type", Json.String "json_schema"); ("name", Json.String schema_name); ("strict", Json.Bool true);
                        ("schema", schema) ] ) ] ) ]
        | _ -> []))

let build ~(provider : Model_registry.provider) ~model ~key (req : request) : Transport.request =
  { Transport.meth = "POST";
    url = provider.Model_registry.endpoint;
    headers = Provider.json_headers @ Provider.auth_headers provider key @ provider.Model_registry.headers;
    body = Json.to_string (body ~provider ~model req) }

(** Maps an error object [{"message", "type", "code"}] of the OpenAI family. *)
let error_of_body (resp : Transport.response) j =
  let err = Option.value (Json.member "error" j) ~default:(Json.Obj []) in
  let message = match err with Json.String s -> s | _ -> Json.string_or "" "message" err in
  let code = match Json.member "code" err with Some (Json.String c) -> c | _ -> "" in
  let kind = Json.string_or "" "type" err in
  match code with
  | "slow_down" | "server_is_overloaded" -> Overloaded message
  | "invalid_api_key" -> Authentication message
  | "insufficient_quota" -> Rate_limited { retry_after = None; detail = message }
  | "rate_limit_exceeded" -> Rate_limited { retry_after = Provider.retry_after resp; detail = message }
  | "model_not_found" -> Not_found message
  | _ -> Provider.http_error resp ~kind:(kind ^ " " ^ code) ~message

let parse ~(provider : Model_registry.provider) ~(model : Model_registry.model) (resp : Transport.response) =
  match Json.parse resp.Transport.body with
  | Error e ->
    if resp.Transport.status >= 200 && resp.Transport.status < 300 then Error (Malformed e)
    else Error (Provider.http_error resp ~kind:"" ~message:"")
  | Ok j ->
    let failed = match Json.member "error" j with None | Some Json.Null -> false | Some _ -> true in
    if resp.Transport.status < 200 || resp.Transport.status >= 300 || failed then Error (error_of_body resp j)
    else begin
      let parts =
        Json.list_or_empty "output" j
        |> List.filter (fun item -> Json.member "type" item = Some (Json.String "message"))
        |> List.concat_map (Json.list_or_empty "content")
      in
      let texts =
        parts |> List.filter (fun p -> Json.member "type" p = Some (Json.String "output_text")) |> List.map (Json.string_or "" "text")
      in
      let refusals =
        parts |> List.filter (fun p -> Json.member "type" p = Some (Json.String "refusal")) |> List.map (Json.string_or "" "refusal")
      in
      let usage = Option.value (Json.member "usage" j) ~default:(Json.Obj []) in
      let status = Json.string_or "completed" "status" j in
      let incomplete =
        Option.value (Option.bind (Json.path [ "incomplete_details"; "reason" ] j) Json.string_opt) ~default:""
      in
      if refusals <> [] && texts = [] then Error (Refused (String.concat " " refusals))
      else if texts = [] && status <> "incomplete" then Error (Malformed "the response has no output text")
      else
        Ok
          { text = String.concat "" texts;
            stop =
              (if status = "incomplete" then (if Text.contains incomplete "max_output_tokens" then Max_tokens else Other incomplete)
               else End_turn);
            usage = { input_tokens = Json.int_or 0 "input_tokens" usage; output_tokens = Json.int_or 0 "output_tokens" usage };
            model = Json.string_or model.Model_registry.id "model" j;
            provider = provider.Model_registry.name;
            id = Json.string_or "" "id" j }
    end
