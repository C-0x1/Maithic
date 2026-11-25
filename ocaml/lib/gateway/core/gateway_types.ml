(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The unified request, response and error types that every pillar programs
    against. A provider adapter translates these to and from one provider's
    wire format; nothing outside the gateway sees a provider-specific shape. *)

open Maithic_base
open Maithic_schema

type role = User | Assistant

type message = { role : role; content : string }

type request = {
  purpose : string;  (** the name of the prompt, such as ["prove.propose"] *)
  system : string;  (** policy and task instructions *)
  messages : message list;
  max_tokens : int;
  temperature : float option;  (** sent only to models that accept it *)
  schema : (string * Json.t) option;  (** a name and a JSON schema for structured output *)
}

type usage = { input_tokens : int; output_tokens : int }

type stop = End_turn | Max_tokens | Stop_sequence | Other of string

type response = {
  text : string;
  stop : stop;
  usage : usage;
  model : string;
  provider : string;
  id : string;  (** the provider's identifier of the response, for support requests *)
}

type error =
  | Disabled  (** the gateway mode is [off] *)
  | Unknown_provider of string
  | Unknown_model of string
  | Missing_key of { provider : string; variable : string }
  | Missing_cassette of { hash : string; purpose : string; provider : string; model : string }
  | Authentication of string
  | Rate_limited of { retry_after : float option; detail : string }
  | Overloaded of string
  | Invalid_request of string
  | Not_found of string
  | Timeout of string
  | Refused of string  (** the model declined to answer *)
  | Server of { status : int; detail : string }
  | Transport of string  (** the request never produced an HTTP response *)
  | Malformed of string  (** an HTTP success whose body could not be understood *)

let role_to_string = function User -> "user" | Assistant -> "assistant"

let stop_to_string = function
  | End_turn -> "end_turn"
  | Max_tokens -> "max_tokens"
  | Stop_sequence -> "stop_sequence"
  | Other s -> s

let stop_of_string = function
  | "end_turn" -> End_turn
  | "max_tokens" -> Max_tokens
  | "stop_sequence" -> Stop_sequence
  | s -> Other s

let user content = { role = User; content }
let assistant content = { role = Assistant; content }

let request ?(system = "") ?(max_tokens = 2048) ?temperature ?schema ~purpose messages =
  { purpose; system; messages; max_tokens; temperature; schema }

(** Errors worth another attempt after a pause. *)
let is_retryable = function
  | Rate_limited _ | Overloaded _ | Timeout _ | Transport _ -> true
  | Server { status; _ } -> status >= 500
  | _ -> false

(* A short, stable name for each kind of failure, for accounts and statistics. *)
let error_class = function
  | Disabled -> "disabled"
  | Unknown_provider _ -> "unknown_provider"
  | Unknown_model _ -> "unknown_model"
  | Missing_key _ -> "missing_key"
  | Missing_cassette _ -> "missing_cassette"
  | Authentication _ -> "authentication"
  | Rate_limited _ -> "rate_limited"
  | Overloaded _ -> "overloaded"
  | Invalid_request _ -> "invalid_request"
  | Not_found _ -> "not_found"
  | Timeout _ -> "timeout"
  | Refused _ -> "refused"
  | Server _ -> "server"
  | Transport _ -> "transport"
  | Malformed _ -> "malformed"

let error_to_string = function
  | Disabled -> "AI is disabled (gateway mode off)"
  | Unknown_provider p -> Printf.sprintf "unknown provider '%s'" p
  | Unknown_model m -> Printf.sprintf "unknown model '%s'" m
  | Missing_key { provider; variable } -> Printf.sprintf "no API key for %s (set %s)" provider variable
  | Missing_cassette { hash; purpose; provider; model } ->
    Printf.sprintf "no cassette for request %s (purpose %s, provider %s, model %s)" hash purpose provider model
  | Authentication d -> "the provider rejected the API key: " ^ d
  | Rate_limited { detail; retry_after } ->
    (match retry_after with
     | Some s -> Printf.sprintf "rate limited (retry after %.0f s): %s" s detail
     | None -> "rate limited: " ^ detail)
  | Overloaded d -> "the provider is overloaded: " ^ d
  | Invalid_request d -> "the provider rejected the request: " ^ d
  | Not_found d -> "not found at the provider: " ^ d
  | Timeout d -> "the provider call timed out: " ^ d
  | Refused d -> "the model declined to answer: " ^ d
  | Server { status; detail } -> Printf.sprintf "provider error (HTTP %d): %s" status detail
  | Transport d -> "could not reach the provider: " ^ d
  | Malformed d -> "could not understand the provider's response: " ^ d

let error_to_fault error =
  let make ?hint number exit = Fault.make ?hint ~area:"GATEWAY" ~number ~exit (error_to_string error) in
  match error with
  | Disabled -> make 1 Exit_code.Unavailable ~hint:"Set gateway.mode to live, record, replay or mock."
  | Unknown_provider _ -> make 2 Exit_code.Config ~hint:"Providers are listed by `maithic models`."
  | Unknown_model _ -> make 3 Exit_code.Config ~hint:"Models are listed by `maithic models`."
  | Missing_key { variable; _ } ->
    make 4 Exit_code.Provider
      ~hint:(Printf.sprintf "Export %s, or run `maithic auth login`. Without a key Maithic still proves with its built-in automation." variable)
  | Authentication _ -> make 5 Exit_code.Provider ~hint:"Check the key with `maithic auth status`."
  | Rate_limited _ -> make 6 Exit_code.Provider
  | Missing_cassette _ ->
    make 7 Exit_code.Provider
      ~hint:"Replay mode never calls a provider. Record the cassette with MAITHIC_GATEWAY_MODE=record and a key, or run in another mode."
  | Overloaded _ -> make 8 Exit_code.Provider
  | Invalid_request _ -> make 9 Exit_code.Provider
  | Not_found _ -> make 10 Exit_code.Provider
  | Timeout _ -> make 11 Exit_code.Timeout
  | Refused _ -> make 12 Exit_code.Provider
  | Server _ -> make 13 Exit_code.Provider
  | Transport _ -> make 14 Exit_code.Provider ~hint:"Live mode needs the `curl` command and network access."
  | Malformed _ -> make 15 Exit_code.Provider

(* ---- JSON forms, used by cassettes and by --json output ---- *)

let message_to_json m = Json.Obj [ ("role", Json.String (role_to_string m.role)); ("content", Json.String m.content) ]

let usage_to_json u = Json.Obj [ ("input_tokens", Json.Int u.input_tokens); ("output_tokens", Json.Int u.output_tokens) ]

let response_to_json r =
  Json.Obj
    [ ("text", Json.String r.text); ("stop", Json.String (stop_to_string r.stop)); ("usage", usage_to_json r.usage);
      ("model", Json.String r.model); ("provider", Json.String r.provider); ("id", Json.String r.id) ]

let response_of_json j =
  match Json.get_string "text" j with
  | Error e -> Error e
  | Ok text ->
    let usage = Option.value (Json.member "usage" j) ~default:(Json.Obj []) in
    Ok
      { text;
        stop = stop_of_string (Json.string_or "end_turn" "stop" j);
        usage = { input_tokens = Json.int_or 0 "input_tokens" usage; output_tokens = Json.int_or 0 "output_tokens" usage };
        model = Json.string_or "" "model" j;
        provider = Json.string_or "" "provider" j;
        id = Json.string_or "" "id" j }
