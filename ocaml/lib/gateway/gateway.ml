(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Gateway_types

type mode = Live | Record | Replay | Mock | Off

let mode_of_string = function
  | "live" -> Some Live
  | "record" -> Some Record
  | "replay" -> Some Replay
  | "mock" -> Some Mock
  | "off" -> Some Off
  | _ -> None

let mode_to_string = function Live -> "live" | Record -> "record" | Replay -> "replay" | Mock -> "mock" | Off -> "off"

type call = {
  purpose : string;
  provider : string;
  model : string;
  served_by : string;
  ok : bool;
  error_class : string option;
  usage : usage;
  started_at : float;
  elapsed_ms : int;
}

type t = {
  mode : mode;
  registry : Model_registry.t;
  provider : string;
  tier : string;
  model : string option;
  cassette_dir : string;
  prompts_dir : string option;
  timeout : float;
  getenv : string -> string option;
  home : string;
  explicit_key : string option;
  transport : Transport.t;
  scripted : Scripted.handler option;
  mutable account : call list; (* newest first *)
}

let adapters : (string * (module Provider.S)) list =
  [ ("anthropic", (module Anthropic)); ("openai", (module Openai)); ("xai", (module Xai)) ]

(* The adapter of a provider: by name for the three built-in providers, by
   wire shape for any other entry of the registry. *)
let adapter (provider : Model_registry.provider) : (module Provider.S) =
  match List.assoc_opt provider.Model_registry.name adapters with
  | Some a -> a
  | None ->
    (match provider.Model_registry.api with
     | Model_registry.Messages -> (module Anthropic)
     | Model_registry.Responses -> (module Openai)
     | Model_registry.Chat_completions -> (module Chat_completions))

let create ?(transport = Transport.curl) ?scripted ?(getenv = Sys.getenv_opt) ?explicit_key config =
  let registry =
    match Config.string_opt config "gateway.models" with
    | None -> Ok Model_registry.builtin
    | Some path -> Model_registry.load (Config.resolve_path config path)
  in
  match registry with
  | Error e -> Error (Fault.make ~area:"GATEWAY" ~number:20 ~exit:Exit_code.Config ("cannot load the model registry: " ^ e))
  | Ok registry ->
    let mode = Option.value (mode_of_string (Config.string config "gateway.mode")) ~default:Live in
    Ok
      { mode; registry;
        provider = Config.string config "ai.provider";
        tier = Config.string config "ai.tier";
        model = Config.string_opt config "ai.model";
        cassette_dir = Config.resolve_path config (Config.string config "gateway.cassette_dir");
        prompts_dir = Option.map (Config.resolve_path config) (Config.string_opt config "gateway.prompts");
        timeout = Config.float config "gateway.timeout_s";
        getenv;
        home = Config.home_dir getenv;
        explicit_key; transport; scripted; account = [] }

let mode t = t.mode
let provider_name t = t.provider
let calls t = List.rev t.account
let prompt t name = Prompt.load ?dir:t.prompts_dir name

let route t = Route.default t.registry ~provider:t.provider ~tier:t.tier ~model:t.model ~timeout:t.timeout
let resolve t = Result.map (fun (r : Route.t) -> (r.Route.provider, r.Route.model)) (route t)

let model_id t =
  match t.scripted, t.mode with
  | Some _, _ -> Ok "scripted"
  | None, Mock -> Ok "mock"
  | None, _ -> Result.map (fun (_, (m : Model_registry.model)) -> m.Model_registry.id) (resolve t)

let key_for t (provider : Model_registry.provider) =
  match Keys.lookup ?explicit:t.explicit_key ~getenv:t.getenv ~home:t.home provider with
  | Some (key, _) -> Ok key
  | None -> Error (Missing_key { provider = provider.Model_registry.name; variable = provider.Model_registry.key_env })

let ready t =
  match t.scripted, t.mode with
  | Some _, _ -> Ok ()
  | None, Off -> Error Disabled
  | None, Mock -> Ok ()
  | None, Replay -> Result.map (fun _ -> ()) (resolve t)
  | None, (Live | Record) -> Result.bind (resolve t) (fun (provider, _) -> Result.map (fun _ -> ()) (key_for t provider))

(* Removes the key from any text that came back from the outside. *)
let scrub_error key error =
  let s text = Keys.scrub ~known:[ key ] text in
  match error with
  | Authentication d -> Authentication (s d)
  | Rate_limited r -> Rate_limited { r with detail = s r.detail }
  | Overloaded d -> Overloaded (s d)
  | Invalid_request d -> Invalid_request (s d)
  | Not_found d -> Not_found (s d)
  | Timeout d -> Timeout (s d)
  | Refused d -> Refused (s d)
  | Server r -> Server { r with detail = s r.detail }
  | Transport d -> Transport (s d)
  | Malformed d -> Malformed (s d)
  | (Disabled | Unknown_provider _ | Unknown_model _ | Missing_key _ | Missing_cassette _) as other -> other

let live t (provider : Model_registry.provider) (model : Model_registry.model) req =
  match key_for t provider with
  | Error e -> Error e
  | Ok key ->
    let (module A : Provider.S) = adapter provider in
    let http = A.build ~provider ~model ~key req in
    (match t.transport ~timeout:t.timeout http with
     | Error detail ->
       let detail = Keys.scrub ~known:[ key ] detail in
       if Text.contains detail "did not finish" || Text.contains detail "timed out" then Error (Timeout detail)
       else Error (Transport detail)
     | Ok resp ->
       (match A.parse ~provider ~model resp with
        | Ok response -> Ok response
        | Error e -> Error (scrub_error key e)))

let complete t req =
  let started = Unix.gettimeofday () in
  let provider_label, model_label, served_by, outcome =
    match t.scripted, t.mode with
    | Some handler, _ -> ("scripted", "scripted", "scripted", handler req)
    | None, Off -> (t.provider, "", "off", Error Disabled)
    | None, Mock -> ("mock", "mock", "mock", Scripted.mock req)
    | None, ((Live | Record | Replay) as mode) ->
      (match resolve t with
       | Error e -> (t.provider, "", mode_to_string mode, Error e)
       | Ok (provider, model) ->
         let name = provider.Model_registry.name and id = model.Model_registry.id in
         (match mode with
          | Replay ->
            let hash = Cassette.hash ~provider:name ~model:id req in
            let outcome =
              match Cassette.load ~dir:t.cassette_dir hash with
              | Ok (Some cassette) -> Ok cassette.Cassette.response
              | Ok None -> Error (Missing_cassette { hash; purpose = req.purpose; provider = name; model = id })
              | Error e -> Error (Malformed ("cassette " ^ hash ^ ": " ^ e))
            in
            (name, id, "replay", outcome)
          | _ ->
            let outcome = live t provider model req in
            (match mode, outcome with
             | Record, Ok response ->
               ignore (Cassette.save ~dir:t.cassette_dir (Cassette.make ~origin:Cassette.Recorded ~provider:name ~model:id req response))
             | _ -> ());
            (name, id, "live", outcome)))
  in
  let usage = match outcome with Ok r -> r.usage | Error _ -> { input_tokens = 0; output_tokens = 0 } in
  t.account <-
    { purpose = req.purpose; provider = provider_label; model = model_label; served_by; ok = Result.is_ok outcome;
      error_class = (match outcome with Ok _ -> None | Error e -> Some (error_class e)); usage; started_at = started;
      elapsed_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) }
    :: t.account;
  outcome

let call_to_json c =
  Json.Obj
    ([ ("purpose", Json.String c.purpose); ("provider", Json.String c.provider); ("model", Json.String c.model);
       ("served_by", Json.String c.served_by); ("ok", Json.Bool c.ok) ]
     @ (match c.error_class with Some name -> [ ("error_class", Json.String name) ] | None -> [])
     @ [ ("input_tokens", Json.Int c.usage.input_tokens); ("output_tokens", Json.Int c.usage.output_tokens);
         ("started_at", Json.String (Text.iso8601 c.started_at)); ("elapsed_ms", Json.Int c.elapsed_ms) ])

let account_to_json t =
  let all = calls t in
  let sum f = List.fold_left (fun acc c -> acc + f c) 0 all in
  Json.Obj
    [ ("mode", Json.String (mode_to_string t.mode));
      ("calls", Json.Int (List.length all));
      ("failed_calls", Json.Int (List.length (List.filter (fun c -> not c.ok) all)));
      ("input_tokens", Json.Int (sum (fun c -> c.usage.input_tokens)));
      ("output_tokens", Json.Int (sum (fun c -> c.usage.output_tokens)));
      ("records", Json.List (List.map call_to_json all)) ]
