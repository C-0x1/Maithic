(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The model registry: which providers exist, how each is reached, and
    which model each quality tier means.

    The registry is data. The built-in one is the text of
    [config/models.toml]; a project can point [gateway.models] at its own
    file. Model identifiers change often, so nothing else in the code names
    a model. *)

open Maithic_base

type api =
  | Messages  (** the Anthropic Messages API *)
  | Responses  (** the Responses API of OpenAI and xAI *)
  | Chat_completions  (** the chat-completions shape, for local servers *)

type auth = Bearer | X_api_key | No_auth

type provider = {
  name : string;
  api : api;
  endpoint : string;
  key_env : string;  (** the environment variable that holds the key; empty for none *)
  auth : auth;
  version : string;  (** the [anthropic-version] header; empty elsewhere *)
  headers : (string * string) list;  (** extra headers, never credentials *)
  max_tokens_field : string;  (** name of the output-limit member of the request *)
  tiers : (string * string) list;  (** tier name to model identifier *)
}

type model = {
  id : string;
  provider : string;
  temperature : bool;  (** the model accepts a sampling temperature *)
  structured : bool;  (** the model accepts a JSON schema for its output *)
}

type t = { providers : provider list; models : model list }

val builtin_toml : string
(** The text of the built-in registry; identical to [config/models.toml]. *)

val builtin : t

val of_toml : Json.t -> (t, string) result
val load : string -> (t, string) result

val provider : t -> string -> provider option

val resolve : t -> provider:string -> tier:string -> model:string option -> (provider * model, string) result
(** The provider entry and the model to use: the explicit model when given,
    otherwise the model of the tier. A model that is not listed is assumed
    to accept structured output and no temperature. *)

val api_to_string : api -> string
val to_json : t -> Json.t
