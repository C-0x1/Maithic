(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The AI gateway: the one door through which every model call passes.

    Callers build a unified request and call {!complete}. The gateway picks
    the provider and model from the configuration, enforces the mode, finds
    the key at the last moment, runs the adapter and keeps an account of
    every call. In [replay] mode it never touches the network. *)

open Maithic_base
open Maithic_schema
open Gateway_types

type mode =
  | Live  (** call the provider *)
  | Record  (** call the provider and save a cassette *)
  | Replay  (** answer from cassettes only; a missing cassette is an error *)
  | Mock  (** deterministic canned answers, no provider *)
  | Off  (** AI disabled: every call fails with [Disabled] *)

val mode_of_string : string -> mode option
val mode_to_string : mode -> string

type t

val create :
  ?transport:Transport.t -> ?scripted:Scripted.handler -> ?getenv:(string -> string option) ->
  ?explicit_key:string -> Maithic_config.Config.t -> (t, Fault.t) result
(** Builds a gateway from the configuration. [scripted] replaces every
    provider by a handler defined in a test. [transport] replaces [curl]. *)

val mode : t -> mode
val provider_name : t -> string

val model_id : t -> (string, error) result
(** The model the configuration resolves to. *)

val ready : t -> (unit, error) result
(** Whether a call could be attempted now: the mode is not [off], the
    provider and model resolve and, in [live] and [record] mode, a key is
    configured. Makes no call. *)

val complete : t -> request -> (response, error) result

val prompt : t -> string -> (Prompt.t, string) result
(** Loads a prompt, honouring the [gateway.prompts] override directory. *)

(** One entry of the account: what was asked, who answered, at what cost. *)
type call = {
  purpose : string;
  provider : string;
  model : string;
  served_by : string;  (** ["live"], ["replay"], ["mock"] or ["scripted"] *)
  ok : bool;
  error_class : string option;  (** the kind of failure, as {!Gateway_types.error_class} names it *)
  usage : usage;
  started_at : float;  (** seconds since the epoch *)
  elapsed_ms : int;
}

val calls : t -> call list
(** Every call made through this gateway, oldest first. *)

val call_to_json : call -> Json.t
(** One usage record: purpose, provider, model, who served it, whether it
    succeeded and how it failed, tokens, start time (UTC) and duration. No
    prompt, no answer and no key is part of a record. *)

val account_to_json : t -> Json.t
(** Call count, token totals and the usage records, for result manifests. *)
