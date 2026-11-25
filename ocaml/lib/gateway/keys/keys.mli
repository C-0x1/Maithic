(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Bring-your-own-key handling that never leaks.

    A key is looked up at the moment of a call and handed to the transport;
    it is never stored in a configuration record, never logged and never
    written to a cassette. Precedence: an explicit key passed by the caller,
    then the provider's environment variable, then the credentials file in
    the Maithic home directory, which is readable by its owner only. *)

type source = Explicit | Environment of string | Credentials_file of string

val source_to_string : source -> string

val lookup :
  ?explicit:string -> getenv:(string -> string option) -> home:string -> Model_registry.provider -> (string * source) option
(** The key for a provider and where it came from. A provider that needs no
    key yields the empty string. *)

val credentials_path : home:string -> string

val store : home:string -> provider:string -> string -> (unit, string) result
(** Saves a key in the credentials file, creating it with owner-only
    permissions. An empty key removes the entry. *)

val redact : string -> string
(** A display form that proves which key is meant without revealing it: the
    last four characters and the length. *)

val looks_like_key : string -> bool
(** [true] for a token with the shape of a provider API key. Used by the
    cassette linter and by the repository secret scan. *)

val find_key_shaped : string -> string list
(** Every key-shaped token in a text. *)

val scrub : ?known:string list -> string -> string
(** Replaces every occurrence of the [known] keys, and every key-shaped
    token, by ["[redacted]"]. Applied to anything that is logged, shown in an
    error or written to disk. *)

type status = { provider : string; variable : string; configured : bool; source : source option; shown : string }

val status : getenv:(string -> string option) -> home:string -> Model_registry.t -> status list
(** For each provider: whether a key is configured and from where, with the
    redacted display form. Never the key. *)
