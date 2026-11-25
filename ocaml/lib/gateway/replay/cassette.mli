(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Cassettes: stored model exchanges that make AI behaviour reproducible.

    A cassette is one JSON file holding the canonical form of a request, the
    response it received and where the response came from. Its file name is
    the SHA-256 digest of the canonical request, so a lookup needs no index
    and two equal requests can never disagree. Cassettes contain no key and
    no header of any kind. *)

open Maithic_base
open Gateway_types

val canonical : provider:string -> model:string -> request -> Json.t
(** The normalized request: provider, model, purpose, system prompt,
    messages, output limit, temperature and schema, with nothing that
    depends on time, machine or credentials. *)

val hash : provider:string -> model:string -> request -> string
(** SHA-256 of the canonical request printed in canonical JSON. *)

type origin =
  | Recorded  (** captured from a live provider call *)
  | Authored  (** written by hand as a test fixture *)

type t = {
  hash : string;
  origin : origin;
  request : Json.t;  (** the canonical request *)
  response : response;
  note : string;
}

val make : ?note:string -> origin:origin -> provider:string -> model:string -> request -> response -> t
val path : dir:string -> string -> string
(** [path ~dir hash] is the file of a cassette. *)

val to_json : t -> Json.t
val of_json : Json.t -> (t, string) result

val load : dir:string -> string -> (t option, string) result
(** The cassette with the given hash; [Ok None] when there is none. *)

val save : dir:string -> t -> (unit, string) result
(** Writes a cassette after scrubbing key-shaped text from every string. *)

val lint : t -> string list
(** Problems with a cassette: a hash that does not match its request, a
    missing response text, or key-shaped text anywhere. Empty when clean. *)

val lint_dir : string -> (string * string list) list
(** Lints every cassette file of a directory; lists only files with problems,
    including files that are not cassettes. *)
