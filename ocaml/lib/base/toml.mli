(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A TOML reader.

    Configuration, the model registry, policies and the capability register
    are TOML files. The reader covers TOML 1.0 except date and time values,
    which are returned as strings: tables, arrays of tables, dotted and
    quoted keys, the four string forms, integers in four bases, floats,
    booleans, arrays and inline tables.

    A document is returned as a {!Json.t} object, so that TOML and JSON data
    are handled by the same accessors. Tables keep the order of the file. *)

val parse : string -> (Json.t, string) result
(** [parse text] reads a document. The error message names the line. *)

val parse_file : string -> (Json.t, string) result
(** Reads and parses a file; the error message names the file. *)

val escape_string : string -> string
(** [escape_string s] is [s] as a TOML basic string, with its quotes. *)
