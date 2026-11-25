(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** JSON values, a strict parser and two printers.

    Every contract between Maithic components is JSON, so this module is the
    one place where JSON is read and written on the OCaml side. The parser
    accepts exactly RFC 8259. The compact printer can emit a canonical form
    (object members sorted by key) that is used wherever a value is hashed. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Obj of (string * t) list

(** {1 Reading} *)

val parse : string -> (t, string) result
(** [parse s] reads one JSON value that spans the whole of [s]. The error
    message names the byte offset of the problem. *)

val parse_exn : string -> t
(** Like {!parse}. @raise Failure on malformed input. *)

(** {1 Writing} *)

val to_string : ?canonical:bool -> t -> string
(** Compact, single-line form. With [~canonical:true] object members are
    sorted by key at every level, so equal values print identically. *)

val pretty : t -> string
(** Indented form for people, two spaces per level, ending without a newline. *)

(** {1 Access} *)

val member : string -> t -> t option
(** [member k v] is the value of member [k] when [v] is an object that has it. *)

val path : string list -> t -> t option
(** [path [k1; k2] v] follows members from the outside in. *)

val string_opt : t -> string option
val int_opt : t -> int option
val float_opt : t -> float option
(** An integer value is accepted as a float. *)

val bool_opt : t -> bool option
val list_opt : t -> t list option
val obj_opt : t -> (string * t) list option

(** {1 Decoding with error messages} *)

val get_string : string -> t -> (string, string) result
val get_int : string -> t -> (int, string) result
val get_bool : string -> t -> (bool, string) result
val get_list : string -> t -> (t list, string) result
(** [get_x key v] reads member [key] of object [v]; the error names the key. *)

val string_or : string -> string -> t -> string
(** [string_or default key v] is the string member [key], or [default]. *)

val int_or : int -> string -> t -> int
val bool_or : bool -> string -> t -> bool
val list_or_empty : string -> t -> t list
val strings : t -> string list
(** The string elements of a list value; anything else is skipped. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** Structural equality that ignores the order of object members and treats
    [Int n] and [Float (float n)] as equal. *)

val is_subset : expected:t -> t -> (unit, string) result
(** [is_subset ~expected actual] checks that every member named in
    [expected] is present in [actual] with a matching value, that lists have
    equal length and match element by element, and that scalars are equal.
    An expected object [{"$contains": "text"}] matches any string containing
    the text. The error is the path of the first mismatch. Used by fixtures
    that pin part of a response. *)
