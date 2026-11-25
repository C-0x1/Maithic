(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** String helpers shared by every library. All functions are byte-based
    unless they say otherwise; inputs are assumed to be UTF-8. *)

val starts_with : prefix:string -> string -> bool
val ends_with : suffix:string -> string -> bool

val find_sub : ?from:int -> string -> string -> int option
(** [find_sub hay needle] is the offset of the first occurrence of [needle]. *)

val contains : string -> string -> bool
(** [contains hay needle]. *)

val split_on : string -> string -> string list
(** [split_on sep s] splits on a non-empty separator string. *)

val replace_all : sub:string -> by:string -> string -> string
val lines : string -> string list
(** Splits on line feeds; a trailing newline does not produce an empty last line. *)

val trim : string -> string
val is_blank : string -> bool
val lowercase : string -> string
val concat_map : string -> ('a -> string) -> 'a list -> string

val utf8_length : string -> int
(** Number of code points, used to align terminal output. *)

val pad_right : int -> string -> string
(** Pads with spaces to a width counted in code points. *)

val truncate : int -> string -> string
(** Shortens to at most [n] code points, ending in an ellipsis when cut. *)

val truncate_words : int -> string -> string
(** Shortens to the first words that fit in [n] code points, ending in an
    ellipsis set apart by a space when words were left out. A text of one long
    word is cut as [truncate] cuts it. *)

val indent : int -> string -> string
(** Prefixes every line with [n] spaces. *)

val is_valid_utf8 : string -> bool

val hex_of_bytes : string -> string

val iso8601 : float -> string
(** A time in seconds since the epoch as [2026-11-22T21:30:05Z]: UTC, whole
    seconds, the form used for every timestamp Maithic writes. *)
