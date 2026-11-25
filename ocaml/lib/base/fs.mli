(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** File-system helpers. Paths use forward slashes; results are [result]
    values so that callers decide how a failure is reported. *)

val read : string -> (string, string) result
(** Whole contents of a file, in binary mode. *)

val write : string -> string -> (unit, string) result
(** Creates parent directories as needed and replaces the file atomically:
    the contents are written to a temporary file in the same directory that
    is then renamed over the target. *)

val exists : string -> bool
val is_dir : string -> bool
val mkdir_p : string -> (unit, string) result

val list_dir : string -> string list
(** Sorted entries of a directory; empty when it cannot be read. *)

val walk : ?skip:(string -> bool) -> string -> string list
(** [walk root] lists every regular file below [root], as sorted paths
    relative to [root]. [skip] is called with each relative path of a
    directory or file; returning [true] leaves it out. Symbolic links are
    not followed. *)

val remove : string -> unit
(** Removes a file if it exists. *)

val temp_dir : string -> string
(** A fresh directory, private to the user, under the system temporary
    directory. *)

val remove_tree : string -> unit
(** Removes a file or a directory with everything below it. *)

val ( / ) : string -> string -> string
(** Joins two path segments with one slash. *)

val extension : string -> string
(** The extension with its dot, lowercased, or [""]. *)

val basename : string -> string
val dirname : string -> string
