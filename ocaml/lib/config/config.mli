(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** One layered, validated configuration for the whole product.

    Settings come from six layers, later ones winning: built-in defaults,
    the system file, the user file, the project file [maithic.toml],
    [MAITHIC_*] environment variables and command-line flags. Every setting
    has a documented key, an environment variable, a type and a default, and
    the loader remembers which layer each effective value came from. *)

open Maithic_base
open Maithic_schema

type kind = String | Path | Int | Float | Bool | Enum of string list

type setting = { key : string; env : string; kind : kind; default : string; doc : string }

val settings : setting list
(** The complete table of settings: the single source for validation, for
    [maithic config] and for the generated configuration reference. *)

type source = Default | File of string | Environment of string | Flag

val source_to_string : source -> string

type t
(** A loaded configuration: effective values with their sources. *)

val load :
  ?cwd:string -> ?getenv:(string -> string option) -> ?flags:(string * string) list -> ?files:string list -> unit ->
  (t, Fault.t) result
(** Loads and validates all layers. [flags] maps setting keys to values.
    [files] replaces the discovered configuration files, lowest precedence
    first; by default they are the system file, the user file and the
    project file found by {!find_workspace}. A file that does not exist is
    skipped. An unknown key, a value of the wrong type or a value outside an
    enumeration is an error that names the file or variable, and suggests
    the nearest known key. *)

val of_defaults : unit -> t

(** {1 Typed access} *)

val string : t -> string -> string
val int : t -> string -> int
val float : t -> string -> float
val bool : t -> string -> bool
(** [string t key] and friends read an effective value. The key must be in
    {!settings}. @raise Invalid_argument on an unknown key. *)

val string_opt : t -> string -> string option
(** [None] when the value is empty. *)

val source : t -> string -> source
val workspace : t -> string option
(** The directory of the project file, when one was found. *)

val resolve_path : t -> string -> string
(** Makes a path setting absolute: relative paths are relative to the
    workspace, or to the current directory when there is none. *)

(** {1 Discovery} *)

val find_workspace : ?getenv:(string -> string option) -> string -> string option
(** [find_workspace dir] is the nearest directory at or above [dir] that
    contains [maithic.toml], or the directory named by [MAITHIC_WORKSPACE]. *)

val home_dir : (string -> string option) -> string
(** [MAITHIC_HOME], or [$XDG_CONFIG_HOME/maithic], or [~/.config/maithic]. *)

(** {1 Display} *)

val to_json : t -> Json.t
(** Effective values with their sources, for [maithic config show --json]. *)

val reference_markdown : unit -> string
(** The configuration reference, generated from {!settings}. *)

val nearest : string -> string list -> string option
(** The candidate closest to a word by edit distance, when it is close
    enough to be a plausible misspelling. *)
