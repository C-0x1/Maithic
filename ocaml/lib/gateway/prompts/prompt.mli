(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Prompts as versioned data.

    A prompt is a file with a system part and a user part and named
    placeholders. The built-in prompts are compiled in; [config/prompts/]
    holds the same text as files, and a project can override any prompt by
    pointing [gateway.prompts] at a directory with a file of the same name.

    File format: a line [--- system ---] starts the system part and a line
    [--- user ---] the user part; [{{name}}] is a placeholder; the line
    [{{> policy}}] includes the shared policy block. Text supplied for a
    placeholder is data: it is inserted verbatim and never read as template
    syntax. *)

type t = { name : string; version : int; system : string; user : string }

val builtin : (string * string) list
(** File name and text of every built-in prompt file. *)

val names : string list

val load : ?dir:string -> string -> (t, string) result
(** [load name] reads a prompt, from [dir] when it holds a file
    [name ^ ".md"], otherwise from the built-in set. *)

val parse : name:string -> policy:string -> string -> (t, string) result

val placeholders : t -> string list
(** The placeholder names used by a prompt, in order of first use. *)

val render : t -> (string * string) list -> (string * string, string) result
(** [render prompt bindings] is the system and user text with every
    placeholder replaced. A placeholder without a binding, or a binding
    without a placeholder, is an error. Values are inserted as they are,
    except that the markers which fence data off are defused inside them;
    see {!neutralize}. *)

val neutralize : string -> string
(** A value in which the data markers can no longer open or close a fence:
    [<data>] and [</data>], in any case, are written with a backslash after
    the angle bracket. Everything else is unchanged. *)
