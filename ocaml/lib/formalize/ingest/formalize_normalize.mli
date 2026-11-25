(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** One spelling for each piece of mathematics.

    Informal mathematics arrives in many spellings. A minus is a hyphen or
    the minus sign of Unicode; "at most" is [≤], [≦], [⩽], [<=] or [\le]; a
    square is [x²], [x^2] or [x^{2}]; a Greek letter is the letter, one of
    its variant shapes, or a command of LaTeX. A model reads all of them,
    and a program that compares two texts does not. This module rewrites a
    text into one spelling: operators as the symbols Lean uses, superscripts
    and subscripts as [^] and [_], Greek letters as themselves, the commands
    of LaTeX as what they stand for, and the dollar signs of LaTeX and the
    marks of Markdown removed.

    Every change is recorded with the place it was made, so that a position
    in the normalized text can be traced back to the text as it was given. *)

type change = {
  start : int;  (** The first byte of the stretch in the text as given. *)
  stop : int;  (** The byte after its last. *)
  given : string;  (** What stood there. *)
  written : string;  (** What stands for it in the normalized text. *)
}

type t = {
  source : string;  (** The text as given. *)
  text : string;  (** The normalized text. *)
  changes : change list;  (** In the order of the text. *)
}

val normalize : string -> t

val text : string -> string
(** The normalized text alone. A text in the spelling this module writes is left as it is. *)

val source_offset : t -> int -> int
(** The byte of the text as given that a byte of the normalized text comes
    from: the start of the change it lies in, or the byte that was copied. *)

val source_end : t -> int -> int
(** The byte of the text as given after what a byte of the normalized text
    comes from: the end of the change it lies in, or the byte after the one
    that was copied. With {!source_offset} it gives the stretch of the text
    as given that a stretch of the normalized text stands for. *)

val symbols : (string * string) list
(** The symbols that are rewritten, each with what it becomes. *)

val commands : (string * string) list
(** The commands of LaTeX that are rewritten, without their backslash, each with what it becomes. *)
