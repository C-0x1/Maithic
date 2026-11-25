(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The intake record: an informal problem, read once.

    A problem arrives as text: a sentence on the command line, a file, or
    standard input, in plain words, Markdown or LaTeX. Before a model sees
    it, it is read by a program. The text is normalized to one spelling, and
    the things a formal statement will have to account for are picked out:
    the formulas, the numbers, the symbols that stand for variables, the
    words that quantify ("for every", "there is"), the words that constrain
    ("at least", "even", "positive") and what is asked ("prove", "find").
    Each is an element with its place in the text as it was given.

    The elements do two jobs. They are the frame a model is asked to fill,
    and they are what a statement is later held against without a model: a
    number of the problem that the statement never mentions is a reason to
    look again. *)

open Maithic_base

type kind =
  | Formula  (** A stretch of mathematics: between dollar signs, or a run of symbols. *)
  | Number  (** A numeral. *)
  | Symbol  (** A letter that stands for a variable. *)
  | Quantifier  (** Words that quantify: "for all", "every", "there exists", "some". *)
  | Constraint  (** Words that constrain: "positive", "even", "at least", "distinct". *)
  | Task  (** What is asked: "prove", "show", "find", "determine". *)

type element = {
  kind : kind;
  text : string;  (** The element in the normalized spelling. *)
  start : int;  (** Its first byte in the text as given. *)
  stop : int;  (** The byte after its last. *)
}

type t = {
  origin : string;  (** Where the text came from: [argument], [stdin], or the path of a file. *)
  source : string;  (** The text as given. *)
  text : string;  (** The text in one spelling. *)
  elements : element list;  (** In the order of the text. *)
}

val kind_name : kind -> string
(** [formula], [number], [symbol], [quantifier], [constraint] or [task]. *)

val of_text : ?origin:string -> string -> (t, string) result
(** Reads a problem. The text must be valid UTF-8 and hold something. *)

val read : ?stdin:(unit -> string) -> argument:string option -> file:string option -> unit -> (t, string) result
(** Takes the problem from exactly one place: the argument, the file, or,
    when neither is given or the file is [-], standard input. *)

val of_kind : t -> kind -> string list
(** The texts of the elements of one kind, each once, in the order of the text. *)

val numbers : t -> string list
val symbols : t -> string list

val task : t -> string
(** What is asked, in a word: [prove] when the text asks for a proof or asks
    nothing, [find] when it asks for a value. *)

val frame : t -> string
(** The elements as the frame a model is given: one line for each kind that occurs. *)

val to_json : t -> Json.t
(** The record: origin, source, text, and the elements with their kinds and places. *)
