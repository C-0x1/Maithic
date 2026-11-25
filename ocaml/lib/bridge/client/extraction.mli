(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What the Bridge reads out of an elaborated file.

    The [extract] request elaborates a whole file and describes it: every
    declaration somebody wrote with its kind, type, place, documentation,
    dependencies and axioms; the imports; and on request the tactic steps
    with the goals before and after, the open [sorry] positions with their
    goals, and the findings of the lint rules. The code graph, the corpus
    and the reviewer are all built from these records.

    The same shape is a document, schema [maithic.bridge.extraction/1], so
    that an extraction can be kept and read again without Lean. *)

open Maithic_base

val schema : string

type declaration = {
  name : string;
  kind : string;  (** [theorem], [definition], [instance], [structure], [class], [inductive], [axiom] or [opaque]. *)
  type_ : string;  (** The type as Lean prints it; for a theorem, its statement. *)
  module_ : string;
  line : int;  (** Where the declaration begins, its documentation included. One-based. *)
  column : int;  (** Zero-based, in code points. *)
  end_line : int;
  end_column : int;
  name_line : int;  (** Where the name stands: the place to send a reader to. *)
  name_column : int;
  doc : string option;
  is_private : bool;
  uses_sorry : bool;  (** Rests on [sorryAx], directly or through what it uses. *)
  axioms : string list;
  type_deps : string list;  (** Constants the type refers to. *)
  value_deps : string list;  (** Constants the value refers to: for a theorem, the premises of its proof. *)
  instance_of : string option;
  parents : string list;  (** For a structure, the structures it extends. *)
  statement_hash : string;
  same_statement : string list;  (** Other theorems with the same statement, when asked for. *)
  source : string option;  (** The text of the declaration. *)
  proof : string option;  (** The text after [:=]. *)
}

type step = {
  decl : string;
  index : int;
  tactic : string;
  at_line : int;
  at_column : int;
  to_line : int;
  to_column : int;
  goals_before : string list;
  goals_after : string list;
  depth : int;  (** Zero for a step of the proof's own sequence. *)
  inner : int;  (** How many steps stand inside this one. *)
}

type hole = { hole_decl : string; hole_line : int; hole_column : int; goals : string list }
type finding = { rule : string; severity : string; declaration : string; finding_line : int; message : string }

type t = {
  file : string;  (** The path the text was read from; empty when unknown. *)
  module_name : string;
  ok : bool;  (** Whether the file elaborated without error. *)
  errors : int;
  warnings : int;
  imports : string list;
  module_docs : string list;
  declarations : declaration list;  (** In the order of the file. *)
  steps : step list;
  holes : hole list;
  findings : finding list;
}

val of_json : ?file:string -> Json.t -> (t, string) result
(** Reads the answer of the Bridge, or a stored document. *)

val to_json : t -> Json.t

val declaration_to_json : declaration -> Json.t
(** One declaration as an object; with [to_string] on one line, it is a line
    of a JSON-lines export. *)

val step_to_json : step -> Json.t

val contains_sorry : declaration -> bool
(** The [sorry] stands in the declaration itself, not in something it uses. *)

val find : t -> string -> declaration option
