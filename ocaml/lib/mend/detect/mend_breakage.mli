(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The record of one breakage.

    A breakage is a declaration that Lean reports a problem with: the
    declaration, what Lean said, the class of the problem, and the text the
    problem is in. Everything a repair needs is in the record, so a repair
    can be planned without asking Lean again. *)

open Maithic_base
open Maithic_bridge

type t = {
  file : string;
  declaration : string;  (** The theorem the problem is in; empty when it is outside every theorem. *)
  kind : string;  (** [theorem] or [lemma]; empty outside a theorem. *)
  line : int;  (** The line of the first diagnostic, one-based. *)
  column : int;  (** Its column, zero-based, in code points. *)
  severity : string;  (** [error], [warning] or [info]: that of the first diagnostic. *)
  class_ : Mend_class.t;  (** The class of the first diagnostic. *)
  error : string;  (** The first line of what Lean said. *)
  diagnostics : Bridge_types.diagnostic list;
      (** Every diagnostic of the declaration that reports a breakage, errors first, each kind in the order of the file. *)
  context : string;  (** The text of the declaration, or the line the diagnostic is on. *)
  first_line : int;  (** The line of the file the context begins on. *)
}

val key : t -> string
(** What makes a breakage the same breakage when the file is checked again:
    its declaration, or its line outside a theorem, with its class and the
    detail of the class. *)

val classes : t -> Mend_class.t list
(** The classes of all its diagnostics, in their order. *)

val has_error : t -> bool

val to_json : t -> Json.t
(** The record as a member of a report: file, declaration, kind, line,
    column, severity, class, detail, error, the messages of the diagnostics,
    and the context with the line it begins on. *)

val describe : t -> string
(** One line: where, what class, what Lean said. *)
