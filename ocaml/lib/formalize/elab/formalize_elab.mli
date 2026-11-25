(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Compiling statements, and repairing the ones that do not compile.

    A drafted statement is given to Lean. If Lean elaborates it, that is
    the end of it here. If not, what Lean said is put in a class, and a
    model is shown the statement, the error and the signatures of the
    library names the statement got right, and asked to repair it. The
    repaired statement is compiled in turn. The loop ends with the first
    statement Lean accepts, or after a bounded number of rounds, or when the
    model repeats itself.

    Elaboration says that a statement is well formed. Whether it says what
    the problem said is another question, asked elsewhere. *)

open Maithic_base

type compiled = {
  ok : bool;  (** Lean elaborated the statement. *)
  statement : string;  (** The statement as Lean prints it; empty when it did not elaborate. *)
  said : string;  (** What Lean, or the guard, said when it did not. *)
}

type compile = Formalize_candidate.t -> compiled

type repair = candidate:Formalize_candidate.t -> error:Formalize_error.t -> said:string -> failures:(string * string) list -> (string * Formalize_candidate.provenance, string) result
(** Asks for a repaired statement, given the candidate, the class of its
    error, what Lean said, and the statements that failed before it with
    what Lean said about each. Returns the new statement and who wrote it. *)

type attempt = {
  round : int;  (** Zero for the statement as drafted. *)
  signature : string;
  elaborated : bool;
  error_class : string;  (** Empty when it elaborated. *)
  said : string;
}

type result = {
  candidate : Formalize_candidate.t;  (** The statement at the end: the one that elaborated, or the last that was tried. *)
  elaborated : bool;
  statement : string;  (** As Lean prints it; empty when nothing elaborated. *)
  history : attempt list;  (** Every statement that was compiled, in order. *)
  note : string;  (** Why the loop ended without a statement, when it did. *)
}

val default_rounds : int
(** Two repair rounds for a candidate. *)

val run : ?rounds:int -> compile:compile -> ?repair:repair -> Formalize_candidate.t -> result

val first_try : result -> bool
(** The statement elaborated as it was drafted. *)

val repairs : result -> int
(** How many repaired statements were compiled. *)

val to_json : result -> Json.t
(** The record of one candidate's elaboration: its status, the statement as
    Lean prints it, whether it elaborated at the first try, and the history. *)
