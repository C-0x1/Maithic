(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Confirming a counterexample.

    A test that fails and a solver that names a point both say that a
    statement is false, and neither is believed. The values they give are
    put into the statement, and Lean is asked to accept a proof that the
    statement is false at those values. Only then is the statement called
    refuted: by Lean's kernel, on a proof anyone can read and check again.

    The refutation of a theorem [t] with the values [a := 1] and [b := 0] is

    {v
theorem _root_.maithic_refutation_of_t : ¬ type_of% @t := by
  intro h
  have h' := h (a := 1) (b := 0)
  revert h'
  decide
    v}

    It names the statement through the theorem, so the text is right
    whatever notation, variables and namespaces the statement was written
    with; it does not use the theorem, which may have no proof at all. What
    is left after the values are put in is a closed statement, hypotheses
    included, and a decision procedure settles it. *)

type assignment = (string * string) list
(** The variables of a statement with a value for each, as Lean text. *)

val closers : string list
(** The tactics that are tried on the closed statement, in order: [decide],
    [decide +kernel], [omega], [grind] and [simp]. *)

val name_of : string -> string
(** The name under which the refutation of a theorem is stated. *)

val text : theorem:string -> closer:string -> assignment -> string
(** The refutation as Lean text. *)

val values : assignment -> string
(** The part of the text that comes from outside Maithic: the values, which
    go through the guard of the Bridge before the text is elaborated. *)

type checked = {
  accepted : bool;  (** Lean elaborated the text without error and the kernel took the theorem. *)
  uses_sorry : bool;
  axioms : string list;
  error : string;  (** What Lean said when it did not accept the text. *)
}

type confirmation = {
  closer : string;
  lean : string;  (** The text Lean accepted. *)
  axioms : string list;  (** The axioms the refutation rests on. *)
}

val confirm : check:(untrusted:string -> string -> checked) -> theorem:string -> assignment -> (confirmation, string) result
(** Tries the refutation with each closer until Lean accepts one. [check]
    is given the text and, as [untrusted], the part of it that came from
    outside ({!values}), for the guard of the Bridge. A proof
    that rests on [sorry] is not a confirmation: with the statement declared
    and unproved, a tactic that uses it would prove anything. The error says
    why the last attempt was not accepted. *)

val to_json : confirmation -> Maithic_base.Json.t
