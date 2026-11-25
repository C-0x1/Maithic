(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Making proofs shorter.

    A proof that grew by trial keeps what the trials left behind: a step
    that does nothing, a lemma handed to [simp] that [simp] never uses, a
    [have] nobody refers to. This module takes such things out. It removes
    one step, or one argument of a lemma list, gives the theorem with the
    shorter proof to Lean, and keeps the removal if Lean accepts it; then it
    goes on from the shorter proof, until nothing more can go.

    A removal is kept only if the theorem still states what it stated and
    rests on no axiom it did not rest on before. On request the whole proof
    is first tried as a single call of automation, under the same
    conditions.

    The result is a proof Lean has checked, with what was removed and the
    size before and after. *)

open Maithic_base
open Maithic_prover

type metrics = {
  lines : int;  (** Lines of the proof that are not blank. *)
  steps : int;  (** Of these, the lines that are tactics: all but a [by] on a line of its own. *)
  chars : int;  (** Characters, in code points. *)
}

val measure : string -> metrics

type result = {
  theorem : string;
  line : int;
  before : string;  (** The proof as it stood. *)
  after : string;  (** The proof that is left; the same text when nothing could go. *)
  size_before : metrics;
  size_after : metrics;
  how : string;  (** [automation], [deletion] or [unchanged]. *)
  removed : string list;  (** What was taken out, in the order it went: steps, and arguments named with the call they stood in. *)
  checks : int;  (** Proofs given to Lean, the first check of the proof as it stood included. *)
  axioms : string list;  (** The axioms the theorem rests on, before and after. *)
  note : string;  (** Why nothing was tried, when nothing was: empty otherwise. *)
}

val automation : string list
(** The single calls that are tried in place of a whole proof, on request. *)

val candidates : string -> (string * string) list
(** The proofs one removal away from a proof, each with what the removal
    takes out: steps from the last to the first, then arguments of lemma
    lists from the last to the first. A proof that is a term has none. *)

val keeps : reference:Mend_check.verdict -> Mend_check.verdict -> bool
(** Whether a shorter proof may stand in for the one that gave [reference]:
    Lean accepts it, it does not rest on [sorry], the statement is the same,
    and every axiom it rests on is one the reference rested on. *)

val minimize : ?automation:bool -> ?max_checks:int -> check:Mend_check.t -> Lean_source.decl -> result
(** Shortens the proof of a theorem. [max_checks] bounds the proofs given to
    Lean: two hundred by default. *)

val changed : result -> bool

val result_to_json : result -> Json.t

val schema : string
(** [maithic.mend.minimization/1]. *)

val to_json : file:string -> result list -> Json.t
(** The document of a run: the file, a summary and the results. *)

val render : ?color:bool -> file:string -> result list -> string
