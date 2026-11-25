(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** From a problem in words to a statement in Lean.

    {v
problem ─▶ intake ─▶ draft several ─▶ compile, repair ─▶ drop what says the same
                                                          ─▶ check each ─▶ rank ─▶ choose
    v}

    A model drafts several statements. Lean compiles each; one that does
    not compile is repaired by the model within a bound, with Lean's error
    before it. Of two statements that are the same once notation and
    definitions are unfolded, the later is dropped. Each of the rest is
    checked: Lean looks for a contradiction
    among its hypotheses and for a counterexample, and a statement with
    either is rejected; the other checks flag and score. The statement with
    the best score is chosen.

    What comes out is a statement, not a theorem: nothing here proves it. And
    that it says what the problem said rests on evidence, which the report
    lays out in full, so that a person can decide. *)

open Maithic_base

(** What a run needs from outside. Each is a function, so that a run can be
    made without Lean and without a model. *)
type tools = {
  draft : count:int -> Formalize_intake.t -> (Formalize_candidate.t list, string) result;
  compile : Formalize_elab.compile;
  repair : Formalize_elab.repair option;  (** Without it, a statement that does not compile is left. *)
  lean : Formalize_faith.lean;
  model : Formalize_faith.model option;  (** Without it, nothing is translated back. *)
  same : Formalize_candidate.t -> Formalize_candidate.t -> string option;
      (** How two statements are related, when Lean shows them to be one statement: [identical], or [definitional] when they
          are the same once notation and definitions are unfolded. *)
}

type settings = {
  candidates : int;  (** Statements asked for. *)
  rounds : int;  (** Repair rounds for a statement that does not compile. *)
  name : string;  (** The name the theorem is given. *)
  imports : string list;  (** The modules a statement may use, besides the core library. *)
}

val default_settings : settings
(** Three candidates, two repair rounds, the name [problem], no import. *)

type verdict =
  | Chosen
  | Kept  (** It passed, and another scored better. *)
  | Duplicate of int * string  (** Lean showed it to be the statement of an earlier candidate: that candidate, and how they are related. *)
  | Rejected of string  (** Vacuous or refuted; why. *)
  | Failed of string  (** It did not elaborate; why the repair ended. *)

val verdict_name : verdict -> string
(** [chosen], [kept], [duplicate], [rejected] or [failed]. *)

type entry = {
  drafted : Formalize_candidate.t;  (** The candidate as the model drafted it. *)
  elaboration : Formalize_elab.result;
  faith : Formalize_faith.t option;  (** The checks, when they were made. *)
  verdict : verdict;
  rank : int option;  (** One for the chosen statement; none for a candidate that is out. *)
}

type status =
  | Formalized  (** A statement was chosen, and no check flagged it. *)
  | Flagged  (** A statement was chosen, and something about it deserves a second look. *)
  | Nothing  (** No candidate was left. *)

val status_name : status -> string
(** [formalized], [flagged] or [none]. *)

type t = { intake : Formalize_intake.t; settings : settings; entries : entry list; status : status; note : string }

val given : name:string -> imports:string list -> string list -> Formalize_candidate.t list
(** Statements a person wrote, as candidates: each once, in their order. *)

val run : ?settings:settings -> tools -> Formalize_intake.t -> (t, string) result
(** Makes the run. The error is why no statement could be drafted at all. *)

val chosen : t -> entry option

val final : entry -> Formalize_candidate.t
(** The candidate of an entry as it stood at the end of its elaboration. *)

val counts : t -> (string * int) list
(** How many candidates were [drafted], [elaborated_first_try], [repaired],
    [elaborated], [failed], [duplicates], [rejected] and [kept]. *)

val lean_text : t -> string option
(** The chosen statement as a Lean file: its imports, a comment with the
    problem and what the checks found, and the theorem with [sorry] for a
    proof. *)

val schema : string
(** [maithic.formalize.run/1]. *)

val to_json : t -> Json.t

val render : ?color:bool -> t -> string
(** The run for a terminal: the problem, each candidate with what became of
    it, and the chosen statement. *)

val summary : t -> string
(** The run in a line. *)

val exit_code : t -> Maithic_schema.Exit_code.t
(** [Success] for a statement that no check flagged; [Not_verified] otherwise. *)
