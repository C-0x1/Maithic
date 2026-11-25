(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Does a statement that compiles say what the problem said?

    Lean accepts a statement that is well formed, whatever it claims. This
    module asks the questions that tell a faithful statement from one that
    only compiles, the cheapest and surest first.

    - {b Vacuity.} Hypotheses that contradict each other make a statement
      true of nothing. Lean proves the contradiction; the statement is
      rejected.
    - {b Counterexample.} A statement that is false is tested on sampled
      values, and a failing case is confirmed by a proof Lean checks. A
      problem that asks for a proof is not formalized by a false statement;
      it is rejected.
    - {b Triviality.} A statement that [trivial], [rfl] or [simp] closes at
      once says less than a problem worth posing. It is flagged.
    - {b Numbers and variables.} A number of the problem that the statement
      never mentions, and a variable of the statement that nothing mentions,
      are flagged, and so is an operation that does not mean in Lean what
      its symbol means on paper.
    - {b Back-translation.} A model that sees the statement and nothing else
      says in English what it claims.
    - {b Round trip.} A second call compares that English with the problem,
      scores their agreement from 0 to 10 and lists the differences.

    The first two checks rest on proofs and reject. The others are evidence:
    they flag, and they move a score by which the candidates are ranked. No
    model decides alone whether a statement is right. *)

open Maithic_base

type inspection = {
  unused : string list;  (** Variables, types and instances of the statement that nothing mentions. *)
  traps : (string * string * string) list;  (** Kind, term and note of each operation that does not mean what its symbol suggests. *)
  annotated : string;  (** The statement with the type of every numeral and coercion. *)
}

type refutation =
  | Refuted of (string * string) list  (** A counterexample, confirmed by a proof Lean accepted: the variables and their values. *)
  | Survived of string  (** Nothing was found; what was tried, such as [300 sampled cases]. *)

type lean = {
  inspect : Formalize_candidate.t -> (inspection, string) result;
  vacuous : Formalize_candidate.t -> (string option, string) result;  (** What makes the statement vacuous, when it is. *)
  refute : Formalize_candidate.t -> (refutation, string) result;  (** An error when the statement could not be tested; why. *)
  closes : Formalize_candidate.t -> string list -> string option;  (** The first of the proofs that Lean accepts for the statement. *)
}

type model = {
  back : statement:string -> (string, string) result;  (** The statement in English. *)
  judge : problem:string -> english:string -> (int * string list, string) result;  (** Agreement from 0 to 10, and the differences. *)
}

type check = {
  name : string;  (** [vacuity], [counterexample], [triviality], [numbers], [variables], [traps], [back-translation] or [round-trip]. *)
  outcome : string;  (** [passed], [failed], [flagged] or [skipped]. *)
  note : string;  (** What was found, or why the check was skipped. *)
}

type t = {
  checks : check list;  (** In the order they were made. *)
  english : string;  (** The back-translation; empty when there is none. *)
  agreement : int option;  (** The score of the round trip, from 0 to 10. *)
  differences : string list;
  rejected : string option;  (** Why the statement was rejected, when it was. *)
  score : float;  (** Between 0 and 1; 0 for a rejected statement. *)
}

val check_names : string list

val trivial_proofs : string list
(** The proofs that mark a statement as trivial: [by trivial], [by rfl] and [by simp]. *)

val agreement_threshold : int
(** The lowest score of the round trip that passes: 8 of 10. *)

val missing_numbers : Formalize_intake.t -> string -> string list
(** The numbers of a problem that a statement does not mention. *)

val flagged : t -> string list
(** The names of the checks that flagged something. *)

val validate : ?model:model -> lean:lean -> Formalize_intake.t -> Formalize_candidate.t -> statement:string -> t
(** Makes the checks on a statement that elaborated. [statement] is the
    statement as Lean prints it, which is what the model is shown. Without a
    model the two checks that need one are skipped. A statement that is
    rejected is not shown to the model at all. *)

val score : agreement:int option -> check list -> float

val to_json : t -> Json.t
