(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The synthesis: the answer of a council, built from what Lean confirmed.

    At the end of a session the board holds everything that was said: the
    proofs that failed, the ideas, the diagnoses. The answer is not a summary
    of that. It is assembled from components that each rest on evidence: the
    claim with its status, the proof Lean accepted, the lemmas of the library
    that proof uses and Lean confirmed to exist, or the counterexample Lean
    confirmed. Everything else is set aside and counted, so that a reader can
    see how much was said and how little of it the answer needs. When no
    proof was accepted the answer says that, and contains no proof. *)

open Maithic_base

type t = {
  status : Council_board.status;  (** The status of the claim at the end. *)
  statement : string;  (** As Lean prints it; as it is written when Lean never elaborated it. *)
  proof : Council_board.item option;  (** The proof Lean accepted. *)
  axioms : string list;  (** The axioms the accepted proof rests on. *)
  counterexample : Council_board.item option;
  components : Council_board.item list;  (** The items the answer is built from, each confirmed by Lean. *)
  set_aside : (string * int) list;  (** The kinds of item that are not part of the answer, with how many of each. *)
  lean : string option;  (** The theorem with the accepted proof, as Lean text. *)
  text : string;  (** What stands behind the status, in a sentence or two. *)
}

val synthesize : Council_problem.t -> Council_board.t -> claim:int -> t

val mentions : string -> string -> bool
(** Whether a name stands in a text as a whole name. *)

val to_json : t -> Json.t
