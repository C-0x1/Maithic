(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The repair agent: a model proposes, Lean checks, and the loop is bounded.

    For a theorem whose proof no longer compiles and that no rule could
    repair, a model is asked for the smallest change to the proof. Every
    proof it returns is screened, put in the place of the old proof and
    elaborated. A proof counts only if Lean accepts it, it does not rest on
    [sorry], and the theorem still states what it stated: the statement, as
    Lean prints it, is compared with that of the theorem before the repair.
    Of the proofs that count, the one that changes the fewest lines is kept.
    If none counts, the model is shown what Lean said about each and asked
    again, for a bounded number of rounds.

    When the rounds are spent, the theorem is handed to the prover, which
    proves the statement from nothing. That is a different remedy: it keeps
    the statement and gives up the old proof. *)

open Maithic_prover

type settings = {
  rounds : int;  (** How often the model is asked: two by default. *)
  candidates : int;  (** Proofs asked for in a round: three. *)
}

val default_settings : settings

type attempt = {
  round : int;  (** The round the proof came from; zero for the prover. *)
  proof : string;
  accepted : bool;
  said : string;  (** What Lean or the guard said when the proof was not accepted. *)
}

type stage =
  | Model  (** A model repaired the proof. *)
  | Prove of string  (** The prover proved the statement again; the proposer that found the proof. *)

type result =
  | Fixed of {
      stage : stage;
      proof : string;
      round : int;  (** The round that gave the proof; zero for the prover. *)
      changed_lines : int;  (** Lines that differ between the old proof and the new. *)
      axioms : string list;
      attempts : attempt list;
    }
  | Unfixed of { reason : string; attempts : attempt list }

type ask = round:int -> failures:(string * string) list -> (string list, string) Stdlib.result
(** Asks the model for proofs, given the proofs that failed with what Lean said. *)

type reprove = unit -> (string * string, string) Stdlib.result
(** Proves the statement from nothing: the proof and the proposer that found it. *)

val reference : check:Mend_check.t -> Lean_source.decl -> (Mend_check.verdict, string) Stdlib.result
(** What the theorem states, as Lean prints it: the verdict on the theorem
    with its proof left open. An error when the statement itself does not
    elaborate, which no change to a proof can repair. *)

val repair : ask:ask -> check:Mend_check.t -> settings -> Lean_source.decl -> result
(** The verify-and-retry loop with a model. *)

val mend : ?ask:ask -> ?reprove:reprove -> check:Mend_check.t -> settings -> Lean_source.decl -> result
(** The whole remedy for one theorem: the loop with the model, when there is
    one, and then the prover, when there is one. The proof the prover
    returns is checked like any other. *)

val stage_name : stage -> string
(** [model] or [prove]. *)
