(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The scheduler: whose turn it is.

    A session is a sequence of turns. One member acts in a turn, and the
    next sees everything the ones before it posted. Within a round the order
    is fixed: the researcher, the prover, the verifier, the critic. That
    order is what makes a round a conversation: the prover has the
    researcher's lemmas before it, the verifier has the prover's proofs, and
    the critic has Lean's answers to them. *)

type turn = { number : int;  (** From one. *) round : int; task : Council_plan.task; role : string }

val order : string list
(** The order of the roles within a round. *)

val turns : rounds:int -> turn list
(** Every turn of a session with the given number of rounds, the
    examination first. A session may end before its last turn: when the
    claim is settled, or when the budget is used up. *)

val after : turn list -> turn -> turn option
(** The turn that follows a turn. *)

val describe : rounds:int -> string
(** The schedule in a line, as in [examine: verifier | round 1: researcher, prover, verifier, critic | …]. *)
