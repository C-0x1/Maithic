(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Back-translation and the round trip, through the gateway.

    Two calls, kept apart on purpose. The first shows a model the statement
    as Lean prints it, and nothing of the problem, and asks what the
    statement says. The second shows the problem and that English, and
    nothing of Lean, and asks whether they claim the same. A model that sees
    the problem while it reads the statement tends to read the problem into
    it; one that has only the statement can only say what is there. *)

open Maithic_gateway

val back_prompt : string
(** [formalize.back]. *)

val judge_prompt : string
(** [formalize.judge]. *)

val english_of_answer : string -> string
(** The English in a model's answer; empty when there is none. *)

val judgement_of_answer : string -> (int * string list, string) result
(** The score, from 0 to 10, and the differences in a model's answer. *)

val model : Gateway.t -> max_tokens:int -> Formalize_faith.model
