(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What an oracle answers, schema [maithic.oracle.outcome/1], and the record
    of a question with its answer, schema [maithic.oracle.result/1]. *)

open Maithic_base

type lean = {
  check : string option;  (** A statement that a decision procedure of Lean confirms. *)
  replay : string option;  (** A theorem that replays the certificate through its verified checker. *)
  scripts : string list;  (** Tactic scripts that close the goal in its own theorem, the most informative first. *)
}

type t =
  | Proved of { kind : string; claim : string; certificate : Json.t; lean : lean option }
  | Refuted of (string * string) list  (** A point, as variable and exact value, where the goal fails. *)
  | Unknown of string  (** The reason. Never a verdict on the goal. *)

val schema : string
val of_json : Json.t -> (t, string) result
val label : t -> string
(** [proved], [refuted] or [unknown]. *)

val certificate_id : Json.t -> string
(** The name of a certificate by its content: the SHA-256 of its kind, claim
    and data in canonical form. The record of the engine that produced it is
    not part of the name, so equal certificates have equal names. *)

type result = {
  oracle : string;
  goal : Oracle_goal.t;
  outcome : t;
  seconds : float;  (** How long the oracle took, start of the process to its answer. *)
}

val result_schema : string
val result_to_json : result -> Json.t
