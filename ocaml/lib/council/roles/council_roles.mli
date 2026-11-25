(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The registry of roles: who there is, and how a role is put to work.

    A role is looked up by its name and instantiated from a belt, which
    holds what roles may be handed: a model, the tools, the trust policy.
    Instantiating hands a role what its declaration lists and nothing more.
    The researcher gets the model and the look-up; the prover and the critic
    get the model alone; the verifier gets the tools that return evidence,
    and no model. *)

open Maithic_ledger

type belt = {
  ask : Council_role.ask option;  (** A model; without one, only the roles that ask none can be instantiated. *)
  tools : Council_tools.t;
  policy : Trust_policy.t;
  candidates : int;  (** Proofs the prover asks for in one turn. *)
}

val all : Council_role.t list
(** The researcher, the prover, the critic and the verifier. *)

val names : string list
val find : string -> (Council_role.t, string) result
(** A role by its name; the error names the roles there are. *)

val for_task : Council_plan.task -> Council_role.t
(** The role a type of task belongs to. *)

val instantiate : belt -> string -> (Council_role.context -> Council_role.work, string) result
(** A role ready to take a turn. The error is that there is no such role, or
    that the role asks a model and the belt holds none. *)
