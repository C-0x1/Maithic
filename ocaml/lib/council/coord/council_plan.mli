(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The plan: what there is to do in a round, and who does it.

    The coordinator cuts the work on a problem into subtasks of a few
    types, and each type belongs to one role. The cut is the same for every
    problem, and it is the cheap work first: before any model is asked, Lean
    is asked whether the statement is well formed, a counterexample is
    looked for, and the prover's automation has its turn. *)

type task =
  | Examine  (** Before the first round: is the statement well formed, is it false, does automation prove it? *)
  | Research  (** Find the definitions and lemmas of the library that bear on the statement. *)
  | Prove  (** Propose proofs. *)
  | Verify  (** Give each proposed proof to Lean. *)
  | Critique  (** Say why the proofs Lean rejected fail, and what to try. *)

val tasks : task list
val task_name : task -> string
(** [examine], [research], [prove], [verify] or [critique]. *)

val describe : task -> string
(** What the coordinator asks for, in a sentence. *)

val role_for : task -> string
(** The role a type of task belongs to: the verifier examines and verifies,
    the researcher researches, the prover proves, the critic criticizes. *)

val subtasks : round:int -> last:bool -> task list
(** The subtasks of a round, in the order they are done. Round zero is the
    examination. Research is done once, in the first round. The critic has
    no task in the last round: nobody would act on what it says. *)

val assign : round:int -> last:bool -> (task * string) list
(** The subtasks of a round, each with the role it is assigned to. *)
