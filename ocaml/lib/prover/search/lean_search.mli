(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Proof search over Lean proof states.

    A node of the search is one goal of a proof state. A tactic is run on
    that goal alone; the goals it leaves in place of the goal are the
    subgoals, and each is searched on its own. A tactic that leaves the goal
    as it was has made no progress, and a goal that comes back on the way is
    a circle; the kernel blocks both.

    The search talks to Lean through a {!driver}, so that its logic can be
    tested against a simulated prover. The proof it returns is a script, and
    a script is a proposal like any other: it is sent through the screen, the
    kernel and the trust policy as a whole declaration before anything is
    reported. *)

open Maithic_bridge

type driver = {
  try_tactics : state:int -> goal:int -> string list -> (Bridge_types.tactic_outcome list, Bridge_error.t) result;
      (** Runs each tactic on the goal at a position of a state, independently. *)
  release : int -> unit;  (** Frees a state. *)
}

val driver_of_bridge : ?limits:Bridge_types.limits -> Bridge_client.t -> driver

type focus = {
  state : int;  (** The proof state. *)
  goals : Bridge_types.goal list;  (** Every goal of the state, in order. *)
  goal : Bridge_types.goal option;  (** The goal in focus; none for the root. *)
}

val goal_key : Bridge_types.goal -> string
(** What makes two goals the same problem: the target and the hypotheses with
    their names, whatever their order. The identifier and the case name of
    the goal are not part of it. *)

type proposer = Bridge_types.goal -> (string * float) list
(** The tactics to try on a goal, each with a prior in [(0, 1]]. *)

val shape_proposer : proposer
(** A proposer that needs no model: closing automation, structural tactics
    chosen from the shape of the target and the hypotheses, and [simp] with
    the first names of the target that may be definitions of the file. *)

val vocabulary : (string * float) list -> proposer
(** The same tactics for every goal. *)

val expand : driver -> proposer -> focus Search_tree.node -> (focus Search_kernel.expansion, Bridge_error.t) result
(** One expansion: the tactics of the proposer, run on the goal in focus. *)

type found = {
  proof : string option;  (** The proof term, [by] followed by the script, when one was found. *)
  stop : Search_kernel.stop;
  expansions : int;
  lean_calls : int;
  states : int;  (** Proof states the search made. *)
  seconds : float;
  record : Search_record.t option;  (** The whole search, as data; none when the statement gave no goal to search. *)
}

val search :
  ?now:(unit -> float) ->
  ?strategy:Search_kernel.strategy ->
  ?budget:Search_kernel.budget ->
  ?proposer:proposer ->
  ?statement:string ->
  driver ->
  state:int ->
  Bridge_types.goal list ->
  (found, Bridge_error.t) result
(** Searches for a proof of the goals of a state. Every state the search
    made is released before it returns; the state it was given is not.
    [statement] is written into the record of the search. *)

val prove_statement :
  ?strategy:Search_kernel.strategy ->
  ?budget:Search_kernel.budget ->
  ?proposer:proposer ->
  ?limits:Bridge_types.limits ->
  Bridge_client.t ->
  env:int ->
  string ->
  (found, Bridge_error.t) result
(** Opens a proof state for a declaration given up to its [:=] and searches
    it. A statement that does not elaborate gives no proof and [Exhausted]. *)

val default_budget : Search_kernel.budget
(** Sixty expansions and sixty seconds. *)

val script_to_proof : string list -> string
(** [by] followed by the lines of a script, indented. *)
