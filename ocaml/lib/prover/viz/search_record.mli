(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The record of a proof search: the tree, the order of the work and the
    proof, as plain data.

    A record is what is left of a search when the prover it ran against is
    gone. It can be written as one JSON document, read back, printed step by
    step and drawn; none of that needs Lean. *)

open Maithic_base

val schema : string
(** [maithic.prove.search/1]. *)

type outcome =
  | Subgoals of int list  (** The nodes of the goals the tactic left; none means that it closed the goal. *)
  | Rejected of { error_class : string; message : string }
  | Blocked of int  (** The tactic leads back to this node. *)

type edge = { tactic : string; prior : float; outcome : outcome }

type node = {
  id : int;
  parent : int option;
  depth : int;
  goal : string;  (** What is to be shown; empty for the node that stands for all goals. *)
  hypotheses : string list;  (** Each as [name : type]. *)
  status : Search_tree.status;  (** For an alias, the status of the node it stands for. *)
  alias_of : int option;
  expanded : bool;
  abandoned : string option;
  edges : edge list;
  proved_by : int option;  (** The position of the edge that proved the node. *)
}

type step = { node : int; started_ms : int; elapsed_ms : int }

type t = {
  statement : string;  (** What was to be proved, as it was given; may be empty. *)
  stop : string;  (** [found], [exhausted] or [out of ...]. *)
  root : int;
  nodes : node list;  (** In the order of their identifiers, from zero. *)
  steps : step list;  (** The expansions in the order they were made. *)
  script : string list option;
  expansions : int;
  lean_calls : int;
  tokens : int;
  elapsed_ms : int;
}

val of_result : ?statement:string -> describe:('a Search_tree.node -> string * string list) -> 'a Search_kernel.result -> t
(** The record of a finished search. [describe] gives the goal and the
    hypotheses of a node. *)

val to_json : t -> Json.t
val of_json : Json.t -> (t, string) result
(** Reads a record, and refuses one that does not hold together: an
    identifier out of place, a reference to a node or an edge that is not
    there. *)

val node : t -> int -> node option

val first_goals : t -> int list
(** The goals the search began with: the subgoals of the first edge of the root. *)

val steps_text : t -> string
(** The search step by step: for every expansion the goal, how long it took,
    and what each tactic did. *)

val summary : t -> string
(** One line: how the search ended and what it cost. *)
