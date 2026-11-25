(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The store of a proof search.

    A node is a problem: one goal to close. An edge of a node is a tactic
    that was tried on it, with what came of it: the subgoals that the tactic
    left, a rejection, or a block because a subgoal repeats a problem already
    on the way to it. The node is an OR over its edges, and an edge is an AND
    over its subgoals: a node is proved when one of its edges has every
    subgoal proved, and it has failed when it was expanded and none of its
    edges can still succeed.

    Nodes live in an arena and are named by their index. A subgoal whose key
    was seen before, elsewhere in the tree, is an alias of the node with that
    key: it is not searched again, and it is proved when that node is.

    Failure is shared with more care, because it can depend on where a node
    stands. A node may fail only because its tactics lead back to goals above
    it, or because the search gave part of it up at a limit; the same goal in
    another place could still be proved. Such a failure is {e tainted}. When
    a node fails with a taint, its aliases are cut loose and become nodes of
    their own, to be searched where they stand; a failure with no taint holds
    everywhere and is shared. *)

type status = Open | Proved | Failed

type outcome =
  | Subgoals of int list  (** The nodes of the subgoals; none means that the tactic closed the goal. *)
  | Rejected of { error_class : string; message : string }  (** The tactic failed. *)
  | Blocked of { node : int }
      (** A subgoal has the key of [node], which is this node or one above it: the tactic leads in a circle. *)

type edge = { tactic : string; prior : float; outcome : outcome }

type 'a node = private {
  id : int;
  parent : int option;  (** The node whose edge made this one. *)
  depth : int;
  cost : float;  (** The sum of [-log prior] along the way from the root. *)
  key : string;
  payload : 'a;
  mutable alias_of : int option;
  mutable aliases : int list;
  mutable status : status;
  mutable expanded : bool;
  mutable edges : edge list;  (** In the order they were tried. *)
  mutable proved_by : int option;  (** The position of the edge that proved the node. *)
  mutable abandoned : string option;  (** Why the node was given up without being expanded. *)
  mutable tainted : bool;  (** For a failed node: the failure depends on where the node stands. *)
}

type 'a t

val create : unit -> 'a t
val size : 'a t -> int
val node : 'a t -> int -> 'a node

val add_root : 'a t -> key:string -> 'a -> int
(** A node with no parent. *)

type 'a step =
  | Leaves of (string * 'a) list  (** The key and payload of every subgoal the tactic left. *)
  | Fails of { error_class : string; message : string }

val add_edge : 'a t -> int -> tactic:string -> prior:float -> 'a step -> edge
(** Records a tactic tried on a node. Subgoals become nodes, or aliases of
    nodes with the same key; if a subgoal has the key of the node or of a
    node above it, no node is made and the edge is [Blocked]. Priors are
    clamped to [(0, 1]]. *)

val finish : 'a t -> int -> unit
(** Says that every tactic for the node has been recorded, and updates the
    status of the node and of everything that depends on it. *)

val abandon : 'a t -> int -> string -> unit
(** Gives a node up for the stated reason: it counts as failed. *)

val take_detached : 'a t -> int list
(** The aliases cut loose since the last call, oldest first. They are open
    nodes of their own now and wait to be expanded. *)

val effective : 'a t -> int -> status
(** The status of a node, or of the node it is an alias of. *)

val relevant : 'a t -> int -> bool
(** Whether expanding the node can still matter: it is open, not expanded,
    not an alias, and some way from it to a root passes only through open
    nodes and edges with no failed subgoal. *)

val script : 'a t -> int -> string list option
(** The proof of a proved node as lines of tactics: the tactic of the edge
    that proved it, followed by the proofs of its subgoals, each under a
    bullet when there are several. *)

val check_invariants : 'a t -> (unit, string) result
(** Checks the tree against its own rules: parents and children agree, every
    status follows from the edges, and every proved node names an edge that
    proves it. For tests and debugging. *)

val status_to_string : status -> string
