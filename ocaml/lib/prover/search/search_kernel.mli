(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The search loop.

    The kernel knows nothing about Lean. It is given the goals to prove and a
    function that expands a node: the tactics tried on it, each with a prior
    and with the subgoals it left or the reason it failed. The kernel decides
    which node to expand next, keeps the tree, stops at its budget, and
    returns the proof it found as a script.

    The order of expansion is a strategy. All three are deterministic: equal
    inputs give the same tree, node for node. *)

type strategy =
  | Best_first  (** The open node whose way from the root is the most probable; ties go to the older node. *)
  | Breadth_first of { width : int }
      (** Level by level. At most [width] nodes of a level are expanded and the rest are given up. *)
  | Depth_first of { depth : int; branching : int }
      (** The newest node first, backing up when a branch is spent. Nodes deeper than [depth] are
          given up, and of the tactics that work on a node only the first [branching] are followed. *)

type budget = {
  expansions : int;  (** Nodes expanded. *)
  seconds : float;  (** Time since the search began. *)
  lean_calls : int;  (** Round trips to Lean, as the expansions report them. *)
  tokens : int;  (** Model tokens, as the expansions report them. *)
}

val unlimited : budget
(** No limit of any kind. *)

type 'a tried = { tactic : string; prior : float; step : 'a Search_tree.step }
type 'a expansion = { tried : 'a tried list; lean_calls : int; tokens : int }

type stop =
  | Found  (** Every goal is proved. *)
  | Exhausted  (** Nothing is left to expand and the goals are not proved. *)
  | Out_of of string  (** The named part of the budget was spent: [expansions], [time], [lean calls] or [tokens]. *)

type step = {
  node : int;  (** The node that was expanded. *)
  started : float;  (** Seconds from the start of the search to the start of the expansion. *)
  seconds : float;  (** How long the expansion took, with the bookkeeping that followed it. *)
}

type 'a result = {
  stop : stop;
  tree : 'a Search_tree.t;
  root : int;  (** The node that stands for the goals together. *)
  script : string list option;  (** The proof, as lines of tactics, when one was found. *)
  steps : step list;  (** The expansions, in the order they were made. *)
  expansions : int;
  lean_calls : int;
  tokens : int;
  seconds : float;
}

val search :
  ?now:(unit -> float) ->
  ?strategy:strategy ->
  ?budget:budget ->
  expand:('a Search_tree.node -> ('a expansion, 'e) Stdlib.result) ->
  root:'a ->
  (string * 'a) list ->
  ('a result, 'e) Stdlib.result
(** [search ~expand ~root goals] proves the goals, each given with its key.
    [root] is the payload of the node that stands for all of them; it is
    never expanded. The budget is checked before every expansion. An error of
    [expand] ends the search and is returned. [now] is the clock, for tests. *)

val stop_to_string : stop -> string
