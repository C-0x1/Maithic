(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Questions about a code graph. *)

val dependencies : ?depth:int -> ?internal_only:bool -> Intel_graph.t -> string -> (Intel_graph.node * int) list
(** Everything a declaration depends on, each with its distance: one for
    what it uses itself, two for what those use, and so on, as far as
    [depth] when it is given. Sorted by distance, then by name. With
    [internal_only], declarations from outside the scanned files are left
    out, and what lies behind them with them. *)

val dependents : ?depth:int -> Intel_graph.t -> string -> (Intel_graph.node * int) list
(** Everything that depends on a declaration, the same way. *)

val unfinished : Intel_graph.t -> Intel_graph.node list
(** The declarations whose own text contains [sorry], by file and line. *)

val affected : Intel_graph.t -> Intel_graph.node list
(** The declarations that rest on [sorry] only through what they use. *)
