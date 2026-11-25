(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A Lean code base as a graph.

    Nodes are declarations, modules, namespaces and files. Edges say that a
    declaration uses another, that a module imports another, that a file,
    module or namespace contains something, that an instance is an instance
    of a class, and that a structure extends another. The graph is built
    from what the Bridge extracted, so an edge is there because the kernel's
    own terms refer to the target, not because a name appears in the text.

    Declarations that the scanned files refer to but do not define, such as
    lemmas of the library, are nodes too, marked as not internal. *)

open Maithic_base
open Maithic_bridge

val schema : string
(** [maithic.intel.graph/1]. *)

type node_kind = Declaration | Module | Namespace | File
type edge_kind = Uses | Imports | Contains | Instance_of | Extends

type node = {
  id : string;  (** The kind and the name: [d:Nat.add_comm], [m:Maithic.Algo], [n:Maithic], [f:lean/Maithic.lean]. *)
  kind : node_kind;
  name : string;
  internal : bool;  (** Defined in the scanned files. *)
  decl_kind : string;  (** For a declaration: [theorem], [definition] and so on. *)
  file : string;
  line : int;  (** Where the declaration begins, its documentation included. *)
  end_line : int;
  name_line : int;  (** Where its name stands: the place to send a reader to. *)
  uses_sorry : bool;
  contains_sorry : bool;  (** The [sorry] stands in the declaration itself. *)
  axioms : string list;
  documented : bool;
}

type edge = { source : string; target : string; kind : edge_kind; via : string  (** For [Uses]: [type], [value] or [both]. *) }
type t

val node_kind_to_string : node_kind -> string
val edge_kind_to_string : edge_kind -> string

val build : Extraction.t list -> t
(** The graph of a set of extracted files. *)

val nodes : t -> node list
(** Sorted by identifier. *)

val edges : t -> edge list
(** Sorted by source, kind and target. *)

val to_json : t -> Json.t
val of_json : Json.t -> (t, string) result

val node : t -> string -> node option
(** By identifier. *)

val find : t -> string -> (node, string) result
(** A declaration by name: the exact name, or the one internal declaration
    whose name ends with it after a dot. The error says when nothing or
    more than one thing matches. *)

val in_file : t -> string -> node list
(** The declarations of a file, in the order of the file. *)

val at : t -> file:string -> line:int -> node option
(** The declaration whose text covers a line of a file. *)

val dependencies : t -> string -> node list
(** What a declaration uses directly, by name. *)

val dependents : t -> string -> node list
(** What uses a declaration directly. *)

val contents : t -> string -> node list
(** What a file, module or namespace contains directly. *)

val targets : t -> edge_kind -> string -> node list
(** The nodes that the edges of a kind lead to from a node. *)

type degrees = { least : int; greatest : int; mean : float; median : float }

type statistics = {
  nodes_by_kind : (string * int) list;
  edges_by_kind : (string * int) list;
  declarations_by_kind : (string * int) list;  (** Internal declarations. *)
  external_declarations : int;
  used_by : degrees;  (** Over internal declarations: how many declarations use each. *)
  uses : degrees;  (** Over internal declarations: how many declarations each uses. *)
  most_used : (string * int) list;  (** The ten declarations used most, internal or not. *)
  most_used_internal : (string * int) list;  (** The ten used most among those the scanned files define. *)
}

val statistics : t -> statistics
val statistics_to_json : statistics -> Json.t
