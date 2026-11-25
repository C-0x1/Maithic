(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The state of a code base: what is unfinished, and what it rests on. *)

open Maithic_base
open Maithic_bridge

val schema : string
(** [maithic.intel.health/1]. *)

type gap = {
  name : string;
  file : string;
  line : int;  (** Of the [sorry] when its place is known, of the declaration otherwise. *)
  column : int;
  goals : string list;  (** What is to be shown at the [sorry], as Lean prints it. *)
  through : string option;  (** For a declaration that is itself complete: the unfinished one it uses. *)
}

val sorry_inventory : Extraction.t list -> gap list
(** Every unfinished proof: one entry for each [sorry] in tactic position,
    with the goals there; one for a declaration with a [sorry] elsewhere in
    its text; and one for each declaration that rests on [sorry] through
    another. By file and line. *)

type axiom_use = { axiom : string; standard : bool; users : (string * string * int) list  (** Name, file and line. *) }

val standard_axioms : string list
(** The axioms of Lean's logic: [propext], [Classical.choice] and [Quot.sound]. *)

val axiom_census : Extraction.t list -> axiom_use list
(** The axioms the declarations rest on, and which declarations rest on
    each. [sorryAx] is not listed: it is in the inventory. Axioms outside
    Lean's logic come first. *)

val snapshot : Extraction.t list -> Json.t
(** Counts, the inventory and the census as one document. *)
