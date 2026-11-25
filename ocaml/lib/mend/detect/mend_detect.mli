(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Collecting what is broken.

    Lean reports problems by position. This module turns what Lean said
    about a file into breakages: it finds the theorem each diagnostic stands
    in, puts the diagnostics of one theorem together, and gives each
    breakage its class. A deprecation and a suggestion of Lean are collected
    with the errors, because they are work of the same kind: the code has to
    change, and Lean has said how. *)

open Maithic_bridge

val of_report : file:string -> source:string -> Bridge_types.report -> Mend_breakage.t list
(** The breakages of a file, from Lean's report on it, in the order of the
    file. The diagnostics of one theorem are one breakage, classed by its
    first error, or by its first deprecation or suggestion when it has no
    error. A diagnostic outside every theorem is a breakage of its own. *)

type check = string -> (Bridge_types.report, string) result
(** Elaborates the text of a whole file. *)

val collect : check:check -> file:string -> string -> (Mend_breakage.t list * Bridge_types.report, string) result
(** Checks a file and collects its breakages. *)

val count_by_class : Mend_breakage.t list -> (string * int) list
(** How many breakages each class has, for every class, in the order of the classes. *)

val first_line : string -> string
(** The first line of a message that is not empty. *)
