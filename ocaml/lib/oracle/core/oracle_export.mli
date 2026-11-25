(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** From a goal of Lean to a goal for an oracle.

    A goal as the Bridge reports it has hypotheses and a target as Lean
    prints them. The export keeps the part that is arithmetic over integer
    or rational variables: the variables, the hypotheses that are comparisons
    of polynomial expressions in them, and the target. A hypothesis that says
    something else is left out, which is sound: an oracle that proves the
    goal from fewer hypotheses has proved it. A target that is not such a
    comparison cannot be exported at all.

    Names are kept as they are, so that what an oracle says about hypothesis
    [h1] can be used on [h1] in Lean. *)

open Maithic_bridge

type exported = {
  goal : Oracle_goal.t;
  skipped : (string * string) list;  (** Hypotheses left out, each with the reason. *)
}

val translate : domain:Oracle_goal.domain -> variables:string list -> string -> (string, string) result
(** A comparison as Lean prints it, in the syntax of the exchange format. The
    error says what stands in the way. *)

val is_linear : string -> bool
(** Whether a translated statement is linear: no power, no product of two
    terms that are not numbers, no division by a term that is not a number. *)

val export : Bridge_types.goal -> (exported, string) result
