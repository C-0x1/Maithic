(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Goals in the oracle exchange format, schema [maithic.oracle.goal/1].

    A goal is what an oracle is asked: hypotheses and a conclusion over
    variables of one number type, written in the small expression language
    that every oracle reads. *)

open Maithic_base

type kind = Linear | Polynomial | Real
type domain = Integers | Rationals | Reals
type hypothesis = { name : string; statement : string }

type t = {
  kind : kind;
  domain : domain;
  variables : string list;
  hypotheses : hypothesis list;
  goal : string;  (** A comparison, or ["false"] to ask whether the hypotheses contradict each other. *)
}

val schema : string
val kind_to_string : kind -> string
val kind_of_string : string -> kind option
val domain_to_string : domain -> string
val domain_of_string : string -> domain option

val to_json : t -> Json.t

val of_json : Json.t -> (t, string) result
(** Reads a goal document. A hypothesis may be an object with a name or just
    its statement, which is then named [h1], [h2], … by position; the domain
    defaults to the rationals, and to the reals for a goal of kind [real]. *)
