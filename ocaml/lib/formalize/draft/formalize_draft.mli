(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Drafting several statements for one problem.

    One informal sentence seldom has one formal reading. "Between 1 and 10"
    is inclusive or not, a "number" is natural or an integer, and a model
    asked for a single statement picks one reading without saying so. So
    the model is asked for several statements that make different choices,
    each with a sentence that says which choices it makes. The choosing is
    done afterwards, by checks. *)

open Maithic_gateway

val prompt : string
(** [formalize.draft]. *)

val default_libraries : string
(** What a statement may use when no import is allowed: the Lean core library. *)

val libraries : string list -> string
(** The same for a list of allowed imports. *)

val bindings : count:int -> imports:string list -> Formalize_intake.t -> (string * string) list
(** The values of the prompt's placeholders: the problem in its normalized
    spelling, the frame the intake found, how many statements are wanted
    and what they may use. *)

val ask :
  Gateway.t -> max_tokens:int -> name:string -> count:int -> imports:string list -> Formalize_intake.t -> (Formalize_candidate.t list, string) result
(** Asks the model for at most [count] distinct statements. An error says
    why no answer was had; an answer without a usable statement is an empty list. *)
