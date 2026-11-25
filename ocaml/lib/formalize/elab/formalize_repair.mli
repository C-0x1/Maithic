(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The prompt that repairs a statement.

    A model that wrote [Even n] where the library has no [Even] does not
    need to be told to try again: it needs to be told what Lean said and
    what the library does have. The prompt carries the statement, the error
    with its class and what a repair of that class has to do, the statements
    that failed before, and the signatures of the names in the statement
    that Lean does know, looked up in the same environment the statement is
    compiled in. *)

open Maithic_gateway

val prompt : string
(** [formalize.repair]. *)

val bound : string -> string list
(** The names a statement binds: what stands before the colon of a binder,
    and after a quantifier or [fun]. *)

val identifiers : string -> string list
(** The names in a statement that could be names of the library: each once,
    in the order of the text, without the names the statement binds and
    without keywords, sixteen at most. *)

type lookup = string list -> (string * string) list
(** The signatures of the names, of those in the list that Lean knows. *)

val signatures_text : (string * string) list -> string
(** The signatures as the prompt shows them, a line each. *)

val bindings :
  imports:string list -> intake:Formalize_intake.t -> candidate:Formalize_candidate.t -> error:Formalize_error.t -> said:string ->
  signatures:(string * string) list -> failures:(string * string) list -> (string * string) list
(** The values of the prompt's placeholders. *)

val statement_of_answer : string -> string
(** The statement in a model's answer, as a signature; empty when there is none. *)

val ask : Gateway.t -> max_tokens:int -> lookup:lookup -> imports:string list -> intake:Formalize_intake.t -> Formalize_elab.repair
(** The repair as the elaboration loop asks for it. *)
