(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What Lean said, kept as the ground a status stands on.

    A claim of the blackboard changes its status only when evidence is
    given for the change, and evidence has no constructor of its own: the
    functions here make it from an answer of Lean, and each refuses an
    answer that does not carry the status asked for. A member that speaks
    with a model is never handed a function that returns evidence, so
    nothing a model writes can become a verified claim without passing
    through Lean on the way. *)

open Maithic_base
open Maithic_ledger
open Maithic_refute

(** What Lean answered about one declaration. *)
type lean = {
  accepted : bool;  (** Lean elaborated the declaration without an error. *)
  statement : string;  (** The statement as Lean prints it; empty when there is none. *)
  axioms : string list;  (** The axioms the declaration rests on. *)
  uses_sorry : bool;
  rechecked : bool;  (** The answer was confirmed in a fresh process. *)
  error : string;  (** What Lean reported when it did not accept the text, or why the guard kept the text from Lean. *)
}

val rejected : string -> lean
(** An answer that is a refusal, with what was said. *)

type kind =
  | Elaborated  (** The statement is well formed: Lean elaborated it with [sorry] for a proof. *)
  | Proved  (** Lean's kernel accepted a proof of the statement, within the trust policy. *)
  | Disproved  (** Lean's kernel accepted a proof that the statement is false at given values. *)

type t

val kind : t -> kind
val kind_name : kind -> string
(** [elaborated], [proved] or [disproved]. *)

val statement : t -> string
(** The statement the evidence is about, as Lean prints it. *)

val axioms : t -> string list
val policy : t -> string
(** The name of the trust policy a proof was held to; empty for other evidence. *)

val elaborated : lean -> (t, string) result
(** Evidence that a statement is well formed. The error is what Lean said. *)

val proved : policy:Trust_policy.t -> statement:string -> lean -> (t, string) result
(** Evidence that a statement is proved: Lean accepted the theorem, the
    proof does not rest on [sorry], the theorem states [statement] and
    nothing else, it rests only on axioms the policy allows, and it was
    checked again in a fresh process when the policy asks for that. The
    error says which of these does not hold. *)

val disproved : statement:string -> Refute_witness.confirmation -> t
(** Evidence that a statement is false: a refutation that Lean accepted. *)

val to_json : t -> Json.t
