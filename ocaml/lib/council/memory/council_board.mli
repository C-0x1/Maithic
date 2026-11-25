(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The blackboard: what a council knows, where every member can read it.

    Members do not pass their work to each other; they post it. An item has
    a kind, an author, the round and the moment it was posted, and may point
    at other items: a proof points at the claim it is a proof of, a remark of
    the critic at the proof it is about. Items are never changed and never
    taken down. Two things are recorded beside them, and both only by what
    Lean said: whether a proof was accepted, and the status of a claim.

    A claim is [conjectured] when it is posted, whoever posts it and whatever
    its text says. It becomes [formalized], [verified] or [refuted] through
    the functions of {!Verifier}, and through nothing else. Each of them
    takes {!Council_evidence}, which is made from an answer of Lean. *)

open Maithic_base

type kind =
  | Definition  (** A definition or a lemma of the library, with its signature. *)
  | Statement  (** What is to be shown, in Lean. *)
  | Claim  (** That a statement holds. The only kind with a status. *)
  | Proof  (** A candidate proof of a claim. *)
  | Counterexample  (** Values at which a statement fails. *)
  | Note  (** A remark: an idea, a diagnosis, a suggestion. *)
  | Question  (** Something a member wants to know. *)

val kinds : kind list
val kind_name : kind -> string
(** [definition], [statement], [claim], [proof], [counterexample], [note] or [question]. *)

val kind_of_string : string -> kind option

type status = Conjectured | Formalized | Refuted | Verified

val status_name : status -> string
(** [conjectured], [formalized], [refuted] or [verified]. *)

(** What Lean said about a proof that was posted. *)
type examined = Unexamined | Accepted | Rejected of string

type item = {
  id : int;  (** From one, in the order of posting. *)
  kind : kind;
  author : string;  (** The role that posted it, or [coordinator]. *)
  round : int;
  at : float;  (** Seconds since the board was set up. *)
  title : string;
  body : string;
  refers : int list;  (** Items this one is about. *)
  checked : bool;  (** Lean confirmed the body when it was posted, as it does for a definition that was looked up. *)
}

type t

val create : ?clock:(unit -> float) -> unit -> t
val set_round : t -> int -> unit

val post : t -> author:string -> kind -> title:string -> ?refers:int list -> ?checked:bool -> string -> item
(** Posts an item and returns it with its number. A claim is posted
    [conjectured]. An item that refers to an item that is not on the board
    is an error of the program, and raises [Invalid_argument]. *)

val items : t -> item list
(** Every item, the first one first. *)

val find : t -> int -> item option
val of_kind : t -> kind -> item list
val by : t -> string -> item list
(** The items a member posted. *)

val about : t -> int -> item list
(** The items that refer to an item. *)

val status : t -> int -> status option
(** The status of a claim; nothing for an item that is not one. *)

val evidence : t -> int -> Council_evidence.t list
(** The evidence a claim's status rests on, in the order it was given. *)

val examined : t -> int -> examined
(** What Lean said about a proof; [Unexamined] for one it has not seen and for an item that is not a proof. *)

val accepted_proof : t -> int -> item option
(** The proof by which a claim was verified. *)

(** The only way to change what the board says about a claim or a proof. *)
module Verifier : sig
  val formalized : t -> claim:int -> Council_evidence.t -> (unit, string) result
  (** The claim's statement is well formed. Needs evidence that it elaborated. *)

  val verified : t -> claim:int -> proof:int -> Council_evidence.t -> (unit, string) result
  (** The claim is proved by the proof. Needs evidence of a proof of the
      statement the claim was formalized as; a claim that was never
      formalized, or was refuted, cannot be verified. *)

  val refuted : t -> claim:int -> counterexample:int -> Council_evidence.t -> (unit, string) result
  (** The claim is false. Needs evidence of a refutation. *)

  val rejected : t -> proof:int -> string -> (unit, string) result
  (** Lean did not accept a proof; what it said. This lowers nothing and raises nothing. *)
end

val item_to_json : t -> item -> Json.t
(** An item with what the board records beside it: the status and the evidence of a claim, the examination of a proof. *)

val to_json : t -> Json.t
(** The items as a JSON array, in order. *)
