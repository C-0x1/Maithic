(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What the members of a council say to each other.

    A message has a sender, the members it is for, a performative that says
    what kind of act it is, a subject and a content. Nothing a member says
    is believed because it was said: a message that claims a proof points at
    an item of the blackboard, and what that item is worth is decided there,
    by Lean. Messages are what makes a session readable afterwards. *)

open Maithic_base

type performative =
  | Request  (** Asks a member to do something. *)
  | Inform  (** Reports what was found or done. *)
  | Propose  (** Puts a candidate before the others: a proof, a lemma. *)
  | Critique  (** Says what is wrong with something, and what could be tried. *)
  | Verdict  (** Says what Lean decided. Only the verifier sends it. *)
  | Decline  (** Says that something asked for was not done, and why. *)

val performatives : performative list
val performative_name : performative -> string
(** [request], [inform], [propose], [critique], [verdict] or [decline]. *)

val performative_of_string : string -> performative option

type t = {
  seq : int;  (** The place of the message in its session, from one. *)
  at : float;  (** Seconds since the session began. *)
  round : int;  (** The round in which it was sent. *)
  sender : string;  (** The name of a role, or [coordinator]. *)
  recipients : string list;  (** Who it is for; never empty. *)
  performative : performative;
  subject : string;  (** What it is about, in a line. *)
  content : string;
  refers : int list;  (** The items of the blackboard it is about. *)
}

val check : t -> (unit, string) result
(** A message is well formed when it has a sender, at least one recipient
    other than nothing, a subject, and a place of one or more. *)

val to_json : t -> Json.t
val of_json : Json.t -> (t, string) result
