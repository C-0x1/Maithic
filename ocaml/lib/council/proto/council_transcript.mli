(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The transcript of a session: every message, in the order it was sent.

    The log only grows. A message gets its place when it is said, and
    nothing said is changed or taken out afterwards, so the transcript of a
    session is the session as it happened. It can be written as JSON, for a
    program, and as Markdown, for a person. *)

open Maithic_base

type t

val create : ?clock:(unit -> float) -> unit -> t
(** An empty transcript. Times are counted from this moment by [clock]. *)

val set_round : t -> int -> unit
(** The round under which the messages that follow are filed. *)

val say :
  t -> sender:string -> recipients:string list -> Council_message.performative -> subject:string -> ?refers:int list -> string -> Council_message.t
(** Appends a message and returns it with its place. A message that is not
    well formed is an error of the program, and raises [Invalid_argument]. *)

val messages : t -> Council_message.t list
(** Every message, the first one first. *)

val length : t -> int
val from : t -> string -> Council_message.t list
(** The messages a member sent. *)

val to_ : t -> string -> Council_message.t list
(** The messages for a member. *)

val to_json : t -> Json.t
(** The messages as a JSON array, in order. *)

val of_json : Json.t -> (Council_message.t list, string) result
(** Reads the array back, and refuses one whose places are not 1, 2, 3, …. *)

val to_markdown : ?title:string -> ?items:(int -> string option) -> t -> string
(** The transcript for a reader: a heading for each round, and under it each
    message with its sender, its recipients and what it says. [items] gives
    the title of an item of the blackboard, for the messages that refer to
    one. *)
