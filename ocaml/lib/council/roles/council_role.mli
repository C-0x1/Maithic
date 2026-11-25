(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What a role is, and what one turn of a role looks like from outside.

    A role is a specialist: it has a name, a purpose, the types of task it
    takes, the tools it is handed and, when it asks a model, the prompt it
    asks with. In a turn it reads the blackboard, does its work, posts what
    it produced and says so in the transcript. What it returns is an account
    of the turn; the work itself is on the board. *)

open Maithic_base

type ask = purpose:string -> schema:string * Json.t -> (string * string) list -> (string, string) result
(** A model, asked through the gateway: the name of a prompt, the schema of
    the answer, and the values of the prompt's placeholders. The answer is
    the text the model returned; the error is why there is none. *)

type context = {
  problem : Council_problem.t;
  board : Council_board.t;
  transcript : Council_transcript.t;
  statement : int;  (** The item of the board that holds the statement. *)
  claim : int;  (** The claim that the statement holds. *)
  round : int;
  task : Council_plan.task;
}

type status =
  | Done  (** The role did its task. *)
  | Idle  (** There was nothing for it to do. *)
  | Failed of string  (** It could not do its task; why. *)

val status_name : status -> string
(** [done], [idle] or [failed]. *)

type work = { status : status; summary : string;  (** The turn in a line. *) posted : int list;  (** The items it posted. *) }

type t = {
  name : string;
  purpose : string;
  tasks : Council_plan.task list;  (** The types of task it takes. *)
  uses_model : bool;
  tools : string list;  (** The tools of the belt it is handed. *)
  prompt : string;  (** The prompt it asks a model with; empty for a role that asks none. *)
}

val formal_statement : context -> string
(** The statement of the claim as Lean prints it, once the claim is formalized; empty before. *)

val say : context -> t -> string list -> Council_message.performative -> subject:string -> ?refers:int list -> string -> unit
(** Says something in the transcript, as the role. *)

val answer : string -> Json.t option
(** The JSON object in a model's answer, wherever in the text it stands. *)

val strings : string -> Json.t -> string list
(** A member of an answer that is a list of texts: each trimmed, none empty. *)

val object_schema : (string * Json.t) list -> Json.t
(** The schema of an answer that is an object with the given members, all required. *)

val text_list : Json.t
(** The schema of a list of texts. *)

val first_line : string -> string
