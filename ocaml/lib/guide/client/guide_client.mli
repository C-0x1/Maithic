(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The typed client of the guidance service.

    The service is a small model, trained on recorded proofs, that reads a
    proof state and says which tactics are likely to come next. It is spoken
    to in JSON, one request and one answer at a time, in the protocol
    [maithic.guide.service/1]. This module turns the three questions a
    search can ask into typed calls and checks every answer before it is
    used: a list of probabilities must have one entry for each candidate and
    add up to one, a value must lie between zero and one.

    The client knows nothing about processes. It talks through a
    {!transport}, which {!Guide_service} makes from the Julia program and a
    test makes from a function. *)

open Maithic_base

type transport = {
  exchange : Json.t -> (Json.t, string) result;  (** Sends one request and returns its answer. *)
  close : unit -> unit;  (** Ends the service. *)
}

type info = {
  dim : int;  (** The size of the model's vectors. *)
  max_length : int;  (** The longest sequence of tokens it reads. *)
  pooling : string;
  mixer : string;
  vocabulary : int;  (** The tokens it knows. *)
  classes : int;  (** The tactic templates it predicts. *)
}

type t

val protocol : string
(** [maithic.guide.service/1]. *)

val connect : transport -> (t, string) result
(** Greets the service. A service that speaks another protocol, or does not
    describe its model, is refused and closed. *)

val info : t -> info
(** The model behind the service, as its greeting described it. *)

type ranking = {
  priors : float list;  (** A probability for each candidate, in the order given; they add up to one. *)
  templates : string list;  (** The template each candidate was read as: the tactic with its arguments taken out. *)
}

val tactic : t -> state:string -> string list -> (ranking, string) result
(** How likely each candidate tactic is to be the next step on a proof
    state, given as Lean prints it. No candidates, no call. *)

val value : t -> state:string -> (float, string) result
(** An estimate between 0 and 1 of how near a state is to being closed. *)

val premise : t -> state:string -> string list -> (float list, string) result
(** A score between 0 and 1 for each premise name: higher for the ones more
    likely to be of use on the state. *)

val requests : t -> int
(** The requests sent so far, the greeting included. *)

val close : t -> unit
(** Says goodbye and ends the service. Calls after this are errors. *)
