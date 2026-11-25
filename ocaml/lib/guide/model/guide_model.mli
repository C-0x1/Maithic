(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A model file, read without running it.

    A trained model is a JSON document of the schema [maithic.guide.model/1].
    This module reads what a person wants to know about one before using
    it: its shape, how many tokens it knows, and the tactic templates it
    predicts with how often each was seen in training. It needs no Julia. *)

open Maithic_base

type template = {
  name : string;  (** The tactic with its arguments taken out, as [simp \[_\] at _]. *)
  count : int;  (** Steps of the training data with this template. *)
  closes : float;  (** The share of those steps that closed a goal. *)
}

type t = {
  dim : int;
  max_length : int;
  pooling : string;
  mixer : string;
  vocabulary : int;
  templates : template list;  (** The first is the class of every other tactic. *)
  parameters : int;  (** The number of weights. *)
}

val schema : string
(** [maithic.guide.model/1]. *)

val of_json : Json.t -> (t, string) result
val load : string -> (t, string) result
(** Reads a model file. The error says what is wrong with the file. *)

val to_json : t -> Json.t
(** The summary as a document: everything but the weights. *)

val render : ?color:bool -> ?top:int -> t -> string
(** The summary for a terminal, with the [top] most frequent templates (15 by default). *)
