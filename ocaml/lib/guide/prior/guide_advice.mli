(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Advice that never fails.

    A search must not depend on its guide. Julia may be missing, the model
    file may be wrong, the service may stop in the middle of a proof; none
    of this is a reason for a search to end. This module stands between the
    two. While the service answers, its answers are passed on. From the
    first failure on, the guide is down for good: every question is answered
    at once with the answer that says nothing, equal probabilities for all
    candidates and a value of zero, the reason is kept for the report, and
    the service is not asked again.

    A search with a guide that is down is the search without a guide. *)

open Maithic_base

type status = Up | Down of string  (** Why the service is not used. *)

type t

val of_client : Guide_client.t -> t
val unavailable : string -> t
(** A guide that is down from the start, with the reason. *)

val start : (unit -> (Guide_client.t, string) result) -> t
(** Connects now. A connection that fails gives a guide that is down. *)

val priors : t -> state:string -> string list -> float list
(** A probability for each candidate: the service's, or [1/n] each. *)

val value : t -> state:string -> float
(** The service's estimate of how near the state is to closed, or [0]. *)

val premise_scores : t -> state:string -> string list -> float list
(** The service's score for each premise name, or [0] each. *)

val uniform : int -> float list
(** [n] equal probabilities. *)

val status : t -> status

type counts = {
  asked : int;  (** Questions put to the guide. *)
  answered : int;  (** Of these, the ones the service answered. *)
}

val counts : t -> counts
val info : t -> Guide_client.info option
(** The model, while the service is up. *)

val close : t -> unit
(** Ends the service. Questions after this are answered as when it is down;
    the status and the counts stay as they were, for the report. *)

val to_json : ?model:string -> t -> Json.t
(** What a report says about the guide of a run: the model file, whether
    the service was up at the end, the reason if it was not, and the counts. *)

val describe : ?model:string -> t -> string
(** The same in a sentence. *)
