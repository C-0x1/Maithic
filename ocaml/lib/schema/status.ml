(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The status vocabulary. These eight words are the only labels the product
    puts on a mathematical claim, in every interface and every document. *)

type t =
  | Verified  (** kernel accepted the requested statement and the trust policy passed *)
  | Checked  (** kernel accepted a proof; a policy requirement is still pending *)
  | Refuted  (** a counterexample was confirmed by the kernel *)
  | Conjectured  (** stated, neither proved nor refuted *)
  | Unverified  (** produced by a model or engine and not checked *)
  | Unknown  (** a verification method ended without a decision *)
  | Failed  (** the attempt ended without a proof, with a recorded reason *)
  | Abstained  (** the system declined to choose *)

let all = [ Verified; Checked; Refuted; Conjectured; Unverified; Unknown; Failed; Abstained ]

let to_string = function
  | Verified -> "verified"
  | Checked -> "checked"
  | Refuted -> "refuted"
  | Conjectured -> "conjectured"
  | Unverified -> "unverified"
  | Unknown -> "unknown"
  | Failed -> "failed"
  | Abstained -> "abstained"

let of_string s = List.find_opt (fun st -> to_string st = s) all

let describe = function
  | Verified -> "The kernel accepted the proof of exactly the requested statement and the trust policy passed."
  | Checked -> "The kernel accepted a proof, but a policy requirement is still pending."
  | Refuted -> "A counterexample to the statement was confirmed by the kernel."
  | Conjectured -> "Stated and not yet proved or refuted."
  | Unverified -> "Produced by a model or engine and not checked."
  | Unknown -> "A verification method ended without a decision."
  | Failed -> "The attempt ended without a proof, with a recorded reason."
  | Abstained -> "The system declined to choose."

(** How much a status may be relied on when it is combined with others:
    kernel-backed statuses rank above claims, and claims above no result. *)
let trust_rank = function
  | Verified -> 3
  | Checked -> 2
  | Refuted -> 2
  | Conjectured | Unverified -> 1
  | Unknown | Failed | Abstained -> 0

(** The status of a whole that depends on two parts: the weaker one decides.
    A result built on a verified lemma and an unverified one is unverified. *)
let weakest a b =
  if trust_rank a < trust_rank b then a
  else if trust_rank b < trust_rank a then b
  else if a = b then a
  else (* same rank, different labels: keep the first in vocabulary order *)
    List.find (fun s -> s = a || s = b) all

let weakest_of = function [] -> Verified | first :: rest -> List.fold_left weakest first rest

(** Only [Verified] may be presented as an established result. *)
let is_established = function Verified -> true | _ -> false

let to_json s = Maithic_base.Json.String (to_string s)
