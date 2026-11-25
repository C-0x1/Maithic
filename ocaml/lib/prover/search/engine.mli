(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The proving loop: propose, check, read the error, try again.

    Candidates come from three proposers. The portfolio is tried first
    because it is free and deterministic. If it fails and the step-wise
    search is switched on, the search looks for a script one tactic at a
    time, over the proof states of the Bridge. If that fails too and a model
    is available, the model is asked for proofs; the ones that fail are shown
    back to it with Lean's errors in the next round. Every candidate,
    whatever its source, passes the screen, is elaborated by Lean as a whole
    declaration and is judged by the trust policy. A proof becomes [verified]
    only after the fresh-process re-check the policy asks for. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_gateway
open Maithic_ledger

type settings = {
  use_ai : bool;
  rounds : int;  (** model rounds after the portfolio *)
  candidates : int;  (** proofs requested per model round *)
  max_tokens : int;
  limits : Bridge_types.limits;
  search : bool;  (** search one tactic at a time when the portfolio fails; off by default *)
  search_expansions : int;  (** goals the search may expand for one theorem *)
}

val default_settings : settings

val search_name : string
(** The proposer name under which proofs found by the step-wise search are recorded. *)

type result = {
  theorem : string;  (** the declaration as Lean names it, or as written when nothing was accepted *)
  statement : string;  (** as Lean prints it; the source text when nothing was accepted *)
  line : int;
  status : Status.t;
  proof : string option;
  proposer : string option;  (** which proposer found the proof *)
  axioms : string list;
  decision : Trust_policy.decision;
  recheck : Recheck.outcome option;
  attempts : Attempts.attempt list;
  rounds_used : int;
  ai_note : string;  (** why the model was not used, or what went wrong; empty otherwise *)
  elapsed_ms : int;
}

type advisor = string * (env:int -> Lean_source.decl -> string list)
(** A source of proofs outside the prover, with the name its proposals are
    recorded under. It is given the environment the theorem is stated in and
    the declaration, and returns proofs to try, best first. An advisor that
    has nothing to say returns the empty list. *)

val prove :
  ?recheck:(source:string -> string -> (Recheck.outcome, Bridge_error.t) Stdlib.result) ->
  ?gateway:Gateway.t -> ?advisors:advisor list -> ?proposer:Lean_search.proposer -> ?on_attempt:(Attempts.attempt -> unit) ->
  ?on_search:(Search_record.t -> unit) -> bridge:Bridge_client.t ->
  policy:Trust_policy.t -> settings -> source:string -> string -> (result, Fault.t) Stdlib.result
(** [prove ~bridge ~policy settings ~source name] proves the theorem called
    [name] in the text of a Lean file. [recheck] performs the fresh-process
    re-check; without it a policy that requires one leaves the result at
    [checked]. [advisors] are asked after the portfolio and before the search
    and the model. [proposer] names the tactics the step-wise search tries on
    a goal, with their priors; without it the search uses
    {!Lean_search.shape_proposer}. [on_search] is given the record of the
    step-wise search, when one was made. An [Error] means the request could not be carried out at
    all (no such theorem, errors before it, Bridge failure); a theorem that
    simply was not proved is an [Ok] with status [failed]. *)

val result_to_json : ?file:string -> result -> Json.t
(** The result manifest, schema [maithic.prove.result/1]. *)
