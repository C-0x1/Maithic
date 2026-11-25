(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A session of the council: one problem, worked on in turns.

    {v
problem ─▶ examine ─▶ round 1: research, prove, verify, critique ─▶ round 2: prove, verify, … ─▶ synthesis
              │                              │
           (verifier)                 every proof goes to Lean
    v}

    The coordinator takes the problem in, puts the statement and the claim
    on the blackboard, and then gives one member after another its turn, in
    the order of the schedule. After each turn it collects what the member
    did: the outcome of the turn, what was posted, and what it cost. The
    session ends when the claim is settled, when the rounds are over, or
    when the budget is used up. Then the answer is synthesized from what
    Lean confirmed.

    One rule holds throughout: a claim is verified when Lean's kernel has
    accepted a proof of it, and by nothing a member says. *)

open Maithic_base
open Maithic_schema
open Maithic_ledger

type settings = {
  rounds : int;  (** Rounds after the examination. *)
  candidates : int;  (** Proofs the prover asks for in one turn. *)
}

val default_settings : settings
(** Three rounds, three candidates. *)

type model = purpose:string -> schema:string * Json.t -> (string * string) list -> (string * int * int, string) result
(** A model: the name of a prompt, the schema of the answer, the values of
    the placeholders; back come the text and the tokens read and written. *)

(** What the coordinator collected from one turn. *)
type turn_result = {
  turn : Council_schedule.turn;
  status : string;  (** [done], [idle] or [failed]. *)
  summary : string;
  posted : int list;  (** The items the member posted. *)
  model_calls : int;
  lean_calls : int;
  seconds : float;
}

type ending =
  | Settled  (** The claim was verified or refuted. *)
  | Rounds_over  (** Every turn was taken. *)
  | Out_of_budget of string  (** Which limit was reached. *)
  | Stopped of string  (** The session could not go on; why. *)

type t = {
  problem : Council_problem.t;
  settings : settings;
  policy : Trust_policy.t;
  board : Council_board.t;
  transcript : Council_transcript.t;
  statement : int;
  claim : int;
  turns : turn_result list;
  ending : ending;
  answer : Council_synthesis.t;
  tools : Council_tools.t;
  with_model : bool;
}

val run : ?settings:settings -> ?clock:(unit -> float) -> ?model:model -> tools:Council_tools.t -> policy:Trust_policy.t -> Council_problem.t -> t
(** Holds a session. Without a model only the verifier takes its turn: the
    statement is examined and automation tries, and no round follows. *)

val status : t -> Council_board.status
(** The status of the claim at the end. *)

val schema : string
(** [maithic.council.session/1]. *)

val to_json : t -> Json.t
(** The whole session: the problem, the settings and the schedule, how it
    ended, the answer, what each turn did, the blackboard, the transcript,
    the budget and the use of the tools. *)

val to_markdown : t -> string
(** The transcript as Markdown, with the answer at its end. *)

val render : ?color:bool -> t -> string
(** The session for a terminal: a line for each turn, and the answer. *)

val summary : t -> string
(** The session in a line. *)

val exit_code : t -> Exit_code.t
(** [Success] for a verified claim, [Refuted] for a refuted one, [Not_verified] otherwise. *)
