(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Looking for what is wrong with a statement before anyone tries to prove it.

    Three methods are tried in turn, within a budget:

    - {b test}: the statement is run on sampled values in Lean, round after
      round with a new seed, until a case fails;
    - {b oracle}: an arithmetic statement is sent to a solver, which answers
      a false one with an exact point where it fails;
    - {b vacuity}: Lean looks for hypotheses that contradict each other and
      for variables that range over nothing, which make a statement true and
      empty.

    A counterexample from the first two is a candidate. It becomes a
    refutation when Lean accepts a proof that the statement is false at its
    values ({!Refute_witness}); a candidate Lean does not confirm is reported
    as what it is. A vacuity finding is certain, because Lean checked the
    proof behind it. Finding nothing proves nothing. *)

open Maithic_base
open Maithic_schema

type method_ = Test | Oracle | Vacuity

val methods : method_ list
(** All of them, in the order they are tried. *)

val method_to_string : method_ -> string
val method_of_string : string -> method_ option

val methods_of_string : string -> (method_ list, string) result
(** A selection as written on the command line: [all], or names separated by
    commas. The order of trying is always that of {!methods}. *)

type budget = {
  cases : int;  (** Cases that must pass in one round of testing. *)
  size : int;  (** The size of the largest case. *)
  seed : int;  (** The seed of the first round; each round takes the next. *)
  rounds : int;  (** Rounds of testing. *)
  seconds : float;  (** No method is started after this much time. *)
}

val default_budget : budget

type oracle_answer =
  | Point of Refute_witness.assignment  (** The solver refuted the goal at this point. *)
  | Proved of string  (** The solver proved the goal; its claim. *)
  | No_answer of string  (** Unknown, or the statement is not one a solver takes; why. *)

(** What the search needs from the outside. Each is a function so that the
    search can be run without Lean and without a solver. *)
type tools = {
  test : cases:int -> size:int -> seed:int -> (Refute_probe.test_report, string) result;
  oracle : unit -> (oracle_answer, string) result;  (** [Error] when no solver could be asked. *)
  vacuity : unit -> (Refute_probe.vacuity, string) result;
  confirm : Refute_witness.assignment -> (Refute_witness.confirmation, string) result;
  clock : unit -> float;
}

type step = {
  method_ : method_;
  outcome : string;
      (** [counterexample], [passed], [gave-up], [not-applicable], [proved],
          [unknown], [unavailable], [vacuous], [trivial], [nothing] or
          [skipped]. *)
  detail : string;
  candidate : Refute_witness.assignment;  (** Empty when the step found none. *)
  confirmation : Refute_witness.confirmation option;
  rejection : string;  (** Why Lean did not confirm the candidate. *)
  seconds : float;
}

type verdict =
  | Refuted of { counterexample : Refute_witness.assignment; by : method_; confirmation : Refute_witness.confirmation }
  | Vacuous of string  (** The reason. *)
  | Unconfirmed of { counterexample : Refute_witness.assignment; by : method_; why : string }
      (** A counterexample was found and Lean did not confirm it. *)
  | Not_refuted

val verdict_to_string : verdict -> string
(** [refuted], [vacuous], [unconfirmed] or [not-refuted]. *)

type result = {
  file : string;
  theorem : string;
  line : int;
  statement : string;
  methods : method_ list;
  budget : budget;
  steps : step list;
  verdict : verdict;
  seconds : float;
}

val search : ?methods:method_ list -> ?budget:budget -> tools -> verdict * step list
(** Runs the methods. The search stops at the first confirmed counterexample. *)

val run : ?methods:method_ list -> ?budget:budget -> file:string -> theorem:string -> line:int -> statement:string -> tools -> result

val schema : string
(** [maithic.refute.report/1]. *)

val result_to_json : result -> Json.t
(** One statement: its verdict, what was found, and every step that was taken. *)

val to_json : file:string -> result list -> Json.t
(** The report of a run: the results of the statements of a file, in its
    order, and their counts by verdict. *)

val render : ?color:bool -> result -> string
(** The result for a terminal: the statement, a line for each step, the
    verdict, and the refutation Lean accepted. *)

val exit_code : result list -> Exit_code.t
(** [Refuted] when a statement was refuted; [Not_verified] when one is
    vacuous or has a counterexample Lean did not confirm; [Success] when
    nothing was found. *)

val summary : result list -> string
