(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What is sent to Lean to examine a statement, and what comes back.

    The statement is examined where it stands: the file up to the theorem is
    elaborated with the statement tests imported, the theorem is declared
    with [sorry] for a proof, and commands that name it are run after it.
    Each command prints its report as JSON. *)

open Maithic_base
open Maithic_bridge
open Maithic_prover

val library : string
(** The module that holds the commands: [Maithic.Refute.Vacuity.Command]. *)

val with_library : string -> string
(** A source with the import of {!library} put first, which is where an
    import may always stand; after the [module] line when the file has one. *)

val declaration : Lean_source.decl -> string
(** The theorem with [sorry] for a proof. *)

val test_command : cases:int -> size:int -> seed:int -> string -> string
(** [#maithic_test json] with its settings, on a theorem by name. *)

val vacuity_command : string -> string

type verdict = Passed | Falsified | Gave_up

type test_report = {
  verdict : verdict;
  cases : int;  (** Cases run to the end, the failing one included. *)
  discarded : int;  (** Cases left out because a hypothesis was false. *)
  shrinks : int;
  seed : int;
  counterexample : (string * string) list;
}

val test_report_of_json : Json.t -> (test_report, string) result

type vacuity = {
  vacuous : bool;
  contradiction : (string list * string) option;  (** The hypotheses that contradict each other, and the tactic that showed it. *)
  empty_domains : (string * string * string) list;  (** Variable, its type, and the tactic that showed the type empty. *)
  trivial_by : string option;  (** The tactic that proved the conclusion with no hypothesis. *)
}

val vacuity_of_json : Json.t -> (vacuity, string) result

val describe_vacuity : vacuity -> string
(** In a sentence: what makes the statement vacuous, or that its conclusion
    holds without its hypotheses; empty when neither. *)

val printed : Bridge_types.report -> (Json.t, string) result
(** The JSON that a command printed: the last message of a report that is
    one. When the command failed instead, the error is what Lean said. *)

val library_missing : string -> bool
(** Whether what Lean said means that {!library} is not on its search path. *)

val witness_check : ?limits:Bridge_types.limits -> Bridge_client.t -> env:int -> theorem:string -> untrusted:string -> string -> Refute_witness.checked
(** The check a confirmation needs ({!Refute_witness.confirm}), on a Bridge
    whose environment [env] declares the theorem: the text of a refutation
    is given to Lean as a candidate, so that the values in it pass the guard
    of the Bridge, and Lean's answer about the refutation is read. *)
