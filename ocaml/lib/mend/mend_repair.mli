(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Repairing a file, from the cheapest remedy to the dearest.

    First the rules, which cost a compilation each and no model call: names
    are replaced, suggestions of Lean are taken. What is left and is a proof
    that no longer compiles goes to the repair agent: a model is asked for
    the smallest change, and then, if that fails, the prover proves the
    statement again from nothing. Each breakage is recorded with what was
    done about it. At the end the whole file is given to Lean once more, and
    what Lean still reports is what the report says is left: a repair that
    the last check does not bear out is not counted. *)

open Maithic_base
open Maithic_prover

type outcome =
  | By_rule of string  (** The rule whose edit was kept. *)
  | By_model of int  (** A model's repair; the round it came from. *)
  | By_prover of string  (** The prover's new proof; the proposer that found it. *)
  | Unrepaired of string  (** Why. *)

type entry = {
  breakage : Mend_breakage.t;
  outcome : outcome;
  description : string;  (** What was done, in a sentence; empty when nothing was. *)
  after : string;  (** The declaration, or the line, after the repair; empty when unrepaired. *)
  tried : int;  (** Edits or proofs given to Lean for this breakage. *)
  changed_lines : int;  (** Lines that differ between the text before and after. *)
}

type t = {
  file : string;
  entries : entry list;  (** One for each breakage that was found, in the order they were dealt with. *)
  remaining : Mend_breakage.t list;  (** What Lean still reports about the file at the end. *)
  source : string;  (** The text of the file after the repairs. *)
  checks : int;  (** How often the whole file was given to Lean. *)
}

type tools = {
  check_file : Mend_detect.check;  (** Elaborates the text of a whole file. *)
  check_theorem : string -> Lean_source.decl -> Mend_check.t;
      (** A check of one theorem of a file text, in the environment before it. *)
  ask : (decl:Lean_source.decl -> breakage:Mend_breakage.t -> Mend_agent.ask) option;  (** The model, when there is one. *)
  reprove : (source:string -> Lean_source.decl -> Mend_agent.reprove) option;  (** The prover, when it may be used. *)
}

val sanitize : string -> except:string -> string list -> string
(** [sanitize source ~except broken] is the file with the proof of every
    theorem named in [broken], other than [except], left open. A theorem can
    then be checked where it stands even though proofs around it are broken:
    an open proof is a warning to Lean, a broken one an error that stops
    the environment from being built. *)

val run :
  ?rules:Mend_rule.registry -> ?renames:(string * string) list -> ?settings:Mend_agent.settings -> tools -> file:string -> string ->
  (t, string) result
(** Repairs the text of a file. An error means Lean could not be asked. *)

val repaired : entry -> bool
val clean : t -> bool
(** Lean reports nothing more about the file. *)

val schema : string
(** [maithic.mend.report/1]. *)

val to_json : t -> Json.t
val render : ?color:bool -> t -> string
val outcome_stage : outcome -> string
(** [rule], [model], [prove] or [none]. *)
