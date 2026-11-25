(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The tool belt: the typed tools through which a council acts.

    A member of a council does not run programs. It is handed tools, and a
    tool is a function with a type: text goes in, and what Lean or the
    prover said comes out. Two tools are the ground everything else stands
    on. The Lean check tool compiles text and returns Lean's diagnostics;
    the prove tool runs one bounded attempt of the prover's automation.

    Every use of a tool is counted and charged to the budget of the session,
    and a tool that is asked after the budget is used up answers with that
    and does nothing. The functions that return {!Council_evidence} are the
    only source of it: what they return is made from the answer of Lean to
    that very call. *)

open Maithic_ledger
open Maithic_refute

(** One thing Lean reported. *)
type diagnostic = { severity : string;  (** [error], [warning] or [info]. *) line : int; text : string }

(** What stands behind the belt: Lean and the prover, as functions, so that
    a session can be run without either. Each is about the problem of the
    session: its theorem, where it stands in its file. *)
type raw = {
  compile : untrusted:string -> string -> (diagnostic list, string) result;
      (** Compiles Lean text after what stands before the theorem, and returns what Lean reported. [untrusted] is the part of the
          text that a model wrote; it is screened by the guard of the Bridge before Lean sees any of the text. With an empty
          [untrusted] the text is the session's own. *)
  statement : unit -> Council_evidence.lean;  (** The theorem with [sorry] for a proof. *)
  proof : string -> Council_evidence.lean;  (** The theorem with the proof given. The proof is screened by the guard of the Bridge first. *)
  lookup : string list -> (string * string) list;  (** The signatures of the names that Lean knows where the theorem stands. *)
  prove : (unit -> (string * string, string) result * int) option;
      (** One bounded attempt of the prover's automation: the proof and who found it, or why there is none; and the proofs that were given to Lean. *)
  refute : (unit -> ((string * string) list * Refute_witness.confirmation) option) option;
      (** A counterexample that Lean confirmed, when a search finds one. *)
}

type t

val make : ?budget:Council_budget.t -> raw -> t

val budget : t -> Council_budget.t

val lean_check : t -> ?untrusted:string -> string -> (diagnostic list, string) result
(** The Lean check tool: compiles the text where the theorem stands and
    returns the diagnostics. [untrusted] is the part of the text that came
    from a model: a name, a term, a proof. It passes the guard of the Bridge
    first, which lets through what a proof may hold and nothing that
    declares, imports or runs something. The error is why Lean was not
    asked: the guard refused the text, or the budget is used up. *)

val errors : diagnostic list -> string list
(** The texts of the errors among diagnostics. *)

val lookup : t -> string list -> (string * string) list
(** The names Lean knows, each with its signature. *)

val elaborate : t -> (Council_evidence.t, string) result
(** Asks Lean whether the statement is well formed. *)

(** Why a proof is not evidence. *)
type refusal =
  | Rejected of string  (** Lean was asked: what it said, or which condition the proof does not meet. *)
  | Unavailable of string  (** Lean was not asked, because the budget is used up. The proof was not judged. *)

val check_proof : t -> policy:Trust_policy.t -> statement:string -> string -> (Council_evidence.t, refusal) result
(** Gives the theorem with a proof to Lean. Evidence comes back only for a
    proof of [statement] that the policy accepts. *)

val prove : t -> (string * string, string) result
(** The prove tool: one bounded attempt of the prover's automation. Returns
    a proof and who found it. The proof is a candidate like any other: it
    becomes evidence through {!check_proof}. *)

val refute : t -> statement:string -> ((string * string) list * Council_evidence.t) option
(** Looks for a counterexample; one that Lean confirmed comes back with its evidence. *)

val can_prove : t -> bool
val can_refute : t -> bool

val uses : t -> (string * int) list
(** How often each tool was used: [lean_check], [lookup], [elaborate], [check_proof], [prove], [refute]. *)
