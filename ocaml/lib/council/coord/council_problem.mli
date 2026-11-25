(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The problem a council is given.

    A problem is a theorem of a Lean file whose proof is to be found, with
    what stands before it in the file, which is its context: the definitions
    it speaks of and the lemmas already proved. A person may add, in words,
    what they are after or what they suspect; every member that asks a model
    passes that on. *)

open Maithic_base
open Maithic_prover

type t = {
  file : string;  (** The file as the person named it. *)
  theorem : string;
  statement : string;  (** The theorem as it is written, on one line, without its proof. *)
  line : int;
  context : string;  (** The file before the theorem. *)
  goal : string;  (** What the person asks for, in words; may be empty. *)
  decl : Lean_source.decl;
  source : string;  (** The whole file. *)
}

val of_source : ?theorem:string -> ?goal:string -> file:string -> string -> (t, string) result
(** Takes the theorem that is named; without a name, the one theorem of the
    file whose proof is unfinished. The error says what the file holds
    instead. *)

val excerpt : ?limit:int -> t -> string
(** The context as a model is shown it: the end of it, [limit] characters
    at most (6000 by default), cut at the start of a line. *)

val with_proof : t -> string -> string
(** The file with the proof of the theorem replaced. *)

val to_json : t -> Json.t
(** The file, the theorem, its statement and line, and the goal when there is one. *)
