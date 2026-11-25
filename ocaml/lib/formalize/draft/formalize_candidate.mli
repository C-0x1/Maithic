(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A candidate statement.

    A candidate is one way of saying an informal problem in Lean: the
    binders and the claim of a theorem, the choices of reading it makes, and
    where it came from. It is text from a model, and it is treated as such:
    it has no name of its own, no proof, and nothing in it is believed until
    Lean has elaborated it. *)

open Maithic_base

type provenance = {
  origin : string;  (** [draft] for a statement a model drafted, [repair] for one it repaired, [given] for one a person wrote. *)
  provider : string;  (** The provider that answered; empty for a statement a person wrote. *)
  model : string;
  prompt : string;  (** The prompt that asked for it. *)
  round : int;  (** Zero for a draft; the repair round otherwise. *)
}

type t = {
  id : int;  (** One for the first candidate of a problem, in the order the model gave them. *)
  name : string;  (** The name the theorem is given, such as [problem_1]. *)
  signature : string;  (** The binders, a colon and the claim: what stands between the name and the [:=]. *)
  imports : string list;  (** The modules the statement is elaborated with. *)
  reading : string;  (** The choices this statement makes, in the model's words. *)
  provenance : provenance;
}

val given : provenance

val signature_of : string -> string
(** What a model returned for a statement, reduced to a signature: a leading
    [theorem NAME] or [lemma NAME] and a trailing [:= sorry] in any of its
    spellings are taken off, and so are code fences. *)

val key : t -> string
(** The signature with every run of blank space as one space: two
    candidates with the same key are the same text. *)

val theorem : ?proof:string -> t -> string
(** The candidate as a theorem: its name, its signature and a proof, [sorry]
    by default. Names that are not declared are errors in it: Lean's habit
    of taking an unknown name for a variable is switched off. *)

val header : t -> string
(** The import lines of the candidate, each on a line; empty when it has none. *)

val answer : string -> Json.t option
(** The JSON object in a model's answer, wherever in the text it stands. *)

val of_answer : name:string -> imports:string list -> provenance:provenance -> count:int -> string -> t list
(** Reads the candidates out of a model's answer: at most [count], without
    empty statements, and without a statement whose text has come before. *)

val to_json : t -> Json.t
