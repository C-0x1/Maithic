(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Edits of source text.

    A repair is a list of replacements: this stretch of the file becomes
    that text. Lean reports positions as a line and a column in code
    points; a replacement is in bytes of the file, which is what text is cut
    by. This module converts the one into the other and applies
    replacements, refusing lists whose stretches overlap. *)

open Maithic_bridge

type replacement = {
  start : int;  (** The first byte that is replaced. *)
  stop : int;  (** The byte after the last. *)
  text : string;  (** What stands there afterwards. *)
}

type t = {
  rule : string;  (** The rule that proposes the edit. *)
  description : string;  (** What the edit does, in a sentence. *)
  replacements : replacement list;
  untrusted : string;  (** The text of the edit that did not come from the file: what the guard screens. *)
}

val offset : string -> line:int -> column:int -> int option
(** The byte at a line, one-based, and a column, zero-based in code points. *)

val span : string -> Bridge_types.diagnostic -> (int * int) option
(** The bytes a diagnostic covers; nothing when it has no end or lies outside the text. *)

val apply : string -> replacement list -> (string, string) result
(** The text with the replacements made. Replacements that overlap, or lie
    outside the text, are an error; their order does not matter. *)

val find_word : string -> string -> from:int -> upto:int -> int option
(** The first place in a stretch where a name stands as a whole name: not as
    a part of a longer one. *)

val declaration : Maithic_prover.Lean_source.decl -> string -> string
(** A theorem with another proof, written the way the file writes its
    proofs: a tactic proof keeps its [by] on the line of the [:=] and its
    steps at the depth they have, so that a repair changes the lines it
    changes and no others. *)

val put_proof : Maithic_prover.Lean_source.decl -> string -> string
(** The whole file with the proof of a theorem replaced, as {!declaration} writes it. *)

val changed_lines : string -> string -> int
(** How many lines of the first text are not lines of the second, plus how
    many of the second are not lines of the first: a measure of how much an
    edit changes. *)
