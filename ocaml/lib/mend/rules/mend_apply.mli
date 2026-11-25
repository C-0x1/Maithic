(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The rule application loop.

    The loop takes the first breakage of a file that a rule has an edit
    for, makes the edit, and gives the changed file to Lean. The edit is
    kept only if Lean no longer reports that breakage, reports nothing that
    was not broken before, and reports no more errors than it did; otherwise
    the edit is undone and the next is tried. After an edit that
    was kept the file is read again from the top, because one repair can
    uncover or resolve another. The loop ends when no rule has an edit left
    that Lean accepts.

    Nothing is taken on trust: a rule proposes, and the edit that stays is
    one Lean has compiled. *)

type repair = {
  breakage : Mend_breakage.t;  (** What was broken, as it was found. *)
  edit : Mend_edit.t;  (** The edit that was kept. *)
  after : string;  (** The text of the declaration, or of the line, after the edit. *)
  tried : int;  (** Edits given to Lean for this breakage, the one that was kept included. *)
}

type outcome = {
  source : string;  (** The text of the file after every repair. *)
  repairs : repair list;  (** In the order they were made. *)
  remaining : Mend_breakage.t list;  (** What no rule could repair. *)
  checks : int;  (** How often the file was given to Lean. *)
}

val default_rules : Mend_rule.registry
(** The rename rule, then the suggestion rule. *)

val max_steps : int
(** The most repairs the loop makes on one file: fifty. *)

val admits : string -> bool
(** Whether the guard lets the new text of an edit through. *)

val run :
  ?max_steps:int -> ?rules:Mend_rule.registry -> ?renames:(string * string) list -> check:Mend_detect.check -> file:string -> string ->
  (outcome, string) result
(** Repairs a file with the rules. An error means Lean could not be asked at
    all; a file no rule can help is an [Ok] with every breakage remaining. *)
