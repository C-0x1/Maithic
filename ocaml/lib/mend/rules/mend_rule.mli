(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Repair rules and their registry.

    A rule is a treatment that needs no model: it looks at a breakage and
    proposes edits, or none when the breakage is not its kind. A rule does
    not decide whether its edit is right; the application loop gives every
    edit to Lean and keeps only what compiles.

    The registry holds the rules of a run in the order they are tried:
    by priority, lower first, and by name among rules of one priority. *)

type context = {
  source : string;  (** The text of the file as it stands. *)
  renames : (string * string) list;  (** A table of renames: old name, new name. *)
}

type t = {
  name : string;
  priority : int;  (** Lower is tried earlier. *)
  summary : string;  (** What the rule does, in a sentence. *)
  propose : context -> Mend_breakage.t -> Mend_edit.t list;  (** Edits for a breakage, best first; none when the rule does not apply. *)
}

type registry

val registry : t list -> registry
(** A registry of the given rules. A name is held once: a later rule of the
    same name replaces the earlier. *)

val rules : registry -> t list
(** The rules in the order they are tried. *)

val names : registry -> string list
val find : registry -> string -> t option

val add : registry -> t -> registry
(** The registry with one more rule, in its place by priority. *)

val without : registry -> string -> registry

val proposals : registry -> context -> Mend_breakage.t -> Mend_edit.t list
(** Every edit the rules propose for a breakage, in the order of the rules. *)
