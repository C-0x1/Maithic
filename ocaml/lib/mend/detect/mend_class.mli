(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The classes of breakage.

    When code stops compiling, what Lean says falls into a few kinds, and
    each kind has a treatment that is worth trying first. A name that is
    gone is looked up in a table of renames. A name that is deprecated is
    replaced by the one Lean names. A suggestion Lean made is taken. A
    tactic that no longer closes its goal is a question for a proof search
    or a model. The class of a breakage is read from the message of its
    first diagnostic, and it decides nothing about correctness: every repair
    is checked by Lean whatever class it came from. *)

open Maithic_bridge

type t =
  | Unknown_identifier of string  (** A name that is used does not exist; the name, as Lean prints it. *)
  | Deprecated of { old_name : string; new_name : string }
      (** A name that is used is deprecated. [new_name] is the replacement Lean names, or empty when it names none. *)
  | Unsolved_goals  (** A tactic does not close the goal it stands on, or goals are left at the end of a proof. *)
  | Type_mismatch  (** A term does not have the type its place asks for. *)
  | Suggestion of string list  (** Lean answered an exploring tactic such as [exact?] with what to write instead. *)
  | Other  (** Any other error. *)

val name : t -> string
(** [unknown-identifier], [deprecated], [unsolved-goals], [type-mismatch], [suggestion] or [other]. *)

val names : string list
(** The names of the classes, in the order above. *)

val detail : t -> string
(** What tells two breakages of one class apart: the name that is unknown or
    deprecated, the first suggestion; empty for the other classes. *)

val treatment : t -> string
(** The treatment that is tried first for the class, in a sentence. *)

val of_diagnostic : Bridge_types.diagnostic -> t option
(** The class of what a diagnostic reports, or nothing when it reports no
    breakage: a warning that is not a deprecation, or an information that is
    not a suggestion. Every error has a class. *)

val quoted : string -> string list
(** The texts between backticks or single quotes in a message, in order:
    the names Lean mentions. *)
