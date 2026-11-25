(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The rename rule.

    Most breakage after an upgrade is names: a lemma was renamed, and every
    use of the old name is an error, or a warning while the old name lives
    on as a deprecated alias. Lean says which name it could not find, and
    for a deprecated name it says what to use instead. This rule replaces
    the names of a breakage where a replacement is known: from Lean's own
    deprecation message, or from a table of renames the user supplies, as a
    library does with its release notes. *)

val rule : Mend_rule.t
(** Priority 10. *)

val parse_table : string -> ((string * string) list, string) result
(** Reads a table of renames from TOML: a table [renames] whose keys are the
    old names and whose values are the new,

    {v
    [renames]
    "Nat.add_commutes" = "Nat.add_comm"
    v}

    in the order of the file. A name that is renamed to itself, or twice, is
    an error. *)

val load_table : string -> ((string * string) list, string) result
(** Reads a table from a file. *)

val replacement : (string * string) list -> Mend_class.t -> string option
(** The new name for what a diagnostic reports, if one is known: Lean's own
    for a deprecated name, the table's for a name that is unknown or
    deprecated without a successor. *)
