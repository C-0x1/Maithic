(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The first line of defence around Lean code that Maithic did not write.

    Lean can run programs while it elaborates a file. Before text from a
    model or from an untrusted repository reaches the Bridge it is screened
    here; the Bridge screens it again on its side with the same rules, and
    the axiom audit after elaboration is the last layer. The module also
    builds the reduced environment that Lean workers are started in. *)

type finding = { construct : string; reason : string }

val violations : string -> finding list
(** Every forbidden construct in a candidate proof: evaluation commands,
    new axioms and declarations, meta-programs, option changes, compiler
    hooks, process and file access, quoted identifiers. Empty for an
    ordinary proof. *)

val admits : string -> bool
(** [true] when {!violations} is empty and the candidate is not too long. *)

val max_candidate_chars : int

val scrub_env : string array -> (string * string) list
(** [scrub_env (Unix.environment ())] keeps only allowlisted variables, the
    ones a Lean toolchain needs, and drops everything else. No variable
    whose name suggests a credential is ever kept. *)

val is_secret_name : string -> bool
(** [true] for names such as [ANTHROPIC_API_KEY], [GITHUB_TOKEN] or
    [DB_PASSWORD]. *)
