(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Reading Lean source text without elaborating it.

    The prover needs to find a theorem in a file, split it into the part
    that states it and the part that proves it, and know what comes before
    and after. That is a matter of text, done here with a scanner that
    understands comments, strings and brackets; everything about meaning is
    left to Lean. *)

val mask : string -> string
(** The source with the contents of comments and string literals replaced by
    spaces, byte for byte, so that offsets agree with the original and a
    search in the result only ever finds code. Line breaks are kept. *)

type decl = {
  name : string;  (** as written after the keyword *)
  keyword : string;  (** ["theorem"] or ["lemma"] *)
  line : int;  (** one-based line of the keyword *)
  prelude : string;  (** the file up to the start of the declaration *)
  doc : string;
      (** what stands before the header and belongs to the declaration: the lines of commands that
          end in [in], such as [set_option maxHeartbeats 400000 in], and the documentation
          comment with the blank space after it; or nothing *)
  header : string;
      (** from the attributes and modifiers of the declaration, whether they stand on the line of
          the keyword or on lines of their own before it, to just before [:=] *)
  proof : string;  (** the text after [:=], trimmed *)
  rest : string;  (** the file after the declaration *)
}

val declarations : string -> decl list
(** Every [theorem] and [lemma] of a file that has a [:=] proof, in order. *)

val find : string -> string -> (decl, string) result
(** [find source name] is the declaration called [name]; a name may be given
    without its namespace. The error says what is in the file instead. *)

val has_sorry : decl -> bool
(** The proof contains [sorry] or [admit] as code. *)

val assemble : decl -> string -> string
(** [assemble decl proof] is the declaration with the given proof, under its
    documentation comment. *)

val with_proof : decl -> string -> string
(** The whole file with the proof of the declaration replaced. *)

val assemble_by : decl -> string -> string
(** As {!assemble}, with a tactic proof laid out the way a person writes it:
    [:= by] at the end of the statement and the tactics under it, indented
    as they were. A proof whose tactics are not indented is laid out as
    {!assemble} does. *)

val with_proof_by : decl -> string -> string
(** The whole file with the proof of the declaration replaced, laid out as {!assemble_by} does. *)

val statement : decl -> string
(** The statement on one line: the header without leading blank space and
    with runs of blank space collapsed. *)
