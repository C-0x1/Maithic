(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Harvesting: from the files of a Lean project to a dataset.

    Every file is given to a tracer, which is the Bridge with tracing
    switched on; what it reports becomes records, and the records become a
    dataset. A file that cannot be traced is noted and left out, and the
    harvest goes on. *)

open Maithic_base
open Maithic_bridge

type outcome = {
  file : string;
  result : string;  (** [ok], [errors] when the file elaborated with errors, or [failed] when the tracer did not answer. *)
  errors : int;
  theorems : int;
  steps : int;
  reason : string;  (** Why the tracer failed; empty otherwise. *)
}

type record = {
  repository : string;
  revision : string;
  toolchain : string;
  files : outcome list;
  dataset : string;  (** The directory the dataset was written to. *)
  manifest : Corpus_store.manifest;
}

val revision_of : string -> string
(** The commit a directory is at, read from the [.git] directory at or above
    it; [unknown] when there is none. *)

val trace : tracer:(string -> (Extraction.t, string) result) -> string list -> Extraction.t list * outcome list
(** Runs the tracer over the files, in order. *)

val harvest :
  ?shard_size:int -> ?revision:string -> tracer:(string -> (Extraction.t, string) result) -> repository:string -> toolchain:string ->
  name:string -> version:string -> dir:string -> string list -> (record, string) result
(** Traces the files, writes the dataset to [dir] and the record of the
    harvest next to its manifest, as [harvest.json]. The revision recorded is
    the commit the repository is at, unless one is stated: for sources that
    were unpacked from an archive, or for a dataset that must not change
    with every commit. *)

val record_to_json : record -> Json.t
