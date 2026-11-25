(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A dataset on disk.

    A dataset is a directory: [manifest.json], and under [shards/] files of
    JSON lines, one record on each line. Records are named by the hash of
    their content and shards by the hash of theirs, so a dataset can be
    checked against its manifest by anyone, and two datasets made from the
    same sources are the same bytes. Nothing in a dataset says when it was
    made. *)

open Maithic_base

type shard = { kind : Corpus_record.kind; file : string;  (** Below the dataset directory. *) records : int; sha256 : string }

type manifest = {
  name : string;
  version : string;
  sources : Json.t list;  (** Each with [repository], [revision], [toolchain] and [files]. *)
  counts : (Corpus_record.kind * int) list;
  shards : shard list;
}

val manifest_to_json : manifest -> Json.t
val manifest_of_json : Json.t -> (manifest, string) result

val validate : Corpus_record.kind -> Json.t -> (unit, string) result
(** Whether a record follows the schema of its kind and carries the
    identifier of its content. *)

val write :
  ?shard_size:int -> dir:string -> name:string -> version:string -> sources:Json.t list -> (Corpus_record.kind * Json.t) list ->
  (manifest, string) result
(** Writes a dataset. Every record is validated first, and one that does not
    follow its schema stops the writing: nothing is written then. A record
    that occurs twice is stored once. Records of a kind go into shards of at
    most [shard_size] records, five thousand unless stated, in the order
    given. Shards that an earlier writing left in the directory and that the
    new manifest does not list are removed, so the directory holds one
    dataset; other files are left alone. *)

val read_manifest : string -> (manifest, string) result

val load : string -> Corpus_record.kind -> (Json.t list, string) result
(** The records of a kind, in the order of the manifest's shards. A shard
    whose content does not have the hash the manifest states is an error. *)

val verify : string -> string list
(** Everything in which a dataset on disk differs from its manifest: missing
    shards, hashes and counts that do not match, records that do not follow
    their schema, and files under [shards/] that the manifest does not list.
    Empty when the dataset is sound. *)
