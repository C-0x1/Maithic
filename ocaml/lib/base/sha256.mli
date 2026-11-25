(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** SHA-256 (FIPS 180-4).

    Maithic identifies requests, cassettes, statements and bundles by their
    SHA-256 digest. The implementation is self-contained so that the core has
    no dependency outside the OCaml distribution; it is checked against the
    standard test vectors in the test suite. *)

val hex : string -> string
(** [hex s] is the digest of [s] as 64 lowercase hexadecimal characters. *)

val file : string -> (string, string) result
(** [file path] is the digest of the contents of the file at [path]. *)
