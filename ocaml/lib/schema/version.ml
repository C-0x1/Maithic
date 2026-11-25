(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The single version number of the product. `tools/hygiene` keeps it equal
    to [PROJECT.toml] and to the Lean, Julia and R packages. *)

let version = "0.1.0"

(** Version of the Bridge wire protocol this build speaks. *)
let bridge_protocol = 1

(** The Lean toolchain the Bridge is built with. *)
let lean_toolchain = "4.36.0"
