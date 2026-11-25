(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A Julia package of the source tree, and how it is run.

    The numeric engines and the guidance service are Julia packages under
    [julia/]. Each is run as a process: an entry point in the package's
    [bin] directory, started with the package as its project. This module
    finds a package and makes it ready to load. *)

type t = {
  julia : string;  (** The Julia executable. *)
  project : string;  (** The directory of the package. *)
}

val locate : package:string -> ?hint:string -> ?julia:string -> ?project:string -> cwd:string -> unit -> (t, string) result
(** Finds the executable on the path and the package [julia/<package>] in
    the source tree around [cwd], unless [project] names its directory.
    [hint] is added to the message when the package is not found: what the
    user can set. *)

val script : t -> string -> string
(** The path of an entry point in the package's [bin] directory. *)

val arguments : t -> string list
(** The arguments that start Julia on the package: no startup file, and the
    package as the project. *)

val offline : (string * string) list
(** The environment that keeps Julia's package manager off the network. The
    packages depend on the standard library and on their siblings by path,
    so nothing is ever fetched. *)

val setup_command : t -> string * string list
(** The program and arguments that resolve the dependencies of the package. *)

val prepare : ?timeout:float -> t -> (unit, string) result
(** Makes the package ready to load. Julia loads a package through a
    manifest of its dependencies, and a source tree that was never used has
    none: it is written once, by {!setup_command}, with no use of the
    network. Nothing is run when the manifest is there. *)
