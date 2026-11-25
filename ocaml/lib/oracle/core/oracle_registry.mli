(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Oracles and where to find them.

    An oracle is a module with a name, the kinds of goal and the number types
    it takes, and a function that answers a goal within a time limit. The
    registry holds the oracles of a session and finds the ones that fit a
    goal. The oracles that ship with Maithic are engines of the Julia package
    [MaithicOracle], reached by running its entry point as a process: the
    goal goes in on standard input and the answer comes back on standard
    output. *)

module type ORACLE = sig
  val name : string
  val description : string
  val kinds : Oracle_goal.kind list
  val domains : Oracle_goal.domain list

  val solve : ?timeout:float -> Oracle_goal.t -> (Oracle_outcome.t, string) result
  (** The answer, or why none could be had: the engine is not installed, it
      did not finish in time, or what it wrote is not an answer. An error is
      about the question, never about the goal. *)
end

type t

val create : (module ORACLE) list -> t
(** A registry. A name may be used once; a later oracle of the same name replaces the earlier. *)

val names : t -> string list
val all : t -> (module ORACLE) list
val find : t -> string -> (module ORACLE) option

val for_goal : t -> Oracle_goal.t -> (module ORACLE) list
(** The oracles that take the kind and the number type of the goal, in the order of registration. *)

val accepts : (module ORACLE) -> Oracle_goal.t -> bool

val solve : ?timeout:float -> t -> Oracle_goal.t -> (Oracle_outcome.result, string) result
(** Asks the first oracle that takes the goal, and records the answer with
    the time it took. No oracle for the goal is an error. *)

(** {1 The engines in Julia} *)

type engine = Maithic_base.Julia_package.t = {
  julia : string;  (** The Julia executable. *)
  project : string;  (** The directory of the package [MaithicOracle]. *)
}

val locate : ?julia:string -> ?project:string -> cwd:string -> unit -> (engine, string) result
(** Finds the executable on the path and the package in the source tree
    around [cwd], unless they are given. *)

val command : ?quick:bool -> engine -> string * string list
(** The program and arguments that run the entry point. With [~quick:true]
    Julia runs the engine without compiling it first, which answers a small
    goal in a fraction of the time and a large one slowly. *)

val quick_seconds : float
(** How long the first, uncompiled try of a built-in oracle may take: ten
    seconds. A goal that needs more is run again, compiled, with the whole
    time limit. *)

val setup_command : engine -> string * string list
(** The program and arguments that resolve the dependencies of the package. *)

val prepare : ?timeout:float -> engine -> (unit, string) result
(** Makes the package ready to load. Julia loads a package through a
    manifest of its dependencies, and a source tree that was never used has
    none: it is written once, by {!setup_command}, with no use of the
    network, because the package depends on the standard library and on its
    sibling by path only. Nothing is run when the manifest is there. The
    built-in oracles call this before every goal. *)

val run : ?timeout:float -> runner:(?timeout:float -> stdin:string -> unit -> (Maithic_base.Proc.output, string) result) ->
  Oracle_goal.t -> (Oracle_outcome.t, string) result
(** Sends a goal to a process and reads its answer. [runner] starts the
    process; it is a parameter so that tests can stand in for Julia. *)

val builtin : engine -> t
(** The oracles of the Julia package: [linear], [polynomial] and [real]. *)

val default_timeout : float
(** Sixty seconds. *)
