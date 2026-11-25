(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Child processes.

    Two shapes cover everything Maithic needs: {!run} for a command that
    takes input, runs to completion and returns its output, and {!spawn} for
    a long-lived worker that is spoken to line by line, such as the Bridge. *)

type output = { status : int; stdout : string; stderr : string }
(** [status] is the exit code, or [128 + n] when the child died from signal [n]. *)

val run :
  ?stdin:string -> ?env:(string * string) list -> ?cwd:string -> ?timeout:float ->
  string -> string list -> (output, string) result
(** [run prog args] runs [prog], found through [PATH], with [args]. [stdin]
    is written to the child's standard input. [env] adds or replaces
    variables of the current environment. With [timeout], in seconds, the
    child is killed when it runs longer and the result is an error. The
    error case means the program could not be started or timed out; a
    non-zero exit is an [Ok] with that status. *)

val find_in_path : string -> string option
(** The absolute path of an executable found through [PATH]. *)

type worker
(** A running child with pipes to its standard input and output. Its
    standard error is inherited. *)

val spawn :
  ?env:(string * string) list -> ?inherit_env:bool -> ?cwd:string -> string -> string list -> (worker, string) result
(** With [~inherit_env:false] the child sees only the variables in [env];
    by default [env] is added to the current environment. *)

val send_line : worker -> string -> (unit, string) result
(** Writes one line and flushes. *)

val read_line : ?timeout:float -> worker -> (string, string) result
(** Reads one line, without its newline. With [timeout], in seconds, gives
    up with an error when no complete line arrives in time. End of input is
    an error. *)

val pid : worker -> int

val stop : worker -> int
(** Closes the pipes, waits briefly for the child to exit on its own, kills
    it if it does not, and returns its exit status as in {!output}. *)
