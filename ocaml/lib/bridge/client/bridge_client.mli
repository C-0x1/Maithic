(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The typed client of the Bridge: one function per request.

    A connection owns one Bridge process and sends one request at a time.
    Responses are matched to requests by identifier. Every call has a
    deadline; when it passes, the process is killed and the connection is
    closed, so a stuck elaboration can never hang the caller. *)

open Maithic_base
open Bridge_types

type t

val start :
  ?env:(string * string) list -> ?cwd:string -> ?timeout:float -> string list -> (t, Bridge_error.t) result
(** [start argv] starts the Bridge with the given command line, in an
    environment reduced to an allowlist plus [env] (see {!Screen.scrub_env}),
    performs the handshake and checks the protocol version. [timeout] is the
    default deadline of each call, in seconds (default 120). *)

val handshake : t -> handshake
val is_open : t -> bool

val call : ?timeout:float -> t -> string -> Json.t -> (Json.t, Bridge_error.t) result
(** [call t method params] sends one request and returns its [result]. *)

val load_header : ?timeout:float -> t -> string list -> (int * bool, Bridge_error.t) result
(** Imports modules; returns the environment handle and whether it was cached. *)

val run_command :
  ?timeout:float -> ?names:string list -> ?keep:bool -> ?limits:limits -> t -> env:int -> string ->
  (report * int option, Bridge_error.t) result
(** Elaborates commands on an environment. With [~keep:true] and no error,
    the derived environment is stored and its handle returned. *)

val check_file :
  ?timeout:float -> ?names:string list -> ?keep:bool -> ?limits:limits -> t -> string ->
  (report * int option, Bridge_error.t) result
(** Elaborates a whole file, header included. *)

val extract :
  ?timeout:float -> ?file:string -> ?module_name:string -> ?trace:bool -> ?duplicates:bool -> ?lint:bool ->
  ?rules:string list -> ?convention:(string * string) list -> ?limits:limits -> t -> string ->
  (Extraction.t, Bridge_error.t) result
(** Elaborates a whole file and describes it: its declarations and imports,
    and on request the tactic steps of its proofs ([trace]), the theorems
    elsewhere that state the same thing ([duplicates]) and the findings of
    the lint rules ([lint], all rules unless [rules] names some;
    [convention] sets the naming convention for [theorems], [definitions]
    and [types]). Nothing is kept in the Bridge. A file with errors is
    described as far as it elaborated. *)

val try_candidates :
  ?timeout:float -> ?names:string list -> ?stop_on_first:bool -> ?limits:limits -> t -> env:int ->
  candidate list -> (candidate_report list, Bridge_error.t) result

val release_env : ?timeout:float -> t -> int -> (bool, Bridge_error.t) result

(** {1 Proof states}

    A proof state is a point in a tactic proof, held by the Bridge under a
    handle. States are immutable: a tactic never changes the state it runs
    on, it produces a new one, so a search may return to any state it has
    seen. A state whose goals are all closed shows that a tactic script
    works; it is not a verified theorem. Verification is {!run_command} or
    {!try_candidates} on the finished declaration, followed by the trust
    policy and the re-check. *)

val start_proof :
  ?timeout:float -> ?limits:limits -> t -> env:int -> [ `Statement of string | `Source of string ] ->
  (report * opened_proof list, Bridge_error.t) result
(** Opens a state at every [sorry] in tactic position. [`Statement s] is a
    declaration up to its [:=], which the Bridge completes with [by sorry];
    [`Source s] is text that already says [sorry] where proofs are to be
    found. When the text has errors the list is empty and the report says
    why. The environment is not changed. *)

val run_tactic :
  ?timeout:float -> ?goal:int -> ?limits:limits -> t -> state:int -> string -> (tactic_outcome, Bridge_error.t) result
(** Runs tactic text on a state. The text passes the guard first. With
    [~goal], the tactic sees only the goal at that position. A tactic that
    fails is [Ok (Stopped _)]: for a search, failure is an outcome. [Error]
    means the request itself could not be served, as for an unknown handle. *)

val try_tactics :
  ?timeout:float -> ?goal:int -> ?stop_on_solved:bool -> ?limits:limits -> t -> state:int -> string list ->
  (tactic_outcome list, Bridge_error.t) result
(** Runs each tactic on the same state, independently, in one round trip:
    one expansion step of a search. With [~stop_on_solved:true] the tactics
    after the first one that closes every goal are not run, and the list is
    shorter. *)

val get_state : ?timeout:float -> t -> int -> (proof_state, Bridge_error.t) result
val release_state : ?timeout:float -> t -> int -> (bool, Bridge_error.t) result
(** Frees a state. States derived from it stay valid. *)

val guard : ?timeout:float -> t -> string -> (bool * string, Bridge_error.t) result
val stats : ?timeout:float -> t -> (Json.t, Bridge_error.t) result

val self_test : ?timeout:float -> t -> (bool * (string * bool) list, Bridge_error.t) result
(** Whether the server is healthy, and the outcome of each of its checks. *)

val shutdown : t -> unit
(** Asks the server to stop and reaps the process. Safe to call twice. *)

val with_bridge :
  ?env:(string * string) list -> ?cwd:string -> ?timeout:float -> string list ->
  (t -> ('a, Bridge_error.t) result) -> ('a, Bridge_error.t) result
(** Starts a Bridge, runs the function and shuts the Bridge down, also when
    the function raises. *)
