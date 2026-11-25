(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A small test framework: named cases, checks with readable failures and
    seeded property tests.

    A test executable builds groups of cases and hands them to {!run}. The
    name of a case starts with the identifier of the capability it covers,
    for example ["P10-07-02 canonical hash ignores member order"]; the
    capability checker finds tests by that text.

    Environment: [MAITHIC_TEST_FILTER] keeps only the cases whose group or
    name contains the given text; [MAITHIC_TEST_SEED] replaces the default
    seed of property tests. *)

type case

val case : string -> (unit -> unit) -> case
(** A named case. It passes when the function returns. *)

(** {1 Checks} *)

val check : string -> bool -> unit
(** [check what cond] fails with [what] when [cond] is false. *)

val equal : ?show:('a -> string) -> string -> 'a -> 'a -> unit
(** [equal what expected actual] compares with structural equality. *)

val equal_string : string -> string -> string -> unit
val equal_int : string -> int -> int -> unit
val equal_bool : string -> bool -> bool -> unit
val equal_strings : string -> string list -> string list -> unit
val equal_ints : string -> int list -> int list -> unit

val is_ok : string -> ('a, string) result -> 'a
(** Returns the value of an [Ok], and fails with the message of an [Error]. *)

val is_error : string -> ('a, 'e) result -> 'e
val contains : string -> string -> string -> unit
(** [contains what hay needle] fails unless [needle] occurs in [hay]. *)

val raises : string -> (unit -> 'a) -> unit
(** Fails unless the function raises an exception. *)

val fail : string -> 'a
val skip : string -> 'a
(** Ends the case as skipped, with a reason. A skipped case is reported but
    does not fail the run. *)

(** {1 Property tests} *)

module Gen : sig
  type 'a t = Random.State.t -> 'a

  val int : int -> int -> int t
  (** Uniform in the closed range. *)

  val bool : bool t
  val float : float -> float -> float t
  val char_of : string -> char t
  val string_of : ?max:int -> string -> string t
  (** A string of at most [max] characters drawn from the given alphabet. *)

  val unicode_string : ?max:int -> unit -> string t
  (** A valid UTF-8 string mixing ASCII, escapes and wider code points. *)

  val list : ?max:int -> 'a t -> 'a list t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val one_of : 'a list -> 'a t
  val choose : 'a t list -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'a t
  val sized : (int -> 'a t) -> 'a t
  (** Passes a size that shrinks at each nested use, for recursive data. *)
end

val property : ?count:int -> ?show:('a -> string) -> string -> 'a Gen.t -> ('a -> bool) -> case
(** [property name gen law] checks [law] on [count] generated values
    (default 200). A failure reports the seed and, with [show], the value. *)

(** {1 Running} *)

val run : string -> (string * case list) list -> unit
(** [run suite groups] runs every case, prints one line per case and a
    summary, and exits with status 1 if any case failed. *)

val temp_dir : string -> string
(** A fresh directory under the system temporary directory. *)
