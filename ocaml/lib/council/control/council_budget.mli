(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The budget of a session: what a council may spend before it has to stop.

    Four things are counted: the tokens models read and write, what those
    tokens cost, the time that passes, and the calls made to Lean. Each has
    a limit, and a limit of zero means there is none. The budget is asked
    before every call, so a council stops between two calls and never in
    the middle of one; what it had established by then stays established. *)

open Maithic_base

type limits = {
  tokens : int;  (** Tokens of all model calls together, read and written. *)
  cost : float;  (** In US dollars, at the prices given. *)
  seconds : float;  (** From the start of the session. *)
  lean_calls : int;  (** Texts given to Lean. *)
  model_calls : int;  (** Questions put to a model. *)
}

val default_limits : limits
(** 200 000 tokens, no limit on cost, ten minutes, 200 calls to Lean and 24 to a model. *)

val unlimited : limits

type prices = {
  input : float;  (** US dollars for a million tokens read. *)
  output : float;  (** US dollars for a million tokens written. *)
}

val no_prices : prices
(** Zero for both: the cost is then zero, and a limit on it never binds. *)

type spent = { input_tokens : int; output_tokens : int; cost : float; seconds : float; lean_calls : int; model_calls : int }

type t

val create : ?clock:(unit -> float) -> ?prices:prices -> limits -> t
val limits : t -> limits
val prices : t -> prices

val charge_model : t -> input_tokens:int -> output_tokens:int -> unit
(** One call to a model, with the tokens it used. *)

val charge_lean : ?calls:int -> t -> unit
(** One call to Lean, or several. *)

val spent : t -> spent

type resource = Model | Lean

val blocked : t -> resource -> string option
(** Which limit keeps a resource from being used, in words, when one does.
    The limits on tokens, on cost and on calls to a model keep a model from
    being asked; the limit on calls to Lean keeps Lean from being asked; the
    limit on time keeps both. A council that may no longer ask a model can
    still give the proofs it has to Lean. *)

val exhausted : t -> string option
(** Which limit is reached, in words, when one is: the first of tokens,
    cost, time, calls to Lean and calls to a model. *)

val to_json : t -> Json.t
(** The limits, the prices when there are any, what was spent, and the limit that was reached. *)
