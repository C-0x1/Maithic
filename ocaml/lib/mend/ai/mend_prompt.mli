(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The prompt of the repair agent.

    The model is shown the theorem with the proof that no longer compiles,
    what Lean reports with lines counted from the theorem's first line, the
    class of the breakage, what stands before the theorem, and the repairs
    that were already tried with what Lean said about each. It is asked for
    proofs, the smallest change first. It is never asked for a statement,
    and whatever it returns is text to be checked. *)

open Maithic_prover
open Maithic_gateway

val name : string
(** [mend.repair]. *)

val errors_text : first_line:int -> Maithic_bridge.Bridge_types.diagnostic list -> string
(** The error diagnostics of a breakage, one to a paragraph, each with its
    line counted from the first line of the declaration. *)

val attempts_text : (string * string) list -> string
(** Proofs that were tried, each with what Lean said; [(none)] for no attempt. *)

val bindings : count:int -> decl:Lean_source.decl -> breakage:Mend_breakage.t -> failures:(string * string) list -> (string * string) list
(** The values of the prompt's placeholders. *)

val ask :
  Gateway.t -> max_tokens:int -> count:int -> decl:Lean_source.decl -> breakage:Mend_breakage.t -> failures:(string * string) list ->
  (string list, string) result
(** Asks the model for at most [count] proofs. An error says why no answer
    was had: the model is switched off, no cassette, a provider failure. *)
