(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The trust policy: what counts as acceptable evidence.

    The Bridge reports facts about a declaration. This module turns facts
    into a status. A model, a heuristic or a configuration switch can never
    produce [verified]; only {!decide} can, and only from kernel facts. *)

open Maithic_base
open Maithic_schema

type axiom_class = Standard | Sorry | Native | Other

val classify : string -> axiom_class
(** Classifies an axiom by name, in the same way as [Maithic.Audit] in Lean. *)

type t = {
  name : string;
  allowed_axioms : string list;  (** axioms a result may depend on *)
  allow_native : bool;  (** accept proofs that trust compiled evaluation *)
  require_recheck : bool;  (** a fresh-process re-check must pass before [verified] *)
}

val default : t
(** Strict defaults: the three standard axioms only, no native evaluation,
    fresh-process re-check required. *)

val of_json : Json.t -> (t, string) result
(** Reads a policy from the [[trust]] table of a parsed policy file. Missing
    members keep their default value. *)

val load : string -> (t, string) result
(** Loads a policy file such as [.maithic/trust.toml]. *)

val to_json : t -> Json.t

(** The facts about one declaration, as reported by the Bridge. *)
type facts = {
  declaration : string;
  found : bool;
  kind : string;
  axioms : string list;
  errors : int;  (** error diagnostics of the elaboration that produced it *)
  rechecked : bool;  (** the facts were confirmed in a fresh process *)
}

type decision = {
  status : Status.t;
  reasons : string list;  (** why the status is not [verified]; empty when it is *)
  violations : string list;  (** axioms outside the policy *)
}

val decide : t -> facts -> decision
(** [verified] when the declaration exists, is a theorem, was produced
    without error, rests only on allowed axioms and, if the policy requires
    it, was re-checked; [checked] when only the re-check is missing;
    [failed] otherwise, with the reasons. *)

val decision_to_json : decision -> Json.t
