(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Routing: which provider and model a call goes to, and with what deadline.

    The default route is the configured provider with the model of the
    configured tier, or the explicitly configured model. Every route carries
    the deadline of the call. *)

open Gateway_types

type t = {
  provider : Model_registry.provider;
  model : Model_registry.model;
  timeout : float;  (** deadline of one call, in seconds *)
}

(** The default route for a configuration. *)
let default registry ~provider ~tier ~model ~timeout =
  match Model_registry.resolve registry ~provider ~tier ~model with
  | Ok (provider, model) -> Ok { provider; model; timeout }
  | Error detail ->
    if Model_registry.provider registry provider = None then Error (Unknown_provider detail) else Error (Unknown_model detail)

let describe route =
  Printf.sprintf "%s / %s (deadline %.0f s)" route.provider.Model_registry.name route.model.Model_registry.id route.timeout
