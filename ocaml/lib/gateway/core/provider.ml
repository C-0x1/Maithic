(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The signature every provider adapter implements.

    An adapter is two pure functions: one builds the HTTP request for a
    unified request, the other reads the HTTP response back into a unified
    response or error. Keeping them pure is what makes an adapter testable
    against recorded exchanges with no network and no key. *)

open Gateway_types

module type S = sig
  val name : string
  (** The provider name used in configuration: ["anthropic"], ["openai"], ["xai"]. *)

  val build :
    provider:Model_registry.provider -> model:Model_registry.model -> key:string -> request -> Transport.request
  (** The HTTP request that carries a unified request to this provider. *)

  val parse :
    provider:Model_registry.provider -> model:Model_registry.model -> Transport.response -> (response, error) result
  (** Reads the provider's answer: a success becomes a {!response}, any
      failure one of the unified errors. *)
end

(** {1 Helpers shared by adapters} *)

open Maithic_base

let json_headers = [ ("content-type", "application/json") ]

let auth_headers (provider : Model_registry.provider) key =
  match provider.auth with
  | Model_registry.Bearer -> [ ("authorization", "Bearer " ^ key) ]
  | Model_registry.X_api_key -> [ ("x-api-key", key) ]
  | Model_registry.No_auth -> []

let retry_after (resp : Transport.response) = Option.bind (Transport.header "retry-after" resp) float_of_string_opt

(** A short, single-line excerpt of a body for error messages. *)
let excerpt body =
  let flat = String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) body in
  Text.truncate 300 (String.trim flat)

(** Maps an HTTP failure to a unified error, given the provider's error type
    or code and message when the body carried them. *)
let http_error (resp : Transport.response) ~kind ~message =
  let detail = if message = "" then excerpt resp.Transport.body else message in
  let kind = String.lowercase_ascii kind in
  let has word = Text.contains kind word in
  match resp.Transport.status with
  | 401 -> Authentication detail
  | 403 -> Authentication detail
  | 404 -> Not_found detail
  | 408 | 504 -> Timeout detail
  | 429 -> Rate_limited { retry_after = retry_after resp; detail }
  | 529 -> Overloaded detail
  | 503 -> Overloaded detail
  | status when status >= 500 ->
    if has "overloaded" || has "slow_down" then Overloaded detail
    else if has "timeout" then Timeout detail
    else Server { status; detail }
  | status when status >= 400 ->
    if has "rate_limit" then Rate_limited { retry_after = retry_after resp; detail }
    else if has "authentication" || has "invalid_api_key" then Authentication detail
    else Invalid_request detail
  | status -> Server { status; detail }
