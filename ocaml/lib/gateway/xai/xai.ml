(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The xAI adapter.

    The xAI API follows the OpenAI request shapes at the base
    [https://api.x.ai/v1], with the key from [XAI_API_KEY] as a bearer
    token. The registry selects the endpoint shape: [responses], the one xAI
    recommends for new integrations, or [chat_completions], the older one. *)

open Gateway_types

let name = "xai"

let build ~(provider : Model_registry.provider) ~model ~key (req : request) : Transport.request =
  match provider.Model_registry.api with
  | Model_registry.Chat_completions -> Chat_completions.build ~provider ~model ~key req
  | Model_registry.Responses | Model_registry.Messages -> Openai.build ~provider ~model ~key req

let parse ~(provider : Model_registry.provider) ~model (resp : Transport.response) =
  match provider.Model_registry.api with
  | Model_registry.Chat_completions -> Chat_completions.parse ~provider ~model resp
  | Model_registry.Responses | Model_registry.Messages -> Openai.parse ~provider ~model resp
