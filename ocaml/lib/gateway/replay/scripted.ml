(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Providers that never leave the process: the scripted provider for tests
    and the mock provider for demonstrations.

    A scripted provider answers from a script written in a test, either a
    fixed list of answers consumed in order or a function of the request.
    The mock provider returns a deterministic canned answer chosen by the
    purpose of the request, so that every AI-backed command can be tried
    without a key. Mock answers are plain automation attempts; they are
    never presented as the output of a model. *)

open Gateway_types

type handler = request -> (response, error) result

let answer ?(model = "scripted") text : response =
  { text; stop = End_turn; usage = { input_tokens = 0; output_tokens = 0 }; model; provider = "scripted"; id = "" }

(** A handler that returns the given outcomes in order, and an invalid
    request error once they are used up. It also records what it was asked. *)
let sequence outcomes : handler * (unit -> request list) =
  let remaining = ref outcomes and seen = ref [] in
  let handler req =
    seen := req :: !seen;
    match !remaining with
    | [] -> Error (Invalid_request "the script has no more answers")
    | outcome :: rest ->
      remaining := rest;
      outcome
  in
  (handler, fun () -> List.rev !seen)

(** A handler that answers every request with the text computed by [f]. *)
let of_function f : handler = fun req -> Ok (answer (f req))

(** The canned answer of the mock provider for each purpose. *)
let mock_text purpose =
  match purpose with
  | "prove.propose" | "prove.repair" ->
    {|{"candidates": ["by omega", "by simp", "by simp_all", "by grind", "by intros; omega", "by intros; simp_all", "by decide"]}|}
  | other -> Printf.sprintf "mock answer for %s" other

let mock : handler =
 fun req -> Ok { (answer ~model:"mock" (mock_text req.purpose)) with provider = "mock" }
