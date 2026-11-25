(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type limits = { tokens : int; cost : float; seconds : float; lean_calls : int; model_calls : int }

let default_limits = { tokens = 200_000; cost = 0.0; seconds = 600.0; lean_calls = 200; model_calls = 24 }
let unlimited = { tokens = 0; cost = 0.0; seconds = 0.0; lean_calls = 0; model_calls = 0 }

type prices = { input : float; output : float }

let no_prices = { input = 0.0; output = 0.0 }

type spent = { input_tokens : int; output_tokens : int; cost : float; seconds : float; lean_calls : int; model_calls : int }

type t = {
  clock : unit -> float;
  started : float;
  limits : limits;
  prices : prices;
  mutable input : int;
  mutable output : int;
  mutable lean : int;
  mutable model : int;
}

let create ?(clock = Unix.gettimeofday) ?(prices = no_prices) limits = { clock; started = clock (); limits; prices; input = 0; output = 0; lean = 0; model = 0 }
let limits t = t.limits
let prices t = t.prices

let charge_model t ~input_tokens ~output_tokens =
  t.model <- t.model + 1;
  t.input <- t.input + max 0 input_tokens;
  t.output <- t.output + max 0 output_tokens

let charge_lean ?(calls = 1) t = t.lean <- t.lean + max 0 calls

let spent t =
  { input_tokens = t.input; output_tokens = t.output;
    cost = ((float_of_int t.input *. t.prices.input) +. (float_of_int t.output *. t.prices.output)) /. 1_000_000.0;
    seconds = Float.max 0.0 (t.clock () -. t.started); lean_calls = t.lean; model_calls = t.model }

type resource = Model | Lean

let blocked t resource =
  let s = spent t in
  let l = t.limits in
  let time = if l.seconds > 0.0 && s.seconds >= l.seconds then Some (Printf.sprintf "the limit of %g seconds is reached" l.seconds) else None in
  match resource with
  | Model ->
    if l.tokens > 0 && s.input_tokens + s.output_tokens >= l.tokens then Some (Printf.sprintf "the limit of %d tokens is reached" l.tokens)
    else if l.cost > 0.0 && s.cost >= l.cost then Some (Printf.sprintf "the limit of %.2f dollars is reached" l.cost)
    else if time <> None then time
    else if l.model_calls > 0 && s.model_calls >= l.model_calls then Some (Printf.sprintf "the limit of %d calls to a model is reached" l.model_calls)
    else None
  | Lean -> if time <> None then time else if l.lean_calls > 0 && s.lean_calls >= l.lean_calls then Some (Printf.sprintf "the limit of %d calls to Lean is reached" l.lean_calls) else None

let exhausted t =
  match (blocked t Model, blocked t Lean) with
  | Some model, Some lean -> Some (if Text.contains model "calls to a model" then lean else model)
  | Some why, None | None, Some why -> Some why
  | None, None -> None

let to_json t =
  let s = spent t in
  let l = t.limits in
  let rounded x = Float.round (x *. 1000.0) /. 1000.0 in
  Json.Obj
    ([ ( "limits",
         Json.Obj
           [ ("tokens", Json.Int l.tokens); ("cost", Json.Float l.cost); ("seconds", Json.Float l.seconds); ("lean_calls", Json.Int l.lean_calls);
             ("model_calls", Json.Int l.model_calls) ] ) ]
     @ (if t.prices = no_prices then [] else [ ("prices", Json.Obj [ ("input", Json.Float t.prices.input); ("output", Json.Float t.prices.output) ]) ])
     @ [ ( "spent",
           Json.Obj
             [ ("input_tokens", Json.Int s.input_tokens); ("output_tokens", Json.Int s.output_tokens); ("cost", Json.Float (Float.round (s.cost *. 1e6) /. 1e6));
               ("seconds", Json.Float (rounded s.seconds)); ("lean_calls", Json.Int s.lean_calls); ("model_calls", Json.Int s.model_calls) ] ) ]
     @ match exhausted t with Some why -> [ ("exhausted", Json.String why) ] | None -> [])
