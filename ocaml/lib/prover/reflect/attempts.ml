(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What the prover remembers about failed proofs, and how it reads Lean's
    errors.

    Each failed candidate is kept with the error Lean reported and a class
    for that error. The memory is consulted before anything is sent to Lean,
    so the same proof is never checked twice, and it is shown to the model
    in the next round so that the model can correct itself. *)

open Maithic_base

(** Classes of Lean failures. The class decides the hint given to the model. *)
type error_class =
  | Unknown_identifier
  | Type_mismatch
  | Unsolved_goals
  | Tactic_failed
  | Syntax_error
  | Timeout
  | Not_closed  (** the proof is accepted but relies on [sorry] *)
  | Guard_rejected  (** the candidate never reached Lean *)
  | Other_error

let class_to_string = function
  | Unknown_identifier -> "unknown-identifier"
  | Type_mismatch -> "type-mismatch"
  | Unsolved_goals -> "unsolved-goals"
  | Tactic_failed -> "tactic-failed"
  | Syntax_error -> "syntax-error"
  | Timeout -> "timeout"
  | Not_closed -> "not-closed"
  | Guard_rejected -> "guard-rejected"
  | Other_error -> "other"

(** Classifies the text of a Lean error message. *)
let classify message =
  let m = String.lowercase_ascii message in
  let has word = Text.contains m word in
  if has "maximum number of heartbeats" || has "maximum recursion depth" || has "deterministic) timeout" then Timeout
  else if has "unknown identifier" || has "unknown constant" || has "unknown tactic" || has "unknown namespace" then Unknown_identifier
  else if has "type mismatch" || has "application type mismatch" || has "failed to synthesize" then Type_mismatch
  else if has "unsolved goals" || has "goals accomplished" || has "no goals" then Unsolved_goals
  else if has "unexpected token" || has "expected" && has "parse" || has "unexpected end of input" then Syntax_error
  else if has "failed" || has "could not" || has "made no progress" || has "is false" then Tactic_failed
  else Other_error

(** A deterministic hint for the model, by error class. *)
let hint = function
  | Unknown_identifier -> "A name does not exist. Use only names from the context and the Lean core library."
  | Type_mismatch -> "A term has the wrong type. Check the argument order and the implicit arguments."
  | Unsolved_goals -> "The tactics ran but goals remain, or a tactic ran after the goal was closed. Handle every case."
  | Tactic_failed -> "A tactic could not make progress. Try a different tactic or first transform the goal."
  | Syntax_error -> "The proof does not parse. Return plain Lean 4 syntax."
  | Timeout -> "The proof exceeded its step budget. Use a more direct argument."
  | Not_closed -> "The proof must not contain sorry."
  | Guard_rejected -> "The proof used a construct that is not allowed in a candidate."
  | Other_error -> "Read the error and change the approach."

(** Candidates that differ only in blank space are the same candidate. *)
let normalize proof =
  String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) proof
  |> String.split_on_char ' '
  |> List.filter (fun w -> w <> "")
  |> String.concat " "

type attempt = {
  round : int;  (** 0 for the portfolio, then the model rounds from 1 *)
  proposer : string;
  proof : string;
  accepted : bool;
  error_class : error_class option;
  error : string;  (** Lean's message, or the reason of the guard *)
  elapsed_ms : int;
}

type memory = { mutable attempts : attempt list (* newest first *); seen : (string, unit) Hashtbl.t }

let create () = { attempts = []; seen = Hashtbl.create 64 }
let already_tried memory proof = Hashtbl.mem memory.seen (normalize proof)

let record memory attempt =
  Hashtbl.replace memory.seen (normalize attempt.proof) ();
  memory.attempts <- attempt :: memory.attempts

let all memory = List.rev memory.attempts
let failures memory = List.filter (fun a -> not a.accepted) (all memory)

(** Keeps the candidates not tried before, without duplicates, in order. *)
let fresh memory proofs =
  let local = Hashtbl.create 16 in
  List.filter
    (fun proof ->
      let key = normalize proof in
      if key = "" || Hashtbl.mem memory.seen key || Hashtbl.mem local key then false
      else begin
        Hashtbl.replace local key ();
        true
      end)
    proofs

(** The failed attempts as text for the repair prompt: most recent last, at
    most [limit] of them, each with its error and a hint. *)
let render_failures ?(limit = 8) memory =
  let recent = failures memory |> List.rev |> List.filteri (fun i _ -> i < limit) |> List.rev in
  recent
  |> List.map (fun a ->
         Printf.sprintf "proof:\n%s\nerror (%s):\n%s\nhint: %s" (Text.indent 2 a.proof)
           (match a.error_class with Some c -> class_to_string c | None -> "none")
           (Text.indent 2 (Text.truncate 600 a.error))
           (match a.error_class with Some c -> hint c | None -> ""))
  |> String.concat "\n\n"

let attempt_to_json a =
  Json.Obj
    ([ ("round", Json.Int a.round); ("proposer", Json.String a.proposer); ("proof", Json.String a.proof);
       ("accepted", Json.Bool a.accepted) ]
     @ (match a.error_class with Some c -> [ ("error_class", Json.String (class_to_string c)) ] | None -> [])
     @ (if a.error = "" then [] else [ ("error", Json.String a.error) ])
     @ [ ("elapsed_ms", Json.Int a.elapsed_ms) ])
