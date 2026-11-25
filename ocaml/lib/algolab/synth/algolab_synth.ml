(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The synthesis loop: a draft, Lean's verdict on it, and a better draft,
    until one compiles and gets every example right, or the rounds are over. *)

open Maithic_base

(** What became of one example. *)
type example_result = {
  example : Algolab_task.example;
  got : string;  (** What the call gave, as Lean prints it; empty when it could not be run. *)
  passed : bool;
  error : string;  (** Why it could not be run; empty when it could. *)
}

(** What Lean said about one implementation. *)
type verdict = {
  errors : string list;  (** The errors of compiling the definition; empty when it compiles. *)
  examples : example_result list;  (** Empty when the definition does not compile. *)
}

type attempt = {
  round : int;  (** 0 for the first draft. *)
  implementation : string;
  provenance : Algolab_draft.provenance;
  refused : string list;  (** Why the draft was not given to Lean; empty when it was. *)
  verdict : verdict option;  (** [None] when the draft was refused. *)
  accepted : bool;
}

type tools = {
  draft : unit -> (string * Algolab_draft.provenance, string) result;
  repair : implementation:string -> report:string -> earlier:string list -> (string * Algolab_draft.provenance, string) result;
  lean : string -> (verdict, string) result;  (** Compiles an implementation and runs the examples on it. *)
}

type status = Accepted | Failed of string

type t = { task : Algolab_task.t; attempts : attempt list; status : status }

let default_rounds = 3
let passed verdict = verdict.errors = [] && List.for_all (fun e -> e.passed) verdict.examples

(** What is said to the model about an implementation that was not accepted. *)
let report (a : attempt) =
  match (a.refused, a.verdict) with
  | (_ :: _ as reasons), _ -> "The definition was not given to Lean:\n" ^ String.concat "\n" (List.map (fun r -> "- " ^ r) reasons)
  | [], Some v when v.errors <> [] -> "Lean reports:\n" ^ String.concat "\n\n" v.errors
  | [], Some v ->
    "The definition compiles. It gets these examples wrong:\n"
    ^ String.concat "\n"
        (List.filter_map
           (fun e ->
             if e.passed then None
             else if e.error <> "" then Some (Printf.sprintf "- %s  could not be run: %s" e.example.Algolab_task.call e.error)
             else Some (Printf.sprintf "- %s  should give  %s  and gives  %s" e.example.Algolab_task.call e.example.Algolab_task.expect e.got))
           v.examples)
  | [], None -> "Lean could not be asked."

(** Looks at one implementation: the screen first, then Lean. *)
let examine tools (task : Algolab_task.t) ~round (implementation, provenance) =
  match Algolab_screen.violations ~name:task.Algolab_task.name implementation with
  | _ :: _ as refused -> Ok { round; implementation; provenance; refused; verdict = None; accepted = false }
  | [] ->
    Result.map (fun verdict -> { round; implementation; provenance; refused = []; verdict = Some verdict; accepted = passed verdict }) (tools.lean implementation)

(** Runs the loop. The first implementation is the one given, or a draft of
    the model. After that the model is shown what happened and asked for a
    correction, at most [rounds] times. The loop ends early when the model
    returns an implementation it returned before, and when it cannot be
    asked. *)
let run ?(rounds = default_rounds) ?given tools (task : Algolab_task.t) =
  let finish attempts status = { task; attempts = List.rev attempts; status } in
  let rec loop attempts round candidate =
    match examine tools task ~round candidate with
    | Error why -> finish attempts (Failed ("Lean could not be asked: " ^ why))
    | Ok attempt ->
      let attempts = attempt :: attempts in
      if attempt.accepted then finish attempts Accepted
      else if round >= rounds then
        finish attempts (Failed (if rounds = 0 then "the implementation was not accepted" else Printf.sprintf "no implementation was accepted after %d correction(s)" rounds))
      else (
        let earlier = List.rev_map (fun (a : attempt) -> a.implementation) attempts in
        match tools.repair ~implementation:attempt.implementation ~report:(report attempt) ~earlier:(List.filter (fun i -> i <> attempt.implementation) earlier) with
        | Error why -> finish attempts (Failed ("the model was not asked for a correction: " ^ why))
        | Ok ((next, _) as candidate) ->
          if List.mem next earlier then finish attempts (Failed "the model returned an implementation it had returned before")
          else loop attempts (round + 1) candidate)
  in
  match given with
  | Some implementation -> loop [] 0 (implementation, Algolab_draft.given)
  | None -> (match tools.draft () with Error why -> finish [] (Failed ("no implementation was drafted: " ^ why)) | Ok candidate -> loop [] 0 candidate)

let final synth = match synth.status with Accepted -> List.find_opt (fun (a : attempt) -> a.accepted) (List.rev synth.attempts) | Failed _ -> None
let accepted synth = synth.status = Accepted

(* ---- the record ---- *)

let schema = "maithic.algolab.synthesis/1"
let strings items = Json.List (List.map (fun s -> Json.String s) items)

let attempt_to_json (a : attempt) =
  let p = a.provenance in
  Json.Obj
    ([ ("round", Json.Int a.round); ("implementation", Json.String a.implementation);
       ( "provenance",
         Json.Obj
           ([ ("origin", Json.String p.Algolab_draft.origin) ]
           @ if p.Algolab_draft.origin = "given" then [] else [ ("provider", Json.String p.Algolab_draft.provider); ("model", Json.String p.Algolab_draft.model); ("prompt", Json.String p.Algolab_draft.prompt) ]) );
       ("refused", strings a.refused) ]
    @ (match a.verdict with
       | None -> []
       | Some v ->
         [ ("errors", strings v.errors);
           ( "examples",
             Json.List
               (List.map
                  (fun e ->
                    Json.Obj
                      ([ ("call", Json.String e.example.Algolab_task.call); ("expect", Json.String e.example.Algolab_task.expect); ("got", Json.String e.got);
                         ("passed", Json.Bool e.passed) ]
                      @ if e.error = "" then [] else [ ("error", Json.String e.error) ]))
                  v.examples) ) ])
    @ [ ("accepted", Json.Bool a.accepted) ])

let to_json synth =
  Json.Obj
    ([ ("schema", Json.String schema); ("task", Algolab_task.to_json synth.task); ("status", Json.String (if accepted synth then "accepted" else "failed")) ]
    @ (match synth.status with Failed why -> [ ("reason", Json.String why) ] | Accepted -> [])
    @ (match final synth with Some a -> [ ("implementation", Json.String a.implementation) ] | None -> [])
    @ [ ("attempts", Json.List (List.map attempt_to_json synth.attempts)) ])

(** The accepted implementation as a Lean file: the definition, and every
    example as a check that Lean makes again whenever the file is compiled. *)
let lean_text synth =
  Option.map
    (fun (a : attempt) ->
      let checks =
        match a.verdict with
        | Some v -> List.filter_map (fun e -> if e.passed then Some (Printf.sprintf "#guard (%s) == (%s)" e.example.Algolab_task.call e.example.Algolab_task.expect) else None) v.examples
        | None -> []
      in
      String.concat "\n"
        ([ Printf.sprintf "-- %s, written by `maithic algo synth`." synth.task.Algolab_task.name;
           "-- Lean compiled the definition and ran it on the examples below; nothing is proved about it."; ""; a.implementation ]
        @ (if checks = [] then [] else "" :: checks))
      ^ "\n")
    (final synth)

let summary synth =
  match synth.status with
  | Accepted ->
    let n = List.length synth.attempts in
    Printf.sprintf "accepted: %s compiles and gets %d example(s) right, after %d attempt(s)" synth.task.Algolab_task.name (List.length synth.task.Algolab_task.examples) n
  | Failed why -> "failed: " ^ why

(** The attempts in a few lines each, for a terminal. *)
let render synth =
  (* What an error says, on one line: Lean breaks some messages after their first words. *)
  let headline text =
    match List.filter (fun line -> line <> "") (List.map Text.trim (String.split_on_char '\n' text)) with
    | first :: second :: _ when Text.ends_with ~suffix:" for" first || Text.ends_with ~suffix:":" first -> first ^ " " ^ second
    | first :: _ -> first
    | [] -> ""
  in
  String.concat "\n"
    (List.concat_map
       (fun (a : attempt) ->
         let outcome =
           match (a.refused, a.verdict) with
           | reason :: _, _ -> "refused: " ^ reason
           | [], Some v when v.errors <> [] -> "does not compile: " ^ headline (List.hd v.errors)
           | [], Some v ->
             let right = List.length (List.filter (fun e -> e.passed) v.examples) in
             Printf.sprintf "compiles; %d of %d example(s) right" right (List.length v.examples)
           | [], None -> "not examined"
         in
         [ Printf.sprintf "attempt %d  (%s)  %s" (a.round + 1) a.provenance.Algolab_draft.origin outcome ]
         @ (match a.verdict with
            | Some v when v.errors = [] ->
              List.filter_map
                (fun e ->
                  if e.passed then None
                  else Some (Printf.sprintf "    %s  should give  %s  and gives  %s" e.example.Algolab_task.call e.example.Algolab_task.expect (if e.error <> "" then "an error" else e.got)))
                v.examples
            | _ -> []))
       synth.attempts
    @ match final synth with Some a -> [ ""; a.implementation; "" ] | None -> [])
