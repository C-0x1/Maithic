(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The differential test: the compiled Lean algorithms, held against
    reference implementations that share no code with them.

    The references and the generators of inputs are the Julia package
    MaithicAlgoRef, run as a process. It starts the executable that Lake
    builds from the verified library, sends both sides the same requests,
    and reports every answer that the reference does not accept. *)

open Maithic_base

let schema = "maithic.algolab.differential/1"
let locate ?julia ?project ~cwd () = Julia_package.locate ~package:"MaithicAlgoRef" ~hint:"set algolab.project" ?julia ?project ~cwd ()
let prepare = Julia_package.prepare

(** The program to test: Lake builds it next to the Bridge. *)
let beside bridge = Filename.concat (Filename.dirname bridge) "maithic-algo"

type settings = { seed : int; count : int;  (** Cases in all, shared out over the algorithms. *) size : int;  (** How large an input may be. *) algorithms : string list }

let default_settings = { seed = 1; count = 190; size = 12; algorithms = [] }

(** Runs the test and reads its report. A report with mismatches is a
    report: [Error] means the test could not be made. *)
let run ?(timeout = 600.0) ?(settings = default_settings) engine ~executable () =
  let args =
    Julia_package.arguments engine
    @ [ Julia_package.script engine "differential.jl"; executable; string_of_int settings.seed; string_of_int settings.count; string_of_int settings.size ]
    @ settings.algorithms
  in
  match Proc.run ~env:Julia_package.offline ~timeout engine.Julia_package.julia args with
  | Error e -> Error e
  | Ok out ->
    if out.Proc.status <> 0 && out.Proc.status <> 1 then
      Error (match Text.trim out.Proc.stderr with "" -> Printf.sprintf "the test ended with status %d" out.Proc.status | said -> List.hd (List.rev (String.split_on_char '\n' said)))
    else
      Result.bind
        (Result.map_error (fun e -> "the test printed something that is not JSON: " ^ e) (Json.parse out.Proc.stdout))
        (fun report -> if Json.string_or "" "schema" report = schema then Ok report else Error ("the test printed a document that is not " ^ schema))

let agrees report = Json.bool_or false "agrees" report && Json.int_or 1 "mismatches" report = 0

let summary report =
  let cases = Json.int_or 0 "cases" report and bad = Json.int_or 0 "mismatches" report in
  let algorithms = List.length (Json.list_or_empty "algorithms" report) in
  if agrees report then Printf.sprintf "agree: %d case(s) over %d algorithm(s), and every answer is accepted by its reference" cases algorithms
  else Printf.sprintf "differ: %d of %d answer(s) are not accepted by the reference" bad cases

(** The rows of the report: an algorithm, its cases, and how many answers differ. *)
let rows report =
  List.map (fun a -> (Json.string_or "" "name" a, Json.int_or 0 "cases" a, Json.int_or 0 "mismatches" a)) (Json.list_or_empty "algorithms" report)

(** The answers that differ, in words. *)
let failures report =
  List.map
    (fun f ->
      Printf.sprintf "%s: the request `%s` should give `%s` and gives `%s`" (Json.string_or "" "algorithm" f) (Json.string_or "" "request" f)
        (Json.string_or "" "expected" f) (Json.string_or "" "answer" f))
    (Json.list_or_empty "failures" report)
