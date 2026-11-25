(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Baselines: what a benchmark suite gave at a moment that someone decided
    to hold later runs against. One file for each suite. *)

open Maithic_base

let schema = "maithic.bench.baseline/1"

type problem = { id : string; status : string; attempts : int; elapsed_ms : int }

type t = {
  suite : string;
  mode : string;  (** The gateway mode of the run it was made from. *)
  use_ai : bool;
  policy : string;
  note : string;  (** Why this baseline was set: a release, a commit, a date. *)
  problems : problem list;  (** In the order of the run. *)
}

let verified b = List.length (List.filter (fun p -> p.status = "verified") b.problems)

(** The suites of a run, in the order they first occur. *)
let suites_of_run run =
  List.fold_left
    (fun seen p -> match Json.string_or "" "suite" p with "" -> seen | suite -> if List.mem suite seen then seen else seen @ [ suite ])
    [] (Json.list_or_empty "problems" run)

(** The problems of one suite as a run reports them. *)
let problems_of_run ~suite run =
  List.filter_map
    (fun p ->
      if Json.string_or "" "suite" p <> suite then None
      else Some { id = Json.string_or "" "id" p; status = Json.string_or "" "status" p; attempts = Json.int_or 0 "attempts" p; elapsed_ms = Json.int_or 0 "elapsed_ms" p })
    (Json.list_or_empty "problems" run)

(** A baseline made of what a run reports about one suite. *)
let of_run ?(note = "") ~suite run =
  match problems_of_run ~suite run with
  | [] -> Error (Printf.sprintf "the run has no problem of the suite '%s'" suite)
  | problems ->
    Ok { suite; mode = Json.string_or "" "mode" run; use_ai = Json.bool_or false "use_ai" run; policy = Json.string_or "" "policy" run; note; problems }

let to_json b =
  Json.Obj
    [ ("schema", Json.String schema); ("suite", Json.String b.suite); ("mode", Json.String b.mode); ("use_ai", Json.Bool b.use_ai); ("policy", Json.String b.policy);
      ("note", Json.String b.note); ("total", Json.Int (List.length b.problems)); ("verified", Json.Int (verified b));
      ( "problems",
        Json.List
          (List.map
             (fun p -> Json.Obj [ ("id", Json.String p.id); ("status", Json.String p.status); ("attempts", Json.Int p.attempts); ("elapsed_ms", Json.Int p.elapsed_ms) ])
             b.problems) ) ]

let of_json json =
  if Json.string_or "" "schema" json <> schema then Error (Printf.sprintf "not a baseline: the schema is not %s" schema)
  else
    match Json.string_or "" "suite" json with
    | "" -> Error "a baseline without a suite"
    | suite ->
      Ok
        { suite; mode = Json.string_or "" "mode" json; use_ai = Json.bool_or false "use_ai" json; policy = Json.string_or "" "policy" json; note = Json.string_or "" "note" json;
          problems =
            List.map
              (fun p -> { id = Json.string_or "" "id" p; status = Json.string_or "" "status" p; attempts = Json.int_or 0 "attempts" p; elapsed_ms = Json.int_or 0 "elapsed_ms" p })
              (Json.list_or_empty "problems" json) }

(* ---- the store: a directory with one file for each suite ---- *)

let valid_suite suite = suite <> "" && String.for_all (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false) suite
let path ~dir suite = Fs.(dir / (suite ^ ".json"))

let load ~dir suite =
  if not (valid_suite suite) then Error (Printf.sprintf "'%s' cannot be the name of a suite" suite)
  else
    let file = path ~dir suite in
    if not (Fs.exists file) then Error (Printf.sprintf "there is no baseline for the suite '%s' (%s)" suite file)
    else
      Result.bind (Fs.read file) (fun text ->
          Result.bind (Result.map_error (fun e -> file ^ ": " ^ e) (Json.parse text)) (fun json ->
              match of_json json with
              | Ok b when b.suite = suite -> Ok b
              | Ok b -> Error (Printf.sprintf "%s is the baseline of the suite '%s'" file b.suite)
              | Error e -> Error (file ^ ": " ^ e)))

let save ~dir b =
  if not (valid_suite b.suite) then Error (Printf.sprintf "'%s' cannot be the name of a suite" b.suite)
  else Result.bind (Fs.mkdir_p dir) (fun () -> Fs.write (path ~dir b.suite) (Json.pretty (to_json b) ^ "\n"))

(** The suites that have a baseline in a directory. *)
let stored ~dir =
  if not (Fs.is_dir dir) then []
  else List.sort compare (List.filter_map (fun f -> if Fs.extension f = ".json" then Some (Filename.remove_extension f) else None) (Fs.list_dir dir))
