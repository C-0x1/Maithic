(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge
open Maithic_prover

let library = "Maithic.Refute.Vacuity.Command"

let with_library source =
  let import = "import " ^ library ^ "\n" in
  (* A file of the module system begins with the word [module], and its imports follow it. *)
  let masked = Lean_source.mask source in
  let n = String.length masked in
  let rec skip i = if i < n && (masked.[i] = ' ' || masked.[i] = '\n' || masked.[i] = '\t' || masked.[i] = '\r') then skip (i + 1) else i in
  let start = skip 0 in
  let word = "module" in
  let w = String.length word in
  let is_module =
    start + w <= n && String.sub masked start w = word
    && (start + w = n || masked.[start + w] = '\n' || masked.[start + w] = ' ' || masked.[start + w] = '\r' || masked.[start + w] = '\t')
  in
  if not is_module then import ^ source
  else
    let line_end = match String.index_from_opt source (start + w) '\n' with Some i -> i + 1 | None -> n in
    String.sub source 0 line_end ^ (if line_end = n && n > 0 && source.[n - 1] <> '\n' then "\n" else "") ^ import
    ^ String.sub source line_end (n - line_end)

let declaration decl = Lean_source.assemble decl "sorry"

let test_command ~cases ~size ~seed name = Printf.sprintf "#maithic_test json (cases := %d) (size := %d) (seed := %d) %s" cases size seed name
let vacuity_command name = Printf.sprintf "#maithic_vacuous json %s" name

type verdict = Passed | Falsified | Gave_up

type test_report = { verdict : verdict; cases : int; discarded : int; shrinks : int; seed : int; counterexample : (string * string) list }

let ( let* ) = Result.bind

let test_report_of_json j =
  let* verdict =
    match Json.string_or "" "verdict" j with
    | "passed" -> Ok Passed
    | "falsified" -> Ok Falsified
    | "gave-up" -> Ok Gave_up
    | other -> Error (Printf.sprintf "a test report with the verdict '%s'" other)
  in
  let pair item = match (Json.string_or "" "name" item, Json.member "value" item) with name, Some (Json.String value) when name <> "" -> Some (name, value) | _ -> None in
  let items = Json.list_or_empty "counterexample" j in
  let counterexample = List.filter_map pair items in
  if List.length counterexample <> List.length items then Error "a counterexample whose values are not all named"
  else
    Ok
      { verdict; cases = Json.int_or 0 "cases" j; discarded = Json.int_or 0 "discarded" j; shrinks = Json.int_or 0 "shrinks" j;
        seed = Json.int_or 0 "seed" j; counterexample }

type vacuity = {
  vacuous : bool;
  contradiction : (string list * string) option;
  empty_domains : (string * string * string) list;
  trivial_by : string option;
}

let strings name j = List.filter_map (function Json.String s -> Some s | _ -> None) (Json.list_or_empty name j)

let vacuity_of_json j =
  match Json.member "vacuous" j with
  | Some (Json.Bool vacuous) ->
    let found_by o = Json.string_or "" "found_by" o in
    Ok
      { vacuous;
        contradiction = (match Json.member "contradiction" j with Some (Json.Obj _ as c) -> Some (strings "hypotheses" c, found_by c) | _ -> None);
        empty_domains =
          List.map (fun d -> (Json.string_or "" "variable" d, Json.string_or "" "type" d, found_by d)) (Json.list_or_empty "empty_domains" j);
        trivial_by = (match Json.member "trivial_conclusion" j with Some (Json.Obj _ as t) -> Some (found_by t) | _ -> None) }
  | _ -> Error "a vacuity report that does not say whether the statement is vacuous"

let describe_vacuity v =
  let parts =
    (match v.contradiction with
     | Some ([], by) -> [ Printf.sprintf "its hypotheses contradict each other (shown by %s)" by ]
     | Some ([ one ], by) -> [ Printf.sprintf "the hypothesis %s cannot hold (shown by %s)" one by ]
     | Some (several, by) -> [ Printf.sprintf "the hypotheses %s contradict each other (shown by %s)" (String.concat ", " several) by ]
     | None -> [])
    @ List.map (fun (variable, type_, by) -> Printf.sprintf "%s ranges over %s, which is empty (shown by %s)" variable type_ by) v.empty_domains
  in
  if parts <> [] then String.concat "; " parts
  else match v.trivial_by with Some by -> Printf.sprintf "the conclusion holds without the hypotheses (shown by %s)" by | None -> ""

let printed (report : Bridge_types.report) =
  let documents =
    List.filter_map
      (fun (d : Bridge_types.diagnostic) ->
        if d.Bridge_types.severity <> "info" then None
        else match Json.parse d.Bridge_types.text with Ok (Json.Obj _ as j) -> Some j | _ -> None)
      report.Bridge_types.diagnostics
  in
  match List.rev documents with
  | last :: _ -> Ok last
  | [] ->
    let said = Bridge_types.error_text report in
    Error (if said = "" then "the command printed no report" else said)

let library_missing said =
  List.exists (fun needle -> Text.contains said needle) [ "unknown module prefix 'Maithic'"; "unknown package 'Maithic'"; "of module Maithic.Refute"; "Maithic/Refute" ]

let witness_check ?limits bridge ~env ~theorem ~untrusted lean =
  let rejected error = { Refute_witness.accepted = false; uses_sorry = false; axioms = []; error } in
  let name = Refute_witness.name_of theorem in
  match Bridge_client.try_candidates ~names:[ name ] ?limits bridge ~env [ { Bridge_types.source = lean; untrusted } ] with
  | Error e -> rejected (Bridge_error.to_string e)
  | Ok [ { Bridge_types.admitted = false; rejection; _ } ] -> rejected ("the guard of the Bridge refused the text: " ^ rejection)
  | Ok [ { Bridge_types.report = Some report; _ } ] ->
    if not report.Bridge_types.ok then rejected (Bridge_types.error_text report)
    else
      (match List.find_opt (fun (d : Bridge_types.decl) -> d.Bridge_types.found && d.Bridge_types.name = name) report.Bridge_types.decls with
       | Some d -> { Refute_witness.accepted = true; uses_sorry = d.Bridge_types.uses_sorry; axioms = d.Bridge_types.axioms; error = "" }
       | None -> rejected "Lean accepted the text but did not produce the theorem")
  | Ok _ -> rejected "the Bridge returned no report"
