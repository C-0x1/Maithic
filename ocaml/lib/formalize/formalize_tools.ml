(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The tools of a formalization, made from a running Bridge and a gateway.

    The logic of Formalize asks its questions through functions, so that it
    can be tested without Lean and without a model. This module makes those
    functions from the real things.

    Two environments are kept. A statement elaborates when it elaborates in
    the first, which holds the modules a statement may use and nothing else.
    The second adds the commands of Maithic that examine statements; a
    statement that elaborated is declared there under the name Maithic gave
    it, and every command names it. Text from a model reaches Lean by one
    road only: as the untrusted part of a candidate, which the guard of the
    Bridge screens before anything is elaborated. *)

open Maithic_base
open Maithic_bridge
open Maithic_ledger
open Maithic_refute

let said (e : Bridge_error.t) = Bridge_error.to_string e

(** The module that holds the commands that inspect and compare statements. *)
let library = "Maithic.Formalize.Command"

type session = {
  bridge : Bridge_client.t;
  limits : Bridge_types.limits option;
  budget : Refute_search.budget;  (** How long a counterexample is looked for. *)
  plain : int;  (** The environment of the allowed imports. *)
  checks : int;  (** The same, with the commands that examine statements. *)
  declared : (string, (int, string) result) Hashtbl.t;  (** An environment for each candidate that was declared. *)
}

(** What Lean reported as errors, with an empty line between one and the
    next, and without the places: they are places in a text of two lines
    that nobody sees. *)
let errors (report : Bridge_types.report) =
  report.Bridge_types.diagnostics
  |> List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error")
  |> List.map (fun (d : Bridge_types.diagnostic) -> String.trim d.Bridge_types.text)
  |> String.concat "\n\n"

let first_line text = match String.split_on_char '\n' (String.trim text) with line :: _ -> line | [] -> ""

(** Whether what Lean said means that the commands of Maithic are not on its search path. *)
let library_missing why = Refute_probe.library_missing why || Text.contains why library

(** Opens the two environments. The first error is that of the imports a
    statement may use; the second, that of the commands. *)
let open_session ?limits ?(budget = Refute_search.default_budget) bridge ~imports =
  match Bridge_client.load_header bridge imports with
  | Error e -> Error (`Imports (said e))
  | Ok (plain, _) ->
    (match Bridge_client.load_header bridge (imports @ [ library; Refute_probe.library ]) with
     | Error e -> Error (`Commands (said e))
     | Ok (checks, _) -> Ok { bridge; limits; budget; plain; checks; declared = Hashtbl.create 8 })

(* One text with an untrusted part, given to Lean through the guard. *)
let screened session ~env ?name source ~untrusted =
  match
    Bridge_client.try_candidates ?names:(Option.map (fun n -> [ n ]) name) ?limits:session.limits session.bridge ~env [ { Bridge_types.source; untrusted } ]
  with
  | Error e -> Error (said e)
  | Ok ({ Bridge_types.admitted = false; rejection; _ } :: _) -> Error ("the guard of the Bridge refused the statement: " ^ rejection)
  | Ok ({ Bridge_types.report = Some report; _ } :: _) -> Ok report
  | Ok _ -> Error "the Bridge returned no report"

(** Compiles a candidate: its statement with [sorry] for a proof, where only the allowed imports are known. *)
let compile session : Formalize_elab.compile =
 fun (c : Formalize_candidate.t) ->
  let failed said = { Formalize_elab.ok = false; statement = ""; said } in
  match screened session ~env:session.plain ~name:c.Formalize_candidate.name (Formalize_candidate.theorem c) ~untrusted:c.Formalize_candidate.signature with
  | Error why -> failed why
  | Ok report ->
    if not report.Bridge_types.ok then failed (match errors report with "" -> "Lean did not accept the statement" | text -> text)
    else
      (match Recheck.find_decl report c.Formalize_candidate.name with
       | Some d -> { Formalize_elab.ok = true; statement = d.Bridge_types.type_; said = "" }
       | None -> failed "Lean accepted the text but did not produce the theorem")

(* The environment of the checks with a candidate declared in it. The text goes through the guard
   there before it is declared for keeps. *)
let declared session (c : Formalize_candidate.t) =
  let key = c.Formalize_candidate.name ^ "\n" ^ Formalize_candidate.key c in
  match Hashtbl.find_opt session.declared key with
  | Some found -> found
  | None ->
    let text = Formalize_candidate.theorem c in
    let found =
      match screened session ~env:session.checks ~name:c.Formalize_candidate.name text ~untrusted:c.Formalize_candidate.signature with
      | Error why -> Error why
      | Ok report when not report.Bridge_types.ok -> Error (first_line (errors report))
      | Ok _ ->
        (match Bridge_client.run_command ~keep:true ?limits:session.limits session.bridge ~env:session.checks text with
         | Ok (_, Some env) -> Ok env
         | Ok (report, None) -> Error (match first_line (errors report) with "" -> "Lean kept no environment" | line -> line)
         | Error e -> Error (said e))
    in
    Hashtbl.replace session.declared key found;
    found

(* A command of Maithic that names a declared candidate, and the JSON it prints. *)
let command session c text read =
  match declared session c with
  | Error why -> Error why
  | Ok env ->
    (match Bridge_client.run_command ?limits:session.limits session.bridge ~env text with
     | Error e -> Error (said e)
     | Ok (report, _) -> Result.map_error first_line (Result.bind (Refute_probe.printed report) read))

let inspect session (c : Formalize_candidate.t) =
  command session c ("#maithic_stmt json " ^ c.Formalize_candidate.name) (fun j ->
      Ok
        { Formalize_faith.unused = Json.strings (Json.List (Json.list_or_empty "unused" j));
          traps = List.map (fun t -> (Json.string_or "" "kind" t, Json.string_or "" "term" t, Json.string_or "" "note" t)) (Json.list_or_empty "traps" j);
          annotated = Json.string_or "" "annotated" j })

let vacuous session (c : Formalize_candidate.t) =
  Result.map
    (fun (v : Refute_probe.vacuity) -> if v.Refute_probe.vacuous then Some (Refute_probe.describe_vacuity v) else None)
    (command session c (Refute_probe.vacuity_command c.Formalize_candidate.name) Refute_probe.vacuity_of_json)

(** Sends a candidate to Refute: its statement is run on sampled values, and
    a case that fails counts when Lean accepts a proof that the statement
    is false there. *)
let refute session (c : Formalize_candidate.t) =
  let name = c.Formalize_candidate.name in
  match declared session c with
  | Error why -> Error why
  | Ok env ->
    let passed = ref 0 in
    let test ~cases ~size ~seed =
      match command session c (Refute_probe.test_command ~cases ~size ~seed name) Refute_probe.test_report_of_json with
      | Error why -> Error (if Text.contains why "cannot be tested by sampling" then "the statement cannot be tested by sampling" else why)
      | Ok report ->
        if report.Refute_probe.verdict = Refute_probe.Passed then passed := !passed + report.Refute_probe.cases;
        Ok report
    in
    let tools =
      { Refute_search.test; oracle = (fun () -> Error "no solver is asked here"); vacuity = (fun () -> Error "asked for on its own");
        confirm = (fun assignment -> Refute_witness.confirm ~check:(Refute_probe.witness_check ?limits:session.limits session.bridge ~env ~theorem:name) ~theorem:name assignment);
        clock = Unix.gettimeofday }
    in
    let point assignment = if assignment = [] then "its only case" else String.concat ", " (List.map (fun (variable, value) -> variable ^ " = " ^ value) assignment) in
    (match Refute_search.search ~methods:[ Refute_search.Test ] ~budget:session.budget tools with
     | Refute_search.Refuted { counterexample; _ }, _ -> Ok (Formalize_faith.Refuted counterexample)
     | Refute_search.Unconfirmed { counterexample; why; _ }, _ ->
       Error (Printf.sprintf "a test failed at %s, and Lean did not confirm it: %s" (point counterexample) (first_line why))
     | _, steps ->
       (match List.find_opt (fun (s : Refute_search.step) -> s.Refute_search.outcome = "not-applicable" || s.Refute_search.outcome = "gave-up") steps with
        | Some step when !passed = 0 -> Error step.Refute_search.detail
        | _ -> Ok (Formalize_faith.Survived (Printf.sprintf "%d sampled cases" !passed))))

(** The first of the proofs that Lean accepts for a candidate, where only the allowed imports are known. *)
let closes session (c : Formalize_candidate.t) proofs =
  let name = c.Formalize_candidate.name in
  let candidates = List.map (fun proof -> { Bridge_types.source = Formalize_candidate.theorem ~proof c; untrusted = c.Formalize_candidate.signature }) proofs in
  match Bridge_client.try_candidates ~names:[ name ] ~stop_on_first:true ?limits:session.limits session.bridge ~env:session.plain candidates with
  | Error _ -> None
  | Ok reports ->
    List.find_map
      (fun (r : Bridge_types.candidate_report) ->
        match r.Bridge_types.report with
        | Some report when r.Bridge_types.admitted && report.Bridge_types.ok ->
          (match Recheck.find_decl report name with Some d when not d.Bridge_types.uses_sorry -> List.nth_opt proofs r.Bridge_types.index | _ -> None)
        | _ -> None)
      reports

(** How two candidates are related, when they are the same statement: the
    same text, or the same once notation and definitions are unfolded, as
    [a > b] is [b < a]. Two statements that Lean can prove from each other
    are not counted as the same: any two true statements can be, and one of
    them may still be the better reading of the problem. The second
    candidate is declared beside the first for this one question. *)
let same session (a : Formalize_candidate.t) (b : Formalize_candidate.t) =
  match declared session a with
  | Error _ -> None
  | Ok env ->
    let source = Formalize_candidate.theorem b ^ Printf.sprintf "#maithic_compare json %s with %s\n" a.Formalize_candidate.name b.Formalize_candidate.name in
    (match screened session ~env source ~untrusted:b.Formalize_candidate.signature with
     | Error _ -> None
     | Ok report ->
       (match Refute_probe.printed report with
        | Ok j -> (match Json.string_or "" "relation" j with ("identical" | "definitional") as relation -> Some relation | _ -> None)
        | Error _ -> None))

(** The signatures of the names Lean knows, as [#check] prints them, where only the allowed imports are known. *)
let lookup session : Formalize_repair.lookup =
 fun names ->
  if names = [] then []
  else
    match
      Bridge_client.try_candidates ?limits:session.limits session.bridge ~env:session.plain
        (List.map (fun name -> { Bridge_types.source = "#check " ^ name ^ "\n"; untrusted = name }) names)
    with
    | Error _ -> []
    | Ok reports ->
      List.filter_map
        (fun (r : Bridge_types.candidate_report) ->
          match (r.Bridge_types.report, List.nth_opt names r.Bridge_types.index) with
          | Some report, Some name when r.Bridge_types.admitted && report.Bridge_types.ok ->
            Option.map
              (fun (d : Bridge_types.diagnostic) ->
                (name, Text.truncate 300 (String.concat " " (List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.concat " " (String.split_on_char '\n' d.Bridge_types.text)))))))
              (List.find_opt (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "info") report.Bridge_types.diagnostics)
          | _ -> None)
        reports

(** The tools of {!Formalize_run.run}. With [statements], those are the
    candidates and nothing is drafted. Without [gateway] no model is asked:
    nothing is repaired and nothing is translated back. *)
let tools ?gateway ?(max_tokens = 2048) ?statements ~name ~imports ~intake session =
  let draft =
    match (statements, gateway) with
    | Some given, _ -> fun ~count:_ _ -> Ok (Formalize_run.given ~name ~imports given)
    | None, Some gateway -> fun ~count intake -> Formalize_draft.ask gateway ~max_tokens ~name ~count ~imports intake
    | None, None -> fun ~count:_ _ -> Error "no model is asked and no statement was given"
  in
  { Formalize_run.draft; compile = compile session;
    repair = Option.map (fun gateway -> Formalize_repair.ask gateway ~max_tokens ~lookup:(lookup session) ~imports ~intake) gateway;
    lean = { Formalize_faith.inspect = inspect session; vacuous = vacuous session; refute = refute session; closes = closes session };
    model = Option.map (fun gateway -> Formalize_back.model gateway ~max_tokens) gateway; same = same session }
