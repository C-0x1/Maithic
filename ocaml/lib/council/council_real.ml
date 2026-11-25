(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A council's tools and its model, made from a running Bridge, the prover and a gateway.

    The logic of the council asks its questions through functions, so that
    it can be tested without Lean and without a model. This module makes
    those functions from the real things. The file up to the theorem is
    elaborated once and kept; every question about the theorem is asked
    there. Text that a model wrote reaches Lean as the untrusted part of a
    candidate, which the guard of the Bridge screens first. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_gateway
open Maithic_ledger
open Maithic_prover
open Maithic_refute

let said (e : Bridge_error.t) = Bridge_error.to_string e

let errors (report : Bridge_types.report) =
  report.Bridge_types.diagnostics
  |> List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error")
  |> List.map (fun (d : Bridge_types.diagnostic) -> String.trim d.Bridge_types.text)
  |> String.concat "\n\n"

(* What Lean answered about the theorem, from a report. *)
let verdict ~name (report : Bridge_types.report) =
  let found = Recheck.find_decl report name in
  if not report.Bridge_types.ok then Council_evidence.rejected (match errors report with "" -> "Lean did not accept the text" | text -> text)
  else
    match found with
    | None -> Council_evidence.rejected "Lean accepted the text but did not produce the theorem"
    | Some d ->
      { Council_evidence.accepted = true; statement = d.Bridge_types.type_; axioms = d.Bridge_types.axioms; uses_sorry = d.Bridge_types.uses_sorry; rechecked = false;
        error = "" }

(** The tools for one problem. [recheck] checks an accepted proof again in a
    fresh process, for a trust policy that asks for it. [prove] is one
    bounded attempt of the prover's automation. The error is that the file
    has errors before the theorem. *)
let raw ?limits ?recheck ?prove ~(policy : Trust_policy.t) bridge (problem : Council_problem.t) =
  let decl = problem.Council_problem.decl in
  let name = decl.Lean_source.name in
  let kept source =
    match Bridge_client.check_file ~keep:true ?limits bridge source with
    | Ok (report, Some env) when report.Bridge_types.ok -> Ok env
    | Ok (report, _) -> Error (match errors report with "" -> "Lean kept no environment" | text -> text)
    | Error e -> Error (said e)
  in
  (* With the statement tests when the library that holds them is there; without them otherwise. *)
  let tested = Result.to_option (kept (Refute_probe.with_library decl.Lean_source.prelude)) in
  match (match tested with Some env -> Ok env | None -> kept decl.Lean_source.prelude) with
  | Error why -> Error (Printf.sprintf "the file has errors before theorem '%s': %s" name why)
  | Ok env ->
    let screened ?names source ~untrusted =
      match Bridge_client.try_candidates ?names ?limits bridge ~env [ { Bridge_types.source; untrusted } ] with
      | Error e -> Error (said e)
      | Ok ({ Bridge_types.admitted = false; rejection; _ } :: _) -> Error ("the guard of the Bridge refused the text: " ^ rejection)
      | Ok ({ Bridge_types.report = Some report; _ } :: _) -> Ok report
      | Ok _ -> Error "the Bridge returned no report"
    in
    let listed (report : Bridge_types.report) =
      List.map
        (fun (d : Bridge_types.diagnostic) -> { Council_tools.severity = d.Bridge_types.severity; line = d.Bridge_types.line; text = String.trim d.Bridge_types.text })
        report.Bridge_types.diagnostics
    in
    let compile ~untrusted text =
      if untrusted = "" then Result.map (fun (report, _) -> listed report) (Result.map_error said (Bridge_client.run_command ?limits bridge ~env text))
      else Result.map listed (screened text ~untrusted)
    in
    let statement () =
      match Bridge_client.run_command ~names:[ name ] ?limits bridge ~env (Lean_source.assemble decl "sorry") with
      | Error e -> Council_evidence.rejected (said e)
      | Ok (report, _) -> verdict ~name report
    in
    let proof text =
      match screened ~names:[ name ] (Lean_source.assemble_by decl text) ~untrusted:text with
      | Error why -> Council_evidence.rejected why
      | Ok report ->
        let first = verdict ~name report in
        if (not first.Council_evidence.accepted) || first.Council_evidence.uses_sorry || not policy.Trust_policy.require_recheck then first
        else
          (match recheck with
           | None -> first
           | Some again ->
             (match again ~source:(Lean_source.with_proof_by decl text) name with
              | Ok (outcome : Recheck.outcome) when outcome.Recheck.passed && outcome.Recheck.statement = first.Council_evidence.statement ->
                { first with Council_evidence.rechecked = true; axioms = outcome.Recheck.axioms }
              | Ok outcome -> Council_evidence.rejected ("the proof did not pass when it was checked again in a fresh process: " ^ outcome.Recheck.first_error)
              | Error e -> Council_evidence.rejected ("the proof could not be checked again in a fresh process: " ^ said e)))
    in
    let lookup names =
      match Bridge_client.try_candidates ?limits bridge ~env (List.map (fun n -> { Bridge_types.source = "#check " ^ n ^ "\n"; untrusted = n }) names) with
      | Error _ -> []
      | Ok reports ->
        List.filter_map
          (fun (r : Bridge_types.candidate_report) ->
            match (r.Bridge_types.report, List.nth_opt names r.Bridge_types.index) with
            | Some report, Some n when r.Bridge_types.admitted && report.Bridge_types.ok ->
              Option.map
                (fun (d : Bridge_types.diagnostic) ->
                  (n, Text.truncate 300 (String.concat " " (List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.concat " " (String.split_on_char '\n' d.Bridge_types.text)))))))
                (List.find_opt (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "info") report.Bridge_types.diagnostics)
            | _ -> None)
          reports
    in
    let refute =
      Option.map
        (fun _ () ->
          (* The theorem is declared, unproved, beside what stands before it; the test names it. *)
          match Bridge_client.run_command ~keep:true ?limits bridge ~env (Refute_probe.declaration decl) with
          | Ok (declared, Some after) when declared.Bridge_types.ok ->
            let command text read =
              match Bridge_client.run_command ?limits bridge ~env:after text with Error e -> Error (said e) | Ok (report, _) -> Result.bind (Refute_probe.printed report) read
            in
            let tools =
              { Refute_search.test = (fun ~cases ~size ~seed -> command (Refute_probe.test_command ~cases ~size ~seed name) Refute_probe.test_report_of_json);
                oracle = (fun () -> Error "no solver is asked here"); vacuity = (fun () -> Error "not asked here");
                confirm = (fun assignment -> Refute_witness.confirm ~check:(Refute_probe.witness_check ?limits bridge ~env:after ~theorem:name) ~theorem:name assignment);
                clock = Unix.gettimeofday }
            in
            let found = match Refute_search.search ~methods:[ Refute_search.Test ] tools with Refute_search.Refuted { counterexample; confirmation; _ }, _ -> Some (counterexample, confirmation) | _ -> None in
            ignore (Bridge_client.release_env bridge after);
            found
          | _ -> None)
        tested
    in
    Ok { Council_tools.compile; statement; proof; lookup; prove; refute }

(** One bounded attempt of the prover's automation on the theorem of a problem: no model is asked. *)
let prover ?recheck ~bridge ~(policy : Trust_policy.t) (settings : Engine.settings) (problem : Council_problem.t) () =
  match Engine.prove ?recheck ~bridge ~policy { settings with Engine.use_ai = false } ~source:problem.Council_problem.source problem.Council_problem.theorem with
  | Error (f : Fault.t) -> (Error f.Fault.message, 1)
  | Ok ({ Engine.proof = Some proof; status = Status.Verified | Status.Checked; _ } as result) ->
    (Ok (proof, Option.value result.Engine.proposer ~default:"prover"), List.length result.Engine.attempts)
  | Ok result ->
    let tried = List.length result.Engine.attempts in
    (Error (Printf.sprintf "its automation found no proof in %d attempt%s" tried (if tried = 1 then "" else "s")), tried)

(** A model, asked through the gateway. *)
let model gateway ~max_tokens : Council_session.model =
 fun ~purpose ~schema bindings ->
  match Gateway.prompt gateway purpose with
  | Error e -> Error e
  | Ok loaded ->
    (match Prompt.render loaded bindings with
     | Error e -> Error e
     | Ok (system, user) ->
       (match Gateway.complete gateway (Gateway_types.request ~purpose ~system ~max_tokens ~schema [ Gateway_types.user user ]) with
        | Error e -> Error (Gateway_types.error_to_string e)
        | Ok response -> Ok (response.Gateway_types.text, response.Gateway_types.usage.Gateway_types.input_tokens, response.Gateway_types.usage.Gateway_types.output_tokens)))
