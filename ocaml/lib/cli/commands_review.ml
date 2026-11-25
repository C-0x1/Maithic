(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [review] command. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_prover
open Maithic_review
open Cli_args
open Commands

let review_fault number ~exit message = Fault.make ~area:"REVIEW" ~number ~exit message

(* Suggestions for the theorems of one file, each tried on the Bridge where the theorem stands. *)
let suggest ctx bridge ~source (e : Extraction.t) =
  let limits = limits ctx in
  List.concat_map
    (fun (d : Extraction.declaration) ->
      match d.Extraction.kind, d.Extraction.uses_sorry, Lean_source.find source d.Extraction.name with
      | "theorem", false, Ok decl when Review_simplify.steps decl.Lean_source.proof <> [] || String.length decl.Lean_source.proof > 40 ->
        (match Bridge_client.check_file ~keep:true ~limits bridge decl.Lean_source.prelude with
         | Ok (_, Some env) ->
           let check proof =
             match
               Bridge_client.try_candidates ~limits bridge ~env [ { Bridge_types.source = Lean_source.assemble decl proof; untrusted = proof } ]
             with
             | Ok [ { Bridge_types.admitted = true; report = Some report; _ } ] -> report.Bridge_types.ok && report.Bridge_types.warnings = 0
             | _ -> false
           in
           let found =
             Review_simplify.suggestions ~check ~file:e.Extraction.file ~line:d.Extraction.name_line ~declaration:d.Extraction.name decl.Lean_source.proof
           in
           ignore (Bridge_client.release_env bridge env);
           found
         | _ -> [])
      | _ -> [])
    e.Extraction.declarations

let review_cmd =
  { path = [ "review" ]; summary = "Review Lean files: unfinished work, axioms, duplicates, conventions and simpler proofs."; synopsis = "PATH...";
    details =
      "Every Lean file among the paths is elaborated and its declarations are checked: for sorry in a proof or behind it, for \
       axioms beyond Lean's logic, for theorems that restate one that exists, for missing documentation and for names out of \
       convention. With --suggest, simpler proofs are tried and reported only when Lean accepts them. With --ai, a model adds \
       findings of its own; it cannot remove or lower any. A comment `-- maithic: allow RULE (reason)` above a declaration \
       suppresses a rule there. The exit status is 7 when a finding is an error, and 0 otherwise.";
    options =
      [ value "project" "DIR" "A Lake project whose modules the files may import.";
        value "profile" "NAME" "The policy to start from: default, strict or minimal.";
        value "review-config" "FILE" "A policy file with [review] tables (maithic-review.toml when it is there).";
        switch "suggest" "Try simpler proofs and report the ones Lean accepts."; switch "ai" "Ask the model for findings as well.";
        value "format" "FORMAT" "terminal, markdown or json (terminal)."; value "out" "FILE" "Write the report to FILE instead of printing it." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* files = Result.map_error usage_fault (Lean_files.expand parsed.positional) in
            let* () = if files = [] then Error (usage_fault "no Lean file among the paths") else Ok () in
            let* policy =
              let from_file = match get parsed "review-config" with Some f -> Some f | None -> if Fs.exists "maithic-review.toml" then Some "maithic-review.toml" else None in
              let* base =
                match from_file with
                | Some file -> Result.map_error (fun e -> review_fault 1 ~exit:Exit_code.Config (file ^ ": " ^ e)) (Review_policy.of_file file)
                | None -> Ok Review_policy.default
              in
              match get parsed "profile" with
              | None -> Ok base
              | Some name ->
                let* profile = Result.map_error (review_fault 1 ~exit:Exit_code.Usage) (Review_policy.of_profile name) in
                Ok { profile with Review_policy.switches = base.Review_policy.switches @ profile.Review_policy.switches;
                                  severities = base.Review_policy.severities @ profile.Review_policy.severities }
            in
            let* format =
              match Option.value ~default:(if ctx.json then "json" else "terminal") (get parsed "format") with
              | ("terminal" | "markdown" | "json") as f -> Ok f
              | other -> Error (usage_fault (Printf.sprintf "there is no format '%s'; the formats are terminal, markdown and json" other))
            in
            let project = match Config.string ctx.config "bridge.project" with "" -> None | dir -> Some dir in
            let* gateway = if has parsed "ai" then Result.map Option.some (Gateway.create ctx.config) else Ok None in
            let* results =
              List.fold_left
                (fun acc file ->
                  let* done_ = acc in
                  let* source = Result.map_error usage_fault (Fs.read file) in
                  let* bridge, _ = start_bridge ctx in
                  let reviewed =
                    let* e =
                      Result.map_error Bridge_error.to_fault
                        (Bridge_client.extract ~file ~module_name:(Lean_files.module_of ?project file) ~lint:true ~duplicates:true ~limits:(limits ctx)
                           bridge source)
                    in
                    let lint = List.map (Review_finding.of_lint ~file) e.Extraction.findings in
                    let suggested = if has parsed "suggest" && e.Extraction.ok then suggest ctx bridge ~source e else [] in
                    let from_model, note =
                      match gateway with
                      | None -> ([], "")
                      | Some g ->
                        (match Review_ai.review g ~file ~source with
                         | Ok found -> (found, "")
                         | Error why -> ([], "the model was not asked: " ^ why))
                    in
                    let note = if e.Extraction.ok then note else String.trim (Printf.sprintf "the file has %d error(s) and was reviewed as far as it elaborated. %s" e.Extraction.errors note) in
                    let starts = List.map (fun (d : Extraction.declaration) -> (d.Extraction.name, d.Extraction.line)) e.Extraction.declarations in
                    let findings, suppressed = Review_policy.apply policy ~source ~starts (lint @ Review_semantic.duplicates e @ suggested @ from_model) in
                    Ok { Review_report.file; source; declarations = List.length e.Extraction.declarations; findings; suppressed; note }
                  in
                  Bridge_client.shutdown bridge;
                  let* reviewed = reviewed in
                  Ok (reviewed :: done_))
                (Ok []) files
            in
            let report = { Review_report.profile = policy.Review_policy.profile; files = List.rev results } in
            let text =
              match format with
              | "json" -> Json.pretty (Review_report.to_json report) ^ "\n"
              | "markdown" -> Review_report.markdown report
              | _ -> Review_report.terminal ~color:(!Term.colour_enabled && get parsed "out" = None) report
            in
            let* () =
              match get parsed "out" with
              | Some out -> Result.map_error usage_fault (Fs.write out text)
              | None -> print_string text; Ok ()
            in
            Ok (if Review_report.worst report = Some Review_finding.Error then Exit_code.to_int Exit_code.Not_verified else 0))) }

let all = [ review_cmd ]
