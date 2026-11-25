(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [refute] command: counterexamples and vacuity, each confirmed by Lean. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_prover
open Maithic_oracle
open Maithic_refute
open Cli_args
open Commands

let refute_fault number ~exit message = Fault.make ~area:"REFUTE" ~number ~exit message
let bridge_fault = Bridge_error.to_fault

(* What the search needs, for one theorem, on a Bridge that holds the file up to the theorem
   ([before]) and the same with the theorem declared ([after]). *)
let tools ctx bridge ~before ~after (decl : Lean_source.decl) =
  let said (e : Bridge_error.t) = Bridge_error.to_string e in
  let command text read =
    match Bridge_client.run_command ~limits:(limits ctx) bridge ~env:after text with
    | Error e -> Error (said e)
    | Ok (report, _) -> Result.bind (Refute_probe.printed report) read
  in
  let first_line text = match String.split_on_char '\n' (String.trim text) with line :: _ -> line | [] -> "" in
  let test ~cases ~size ~seed =
    (* What Lean says when a statement cannot be sampled runs over several lines; the first says it. *)
    Result.map_error
      (fun why -> match String.index_opt why ':' with Some _ when Text.contains why "cannot be tested by sampling" -> "the statement cannot be tested by sampling" | _ -> first_line why)
      (command (Refute_probe.test_command ~cases ~size ~seed decl.Lean_source.name) Refute_probe.test_report_of_json)
  in
  let vacuity () = Result.map_error first_line (command (Refute_probe.vacuity_command decl.Lean_source.name) Refute_probe.vacuity_of_json) in
  let oracle () =
    match oracle_registry ctx with
    | Error (f : Fault.t) -> Error f.Fault.message
    | Ok registry ->
      (match Bridge_client.start_proof ~limits:(limits ctx) bridge ~env:before (`Statement decl.Lean_source.header) with
       | Error e -> Error (said e)
       | Ok (_, ({ Bridge_types.goals = [ goal ]; _ } as opened) :: _) ->
         ignore (Bridge_client.release_state bridge opened.Bridge_types.state);
         (match Oracle_export.export goal with
          | Error why -> Ok (Refute_search.No_answer ("not a statement a solver takes: " ^ why))
          | Ok exported ->
            (match Oracle_registry.solve ~timeout:(Config.float ctx.config "oracle.timeout_s") registry exported.Oracle_export.goal with
             | Error why -> Error why
             | Ok result ->
               Ok
                 (match result.Oracle_outcome.outcome with
                  | Oracle_outcome.Refuted point -> Refute_search.Point point
                  | Oracle_outcome.Proved { claim; _ } -> Refute_search.Proved claim
                  | Oracle_outcome.Unknown why -> Refute_search.No_answer why)))
       | Ok _ -> Ok (Refute_search.No_answer "the statement does not open a single goal"))
  in
  let check = Refute_probe.witness_check ~limits:(limits ctx) bridge ~env:after ~theorem:decl.Lean_source.name in
  { Refute_search.test; oracle; vacuity; confirm = (fun assignment -> Refute_witness.confirm ~check ~theorem:decl.Lean_source.name assignment);
    clock = Unix.gettimeofday }

(* One theorem, examined on a Bridge of its own state: the file up to the theorem, then the theorem. *)
let examine ctx bridge ~methods ~budget ~file (decl : Lean_source.decl) =
  let name = decl.Lean_source.name in
  let* report, before =
    Result.map_error bridge_fault
      (Bridge_client.check_file ~keep:true ~limits:(limits ctx) bridge (Refute_probe.with_library decl.Lean_source.prelude))
  in
  let* before =
    match before with
    | Some env when report.Bridge_types.ok -> Ok env
    | _ ->
      let said = Bridge_types.error_text report in
      if Refute_probe.library_missing said then
        Error
          (refute_fault 2 ~exit:Exit_code.Unavailable
             "the statement tests are not on Lean's search path. They are part of the Maithic Lean package: run the Bridge from a \
              build of the source tree, or give --project a Lake project that depends on Maithic")
      else Error (refute_fault 3 ~exit:Exit_code.Failure (Printf.sprintf "the file has errors before theorem '%s':\n%s" name said))
  in
  let* declared, after =
    Result.map_error bridge_fault (Bridge_client.run_command ~keep:true ~limits:(limits ctx) bridge ~env:before (Refute_probe.declaration decl))
  in
  let* after =
    match after with
    | Some env when declared.Bridge_types.ok -> Ok env
    | _ ->
      Error
        (refute_fault 3 ~exit:Exit_code.Failure
           (Printf.sprintf "the statement of '%s' does not elaborate:\n%s" name (Bridge_types.error_text declared)))
  in
  let result =
    Refute_search.run ~methods ~budget ~file ~theorem:name ~line:decl.Lean_source.line ~statement:(Lean_source.statement decl)
      (tools ctx bridge ~before ~after decl)
  in
  ignore (Bridge_client.release_env bridge after);
  ignore (Bridge_client.release_env bridge before);
  Ok result

let number parsed name ~default ~least =
  match get parsed name with
  | None -> Ok default
  | Some text ->
    (match int_of_string_opt text with
     | Some n when n >= least -> Ok n
     | _ -> Error (usage_fault (Printf.sprintf "--%s needs a whole number, %d or more" name least)))

let refute_cmd =
  { path = [ "refute" ]; summary = "Look for a counterexample to a statement, or for hypotheses that make it empty."; synopsis = "FILE";
    details =
      "The theorems of FILE whose proofs are unfinished are examined, or the one named by --theorem. Three methods are tried in \
       turn: the statement is tested on sampled values in Lean; an arithmetic statement is sent to a solver, which answers a \
       false one with an exact point; and Lean looks for hypotheses that contradict each other. A counterexample counts only \
       when Lean accepts a proof that the statement is false at its values, and that proof is printed. The exit status is 8 \
       when a statement is refuted, 7 when one is vacuous or has a counterexample that Lean did not confirm, and 0 when nothing \
       was found, which proves nothing.";
    options =
      [ value "theorem" "NAME" "Examine this theorem, whatever its proof."; value "method" "LIST" "The methods to try: test, oracle, vacuity, or all (all).";
        value "cases" "N" "Cases that must pass in one round of testing (100)."; value "size" "N" "The size of the largest case (30).";
        value "seed" "N" "The seed of the first round (0)."; value "rounds" "N" "Rounds of testing, each with the next seed (3).";
        value "seconds" "S" "No method is started after this many seconds (60).";
        value "project" "DIR" "A Lake project whose modules the file may import."; value "out" "FILE" "Write the results as JSON to FILE as well." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "refute needs exactly one FILE argument") in
            let* source = read_source file in
            let* methods =
              match get parsed "method" with
              | None -> Ok Refute_search.methods
              | Some text -> Result.map_error usage_fault (Refute_search.methods_of_string text)
            in
            let standard = Refute_search.default_budget in
            let* cases = number parsed "cases" ~default:standard.Refute_search.cases ~least:1 in
            let* size = number parsed "size" ~default:standard.Refute_search.size ~least:0 in
            let* seed = number parsed "seed" ~default:standard.Refute_search.seed ~least:0 in
            let* rounds = number parsed "rounds" ~default:standard.Refute_search.rounds ~least:1 in
            let* seconds =
              match get parsed "seconds" with
              | None -> Ok standard.Refute_search.seconds
              | Some text -> (match float_of_string_opt text with Some s when s > 0.0 -> Ok s | _ -> Error (usage_fault "--seconds needs a number above zero"))
            in
            let budget = { Refute_search.cases; size; seed; rounds; seconds } in
            let* decls =
              match get parsed "theorem" with
              | Some name -> Result.map (fun d -> [ d ]) (Result.map_error (refute_fault 1 ~exit:Exit_code.Usage) (Lean_source.find source name))
              | None ->
                (match List.filter Lean_source.has_sorry (Lean_source.declarations source) with
                 | [] ->
                   Error
                     (refute_fault 1 ~exit:Exit_code.Usage
                        (Printf.sprintf "%s has no theorem with an unfinished proof; name one with --theorem" file))
                 | unfinished -> Ok unfinished)
            in
            let* bridge, _ = start_bridge ctx in
            let results =
              List.fold_left
                (fun acc decl ->
                  let* done_ = acc in
                  let* result = examine ctx bridge ~methods ~budget ~file decl in
                  if (not ctx.json) && not ctx.quiet then print_string (Refute_search.render ~color:!Term.colour_enabled result ^ "\n");
                  Ok (result :: done_))
                (Ok []) decls
            in
            Bridge_client.shutdown bridge;
            let* results = Result.map List.rev results in
            let document = Refute_search.to_json ~file results in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document else print_endline (Refute_search.summary results);
            Ok (Exit_code.to_int (Refute_search.exit_code results)))) }

let all = [ refute_cmd ]
