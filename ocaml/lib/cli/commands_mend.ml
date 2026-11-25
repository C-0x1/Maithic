(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [mend] commands: find what is broken, repair it, and shorten proofs. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_prover
open Maithic_mend
open Cli_args
open Commands

let mend_fault number ~exit message = Fault.make ~area:"MEND" ~number ~exit message
let bridge_said (e : Bridge_error.t) = Bridge_error.to_string e
let scan_schema = "maithic.mend.scan/1"

let file_check ctx bridge = Mend_tools.file_check ~limits:(limits ctx) bridge
let theorem_check ctx bridge decl = Mend_tools.theorem_check ~limits:(limits ctx) bridge decl

let number parsed name ~default =
  match get parsed name with
  | None -> Ok default
  | Some text -> (match int_of_string_opt text with Some n when n >= 0 -> Ok n | _ -> Error (usage_fault (Printf.sprintf "--%s needs a number, and got %s" name text)))

(* ---- mend scan ---- *)

let scan_cmd =
  { path = [ "mend"; "scan" ]; summary = "Find what is broken in Lean files, and why."; synopsis = "PATH...";
    details =
      "Each PATH is a Lean file or a directory of them. Every file is given to Lean, and what Lean reports is collected by \
       declaration and put in a class with a known treatment: a name that is gone, a name that is deprecated, a tactic that no \
       longer closes its goal, a term of the wrong type, a suggestion of Lean that was never written down. The command changes \
       nothing; `maithic mend repair` does. It exits with status 7 when a file has an error.";
    options = [ value "project" "DIR" "A Lake project whose modules the files may import."; value "out" "FILE" "Write the result as JSON to FILE as well." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* () = if parsed.positional = [] then Error (usage_fault "mend scan needs at least one PATH") else Ok () in
            let* files = Result.map_error usage_fault (Lean_files.expand parsed.positional) in
            let* scanned =
              List.fold_left
                (fun acc file ->
                  let* done_ = acc in
                  let* source = read_source file in
                  (* One Bridge for each file: what a file imports is not kept for the next. *)
                  let* bridge, _ = start_bridge ctx in
                  let found = Mend_detect.collect ~check:(file_check ctx bridge) ~file source in
                  Bridge_client.shutdown bridge;
                  let* breakages, _ = Result.map_error (mend_fault 2 ~exit:Exit_code.Failure) found in
                  Ok (done_ @ [ (file, breakages) ]))
                (Ok []) files
            in
            let every = List.concat_map snd scanned in
            let errors = List.filter Mend_breakage.has_error every in
            let document =
              Json.Obj
                [ ("schema", Json.String scan_schema);
                  ( "summary",
                    Json.Obj
                      [ ("files", Json.Int (List.length scanned)); ("broken_files", Json.Int (List.length (List.filter (fun (_, b) -> List.exists Mend_breakage.has_error b) scanned)));
                        ("breakages", Json.Int (List.length every)); ("errors", Json.Int (List.length errors));
                        ("by_class", Json.Obj (List.map (fun (name, n) -> (name, Json.Int n)) (Mend_detect.count_by_class every))) ] );
                  ( "files",
                    Json.List (List.map (fun (file, breakages) -> Json.Obj [ ("file", Json.String file); ("breakages", Json.List (List.map Mend_breakage.to_json breakages)) ]) scanned) ) ]
            in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else if not ctx.quiet then begin
              List.iter
                (fun (file, breakages) ->
                  if breakages = [] then Printf.printf "%s  %s\n" file (Term.green "nothing is broken")
                  else begin
                    Printf.printf "%s  %d breakage(s)\n" (Term.bold file) (List.length breakages);
                    let rows =
                      List.map
                        (fun (b : Mend_breakage.t) ->
                          [ string_of_int b.Mend_breakage.line; (if b.Mend_breakage.declaration = "" then "-" else b.Mend_breakage.declaration);
                            (match b.Mend_breakage.severity with "error" -> Term.coral "error" | "warning" -> Term.gold "warning" | other -> Term.dim other);
                            Mend_class.name b.Mend_breakage.class_; Text.truncate 64 b.Mend_breakage.error ])
                        breakages
                    in
                    print_string (Text.indent 2 (Term.table ([ "line"; "theorem"; "severity"; "class"; "what Lean says" ] :: rows)))
                  end)
                scanned;
              let counted = List.filter (fun (_, n) -> n > 0) (Mend_detect.count_by_class every) in
              Printf.printf "%d file(s), %d breakage(s)%s\n" (List.length scanned) (List.length every)
                (if counted = [] then "" else ": " ^ String.concat ", " (List.map (fun (name, n) -> Printf.sprintf "%d %s" n name) counted))
            end;
            Ok (if errors = [] then 0 else Exit_code.to_int Exit_code.Not_verified))) }

(* ---- mend repair ---- *)

let repair_cmd =
  { path = [ "mend"; "repair" ]; summary = "Repair a Lean file that stopped compiling, and check every repair."; synopsis = "FILE";
    details =
      "Tries the cheapest remedy first. Rules need no model: a name that is gone or deprecated is replaced by its successor, from \
       Lean's deprecation message or from a table of renames, and a suggestion of Lean is written in place of the call that asked \
       for it. A proof that still does not compile is shown to a model, which is asked for the smallest change; if that fails, \
       the prover's automation proves the statement again from nothing. An edit is kept only if Lean accepts it, and a model may \
       change a proof and never a statement. At the end the whole file is checked once more, and the report says what is left. Without \
       --write the file is not touched. The command exits with status 7 when something is left.";
    options =
      [ value "renames" "FILE" "A table of renames in TOML: a table [renames] of old names and their new names.";
        switch "no-ai" "Use the rules and the prover's automation only: ask no model.";
        switch "no-prove" "Do not prove a statement again when its proof cannot be repaired.";
        value "rounds" "N" "How often the model is asked about one proof (2).";
        value "candidates" "N" "Proofs asked for in each round (3).";
        switch "write" "Write the repaired file back.";
        value "out" "FILE" "Write the report as JSON to FILE as well.";
        value "project" "DIR" "A Lake project whose modules the file may import.";
        value "policy" "FILE" "A trust policy file, for the proofs of the prover." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "mend repair needs exactly one FILE argument") in
            let* source = read_source file in
            let* renames =
              match get parsed "renames" with
              | None -> Ok []
              | Some table -> Result.map_error (mend_fault 1 ~exit:Exit_code.Usage) (Mend_rename.load_table table)
            in
            let* rounds = number parsed "rounds" ~default:Mend_agent.default_settings.Mend_agent.rounds in
            let* candidates = number parsed "candidates" ~default:Mend_agent.default_settings.Mend_agent.candidates in
            let* policy = load_policy ctx in
            let* gateway = Gateway.create ctx.config in
            let* bridge, _ = start_bridge ctx in
            let tools =
              Mend_tools.tools ~limits:(limits ctx)
                ?gateway:(if Config.bool ctx.config "prove.use_ai" then Some gateway else None)
                ~max_tokens:(Config.int ctx.config "gateway.max_tokens") ~candidates
                ?prove:(if has parsed "no-prove" then None else Some (policy, engine_settings ctx))
                bridge
            in
            let outcome = Mend_repair.run ~renames ~settings:{ Mend_agent.rounds; candidates } tools ~file source in
            Bridge_client.shutdown bridge;
            let* report = Result.map_error (mend_fault 2 ~exit:Exit_code.Failure) outcome in
            let written = has parsed "write" && report.Mend_repair.source <> source in
            let* () = if written then Result.map_error usage_fault (Fs.write file report.Mend_repair.source) else Ok () in
            let document =
              match Mend_repair.to_json report with
              | Json.Obj members -> Json.Obj (members @ [ ("written", Json.Bool written); ("gateway", Gateway.account_to_json gateway) ])
              | other -> other
            in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else if not ctx.quiet then begin
              print_string (Mend_repair.render ~color:!Term.colour_enabled report);
              if written then print_endline (Term.dim (Printf.sprintf "wrote %s" file))
              else if report.Mend_repair.source <> source then print_endline (Term.dim "the file was not changed; --write writes the repairs back")
            end;
            Ok (if Mend_repair.clean report then 0 else Exit_code.to_int Exit_code.Not_verified))) }

(* ---- mend minimize ---- *)

let minimize_cmd =
  { path = [ "mend"; "minimize" ]; summary = "Shorten the proofs of a Lean file, keeping only what Lean accepts."; synopsis = "FILE";
    details =
      "For each theorem with a tactic proof, takes out one step at a time, and one argument of a lemma list at a time, and gives \
       the theorem with the shorter proof to Lean. A removal is kept when Lean accepts the proof, the theorem states what it \
       stated, and it rests on no axiom it did not rest on before. With --automation the whole proof is first tried as a single \
       call such as `by omega`. The result lists what was removed and the size of each proof before and after. Without --write \
       the file is not touched.";
    options =
      [ value "theorem" "NAME" "A theorem to shorten. May be given several times. Default: every theorem with a proof.";
        switch "automation" "First try to replace a whole proof by one call of automation.";
        value "max-checks" "N" "The most proofs given to Lean for one theorem (200).";
        switch "write" "Write the shorter proofs back into the file.";
        value "out" "FILE" "Write the result as JSON to FILE as well.";
        value "project" "DIR" "A Lake project whose modules the file may import." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "mend minimize needs exactly one FILE argument") in
            let* source = read_source file in
            let* max_checks = number parsed "max-checks" ~default:200 in
            let* names =
              match get_all parsed "theorem" with
              | _ :: _ as names ->
                List.fold_left
                  (fun acc name ->
                    let* done_ = acc in
                    let* decl = Result.map_error (mend_fault 3 ~exit:Exit_code.Usage) (Lean_source.find source name) in
                    Ok (done_ @ [ decl.Lean_source.name ]))
                  (Ok []) names
              | [] -> Ok (List.map (fun (d : Lean_source.decl) -> d.Lean_source.name) (List.filter (fun d -> not (Lean_source.has_sorry d)) (Lean_source.declarations source)))
            in
            let* () = if names = [] then Error (mend_fault 3 ~exit:Exit_code.Usage "the file has no theorem with a finished proof to shorten") else Ok () in
            let* bridge, _ = start_bridge ctx in
            (* Each theorem is shortened in the file as the theorems before it left it. *)
            let current = ref source in
            let results =
              List.filter_map
                (fun name ->
                  match Lean_source.find !current name with
                  | Error _ -> None
                  | Ok decl ->
                    let result = Mend_minimize.minimize ~automation:(has parsed "automation") ~max_checks ~check:(theorem_check ctx bridge decl) decl in
                    if Mend_minimize.changed result then current := Mend_edit.put_proof decl result.Mend_minimize.after;
                    Some result)
                names
            in
            Bridge_client.shutdown bridge;
            let written = has parsed "write" && !current <> source in
            let* () = if written then Result.map_error usage_fault (Fs.write file !current) else Ok () in
            let document =
              match Mend_minimize.to_json ~file results with Json.Obj members -> Json.Obj (members @ [ ("written", Json.Bool written) ]) | other -> other
            in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else if not ctx.quiet then begin
              print_string (Mend_minimize.render ~color:!Term.colour_enabled ~file results);
              if written then print_endline (Term.dim (Printf.sprintf "wrote %s" file))
              else if !current <> source then print_endline (Term.dim "the file was not changed; --write writes the shorter proofs back")
            end;
            Ok 0)) }

let all = [ scan_cmd; repair_cmd; minimize_cmd ]
