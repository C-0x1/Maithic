(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [oracle] commands, and the hand-off of certificates to the prover. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_prover
open Maithic_oracle
open Cli_args
open Commands

let kinds_text (module O : Oracle_registry.ORACLE) = String.concat ", " (List.map Oracle_goal.kind_to_string O.kinds)
let domains_text (module O : Oracle_registry.ORACLE) = String.concat ", " (List.map Oracle_goal.domain_to_string O.domains)

let oracle_list_cmd =
  { path = [ "oracle"; "list" ]; summary = "List the oracles, the goals each takes and what it answers with."; synopsis = "";
    details = "An oracle computes: it answers a goal with a certificate, with an exact counterexample, or with the reason it does not know.";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* registry = oracle_registry ctx in
            let oracles = Oracle_registry.all registry in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ( "oracles",
                       Json.List
                         (List.map
                            (fun (module O : Oracle_registry.ORACLE) ->
                              Json.Obj
                                [ ("name", Json.String O.name); ("kinds", Json.List (List.map (fun k -> Json.String (Oracle_goal.kind_to_string k)) O.kinds));
                                  ("domains", Json.List (List.map (fun d -> Json.String (Oracle_goal.domain_to_string d)) O.domains));
                                  ("description", Json.String O.description) ])
                            oracles) ) ])
            else
              print_string
                (Term.table
                   ([ "oracle"; "goals"; "numbers"; "what it does" ]
                    :: List.map (fun ((module O : Oracle_registry.ORACLE) as o) -> [ O.name; kinds_text o; domains_text o; O.description ]) oracles));
            Ok 0)) }

(* The goal of a theorem of a Lean file, exported for the oracles. *)
let goal_of_theorem ctx file name =
  let* source = read_source file in
  let* decl =
    Result.map_error (fun e -> Fault.make ~area:"PROVE" ~number:1 ~exit:Exit_code.Usage e) (Lean_source.find source name)
  in
  let* bridge, _ = start_bridge ctx in
  let bridge_fault = Bridge_error.to_fault in
  let result =
    let* report, env = Result.map_error bridge_fault (Bridge_client.check_file ~keep:true ~limits:(limits ctx) bridge decl.Lean_source.prelude) in
    match env with
    | Some env when report.Bridge_types.ok ->
      let* _, opened = Result.map_error bridge_fault (Bridge_client.start_proof ~limits:(limits ctx) bridge ~env (`Statement decl.Lean_source.header)) in
      (match opened with
       | { Bridge_types.goals = [ goal ]; _ } :: _ ->
         Result.map_error (fun e -> oracle_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "theorem '%s' cannot be sent to an oracle: %s" name e))
           (Oracle_export.export goal)
       | _ -> Error (oracle_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "the statement of '%s' does not open a single goal" name)))
    | _ -> Error (Fault.make ~area:"PROVE" ~number:2 ~exit:Exit_code.Failure (Printf.sprintf "the file has errors before theorem '%s'" name))
  in
  Bridge_client.shutdown bridge;
  result

let print_result ctx (result : Oracle_outcome.result) skipped =
  if ctx.json then Term.print_json (Oracle_outcome.result_to_json result)
  else begin
    let goal = result.Oracle_outcome.goal in
    Printf.printf "%s  %s  (%s over %s, %d ms)\n" result.Oracle_outcome.oracle
      (match result.Oracle_outcome.outcome with
       | Oracle_outcome.Proved _ -> Term.green "proved"
       | Oracle_outcome.Refuted _ -> Term.coral "refuted"
       | Oracle_outcome.Unknown _ -> Term.dim "unknown")
      (Oracle_goal.kind_to_string goal.Oracle_goal.kind)
      (Oracle_goal.domain_to_string goal.Oracle_goal.domain)
      (int_of_float (result.Oracle_outcome.seconds *. 1000.0));
    (match result.Oracle_outcome.outcome with
     | Oracle_outcome.Proved { kind; claim; certificate; lean } ->
       Printf.printf "  claim:        %s\n  certificate:  %s, %s\n" claim kind (String.sub (Oracle_outcome.certificate_id certificate) 0 16);
       (match lean with
        | Some { Oracle_outcome.scripts = first :: _; _ } ->
          Printf.printf "  in Lean:      %s\n" (String.concat "\n                " (String.split_on_char '\n' first))
        | _ -> print_endline "  in Lean:      no checker for this kind of certificate yet")
     | Oracle_outcome.Refuted model ->
       Printf.printf "  fails at:     %s\n" (String.concat ", " (List.map (fun (name, value) -> name ^ " = " ^ value) model))
     | Oracle_outcome.Unknown reason -> Printf.printf "  reason:       %s\n" reason);
    List.iter (fun (name, reason) -> Printf.printf "  left out:     %s (%s)\n" name reason) skipped
  end

let oracle_solve_cmd =
  { path = [ "oracle"; "solve" ]; summary = "Send a goal to an oracle and print its answer."; synopsis = "FILE";
    details =
      "FILE is a goal document (schema maithic.oracle.goal/1), or a Lean file together with --theorem NAME, whose goal is then \
       exported: the integer or rational variables, the hypotheses that are comparisons, and the target. The answer is proved \
       with a certificate, refuted with an exact counterexample, or unknown with the reason. An answer is a claim of the \
       oracle; `maithic prove --oracle` has Lean check it.";
    options =
      [ value "theorem" "NAME" "A theorem of a Lean file, whose goal is exported."; value "oracle" "NAME" "Ask this oracle instead of the first that takes the goal.";
        value "project" "DIR" "A Lake project whose modules the Lean file may import." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "oracle solve needs exactly one FILE argument") in
            let* goal, skipped =
              match get parsed "theorem" with
              | Some name -> Result.map (fun (e : Oracle_export.exported) -> (e.Oracle_export.goal, e.Oracle_export.skipped)) (goal_of_theorem ctx file name)
              | None ->
                let* text = read_source file in
                let* json = Result.map_error (fun e -> oracle_fault 2 ~exit:Exit_code.Usage (file ^ " is not JSON: " ^ e)) (Json.parse text) in
                Result.map (fun g -> (g, [])) (Result.map_error (fun e -> oracle_fault 2 ~exit:Exit_code.Usage (file ^ ": " ^ e)) (Oracle_goal.of_json json))
            in
            let* registry = oracle_registry ctx in
            let* registry =
              match get parsed "oracle" with
              | None -> Ok registry
              | Some name ->
                (match Oracle_registry.find registry name with
                 | Some o -> Ok (Oracle_registry.create [ o ])
                 | None ->
                   Error (oracle_fault 3 ~exit:Exit_code.Usage (Printf.sprintf "there is no oracle '%s'; the oracles are %s" name (String.concat ", " (Oracle_registry.names registry)))))
            in
            let* result =
              Result.map_error (oracle_fault 4 ~exit:Exit_code.Failure)
                (Oracle_registry.solve ~timeout:(Config.float ctx.config "oracle.timeout_s") registry goal)
            in
            print_result ctx result skipped;
            Ok
              (match result.Oracle_outcome.outcome with
               | Oracle_outcome.Proved _ -> 0
               | Oracle_outcome.Refuted _ -> Exit_code.to_int Exit_code.Refuted
               | Oracle_outcome.Unknown _ -> Exit_code.to_int Exit_code.Not_verified))) }
