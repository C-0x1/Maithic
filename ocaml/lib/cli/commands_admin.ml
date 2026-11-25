(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Commands about the installation itself: configuration, keys, models,
    the Bridge, the benchmark runner and the reference tables. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_prover
open Cli_args
open Commands

(* ---- config ---- *)

let config_show_cmd =
  { path = [ "config"; "show" ]; summary = "Show the effective configuration and where each value came from."; synopsis = "";
    details = ""; options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            if ctx.json then Term.print_json (Config.to_json ctx.config)
            else begin
              print_string
                (Term.table
                   ([ "setting"; "value"; "source" ]
                    :: List.map
                         (fun (st : Config.setting) ->
                           let v = Config.string ctx.config st.Config.key in
                           [ st.Config.key; (if v = "" then "(empty)" else v);
                             Config.source_to_string (Config.source ctx.config st.Config.key) ])
                         Config.settings));
              Printf.printf "\nworkspace: %s\n" (Option.value (Config.workspace ctx.config) ~default:"(none found)")
            end;
            Ok 0)) }

let config_reference_cmd =
  { path = [ "config"; "reference" ]; summary = "Print the configuration reference as a Markdown table."; synopsis = "";
    details = "The table is generated from the same declarations that validate the configuration."; options = [];
    run = (fun _ -> print_string (Config.reference_markdown ()); 0) }

let config_validate_cmd =
  { path = [ "config"; "validate" ]; summary = "Validate the configuration files and environment."; synopsis = ""; details = "";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            if ctx.json then Term.print_json (Json.Obj [ ("valid", Json.Bool true) ]) else print_endline "the configuration is valid";
            Ok 0)) }

(* ---- auth ---- *)

let registry_of ctx =
  match Config.string_opt ctx.config "gateway.models" with
  | None -> Ok Model_registry.builtin
  | Some path ->
    Result.map_error
      (fun e -> Fault.make ~area:"GATEWAY" ~number:20 ~exit:Exit_code.Config ("cannot load the model registry: " ^ e))
      (Model_registry.load (Config.resolve_path ctx.config path))

let auth_status_cmd =
  { path = [ "auth"; "status" ]; summary = "Show which providers have a key configured. Never prints a key."; synopsis = "";
    details = ""; options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* registry = registry_of ctx in
            let statuses = Keys.status ~getenv:Sys.getenv_opt ~home:(Config.home_dir Sys.getenv_opt) registry in
            if ctx.json then
              Term.print_json
                (Json.List
                   (List.map
                      (fun (s : Keys.status) ->
                        Json.Obj
                          [ ("provider", Json.String s.Keys.provider); ("variable", Json.String s.Keys.variable);
                            ("configured", Json.Bool s.Keys.configured);
                            ("source", Json.String (match s.Keys.source with Some src -> Keys.source_to_string src | None -> "")) ])
                      statuses))
            else
              print_string
                (Term.table
                   ([ "provider"; "key"; "variable"; "source" ]
                    :: List.map
                         (fun (s : Keys.status) ->
                           [ s.Keys.provider; (if s.Keys.configured then Term.green s.Keys.shown else Term.gold "not set"); s.Keys.variable;
                             (match s.Keys.source with Some src -> Keys.source_to_string src | None -> "") ])
                         statuses));
            Ok 0)) }

(* Reads one line from the terminal without echoing it. *)
let read_secret prompt =
  prerr_string prompt;
  flush stderr;
  let restore =
    match Unix.tcgetattr Unix.stdin with
    | attrs ->
      Unix.tcsetattr Unix.stdin Unix.TCSANOW { attrs with Unix.c_echo = false };
      fun () -> Unix.tcsetattr Unix.stdin Unix.TCSANOW attrs; prerr_newline ()
    | exception Unix.Unix_error _ -> fun () -> ()
  in
  let line = try Some (input_line stdin) with End_of_file -> None in
  restore ();
  Option.map String.trim line

let auth_login_cmd =
  { path = [ "auth"; "login" ]; summary = "Store an API key for a provider in the private credentials file."; synopsis = "";
    details =
      "Prompts for the key without echoing it, or reads it from standard input when that is not a terminal. The key is \
       written to a file readable by its owner only, never to the project configuration.";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* registry = registry_of ctx in
            let name = Config.string ctx.config "ai.provider" in
            match Model_registry.provider registry name with
            | None -> Error (Gateway_types.error_to_fault (Gateway_types.Unknown_provider name))
            | Some p when p.Model_registry.key_env = "" -> Error (usage_fault (Printf.sprintf "provider '%s' needs no key" name))
            | Some _ ->
              (match read_secret (Printf.sprintf "API key for %s: " name) with
               | None | Some "" -> Error (usage_fault "no key was entered")
               | Some key ->
                 let home = Config.home_dir Sys.getenv_opt in
                 (match Keys.store ~home ~provider:name key with
                  | Error e -> Error (Fault.make ~area:"GATEWAY" ~number:21 ~exit:Exit_code.Failure e)
                  | Ok () ->
                    say ctx (Printf.sprintf "stored the key for %s (%s) in %s" name (Keys.redact key) (Keys.credentials_path ~home));
                    Ok 0)))) }

let auth_logout_cmd =
  { path = [ "auth"; "logout" ]; summary = "Remove the stored key of a provider."; synopsis = ""; details = ""; options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let name = Config.string ctx.config "ai.provider" in
            let home = Config.home_dir Sys.getenv_opt in
            match Keys.store ~home ~provider:name "" with
            | Error e -> Error (Fault.make ~area:"GATEWAY" ~number:21 ~exit:Exit_code.Failure e)
            | Ok () -> say ctx (Printf.sprintf "removed the stored key for %s" name); Ok 0)) }

let auth_check_cmd =
  { path = [ "auth"; "check" ]; summary = "Confirm that the configured key works, with one small provider call."; synopsis = "";
    details = "Sends a request of a few tokens. In replay and mock mode no provider is called."; options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* gateway = Gateway.create ctx.config in
            let request =
              Gateway_types.request ~purpose:"auth.check" ~system:"Reply with the single word: ok" ~max_tokens:16
                [ Gateway_types.user "ok?" ]
            in
            match Gateway.complete gateway request with
            | Error e -> Error (Gateway_types.error_to_fault e)
            | Ok response ->
              if ctx.json then Term.print_json (Gateway_types.response_to_json response)
              else
                Printf.printf "%s answered through model %s (%d input, %d output tokens)\n" response.Gateway_types.provider
                  response.Gateway_types.model response.Gateway_types.usage.Gateway_types.input_tokens
                  response.Gateway_types.usage.Gateway_types.output_tokens;
              Ok 0)) }

(* ---- models ---- *)

let models_cmd =
  { path = [ "models" ]; summary = "List the providers and the model of each quality tier."; synopsis = ""; details = "";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* registry = registry_of ctx in
            if ctx.json then Term.print_json (Model_registry.to_json registry)
            else
              print_string
                (Term.table
                   ([ "provider"; "api"; "fast"; "balanced"; "best"; "endpoint" ]
                    :: List.map
                         (fun (p : Model_registry.provider) ->
                           let tier t = Option.value (List.assoc_opt t p.Model_registry.tiers) ~default:"-" in
                           [ p.Model_registry.name; Model_registry.api_to_string p.Model_registry.api; tier "fast"; tier "balanced";
                             tier "best"; p.Model_registry.endpoint ])
                         registry.Model_registry.providers));
            Ok 0)) }

(* ---- bridge ---- *)

let bridge_selftest_cmd =
  { path = [ "bridge"; "selftest" ]; summary = "Start the Bridge and run its self-test."; synopsis = ""; details = "";
    options = [ value "project" "DIR" "A Lake project to run the Bridge in." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* bridge, located = start_bridge ctx in
            let outcome = Bridge_client.self_test bridge in
            let h = Bridge_client.handshake bridge in
            Bridge_client.shutdown bridge;
            let* healthy, checks = Result.map_error Bridge_error.to_fault outcome in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ("healthy", Json.Bool healthy); ("bridge", Json.String h.Bridge_types.bridge); ("lean", Json.String h.Bridge_types.lean);
                     ("path", Json.String located.Bridge_locate.path);
                     ("checks", Json.List (List.map (fun (n, ok) -> Json.Obj [ ("name", Json.String n); ("passed", Json.Bool ok) ]) checks)) ])
            else begin
              Printf.printf "maithic-bridge %s, Lean %s (%s)\n" h.Bridge_types.bridge h.Bridge_types.lean located.Bridge_locate.path;
              List.iter (fun (n, ok) -> Printf.printf "  %s  %s\n" (if ok then Term.green "ok  " else Term.coral "FAIL") n) checks
            end;
            Ok (if healthy then 0 else Exit_code.to_int Exit_code.Toolchain))) }

(* ---- bench ---- *)

let find_upward ~cwd relative =
  let rec up dir depth =
    let candidate = Fs.(dir / relative) in
    if Fs.exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir || depth > 32 then None else up parent (depth + 1)
  in
  up cwd 0

(* A proof for a table cell: on one line, with the long name of the problem
   shortened to its last part. *)
let display_proof decl proof =
  let short = match List.rev (String.split_on_char '.' decl) with last :: _ -> last | [] -> decl in
  let flat = Attempts.normalize proof in
  if decl = "" then flat else Text.replace_all ~sub:decl ~by:short flat

let bench_run_cmd =
  { path = [ "bench"; "run" ]; summary = "Run the prover on a benchmark suite and report per-problem results."; synopsis = "";
    details =
      "Each problem of the manifest is a proposition defined in the MaithicBench library. The runner states it as a theorem, \
       proves it and verifies the result exactly as `maithic prove` does. The Bridge runs inside the Lean project so that it \
       can import the suite.";
    options =
      [ value "manifest" "FILE" "The suite manifest (default: bench/suites/manifest.json at or above the current directory).";
        value "project" "DIR" "The Lake project that contains MaithicBench (default: lean/ at or above the current directory).";
        value "suite" "NAME" "Run only the problems of one suite.";
        value "out" "FILE" "Also write the results as JSON to this file.";
        switch "no-ai" "Use only the built-in automation portfolio.";
        value "rounds" "N" "Model rounds per problem."; value "policy" "FILE" "A trust policy file." ];
    run =
      (fun parsed ->
        let cwd = Sys.getcwd () in
        let parsed =
          match get parsed "project", find_upward ~cwd "lean/lakefile.toml" with
          | None, Some lakefile -> { parsed with values = ("project", Filename.dirname lakefile) :: parsed.values }
          | _ -> parsed
        in
        guarded parsed (fun ctx ->
            let* manifest_path =
              match get parsed "manifest", find_upward ~cwd "bench/suites/manifest.json" with
              | Some path, _ | None, Some path -> Ok path
              | None, None -> Error (usage_fault "no manifest found; pass --manifest FILE")
            in
            let* manifest =
              Result.map_error
                (fun e -> Fault.make ~area:"BENCH" ~number:1 ~exit:Exit_code.Usage ("cannot read the manifest: " ^ e))
                (Result.bind (Fs.read manifest_path) Json.parse)
            in
            let problems =
              Json.list_or_empty "problems" manifest
              |> List.filter (fun p -> match get parsed "suite" with Some s -> Json.string_or "" "suite" p = s | None -> true)
            in
            let* () = if problems = [] then Error (usage_fault "the manifest lists no matching problem") else Ok () in
            let* policy = load_policy ctx in
            let* gateway = Gateway.create ctx.config in
            let* bridge, located = start_bridge ctx in
            let settings = engine_settings ctx in
            let run_problem p =
              let id = Json.string_or "" "id" p and decl = Json.string_or "" "decl" p in
              let source = Printf.sprintf "import MaithicBench\n\ntheorem maithic_solution : %s := sorry\n" decl in
              let result = Engine.prove ~recheck:(recheck_with ctx located) ~gateway ~bridge ~policy settings ~source "maithic_solution" in
              (id, p, result)
            in
            let rows = List.map run_problem problems in
            Bridge_client.shutdown bridge;
            let results = List.filter_map (fun (_, _, r) -> Result.to_option r) rows in
            let verified = List.length (List.filter (fun (r : Engine.result) -> r.Engine.status = Status.Verified) results) in
            let report =
              Json.Obj
                [ ("schema", Json.String "maithic.bench.run/1"); ("manifest", Json.String (Filename.basename manifest_path));
                  ("mode", Json.String (Gateway.mode_to_string (Gateway.mode gateway)));
                  ("use_ai", Json.Bool settings.Engine.use_ai); ("policy", Json.String policy.Maithic_ledger.Trust_policy.name);
                  ("total", Json.Int (List.length rows)); ("verified", Json.Int verified);
                  ( "problems",
                    Json.List
                      (List.map
                         (fun (id, p, result) ->
                           Json.Obj
                             ([ ("id", Json.String id); ("suite", Json.String (Json.string_or "" "suite" p));
                                ("tags", Json.List (Json.list_or_empty "tags" p));
                                ("difficulty", Json.Int (Json.int_or 1 "difficulty" p)) ]
                              @
                              match result with
                              | Ok (r : Engine.result) ->
                                [ ("status", Status.to_json r.Engine.status);
                                  ("proposer", Json.String (Option.value r.Engine.proposer ~default:""));
                                  ("proof", Json.String (Option.value r.Engine.proof ~default:""));
                                  ("attempts", Json.Int (List.length r.Engine.attempts)); ("rounds", Json.Int r.Engine.rounds_used);
                                  ("elapsed_ms", Json.Int r.Engine.elapsed_ms) ]
                              | Error fault -> [ ("status", Status.to_json Status.Failed); ("error", Json.String fault.Fault.code) ]))
                         rows) );
                  ("gateway", Gateway.account_to_json gateway) ]
            in
            Option.iter (fun path -> ignore (Fs.write path (Json.pretty report ^ "\n"))) (get parsed "out");
            if ctx.json then Term.print_json report
            else if ctx.quiet then Printf.printf "%d of %d verified\n" verified (List.length rows)
            else begin
              print_string
                (Term.table
                   ([ "problem"; "status"; "by"; "proof"; "ms" ]
                    :: List.map
                         (fun (id, p, result) ->
                           match result with
                           | Ok (r : Engine.result) ->
                             [ id; Term.status r.Engine.status; Option.value r.Engine.proposer ~default:"-";
                               Text.truncate 48 (display_proof (Json.string_or "" "decl" p) (Option.value r.Engine.proof ~default:"-"));
                               string_of_int r.Engine.elapsed_ms ]
                           | Error fault -> [ id; Term.status Status.Failed; "-"; fault.Fault.code; "-" ])
                         rows));
              Printf.printf "%d of %d verified\n" verified (List.length rows)
            end;
            Ok (if verified = List.length rows then 0 else Exit_code.to_int Exit_code.Not_verified))) }

(* ---- reference tables ---- *)

let exit_codes_cmd =
  { path = [ "help"; "exit-codes" ]; summary = "Print the table of exit codes."; synopsis = ""; details = ""; options = [];
    run =
      (fun parsed ->
        if has parsed "json" then
          Term.print_json
            (Json.List
               (List.map
                  (fun c -> Json.Obj [ ("code", Json.Int (Exit_code.to_int c)); ("meaning", Json.String (Exit_code.describe c)) ])
                  Exit_code.all))
        else
          print_string
            (Term.table
               ([ "code"; "meaning" ] :: List.map (fun c -> [ string_of_int (Exit_code.to_int c); Exit_code.describe c ]) Exit_code.all));
        0) }

let status_cmd =
  { path = [ "help"; "status" ]; summary = "Print the status vocabulary."; synopsis = ""; details = ""; options = [];
    run =
      (fun parsed ->
        if has parsed "json" then
          Term.print_json
            (Json.List
               (List.map (fun s -> Json.Obj [ ("status", Status.to_json s); ("meaning", Json.String (Status.describe s)) ]) Status.all))
        else print_string (Term.table ([ "status"; "meaning" ] :: List.map (fun s -> [ Status.to_string s; Status.describe s ]) Status.all));
        0) }

let all =
  [ Commands.prove_cmd; Commands.tree_cmd; Commands.check_cmd; bench_run_cmd ] @ Commands_bench.all @ [ Commands_oracle.oracle_list_cmd; Commands_oracle.oracle_solve_cmd ]
  @ Commands_intel.all @ Commands_corpus.all @ Commands_review.all @ Commands_refute.all @ Commands_guide.all @ Commands_mend.all @ Commands_formalize.all @ Commands_council.all @ Commands_blueprint.all @ Commands_search.all @ Commands_nnv.all @ Commands_algo.all
  @ [
    Commands.doctor_cmd; config_show_cmd; config_reference_cmd;
    config_validate_cmd; auth_status_cmd; auth_login_cmd; auth_logout_cmd; auth_check_cmd; models_cmd; bridge_selftest_cmd;
    exit_codes_cmd; status_cmd; Commands.version_cmd ]
