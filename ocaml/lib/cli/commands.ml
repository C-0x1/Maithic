(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The commands of [maithic]. Each one parses its options, does its work
    through the libraries and returns an exit code from the shared table. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_ledger
open Maithic_prover
open Maithic_oracle
open Maithic_guide
open Cli_args

let program = "maithic"
let tagline = "AI proposes. The kernel decides."

type context = { parsed : parsed; json : bool; quiet : bool; verbose : bool; config : Config.t }

let ( let* ) = Result.bind

(* Loads the configuration with the global options applied as the top layer. *)
let context parsed =
  Term.init ~no_color:(has parsed "no-color" || has parsed "json");
  let flag key name = match get parsed name with Some v -> [ (key, v) ] | None -> [] in
  let flags =
    flag "ai.provider" "provider" @ flag "ai.model" "model" @ flag "ai.tier" "tier" @ flag "gateway.mode" "mode"
    @ flag "prove.rounds" "rounds" @ flag "prove.candidates" "candidates" @ flag "bridge.project" "project"
    @ flag "ledger.policy" "policy" @ flag "bridge.max_heartbeats" "heartbeats"
    @ flag "prove.search_expansions" "search-expansions" @ flag "guide.model" "guide"
    @ (if has parsed "search" || get parsed "guide" <> None then [ ("prove.search", "true") ] else [])
    @ (if has parsed "oracle" then [ ("prove.oracle", "true") ] else [])
    @ if has parsed "no-ai" then [ ("prove.use_ai", "false") ] else []
  in
  let* files =
    match get parsed "config" with
    | None -> Ok None
    | Some file ->
      if Fs.exists file then Ok (Some [ file ])
      else Error (Fault.make ~area:"CONFIG" ~number:4 ~exit:Exit_code.Config ("configuration file not found: " ^ file))
  in
  let* config = Config.load ~flags ?files () in
  Ok { parsed; json = has parsed "json"; quiet = has parsed "quiet"; verbose = has parsed "verbose"; config }

(* Runs a command body that may fail with a fault. *)
let guarded parsed body =
  match Result.bind (context parsed) body with
  | Ok code -> code
  | Error fault -> Term.fault ~json:(has parsed "json") fault

let usage_fault message = Fault.make ~area:"CLI" ~number:1 ~exit:Exit_code.Usage message
let say ctx text = if not (ctx.json || ctx.quiet) then print_endline text

let locate_bridge ctx =
  Result.map_error Bridge_error.to_fault
    (Bridge_locate.locate
       ~command:(Config.string ctx.config "bridge.command")
       ~project:(Config.resolve_path ctx.config (Config.string ctx.config "bridge.project"))
       ~lake:(Config.string ctx.config "bridge.lake") ~cwd:(Sys.getcwd ()) ())

let start_bridge ctx =
  let* located = locate_bridge ctx in
  let timeout = Config.float ctx.config "bridge.timeout_s" in
  let* bridge =
    Result.map_error Bridge_error.to_fault (Bridge_client.start ?cwd:located.Bridge_locate.cwd ~timeout located.Bridge_locate.argv)
  in
  (* A project built with another Lean version cannot be loaded by this Bridge. *)
  let bridge_lean = (Bridge_client.handshake bridge).Bridge_types.lean in
  match Option.bind located.Bridge_locate.cwd Bridge_locate.toolchain_of_project with
  | Some project_lean when project_lean <> bridge_lean ->
    Bridge_client.shutdown bridge;
    Error
      (Bridge_error.to_fault
         (Bridge_error.Incompatible
            (Printf.sprintf "the project pins Lean %s but the Bridge was built with Lean %s" project_lean bridge_lean)))
  | _ -> Ok (bridge, located)

let load_policy ctx =
  match Config.string_opt ctx.config "ledger.policy" with
  | None -> Ok Trust_policy.default
  | Some path ->
    Result.map_error
      (fun e -> Fault.make ~area:"LEDGER" ~number:1 ~exit:Exit_code.Config ("cannot load the trust policy: " ^ e))
      (Trust_policy.load (Config.resolve_path ctx.config path))

let limits ctx = { Bridge_types.no_limits with Bridge_types.max_heartbeats = Config.int ctx.config "bridge.max_heartbeats" }

let read_source file =
  Result.map_error (fun e -> Fault.make ~area:"CLI" ~number:2 ~exit:Exit_code.Usage ("cannot read " ^ file ^ ": " ^ e)) (Fs.read file)

(* ---- version ---- *)

let version_cmd =
  { path = [ "version" ]; summary = "Print the version and the versions of its components."; synopsis = ""; details = "";
    options = [];
    run =
      (fun parsed ->
        let fields =
          [ ("maithic", Version.version); ("bridge_protocol", string_of_int Version.bridge_protocol);
            ("lean_toolchain", Version.lean_toolchain); ("ocaml", Sys.ocaml_version) ]
        in
        if has parsed "json" then Term.print_json (Json.Obj (List.map (fun (k, v) -> (k, Json.String v)) fields))
        else begin
          Printf.printf "%s %s\n" program Version.version;
          List.iter (fun (k, v) -> if k <> "maithic" then Printf.printf "  %s %s\n" (Text.pad_right 16 k) v) fields
        end;
        0) }

(* ---- doctor ---- *)

let tool_version prog args =
  match Proc.run ~timeout:20.0 prog args with
  | Ok out when out.Proc.status = 0 -> Some (String.trim (List.hd (String.split_on_char '\n' (out.Proc.stdout ^ out.Proc.stderr))))
  | _ -> None

let doctor_cmd =
  { path = [ "doctor" ]; summary = "Check toolchains, the Bridge, the provider and keys, and print remedies."; synopsis = "";
    details = "Exits with status 4 when a required component is missing. Optional components only produce a note.";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let tool name args required remedy =
              match Proc.find_in_path name with
              | None -> (name, false, required, remedy)
              | Some path -> (name, true, required, Text.truncate_words 72 (Option.value (tool_version name args) ~default:path))
            in
            let bridge_row =
              match start_bridge ctx with
              | Error fault -> ("maithic-bridge", false, true, fault.Fault.message)
              | Ok (bridge, located) ->
                let h = Bridge_client.handshake bridge in
                let healthy = match Bridge_client.self_test bridge with Ok (healthy, _) -> healthy | Error _ -> false in
                Bridge_client.shutdown bridge;
                ( "maithic-bridge", healthy, true,
                  Printf.sprintf "%s, protocol %d, Lean %s, self-test %s (%s)" h.Bridge_types.bridge h.Bridge_types.protocol
                    h.Bridge_types.lean (if healthy then "healthy" else "FAILED") located.Bridge_locate.path )
            in
            let rows =
              [ tool "lean" [ "--version" ] true "Install Lean 4 with elan: https://lean-lang.org/install/";
                tool "lake" [ "--version" ] true "Lake ships with Lean; install Lean 4 with elan.";
                bridge_row;
                tool "curl" [ "--version" ] false "Needed only for live provider calls.";
                tool "julia" [ "--version" ] false "Needed only for the numeric services.";
                tool "Rscript" [ "--version" ] false "Needed only for statistics and reports." ]
            in
            let gateway = Gateway.create ctx.config in
            let ai_rows =
              match gateway with
              | Error fault -> [ ("gateway", fault.Fault.message) ]
              | Ok g ->
                [ ("gateway mode", Gateway.mode_to_string (Gateway.mode g)); ("provider", Gateway.provider_name g);
                  ("model", (match Gateway.model_id g with Ok id -> id | Error e -> Gateway_types.error_to_string e));
                  ( "ai ready",
                    match Gateway.ready g with
                    | Ok () -> "yes"
                    | Error e -> "no: " ^ Gateway_types.error_to_string e ^ " (proving still works with built-in automation)" ) ]
            in
            let isolation = [ ("environment scrubbing", "active: Lean workers never see API keys"); ("candidate screening", "active") ] in
            let missing_required = List.exists (fun (_, ok, required, _) -> required && not ok) rows in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ( "components",
                       Json.List
                         (List.map
                            (fun (name, ok, required, detail) ->
                              Json.Obj
                                [ ("name", Json.String name); ("ok", Json.Bool ok); ("required", Json.Bool required);
                                  ("detail", Json.String detail) ])
                            rows) );
                     ("ai", Json.Obj (List.map (fun (k, v) -> (k, Json.String v)) ai_rows));
                     ("isolation", Json.Obj (List.map (fun (k, v) -> (k, Json.String v)) isolation));
                     ("healthy", Json.Bool (not missing_required)) ])
            else begin
              print_string
                (Term.table
                   ([ "component"; "status"; "detail" ]
                    :: List.map
                         (fun (name, ok, required, detail) ->
                           [ name; (if ok then Term.green "ok" else if required then Term.coral "missing" else Term.gold "absent"); detail ])
                         rows));
              print_newline ();
              List.iter (fun (k, v) -> Printf.printf "%s %s\n" (Text.pad_right 22 k) v) (ai_rows @ isolation)
            end;
            Ok (if missing_required then Exit_code.to_int Exit_code.Toolchain else 0))) }

(* ---- prove ---- *)

let prove_options =
  [ value "theorem" "NAME" "The theorem to prove. May be given several times. Default: every theorem whose proof contains sorry.";
    switch "no-ai" "Use only the built-in automation portfolio.";
    switch "oracle" "Ask the oracles for a certificate when the portfolio fails on an arithmetic goal.";
    switch "search" "Search one tactic at a time when the portfolio fails, before any model is asked.";
    value "search-expansions" "N" "Goals the step-wise search may expand for one theorem.";
    value "search-trees" "DIR" "Write the record of every step-wise search, and a picture of it, into DIR.";
    value "guide" "MODEL" "A trained tactic model that orders the candidates of the step-wise search. Implies --search.";
    value "rounds" "N" "Model rounds per theorem.";
    value "candidates" "N" "Proofs requested from the model in each round.";
    value "project" "DIR" "A Lake project whose modules the file may import.";
    value "policy" "FILE" "A trust policy file.";
    value "heartbeats" "N" "Step budget of one elaboration, in thousands of heartbeats.";
    switch "write" "Write each verified proof back into the file." ]

let print_result ctx (r : Engine.result) =
  let via = match r.Engine.proposer with Some p -> p | None -> "-" in
  Printf.printf "%s  %s  (%s, %d attempt(s), %d ms)\n" (Term.bold r.Engine.theorem) (Term.status r.Engine.status) via
    (List.length r.Engine.attempts) r.Engine.elapsed_ms;
  Printf.printf "  statement: %s\n" r.Engine.statement;
  (match r.Engine.proof with Some proof -> Printf.printf "  proof:     %s\n" (String.concat "\n             " (String.split_on_char '\n' proof)) | None -> ());
  if r.Engine.proof <> None then
    Printf.printf "  axioms:    %s\n" (if r.Engine.axioms = [] then "none" else String.concat ", " r.Engine.axioms);
  List.iter (fun reason -> Printf.printf "  reason:    %s\n" reason) r.Engine.decision.Trust_policy.reasons;
  if r.Engine.ai_note <> "" then Printf.printf "  note:      %s\n" r.Engine.ai_note;
  if ctx.verbose then
    List.iter
      (fun (a : Attempts.attempt) ->
        Printf.printf "    %s round %d %s: %s%s\n"
          (if a.Attempts.accepted then Term.green "ok  " else Term.dim "fail")
          a.Attempts.round a.Attempts.proposer
          (Text.truncate_words 70 (Attempts.normalize a.Attempts.proof))
          (match a.Attempts.error_class with Some c -> Term.dim (" [" ^ Attempts.class_to_string c ^ "]") | None -> ""))
      r.Engine.attempts

(* ---- oracles ---- *)

let oracle_fault number ~exit message = Fault.make ~area:"ORACLE" ~number ~exit message

let oracle_registry ctx =
  let project = Config.string ctx.config "oracle.project" in
  Result.map Oracle_registry.builtin
    (Result.map_error
       (oracle_fault 1 ~exit:Exit_code.Toolchain)
       (Oracle_registry.locate ~julia:(Config.string ctx.config "oracle.julia")
          ~project:(if project = "" then "" else Config.resolve_path ctx.config project)
          ~cwd:(Sys.getcwd ()) ()))

(* The advisor that `prove --oracle` adds: the goal of the theorem is exported and sent to the
   oracles, and the scripts of a proved answer are returned as proofs to try. Whatever goes wrong
   on the way, the advisor has nothing to say, and the prover goes on without it. *)
let oracle_advisor ctx bridge : Engine.advisor =
  ( "oracle",
    fun ~env (decl : Lean_source.decl) ->
      match oracle_registry ctx with
      | Error _ -> []
      | Ok registry ->
        (match Bridge_client.start_proof ~limits:(limits ctx) bridge ~env (`Statement decl.Lean_source.header) with
         | Ok (_, ({ Bridge_types.goals = [ goal ]; _ } as opened) :: _) ->
           ignore (Bridge_client.release_state bridge opened.Bridge_types.state);
           (match Oracle_export.export goal with
            | Error _ -> []
            | Ok exported ->
              (match Oracle_registry.solve ~timeout:(Config.float ctx.config "oracle.timeout_s") registry exported.Oracle_export.goal with
               | Ok { Oracle_outcome.outcome = Oracle_outcome.Proved { lean = Some lean; _ }; _ } ->
                 List.map (fun script -> Lean_search.script_to_proof (String.split_on_char '\n' script)) lean.Oracle_outcome.scripts
               | _ -> []))
         | _ -> []) )

(* ---- the guide ---- *)

let guide_fault number ~exit message = Fault.make ~area:"GUIDE" ~number ~exit message

let guide_engine ctx =
  let project = Config.string ctx.config "guide.project" in
  Result.map_error
    (guide_fault 1 ~exit:Exit_code.Toolchain)
    (Guide_service.locate ~julia:(Config.string ctx.config "oracle.julia")
       ~project:(if project = "" then "" else Config.resolve_path ctx.config project)
       ~cwd:(Sys.getcwd ()) ())

(* The model of a run, as the user named it and where it is. A path given on the command line is
   relative to where the command was run; one from a configuration file, to the workspace. *)
let guide_model ctx =
  match Config.string ctx.config "guide.model" with
  | "" -> None
  | named -> Some (named, if Config.source ctx.config "guide.model" = Config.Flag then named else Config.resolve_path ctx.config named)

let guide_connect ctx path =
  match guide_engine ctx with
  | Error (f : Fault.t) -> Error f.Fault.message
  | Ok engine -> Guide_service.connect ~timeout:(Config.float ctx.config "guide.timeout_s") engine ~model:path

(* The guide of a search is started when a search first asks it: a run whose theorems all fall to
   the portfolio never starts Julia. Whatever goes wrong, the search goes on with its own order. *)
let guide_of_run ctx = Option.map (fun (named, path) -> (named, lazy (Guide_advice.start (fun () -> guide_connect ctx path)))) (guide_model ctx)

let guide_proposer guide : Lean_search.proposer option =
  Option.map (fun (_, advice) goal -> Guide_prior.proposer (Lazy.force advice) ~base:Lean_search.shape_proposer goal) guide

(* What a run says about its guide at the end, as a document and as a line; the service is ended. *)
let guide_report guide =
  Option.map
    (fun (named, advice) ->
      if Lazy.is_val advice then begin
        let advice = Lazy.force advice in
        Guide_advice.close advice;
        (Guide_advice.to_json ~model:named advice, Guide_advice.describe ~model:named advice)
      end
      else
        ( Json.Obj [ ("model", Json.String named); ("status", Json.String "idle"); ("asked", Json.Int 0); ("answered", Json.Int 0) ],
          Printf.sprintf "the guide %s was not needed: no theorem reached the step-wise search" named ))
    guide

let engine_settings ctx =
  { Engine.use_ai = Config.bool ctx.config "prove.use_ai";
    rounds = Config.int ctx.config "prove.rounds";
    candidates = Config.int ctx.config "prove.candidates";
    max_tokens = Config.int ctx.config "gateway.max_tokens";
    limits = limits ctx;
    search = Config.bool ctx.config "prove.search";
    search_expansions = Config.int ctx.config "prove.search_expansions" }

let recheck_with ctx (located : Bridge_locate.located) ~source name =
  Recheck.fresh ?cwd:located.Bridge_locate.cwd ~timeout:(Config.float ctx.config "bridge.timeout_s") ~limits:(limits ctx)
    ~argv:located.Bridge_locate.argv ~source name

let summary_json results =
  let count st = List.length (List.filter (fun (r : Engine.result) -> r.Engine.status = st) results) in
  Json.Obj (("total", Json.Int (List.length results)) :: List.map (fun st -> (Status.to_string st, Json.Int (count st))) Status.all)

let exit_for results =
  let worst = Status.weakest_of (List.map (fun (r : Engine.result) -> r.Engine.status) results) in
  if List.exists (fun (r : Engine.result) -> r.Engine.status = Status.Refuted) results then Exit_code.to_int Exit_code.Refuted
  else Exit_code.to_int (Exit_code.of_status worst)

let prove_cmd =
  { path = [ "prove" ]; summary = "Prove theorems of a Lean file and verify the result."; synopsis = "FILE";
    details =
      "Tries the built-in automation portfolio first. If that fails and a model is available, asks the model for proofs and \
       feeds Lean's errors back to it. A proof is reported as verified only after Lean's kernel accepted it, the trust policy \
       passed and a fresh process re-checked it.";
    options = prove_options;
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file =
              match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "prove needs exactly one FILE argument")
            in
            let* source = read_source file in
            let* names =
              match get_all parsed "theorem" with
              | _ :: _ as names -> Ok names
              | [] ->
                (match List.filter Lean_source.has_sorry (Lean_source.declarations source) with
                 | [] -> Error (usage_fault "no theorem in the file contains sorry; name one with --theorem NAME")
                 | open_ -> Ok (List.map (fun (d : Lean_source.decl) -> d.Lean_source.name) open_))
            in
            let* policy = load_policy ctx in
            let* gateway = Gateway.create ctx.config in
            let* bridge, located = start_bridge ctx in
            let settings = engine_settings ctx in
            let advisors = if Config.bool ctx.config "prove.oracle" then [ oracle_advisor ctx bridge ] else [] in
            let guide = guide_of_run ctx in
            let proposer = guide_proposer guide in
            let current = ref source in
            let run_one name =
              let on_search record =
                match get parsed "search-trees" with
                | None -> ()
                | Some dir ->
                  ignore (Fs.mkdir_p dir);
                  ignore (Fs.write Fs.(dir / (name ^ ".json")) (Json.pretty (Search_record.to_json record) ^ "\n"));
                  ignore (Fs.write Fs.(dir / (name ^ ".svg")) (Search_svg.render record))
              in
              let* result =
                Engine.prove ~recheck:(recheck_with ctx located) ~gateway ~advisors ?proposer ~on_search ~bridge ~policy settings ~source:!current
                  name
              in
              if has parsed "write" && result.Engine.status = Status.Verified then begin
                match result.Engine.proof, Lean_source.find !current name with
                | Some proof, Ok decl ->
                  current := Lean_source.with_proof decl proof;
                  ignore (Fs.write file !current)
                | _ -> ()
              end;
              if not ctx.json then print_result ctx result;
              Ok result
            in
            let outcome =
              List.fold_left
                (fun acc name ->
                  let* done_ = acc in
                  let* result = run_one name in
                  Ok (result :: done_))
                (Ok []) names
            in
            Bridge_client.shutdown bridge;
            let guided = guide_report guide in
            let* results = Result.map List.rev outcome in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   ([ ("schema", Json.String "maithic.prove.run/1"); ("file", Json.String file); ("policy", Trust_policy.to_json policy);
                      ("results", Json.List (List.map (Engine.result_to_json ~file) results));
                      ("summary", summary_json results); ("gateway", Gateway.account_to_json gateway) ]
                    @ match guided with Some (document, _) -> [ ("guide", document) ] | None -> []))
            else Option.iter (fun (_, line) -> say ctx (Term.dim line)) guided;
            Ok (exit_for results))) }

(* ---- tree ---- *)

let tree_cmd =
  { path = [ "tree" ]; summary = "Show the record of a step-wise search, and draw it."; synopsis = "FILE";
    details =
      "FILE is a record written by `prove --search-trees`. The command prints how the search ended and what it cost; it needs neither Lean nor the file the search was about.";
    options =
      [ switch "steps" "Print the search step by step: every goal that was expanded and what each tactic did on it.";
        value "svg" "OUT" "Write a picture of the search tree to OUT."; value "max-nodes" "N" "The most goals the picture shows (60)." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file =
              match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "tree needs exactly one FILE argument")
            in
            let* text = Result.map_error usage_fault (Fs.read file) in
            let* record =
              Result.map_error
                (fun why -> usage_fault (Printf.sprintf "%s is not the record of a search: %s" file why))
                (Result.bind (Json.parse text) Search_record.of_json)
            in
            let* max_nodes =
              match get parsed "max-nodes" with
              | None -> Ok 60
              | Some n ->
                (match int_of_string_opt n with
                 | Some n when n >= 1 -> Ok n
                 | _ -> Error (usage_fault "--max-nodes needs a number, one or more"))
            in
            let* () =
              match get parsed "svg" with
              | None -> Ok ()
              | Some out -> Result.map_error usage_fault (Fs.write out (Search_svg.render ~max_nodes record))
            in
            if ctx.json then Term.print_json (Search_record.to_json record)
            else begin
              if record.Search_record.statement <> "" then print_endline record.Search_record.statement;
              print_endline (Search_record.summary record);
              (match record.Search_record.script with
               | Some lines -> print_endline (Text.indent 2 (Lean_search.script_to_proof lines))
               | None -> ());
              if has parsed "steps" then print_string (Search_record.steps_text record)
            end;
            Ok 0)) }

(* ---- check ---- *)

let check_cmd =
  { path = [ "check" ]; summary = "Check a Lean file and report the status of each theorem."; synopsis = "FILE";
    details =
      "Elaborates the file from its imports in a new Bridge process and audits every theorem: verified when the kernel accepted \
       it and it rests only on axioms the trust policy allows.";
    options =
      [ value "project" "DIR" "A Lake project whose modules the file may import."; value "policy" "FILE" "A trust policy file.";
        value "heartbeats" "N" "Step budget of one elaboration, in thousands of heartbeats." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file =
              match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "check needs exactly one FILE argument")
            in
            let* source = read_source file in
            let* policy = load_policy ctx in
            let* bridge, _ = start_bridge ctx in
            let checked = Bridge_client.check_file ~limits:(limits ctx) bridge source in
            Bridge_client.shutdown bridge;
            let* report, _ = Result.map_error Bridge_error.to_fault checked in
            let theorems = List.filter (fun (d : Bridge_types.decl) -> d.Bridge_types.kind = "theorem") report.Bridge_types.decls in
            let decisions =
              List.map
                (fun (d : Bridge_types.decl) ->
                  ( d,
                    Trust_policy.decide policy
                      { Trust_policy.declaration = d.Bridge_types.name; found = d.Bridge_types.found; kind = d.Bridge_types.kind;
                        axioms = d.Bridge_types.axioms; errors = 0; rechecked = true } ))
                theorems
            in
            let verified = List.length (List.filter (fun (_, dec) -> dec.Trust_policy.status = Status.Verified) decisions) in
            let clean = report.Bridge_types.ok && verified = List.length decisions in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ("schema", Json.String "maithic.check.report/1"); ("file", Json.String file);
                     ("ok", Json.Bool clean); ("errors", Json.Int report.Bridge_types.errors);
                     ("warnings", Json.Int report.Bridge_types.warnings);
                     ("declarations", Json.Int (List.length report.Bridge_types.decls));
                     ( "theorems",
                       Json.List
                         (List.map
                            (fun ((d : Bridge_types.decl), dec) ->
                              Json.Obj
                                [ ("name", Json.String d.Bridge_types.name); ("statement", Json.String d.Bridge_types.type_);
                                  ("status", Status.to_json dec.Trust_policy.status);
                                  ("axioms", Json.List (List.map (fun a -> Json.String a) d.Bridge_types.axioms));
                                  ("reasons", Json.List (List.map (fun r -> Json.String r) dec.Trust_policy.reasons)) ])
                            decisions) );
                     ("diagnostics", Json.List (List.map Bridge_types.diagnostic_to_json report.Bridge_types.diagnostics)) ])
            else begin
              if decisions <> [] then
                print_string
                  (Term.table
                     ([ "theorem"; "status"; "axioms or reason" ]
                      :: List.map
                           (fun ((d : Bridge_types.decl), dec) ->
                             [ d.Bridge_types.name; Term.status dec.Trust_policy.status;
                               (match dec.Trust_policy.reasons with
                                | [] -> if d.Bridge_types.axioms = [] then "no axioms" else String.concat ", " d.Bridge_types.axioms
                                | reasons -> String.concat "; " reasons) ])
                           decisions));
              List.iter
                (fun (d : Bridge_types.diagnostic) ->
                  if d.Bridge_types.severity <> "info" then
                    Printf.printf "%s:%d:%d: %s: %s\n" file d.Bridge_types.line d.Bridge_types.column d.Bridge_types.severity
                      (List.hd (String.split_on_char '\n' d.Bridge_types.text)))
                report.Bridge_types.diagnostics;
              Printf.printf "%d of %d theorem(s) verified, %d error(s), %d warning(s), %d declaration(s) in %d ms\n" verified
                (List.length decisions) report.Bridge_types.errors report.Bridge_types.warnings
                (List.length report.Bridge_types.decls) report.Bridge_types.elapsed_ms
            end;
            Ok (if clean then 0 else Exit_code.to_int Exit_code.Not_verified))) }
