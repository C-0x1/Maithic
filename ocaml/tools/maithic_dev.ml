(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* maithic-dev: the repository's own gates, run by `make hygiene` and by CI.

     maithic-dev langshare    measure the language share and enforce its floors
     maithic-dev capcheck     check the capability register against the tests
     maithic-dev cap-sync     update the register from the tests, then check
     maithic-dev hygiene      placeholder, secret, neutrality, version and header scans
     maithic-dev readme       check the README images
     maithic-dev sync-check   check that files copied from built-in data are current
     maithic-dev contracts    validate every document that names a schema, and the schemas themselves
     maithic-dev sync         rewrite those files from the built-in data
     maithic-dev bridge-record  record Bridge transcripts for the stub (needs Lean)
     maithic-dev cassettes    author the replay cassettes of the demonstrations (needs Lean)
     maithic-dev all          every gate above except cap-sync

   Each command exits with status 0 when its gate passes and 1 when it fails. *)

open Maithic_base
open Maithic_devtools

let root () =
  match Repo.find_root (Sys.getcwd ()) with
  | Some root -> root
  | None -> prerr_endline "maithic-dev: PROJECT.toml not found at or above the current directory"; exit 2

let report name problems =
  match problems with
  | [] -> Printf.printf "%-12s ok\n%!" name; true
  | _ ->
    Printf.printf "%-12s FAILED (%d)\n" name (List.length problems);
    List.iter (fun p -> Printf.printf "  %s\n" p) problems;
    flush stdout;
    false

let langshare ~json root =
  let measured = Langshare.of_repository root in
  let thresholds =
    match Repo.project root with Ok doc -> Langshare.thresholds_of_project doc | Error _ -> Langshare.default_thresholds
  in
  if json then print_endline (Json.pretty (Langshare.to_json measured)) else print_string (Langshare.render measured);
  report "langshare" (Langshare.violations thresholds measured)

let load_register root =
  match Register.load root with
  | Ok pillars -> pillars
  | Error e -> prerr_endline ("maithic-dev: cannot load the register: " ^ e); exit 1

let write_status root pillars =
  ignore (Fs.write Fs.(root / "registry" / "STATUS.md") (Register.status_markdown pillars));
  ignore (Fs.write Fs.(root / "registry" / "status.json") (Json.pretty (Register.status_json pillars) ^ "\n"))

let capcheck root =
  let pillars = load_register root in
  let c = Register.count (Register.all_capabilities pillars) in
  Printf.printf "register: %d done, %d in progress, %d planned\n" c.Register.done_ c.Register.in_progress c.Register.planned;
  let current = Repo.read root "registry/STATUS.md" in
  let stale = if current <> Register.status_markdown pillars then [ "registry/STATUS.md is out of date (run `make cap-sync`)" ] else [] in
  let min_per_pillar =
    match Repo.project root with
    | Ok doc -> Option.value (Option.bind (Json.path [ "register"; "min_per_pillar" ] doc) Json.int_opt) ~default:0
    | Error _ -> 0
  in
  report "capcheck" (Register.check ~min_per_pillar root pillars (Register.scan_tests root) @ stale)

let cap_sync root =
  let pillars = Register.sync (load_register root) (Register.scan_tests root) in
  List.iter (fun p -> ignore (Fs.write Fs.(Register.directory root / Register.file_of p) (Register.emit p))) pillars;
  if not (Fs.exists (Register.digest_file root)) then ignore (Fs.write (Register.digest_file root) (Register.titles_digest pillars ^ "\n"));
  write_status root pillars;
  capcheck root

let hygiene root = report "hygiene" (List.map (fun f -> String.trim (Hygiene.render [ f ])) (Hygiene.run root))

let readme root =
  let min_images = match Repo.project root with Ok doc -> Readmecheck.min_images_of_project doc | Error _ -> 0 in
  let checked = Readmecheck.check ~min_images root in
  Printf.printf "README: %d image(s), %d local\n" (List.length checked.Readmecheck.images) checked.Readmecheck.local;
  report "readme" checked.Readmecheck.problems

let contracts root =
  let summary = Contracts.run root in
  Printf.printf "contracts: %d schema(s), %d document(s) that name one, %d Bridge transcript(s)\n" summary.Contracts.schema_files
    summary.Contracts.documents summary.Contracts.transcripts;
  report "contracts" summary.Contracts.problems

(* Files in the repository that mirror data compiled into the program. *)
let mirrored root =
  [ ("config/models.toml", Maithic_gateway.Model_registry.builtin_toml);
    ("docs/reference/configuration-table.md", Maithic_config.Config.reference_markdown ()) ]
  @ List.map (fun (file, text) -> (Fs.("config" / "prompts" / file), text)) Maithic_gateway.Prompt.builtin
  @ List.map (fun (file, schema) -> (Fs.("schemas" / file), Json.pretty schema ^ "\n")) Maithic_corpus.Corpus_schema.files
  |> List.map (fun (rel, text) -> (rel, text, Repo.read root rel))

let sync_check root =
  report "sync-check"
    (List.filter_map
       (fun (rel, expected, actual) -> if expected = actual then None else Some (rel ^ " differs from the built-in data (run `make sync`)"))
       (mirrored root))

let sync_write root =
  List.iter (fun (rel, expected, _) -> ignore (Fs.write Fs.(root / rel) expected)) (mirrored root);
  sync_check root

(* Replays every scripted session against a real Bridge and stores the full
   exchanges, which the unit tests replay through a stub that needs no Lean. *)
(* What Lean says when a module is not found ends with the folders it looked in, which are those of
   the machine that made the recording. A transcript keeps the message and leaves the folders out. *)
let rec portable = function
  | Json.String text ->
    let marker = "search path entries:" in
    (match Text.find_sub text marker with
     | Some i -> Json.String (String.sub text 0 (i + String.length marker) ^ " (left out of the recording)")
     | None -> Json.String text)
  | Json.List items -> Json.List (List.map portable items)
  | Json.Obj members -> Json.Obj (List.map (fun (key, value) -> (key, portable value)) members)
  | other -> other

let bridge_record root =
  let sessions = Fs.(root / "fixtures" / "bridge" / "sessions") and out = Fs.(root / "fixtures" / "bridge" / "transcripts") in
  match Maithic_bridge.Bridge_locate.locate ~cwd:root () with
  | Error e -> report "bridge-record" [ Maithic_bridge.Bridge_error.to_string e ]
  | Ok located ->
    let problems =
      List.concat_map
        (fun file ->
          if Fs.extension file <> ".json" then []
          else
            match Result.bind (Fs.read Fs.(sessions / file)) Json.parse with
            | Error e -> [ file ^ ": " ^ e ]
            | Ok session ->
              (match Proc.spawn (List.hd located.Maithic_bridge.Bridge_locate.argv) [] with
               | Error e -> [ file ^ ": " ^ e ]
               | Ok worker ->
                 let lines =
                   List.filter_map
                     (fun step ->
                       let request = Json.member "request" step in
                       let line =
                         match Json.member "raw" step, request with
                         | Some (Json.String raw), _ -> Some raw
                         | _, Some rq -> Some (Json.to_string rq)
                         | _ -> None
                       in
                       Option.bind line (fun line ->
                           match Result.bind (Proc.send_line worker line) (fun () -> Proc.read_line ~timeout:120.0 worker) with
                           | Error _ -> None
                           | Ok answer ->
                             (match request, Json.parse answer with
                              | Some rq, Ok response -> Some (Json.to_string (Json.Obj [ ("request", rq); ("response", portable response) ]))
                              | _ -> None)))
                     (Json.list_or_empty "steps" session)
                 in
                 ignore (Proc.stop worker);
                 let name = Filename.remove_extension file in
                 (match Fs.write Fs.(out / (name ^ ".jsonl")) (String.concat "\n" lines ^ "\n") with
                  | Ok () -> []
                  | Error e -> [ file ^ ": " ^ e ])))
        (Fs.list_dir sessions)
    in
    report "bridge-record" problems

(* Authors the replay cassettes of the demonstrations: runs each one with the
   answers of fixtures/cassettes/authored.toml in place of a model and stores
   every exchange, once per provider, under the model of the default tier. *)
let cassettes root =
  let open Maithic_gateway in
  let dir = Fs.(root / "fixtures" / "cassettes") in
  let providers = [ "anthropic"; "openai"; "xai" ] in
  match Toml.parse_file Fs.(dir / "authored.toml"), Maithic_bridge.Bridge_locate.locate ~cwd:root () with
  | Error e, _ -> report "cassettes" [ e ]
  | _, Error e -> report "cassettes" [ Maithic_bridge.Bridge_error.to_string e ]
  | Ok doc, Ok located ->
    List.iter (fun f -> if Fs.extension f = ".json" then Fs.remove Fs.(dir / f)) (Fs.list_dir dir);
    (* Stores the exchanges of one demonstration: each request that was made, with the answer it got. *)
    let save ~what requests answers =
      List.concat_map
        (fun provider ->
          match Model_registry.resolve Model_registry.builtin ~provider ~tier:"balanced" ~model:None with
          | Error e -> [ e ]
          | Ok (_, model) ->
            List.filter_map
              (fun (request, text) ->
                let response = { (Scripted.answer text) with Gateway_types.provider; model = model.Model_registry.id } in
                let cassette =
                  Cassette.make ~note:("Authored answer for the demonstration " ^ what ^ "; not a recording of a model.") ~origin:Cassette.Authored
                    ~provider ~model:model.Model_registry.id request response
                in
                match Cassette.save ~dir cassette with Ok () -> None | Error e -> Some e)
              (List.combine requests (List.filteri (fun i _ -> i < List.length requests) answers)))
        providers
    in
    (* Runs a demonstration with its answers in place of a model, on a Bridge of its own. *)
    let scripted demo run =
      let file = match Json.string_or "" "file" demo with "" -> Json.string_or "" "problem" demo | file -> file in
      let answers = Json.strings (Json.List (Json.list_or_empty "answers" demo)) in
      let handler, seen = Scripted.sequence (List.map (fun text -> Ok (Scripted.answer text)) answers) in
      let config = Maithic_config.Config.of_defaults () in
      match Gateway.create ~scripted:handler config, Fs.read Fs.(root / file) with
      | Error f, _ -> [ Maithic_schema.Fault.to_string f ]
      | _, Error e -> [ e ]
      | Ok gateway, Ok source ->
        (match Maithic_bridge.Bridge_client.with_bridge located.Maithic_bridge.Bridge_locate.argv (fun bridge -> Ok (run ~gateway ~bridge ~file ~source)) with
         | Error e -> [ file ^ ": " ^ Maithic_bridge.Bridge_error.to_string e ]
         | Ok (Error problem) -> [ file ^ ": " ^ problem ]
         | Ok (Ok problems) ->
           let requests = seen () in
           let unused = List.length answers - List.length requests in
           problems @ save ~what:file requests answers
           @ if unused > 0 then [ Printf.sprintf "%s: %d authored answer(s) were never asked for" file unused ] else [])
    in
    let proofs =
      List.concat_map
        (fun demo ->
          let theorem = Json.string_or "" "theorem" demo in
          scripted demo (fun ~gateway ~bridge ~file ~source ->
              match
                Maithic_prover.Engine.prove ~gateway ~bridge ~policy:Maithic_ledger.Trust_policy.default Maithic_prover.Engine.default_settings
                  ~source theorem
              with
              | Error f -> Error (Maithic_schema.Fault.to_string f)
              | Ok result -> Ok (if result.Maithic_prover.Engine.proof = None then [ file ^ ": the answers do not prove " ^ theorem ] else [])))
        (Json.list_or_empty "demo" doc)
    in
    let repairs =
      List.concat_map
        (fun demo ->
          scripted demo (fun ~gateway ~bridge ~file ~source ->
              let renames =
                match Json.string_or "" "renames" demo with
                | "" -> Ok []
                | table -> Maithic_mend.Mend_rename.load_table Fs.(root / table)
              in
              match renames with
              | Error e -> Error e
              | Ok renames ->
                let tools =
                  Maithic_mend.Mend_tools.tools ~gateway
                    ~prove:(Maithic_ledger.Trust_policy.default, Maithic_prover.Engine.default_settings)
                    bridge
                in
                (match Maithic_mend.Mend_repair.run ~renames tools ~file source with
                 | Error e -> Error e
                 | Ok outcome -> Ok (if Maithic_mend.Mend_repair.clean outcome then [] else [ file ^ ": the answers do not repair the file" ]))))
        (Json.list_or_empty "mend" doc)
    in
    let statements =
      let open Maithic_formalize in
      List.concat_map
        (fun demo ->
          scripted demo (fun ~gateway ~bridge ~file ~source ->
              let standard = Formalize_run.default_settings in
              let settings = { standard with Formalize_run.candidates = Json.int_or standard.Formalize_run.candidates "candidates" demo } in
              match Formalize_intake.of_text ~origin:file source, Formalize_tools.open_session bridge ~imports:[] with
              | Error e, _ -> Error e
              | _, Error (`Imports why | `Commands why) -> Error why
              | Ok intake, Ok session ->
                let tools = Formalize_tools.tools ~gateway ~name:settings.Formalize_run.name ~imports:[] ~intake session in
                (match Formalize_run.run ~settings tools intake with
                 | Error e -> Error e
                 | Ok run ->
                   let meant = Json.string_or "formalized" "status" demo and got = Formalize_run.status_name run.Formalize_run.status in
                   Ok (if got = meant then [] else [ Printf.sprintf "%s: the answers end in the status '%s', and '%s' was meant" file got meant ]))))
        (Json.list_or_empty "formalize" doc)
    in
    let councils =
      let open Maithic_council in
      List.concat_map
        (fun demo ->
          scripted demo (fun ~gateway ~bridge ~file ~source ->
              let policy = Maithic_ledger.Trust_policy.default in
              let standard = Council_session.default_settings in
              let settings = { standard with Council_session.rounds = Json.int_or standard.Council_session.rounds "rounds" demo } in
              let recheck ~source name = Maithic_ledger.Recheck.fresh ?cwd:located.Maithic_bridge.Bridge_locate.cwd ~argv:located.Maithic_bridge.Bridge_locate.argv ~source name in
              match Council_problem.of_source ?theorem:(Option.bind (Json.member "theorem" demo) Json.string_opt) ~file source with
              | Error e -> Error e
              | Ok problem ->
                let prove = Council_real.prover ~recheck ~bridge ~policy Maithic_prover.Engine.default_settings problem in
                (match Council_real.raw ~recheck ~prove ~policy bridge problem with
                 | Error e -> Error e
                 | Ok raw ->
                   let session = Council_session.run ~settings ~model:(Council_real.model gateway ~max_tokens:2048) ~tools:(Council_tools.make raw) ~policy problem in
                   let meant = Json.string_or "verified" "status" demo and got = Council_board.status_name (Council_session.status session) in
                   Ok (if got = meant then [] else [ Printf.sprintf "%s: the answers end with the claim '%s', and '%s' was meant" file got meant ]))))
        (Json.list_or_empty "council" doc)
    in
    let plans =
      List.concat_map
        (fun demo ->
          scripted demo (fun ~gateway ~bridge:_ ~file ~source ->
              match
                Maithic_blueprint.Blueprint_plan.ask gateway ~max_tokens:2048 ?namespace:(Option.bind (Json.member "namespace" demo) Json.string_opt) source
              with
              | Error e -> Error e
              | Ok outcome ->
                Ok (if outcome.Maithic_blueprint.Blueprint_plan.doc.Maithic_blueprint.Blueprint_doc.nodes = [] then [ file ^ ": the answer is an empty plan" ] else [])))
        (Json.list_or_empty "blueprint" doc)
    in
    let functions =
      let open Maithic_algolab in
      List.concat_map
        (fun demo ->
          scripted demo (fun ~gateway ~bridge ~file ~source ->
              match Result.bind (Toml.parse source) Algolab_task.of_document with
              | Error e -> Error e
              | Ok task ->
                let synth = Algolab_synth.run (Algolab_lean.tools ~gateway (Algolab_lean.session (fun () -> Ok bridge)) task) task in
                Ok (if Algolab_synth.accepted synth then [] else [ file ^ ": the answers end with no accepted implementation" ])))
        (Json.list_or_empty "algolab" doc)
    in
    Printf.printf "cassettes: %d file(s) in fixtures/cassettes\n" (List.length (List.filter (fun f -> Fs.extension f = ".json") (Fs.list_dir dir)));
    report "cassettes" (proofs @ repairs @ statements @ councils @ plans @ functions)

(* The pictures of terminal sessions under assets/readme, drawn from what the built command prints. *)
let shots root only =
  let maithic = Fs.(root / "ocaml" / "_build" / "default" / "bin" / "maithic" / "main.exe") in
  if not (Fs.exists maithic) then report "shots" [ "the command is not built: " ^ maithic ]
  else begin
    let unknown = List.filter (fun name -> not (List.exists (fun (s : Shots.shot) -> s.Shots.name = name) Shots.all)) only in
    if unknown <> [] then report "shots" [ "no picture called " ^ String.concat ", " unknown ] else report "shots" (Shots.write ~only ~root ~maithic ())
  end

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let json = List.mem "--json" args in
  let ok =
    match List.filter (fun a -> a <> "--json") args with
    | [ "langshare" ] -> langshare ~json (root ())
    | [ "capcheck" ] -> capcheck (root ())
    | [ "cap-sync" ] -> cap_sync (root ())
    | [ "hygiene" ] -> hygiene (root ())
    | [ "readme" ] -> readme (root ())
    | [ "sync-check" ] -> sync_check (root ())
    | [ "contracts" ] -> contracts (root ())
    | [ "sync" ] -> sync_write (root ())
    | [ "bridge-record" ] -> bridge_record (root ())
    | [ "cassettes" ] -> cassettes (root ())
    | "shots" :: only -> shots (root ()) only
    | [ "all" ] ->
      let r = root () in
      let results = [ langshare ~json:false r; capcheck r; hygiene r; readme r; sync_check r; contracts r ] in
      List.for_all Fun.id results
    | _ ->
      prerr_endline
        "Usage: maithic-dev (langshare | capcheck | cap-sync | hygiene | readme | sync-check | contracts | sync | bridge-record | cassettes | shots [NAME...] | all) [--json]";
      exit 2
  in
  exit (if ok then 0 else 1)
