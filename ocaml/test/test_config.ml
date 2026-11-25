(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Testkit

let no_env _ = None

(* A workspace directory with the given project file. *)
let workspace text =
  let dir = temp_dir "maithic-config" in
  ignore (is_ok "write" (Fs.write Fs.(dir / "maithic.toml") text));
  dir

let load ?(getenv = no_env) ?flags ?files ?cwd () =
  match Config.load ?cwd ~getenv ?flags ?files () with
  | Ok c -> Ok c
  | Error f -> Error (Fault.to_string f)

let tests =
  [ case "P00-02-06 the defaults work with no file, no variable and no key" (fun () ->
        let c = is_ok "load" (load ~files:[] ()) in
        equal_string "provider" "anthropic" (Config.string c "ai.provider");
        equal_string "tier" "balanced" (Config.string c "ai.tier");
        equal_string "mode" "live" (Config.string c "gateway.mode");
        check "ai on" (Config.bool c "prove.use_ai");
        equal_int "rounds" 3 (Config.int c "prove.rounds");
        check "source" (Config.source c "ai.provider" = Config.Default));
    case "P00-02-01 reads the documented sections of maithic.toml" (fun () ->
        let dir = workspace "[ai]\nprovider = \"xai\"\ntier = \"best\"\n\n[gateway]\nmode = \"replay\"\ncassette_dir = \"tapes\"\n\n[prove]\nrounds = 5\nuse_ai = false\n" in
        let c = is_ok "load" (load ~cwd:dir ~files:[ Fs.(dir / "maithic.toml") ] ()) in
        equal_string "provider" "xai" (Config.string c "ai.provider");
        equal_string "mode" "replay" (Config.string c "gateway.mode");
        equal_int "rounds" 5 (Config.int c "prove.rounds");
        check "use_ai" (not (Config.bool c "prove.use_ai"));
        Fs.remove_tree dir);
    case "P00-02-02 later layers win: defaults, files in order, environment, flags" (fun () ->
        let dir = temp_dir "maithic-layers" in
        let user = Fs.(dir / "user.toml") and project = Fs.(dir / "project.toml") in
        ignore (is_ok "w" (Fs.write user "[ai]\nprovider = \"openai\"\ntier = \"fast\"\n[prove]\nrounds = 1\n"));
        ignore (is_ok "w" (Fs.write project "[ai]\ntier = \"best\"\n[prove]\nrounds = 2\ncandidates = 9\n"));
        let getenv = Support.env [ ("MAITHIC_ROUNDS", "7"); ("MAITHIC_CANDIDATES", "8") ] in
        let c = is_ok "load" (load ~getenv ~files:[ user; project ] ~flags:[ ("prove.candidates", "6") ] ()) in
        equal_string "from user file" "openai" (Config.string c "ai.provider");
        equal_string "project over user" "best" (Config.string c "ai.tier");
        equal_int "environment over files" 7 (Config.int c "prove.rounds");
        equal_int "flag over environment" 6 (Config.int c "prove.candidates");
        check "sources"
          (Config.source c "ai.provider" = Config.File user
           && Config.source c "ai.tier" = Config.File project
           && Config.source c "prove.rounds" = Config.Environment "MAITHIC_ROUNDS"
           && Config.source c "prove.candidates" = Config.Flag);
        Fs.remove_tree dir);
    case "P00-02-03 an unknown key is rejected with its file and a suggestion" (fun () ->
        let dir = workspace "[ai]\nprovidr = \"xai\"\n" in
        let e = is_error "load" (load ~files:[ Fs.(dir / "maithic.toml") ] ()) in
        contains "code" e "MTH-CONFIG-0001";
        contains "file" e "maithic.toml";
        contains "key" e "ai.providr";
        contains "suggestion" e "Did you mean 'ai.provider'?";
        Fs.remove_tree dir);
    case "P00-02-03 a wrong type or value names the setting and what is allowed" (fun () ->
        let dir = workspace "[gateway]\nmode = \"online\"\n" in
        let e = is_error "enum" (load ~files:[ Fs.(dir / "maithic.toml") ] ()) in
        contains "enum" e "one of live, record, replay, mock, off";
        let e = is_error "int" (load ~files:[] ~getenv:(Support.env [ ("MAITHIC_ROUNDS", "many") ]) ()) in
        contains "variable" e "environment variable MAITHIC_ROUNDS";
        contains "type" e "non-negative integer";
        let dir2 = workspace "[ai\nprovider = 1\n" in
        contains "syntax" (is_error "syntax" (load ~files:[ Fs.(dir2 / "maithic.toml") ] ())) "line 1: unterminated table header";
        Fs.remove_tree dir;
        Fs.remove_tree dir2);
    case "P00-02-04 every setting has its own MAITHIC_ variable" (fun () ->
        let names = List.map (fun (s : Config.setting) -> s.Config.env) Config.settings in
        check "prefix" (List.for_all (fun n -> Text.starts_with ~prefix:"MAITHIC_" n) names);
        equal_int "distinct" (List.length names) (List.length (List.sort_uniq compare names));
        List.iter
          (fun (s : Config.setting) ->
            let value =
              match s.Config.kind with
              | Config.Enum choices -> List.nth choices (List.length choices - 1)
              | Config.Int -> "11"
              | Config.Float -> "12.5"
              | Config.Bool -> "false"
              | Config.String | Config.Path -> "value"
            in
            let c = is_ok s.Config.env (load ~files:[] ~getenv:(Support.env [ (s.Config.env, value) ]) ()) in
            equal_string s.Config.key value (Config.string c s.Config.key))
          Config.settings);
    case "P00-02-05 typed accessors return typed values" (fun () ->
        let c = is_ok "load" (load ~files:[] ~flags:[ ("gateway.timeout_s", "2.5"); ("prove.use_ai", "no"); ("ai.model", "m-1") ] ()) in
        check "float" (Config.float c "gateway.timeout_s" = 2.5);
        check "bool" (not (Config.bool c "prove.use_ai"));
        check "option" (Config.string_opt c "ai.model" = Some "m-1" && Config.string_opt c "bridge.command" = None);
        raises "unknown key" (fun () -> Config.string c "no.such.key"));
    case "P00-02-07 the reference is generated from the settings table" (fun () ->
        let reference = Config.reference_markdown () in
        List.iter
          (fun (s : Config.setting) ->
            contains "key" reference ("`" ^ s.Config.key ^ "`");
            contains "variable" reference ("`" ^ s.Config.env ^ "`"))
          Config.settings;
        equal_string "committed copy" reference (Support.read "docs/reference/configuration-table.md"));
    case "P00-02-08 a missing file is skipped and an empty variable is ignored" (fun () ->
        let c = is_ok "load" (load ~files:[ "/nonexistent/maithic.toml" ] ~getenv:(Support.env [ ("MAITHIC_PROVIDER", "") ]) ()) in
        equal_string "default kept" "anthropic" (Config.string c "ai.provider"));
    case "P00-02-09 the workspace is found from a directory below it" (fun () ->
        let dir = workspace "[ai]\nprovider = \"openai\"\n" in
        let deep = Fs.(dir / "a" / "b") in
        ignore (is_ok "mkdir" (Fs.mkdir_p deep));
        check "found" (Config.find_workspace ~getenv:no_env deep = Some dir);
        check "override" (Config.find_workspace ~getenv:(Support.env [ ("MAITHIC_WORKSPACE", "/w") ]) deep = Some "/w");
        check "none" (Config.find_workspace ~getenv:no_env "/" = None);
        let c = is_ok "load" (load ~cwd:deep ~getenv:(Support.env [ ("HOME", dir) ]) ()) in
        equal_string "project file applied" "openai" (Config.string c "ai.provider");
        equal_string "relative paths resolve against the workspace" Fs.(dir / "fixtures/cassettes")
          (Config.resolve_path c (Config.string c "gateway.cassette_dir"));
        Fs.remove_tree dir);
    case "P00-02-10 a key in the project file is rejected: secrets live elsewhere" (fun () ->
        let dir = workspace "[ai]\napi_key = \"value\"\n" in
        contains "rejected" (is_error "load" (load ~files:[ Fs.(dir / "maithic.toml") ] ())) "unknown setting 'ai.api_key'";
        check "no setting stores a secret"
          (List.for_all (fun (s : Config.setting) -> not (Text.contains s.Config.key "key" || Text.contains s.Config.key "secret")) Config.settings);
        Fs.remove_tree dir);
    case "P00-02-11 the effective configuration shows each value with its source" (fun () ->
        let c = is_ok "load" (load ~files:[] ~getenv:(Support.env [ ("MAITHIC_TIER", "fast") ]) ~flags:[ ("gateway.mode", "mock") ] ()) in
        let j = Config.to_json c in
        equal ~show:Json.to_string "tier" (Json.parse_exn {|{"value":"fast","source":"environment MAITHIC_TIER"}|}) (Option.get (Json.member "ai.tier" j));
        equal ~show:Json.to_string "mode" (Json.parse_exn {|{"value":"mock","source":"command line"}|}) (Option.get (Json.member "gateway.mode" j));
        equal ~show:Json.to_string "provider" (Json.parse_exn {|{"value":"anthropic","source":"default"}|}) (Option.get (Json.member "ai.provider" j)));
    case "P00-02-01 the sample project file is valid" (fun () ->
        let c = is_ok "load" (load ~files:[ Support.path "maithic.toml.example" ] ()) in
        equal_string "provider" "anthropic" (Config.string c "ai.provider");
        check "from the file" (Config.source c "prove.rounds" = Config.File (Support.path "maithic.toml.example")));
    case "suggests the nearest name only when it is close" (fun () ->
        check "close" (Config.nearest "provder" [ "provider"; "model" ] = Some "provider");
        check "far" (Config.nearest "zzz" [ "provider"; "model" ] = None)) ]

let () = run "config" [ ("configuration", tests) ]
