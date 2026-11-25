(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_cli
open Cli_args
open Testkit

let specs = [ value "theorem" "NAME" "doc"; switch "write" "doc" ] @ global_options

let tests =
  [ case "P00-01-01 the global options are accepted by every command" (fun () ->
        equal_strings "names" [ "config"; "provider"; "model"; "tier"; "mode"; "json"; "quiet"; "verbose"; "no-color"; "help" ]
          (List.map (fun s -> s.long) global_options);
        List.iter
          (fun (cmd : command) ->
            let argv = [ "--config"; "f.toml"; "--provider=xai"; "--model"; "m"; "--json"; "--quiet"; "--verbose"; "--no-color" ] in
            let parsed = is_ok (String.concat " " cmd.path) (parse (cmd.options @ global_options) argv) in
            check "values" (get parsed "provider" = Some "xai" && get parsed "model" = Some "m" && get parsed "config" = Some "f.toml");
            check "switches" (has parsed "json" && has parsed "quiet" && has parsed "verbose" && has parsed "no-color"))
          Commands_admin.all);
    case "P00-01-05 every command accepts --json" (fun () ->
        List.iter
          (fun (cmd : command) -> check (String.concat " " cmd.path) (has (is_ok "parse" (parse (cmd.options @ global_options) [ "--json" ])) "json"))
          Commands_admin.all);
    case "parses values, switches, repeated options and positional arguments" (fun () ->
        let p = is_ok "parse" (parse specs [ "file.lean"; "--theorem"; "a"; "--theorem=b"; "--write"; "--"; "--not-an-option" ]) in
        equal_strings "positional" [ "file.lean"; "--not-an-option" ] p.positional;
        equal_strings "repeated" [ "a"; "b" ] (get_all p "theorem");
        check "switch" (has p "write"));
    case "P00-01-06 a wrong option is explained with the nearest name" (fun () ->
        contains "suggestion" (is_error "unknown" (parse specs [ "--theorm"; "x" ])) "did you mean --theorem?";
        contains "missing value" (is_error "value" (parse specs [ "--theorem" ])) "needs a value (NAME)";
        contains "switch with value" (is_error "switch" (parse specs [ "--write=yes" ])) "takes no value");
    case "selects the command with the longest matching path" (fun () ->
        let pick argv = Option.map (fun ((c : command), rest) -> (String.concat " " c.path, rest)) (select Commands_admin.all argv) in
        check "two words" (pick [ "config"; "show"; "--json" ] = Some ("config show", [ "--json" ]));
        check "one word" (pick [ "prove"; "f.lean" ] = Some ("prove", [ "f.lean" ]));
        check "unknown" (pick [ "frobnicate" ] = None));
    case "the overview lists every command and each help page lists its options" (fun () ->
        let overview = overview Commands.program ~tagline:Commands.tagline Commands_admin.all in
        contains "tagline" overview "AI proposes. The kernel decides.";
        List.iter
          (fun (cmd : command) ->
            contains "listed" overview (String.concat " " cmd.path);
            let help = command_help Commands.program cmd in
            contains "usage" help ("Usage: maithic " ^ String.concat " " cmd.path);
            List.iter (fun spec -> contains "option" help ("--" ^ spec.long)) (cmd.options @ global_options))
          Commands_admin.all;
        equal_int "paths are unique" (List.length Commands_admin.all)
          (List.length (List.sort_uniq compare (List.map (fun (c : command) -> c.path) Commands_admin.all))));
    case "a table that is too wide goes on under its last column" (fun () ->
        let rows = [ [ "name"; "what it does" ]; [ "linear"; "Linear arithmetic by exact simplex: multipliers, or a counterexample." ]; [ "real"; "Boxes." ] ] in
        equal_string "wrapped at a word"
          "name    what it does\n------  -----------------------------------\nlinear  Linear arithmetic by exact simplex:\n        multipliers, or a counterexample.\nreal    Boxes.\n"
          (Term.table ~width:43 rows);
        equal_string "a table that fits is left as it is"
          ("name    what it does\n------  " ^ String.make 69 '-' ^ "\nlinear  Linear arithmetic by exact simplex: multipliers, or a counterexample.\nreal    Boxes.\n")
          (Term.table ~width:200 rows);
        (* A word that is longer than the room is not cut, and a coloured cell is not taken apart. *)
        let address = "https://example.org/a/very/long/path/that/has/no/space/to/break/at" in
        equal_string "a long word stays whole" ("id  where\n--  " ^ String.make (String.length address) '-' ^ "\na   " ^ address ^ "\n")
          (Term.table ~width:40 [ [ "id"; "where" ]; [ "a"; address ] ]);
        let coloured = "\027[32mone two three four five six seven eight nine ten eleven twelve\027[0m" in
        equal_bool "a coloured cell stays whole" true (Text.contains (Term.table ~width:40 [ [ "id"; "state" ]; [ "a"; coloured ] ]) coloured);
        equal_bool "the width follows COLUMNS only when it is a sensible number" true (Term.width () >= 40));
    case "tables align columns by code point" (fun () ->
        equal_string "table" "name  value\n----  -----\n\xE2\x88\x80x    1\ny     22\n" (Term.table [ [ "name"; "value" ]; [ "\xE2\x88\x80x"; "1" ]; [ "y"; "22" ] ]);
        (* a coloured cell is as wide as what it shows *)
        let green = "\027[32mok\027[0m" in
        equal_int "a colour code takes no room" 2 (Term.visible_length green);
        equal_string "a table with a coloured cell" ("state   n\n------  -\n" ^ green ^ "      1\nfailed  2\n")
          (Term.table [ [ "state"; "n" ]; [ green; "1" ]; [ "failed"; "2" ] ]));
    case "an unknown command exits with the usage code" (fun () -> equal_int "exit" 2 (Main_cli.run [ "frobnicate" ])) ]

let () = run "cli" [ ("command line", tests) ]
