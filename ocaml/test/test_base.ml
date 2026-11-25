(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Testkit

let json_gen : Json.t Gen.t =
  let rec value size st =
    let scalar =
      Gen.choose
        [ Gen.return Json.Null; Gen.map (fun b -> Json.Bool b) Gen.bool; Gen.map (fun n -> Json.Int n) (Gen.int (-1000000) 1000000);
          Gen.map (fun s -> Json.String s) (Gen.unicode_string ()); Gen.map (fun f -> Json.Float (Float.round (f *. 1000.0) /. 1000.0 +. 0.5)) (Gen.float (-1000.0) 1000.0) ]
    in
    if size <= 0 then scalar st
    else
      Gen.choose
        [ scalar;
          Gen.map (fun items -> Json.List items) (Gen.list ~max:4 (value (size / 2)));
          Gen.map
            (fun members ->
              let seen = Hashtbl.create 8 in
              Json.Obj (List.filter (fun (k, _) -> if Hashtbl.mem seen k then false else (Hashtbl.replace seen k (); true)) members))
            (Gen.list ~max:4 (Gen.pair (Gen.string_of ~max:6 "abcdefxyz_") (value (size / 2)))) ]
        st
  in
  Gen.sized value

let json_tests =
  [ case "parses scalars, arrays and objects" (fun () ->
        equal ~show:Json.to_string "value"
          (Json.Obj [ ("a", Json.List [ Json.Int 1; Json.Float 2.5; Json.Bool true; Json.Null ]); ("b", Json.String "x") ])
          (Json.parse_exn {| { "a" : [1, 2.5, true, null], "b": "x" } |}));
    case "decodes escapes and surrogate pairs to UTF-8" (fun () ->
        equal_string "text" "tab\t quote\" \xC3\xA9 \xF0\x9D\x94\xB8" (Option.get (Json.string_opt (Json.parse_exn {|"tab\t quote\" \u00e9 \ud835\udd38"|}))));
    case "rejects malformed input with an offset" (fun () ->
        List.iter
          (fun bad -> contains bad (is_error bad (Json.parse bad)) "offset")
          [ "{"; "[1,]"; "{\"a\":}"; "tru"; "\"unterminated"; "1 2"; "{'a':1}"; "[1 2]"; "\"\\ud800\""; "01"; "" ]);
    case "keeps integers exact and falls back to floats when they do not fit" (fun () ->
        equal ~show:Json.to_string "int" (Json.Int 9007199254740993) (Json.parse_exn "9007199254740993");
        check "big" (match Json.parse_exn "123456789012345678901234567890" with Json.Float _ -> true | _ -> false));
    case "prints canonical form with sorted members" (fun () ->
        equal_string "canonical" {|{"a":1,"b":{"c":2,"d":3}}|}
          (Json.to_string ~canonical:true (Json.parse_exn {|{"b":{"d":3,"c":2},"a":1}|})));
    case "escapes control characters when printing" (fun () ->
        equal_string "escaped" {|"a\nb\u0001\"\\"|} (Json.to_string (Json.String "a\nb\001\"\\")));
    case "pretty output parses back to the same value" (fun () ->
        let v = Json.parse_exn {|{"a":[1,{"b":[]}],"c":{}}|} in
        check "round trip" (Json.equal v (Json.parse_exn (Json.pretty v))));
    case "equality ignores member order and number representation" (fun () ->
        check "order" (Json.equal (Json.parse_exn {|{"a":1,"b":2}|}) (Json.parse_exn {|{"b":2.0,"a":1}|}));
        check "different" (not (Json.equal (Json.parse_exn "[1,2]") (Json.parse_exn "[2,1]"))));
    case "subset match reports the path of the first mismatch" (fun () ->
        let actual = Json.parse_exn {|{"a":{"b":[1,{"c":"hello world"}]},"d":2}|} in
        ignore (is_ok "subset" (Json.is_subset ~expected:(Json.parse_exn {|{"a":{"b":[1,{"c":{"$contains":"world"}}]}}|}) actual));
        contains "path" (is_error "mismatch" (Json.is_subset ~expected:(Json.parse_exn {|{"a":{"b":[1,{"c":"x"}]}}|}) actual)) "$.a.b[1].c";
        contains "length" (is_error "length" (Json.is_subset ~expected:(Json.parse_exn {|{"a":{"b":[1]}}|}) actual)) "expected 1 elements");
    property ~count:300 ~show:Json.to_string "printing then parsing is the identity" json_gen (fun v ->
        Json.equal v (Json.parse_exn (Json.to_string v)) && Json.equal v (Json.parse_exn (Json.pretty v)));
    property ~count:300 ~show:Json.to_string "canonical form does not depend on member order" json_gen (fun v ->
        let rec shuffle = function
          | Json.Obj members -> Json.Obj (List.rev_map (fun (k, x) -> (k, shuffle x)) members)
          | Json.List items -> Json.List (List.map shuffle items)
          | other -> other
        in
        Json.to_string ~canonical:true v = Json.to_string ~canonical:true (shuffle v)) ]

let sha_tests =
  [ case "matches the standard test vectors" (fun () ->
        equal_string "empty" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" (Sha256.hex "");
        equal_string "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" (Sha256.hex "abc");
        equal_string "two blocks" "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
          (Sha256.hex "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
        equal_string "million a" "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" (Sha256.hex (String.make 1000000 'a')));
    case "handles every padding boundary" (fun () ->
        (* Lengths around the 55/56 and 63/64 byte boundaries, checked against sha256sum. *)
        equal_string "55" "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318" (Sha256.hex (String.make 55 'a'));
        equal_string "56" "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a" (Sha256.hex (String.make 56 'a'));
        equal_string "64" "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb" (Sha256.hex (String.make 64 'a')));
    case "agrees with the sha256sum command when it is available" (fun () ->
        match Proc.run ~stdin:"Maithic \xE2\x88\x80" "sha256sum" [] with
        | Ok out when out.Proc.status = 0 ->
          equal_string "digest" (String.sub out.Proc.stdout 0 64) (Sha256.hex "Maithic \xE2\x88\x80")
        | _ -> skip "sha256sum is not installed") ]

let toml_tests =
  [ case "reads tables, dotted keys and every value form" (fun () ->
        let doc =
          is_ok "parse"
            (Toml.parse
               "title = \"x\" # comment\n[a]\nb.c = 1\nd = [1, 2,\n  3]\ne = { f = true, g = 'lit' }\nh = 0x1F\ni = 1_000\nj = -2.5e3\n[a.k]\nl = \"\"\"\nmulti\nline\"\"\"\nm = 1979-05-27T07:32:00Z\n")
        in
        equal ~show:Json.to_string "doc"
          (Json.parse_exn
             {|{"title":"x","a":{"b":{"c":1},"d":[1,2,3],"e":{"f":true,"g":"lit"},"h":31,"i":1000,"j":-2500.0,"k":{"l":"multi\nline","m":"1979-05-27T07:32:00Z"}}}|})
          doc);
    case "reads arrays of tables with nested arrays of tables" (fun () ->
        let doc = is_ok "parse" (Toml.parse "[[g]]\nid = 1\n  [[g.c]]\n  n = \"a\"\n  [[g.c]]\n  n = \"b\"\n[[g]]\nid = 2\n") in
        equal ~show:Json.to_string "doc" (Json.parse_exn {|{"g":[{"id":1,"c":[{"n":"a"},{"n":"b"}]},{"id":2}]}|}) doc);
    case "decodes string escapes" (fun () ->
        let doc = is_ok "parse" (Toml.parse {|s = "tab\t quote\" back\\ \u00e9 \U0001D538"|}) in
        equal_string "s" "tab\t quote\" back\\ \xC3\xA9 \xF0\x9D\x94\xB8" (Json.string_or "" "s" doc));
    case "reports the line of an error" (fun () ->
        contains "line" (is_error "dup" (Toml.parse "a = 1\nb = 2\na = 3\n")) "line 3";
        contains "line" (is_error "bad" (Toml.parse "a = 1\n[t\n")) "line 2";
        List.iter
          (fun bad -> ignore (is_error bad (Toml.parse bad)))
          [ "a = "; "a = 01"; "= 1"; "a = \"x"; "[a]\n[a]\n"; "a = 1 b = 2"; "a = {b = 1}\na.c = 2"; "a = [1 2]"; "a.b = 1\na = 2" ]);
    case "escape_string output is read back unchanged" (fun () ->
        List.iter
          (fun s -> equal_string s s (Json.string_or "?" "v" (is_ok s (Toml.parse ("v = " ^ Toml.escape_string s)))))
          [ ""; "plain"; "quote \" and \\ backslash"; "line\nbreak\ttab"; "unicode \xE2\x88\x80\xCE\xB5 \xE6\x95\xB0" ]);
    property ~count:200 ~show:(Printf.sprintf "%S") "any string survives escaping and parsing" (Gen.unicode_string ~max:24 ())
      (fun s -> Json.string_or "?" "v" (is_ok s (Toml.parse ("v = " ^ Toml.escape_string s))) = s) ]

let text_tests =
  [ case "splits, replaces and finds" (fun () ->
        equal_strings "split" [ "a"; "b"; ""; "c" ] (Text.split_on "--" "a--b----c");
        equal_string "replace" "x.y.z" (Text.replace_all ~sub:"::" ~by:"." "x::y::z");
        equal ~show:(function Some i -> string_of_int i | None -> "none") "find" (Some 4) (Text.find_sub "abc abc" "abc" ~from:1);
        check "contains" (Text.contains "hello world" "o w" && not (Text.contains "hello" "world")));
    case "counts and truncates by code point" (fun () ->
        equal_int "length" 5 (Text.utf8_length "\xE2\x88\x80x \xCE\xB5!");
        equal_string "pad" "\xCE\xB5  |" (Text.pad_right 3 "\xCE\xB5" ^ "|");
        equal_string "truncate" "\xE2\x88\x80\xE2\x88\x80\xE2\x80\xA6" (Text.truncate 3 "\xE2\x88\x80\xE2\x88\x80\xE2\x88\x80\xE2\x88\x80");
        equal_string "no cut" "abc" (Text.truncate 3 "abc");
        equal_string "between words" "one two \xE2\x80\xA6" (Text.truncate_words 11 "one two three four");
        equal_string "the words fit" "one two" (Text.truncate_words 7 "one two");
        equal_string "one long word" "abcd\xE2\x80\xA6" (Text.truncate_words 5 "abcdefghij"));
    case "validates UTF-8" (fun () ->
        check "valid" (Text.is_valid_utf8 "plain \xC3\xA9 \xE2\x88\x80 \xF0\x9D\x94\xB8");
        List.iter (fun bad -> check "invalid" (not (Text.is_valid_utf8 bad))) [ "\xC3"; "\xE2\x88"; "\xFF"; "\xC0\xAF"; "\xED\xA0\x80" ]);
    property ~count:200 "generated strings are valid UTF-8 and lines round-trip" (Gen.unicode_string ~max:30 ()) (fun s ->
        Text.is_valid_utf8 s && String.concat "\n" (String.split_on_char '\n' s) = s) ]

let fs_proc_tests =
  [ case "writes atomically, creates parents and walks in order" (fun () ->
        let dir = temp_dir "maithic-fs" in
        ignore (is_ok "write" (Fs.write Fs.(dir / "a" / "b" / "one.txt") "1"));
        ignore (is_ok "write" (Fs.write Fs.(dir / "a" / "two.txt") "2"));
        ignore (is_ok "write" (Fs.write Fs.(dir / "_build" / "skip.txt") "x"));
        equal_strings "walk" [ "a/b/one.txt"; "a/two.txt" ] (Fs.walk ~skip:(fun rel -> Filename.basename rel = "_build") dir);
        equal_string "read" "1" (is_ok "read" (Fs.read Fs.(dir / "a" / "b" / "one.txt")));
        ignore (is_error "missing" (Fs.read Fs.(dir / "nope")));
        Fs.remove_tree dir;
        check "removed" (not (Fs.exists dir)));
    case "runs a command with input, output and exit status" (fun () ->
        let out = is_ok "run" (Proc.run ~stdin:"hello\n" "sh" [ "-c"; "cat; echo err >&2; exit 3" ]) in
        equal_string "stdout" "hello\n" out.Proc.stdout;
        equal_string "stderr" "err\n" out.Proc.stderr;
        equal_int "status" 3 out.Proc.status);
    case "passes large input and output without deadlock" (fun () ->
        let big = String.make 300000 'x' in
        let out = is_ok "run" (Proc.run ~stdin:big "cat" []) in
        equal_int "length" 300000 (String.length out.Proc.stdout));
    case "reports a missing program and a timeout as errors" (fun () ->
        contains "missing" (is_error "missing" (Proc.run "maithic-no-such-program" [])) "not found";
        contains "timeout" (is_error "timeout" (Proc.run ~timeout:0.2 "sleep" [ "5" ])) "did not finish");
    case "sets the working directory and the environment of a child" (fun () ->
        let dir = temp_dir "maithic-cwd" in
        let out = is_ok "run" (Proc.run ~cwd:dir ~env:[ ("MAITHIC_TEST_VALUE", "42") ] "sh" [ "-c"; "pwd -P; echo $MAITHIC_TEST_VALUE" ]) in
        contains "cwd" out.Proc.stdout (Filename.basename dir);
        contains "env" out.Proc.stdout "42";
        Fs.remove_tree dir);
    case "talks to a worker line by line and stops it" (fun () ->
        let w = is_ok "spawn" (Proc.spawn "cat" []) in
        ignore (is_ok "send" (Proc.send_line w "one"));
        equal_string "echo" "one" (is_ok "read" (Proc.read_line ~timeout:5.0 w));
        ignore (is_ok "send" (Proc.send_line w "two"));
        equal_string "echo" "two" (is_ok "read" (Proc.read_line ~timeout:5.0 w));
        contains "timeout" (is_error "silence" (Proc.read_line ~timeout:0.1 w)) "timed out";
        equal_int "exit" 0 (Proc.stop w);
        ignore (is_error "after stop" (Proc.send_line w "three"))) ]

let () =
  run "base"
    [ ("json", json_tests); ("sha256", sha_tests); ("toml", toml_tests); ("text", text_tests); ("files and processes", fs_proc_tests) ]
