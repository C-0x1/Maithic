(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_devtools
open Testkit

let root = Lazy.force Support.root

let langshare_tests =
  [ case "P00-03-02 files are classified as a language bar classifies them" (fun () ->
        let name rel = Option.map Langshare.name (Langshare.classify rel) in
        equal ~show:(fun l -> String.concat "," (List.map (Option.value ~default:"-") l)) "languages"
          [ Some "OCaml"; Some "OCaml"; Some "Lean"; Some "Julia"; Some "R"; Some "R"; Some "Python"; Some "Shell"; Some "Makefile"; None; None; None; None ]
          (List.map name
             [ "ocaml/lib/a.ml"; "ocaml/lib/a.mli"; "lean/A.lean"; "julia/P/src/P.jl"; "r/p/R/a.R"; "r/p/tests/testthat/test-a.r"; "tools/x.py";
               "tools/x.sh"; "Makefile"; "README.md"; "config/models.toml"; "schemas/a.json"; "ocaml/lib/dune" ]));
    case "P00-03-02 shares are measured in bytes, without fixtures, assets and marked paths" (fun () ->
        let report =
          Langshare.measure ~exclusions:[ "docs/"; "*.min.js"; "vendor/**" ]
            [ ("a.ml", 500); ("b.lean", 200); ("c.jl", 150); ("d.R", 100); ("e.py", 50); ("fixtures/big.ml", 99999);
              ("assets/site.css", 99999); ("docs/example.ml", 99999); ("web/app.min.js", 99999); ("vendor/lib/x.jl", 99999); ("README.md", 99999) ]
        in
        equal_int "total" 1000 report.Langshare.total_bytes;
        check "project share" (Float.abs (report.Langshare.project_percent -. 95.0) < 1e-9);
        equal_strings "largest first" [ "OCaml"; "Lean"; "Julia"; "R"; "Python" ]
          (List.map (fun (s : Langshare.share) -> Langshare.name s.Langshare.language) report.Langshare.shares));
    case "P00-03-02 the gate enforces the combined share and each floor" (fun () ->
        let t = Langshare.default_thresholds in
        let passing = Langshare.measure [ ("a.ml", 450); ("b.lean", 200); ("c.jl", 150); ("d.R", 100); ("e.py", 100) ] in
        equal_strings "passes" [] (Langshare.violations t passing);
        let other_heavy = Langshare.measure [ ("a.ml", 400); ("b.lean", 200); ("c.jl", 130); ("d.R", 90); ("e.py", 180) ] in
        contains "combined" (String.concat "\n" (Langshare.violations t other_heavy)) "together are 82.0%";
        let no_r = Langshare.measure [ ("a.ml", 600); ("b.lean", 250); ("c.jl", 150) ] in
        equal_strings "floor" [ "R is 0.0% of the code, below its floor of 8.0%" ] (Langshare.violations t no_r);
        check "defaults are the rule" (t.Langshare.combined = 85.0 && t.Langshare.ocaml = 30.0 && t.Langshare.lean = 15.0 && t.Langshare.julia = 12.0 && t.Langshare.r = 8.0));
    case "P00-03-02 the repository meets the shares set in PROJECT.toml" (fun () ->
        let thresholds = Langshare.thresholds_of_project (is_ok "project" (Repo.project root)) in
        equal_strings "violations" [] (Langshare.violations thresholds (Langshare.of_repository root))) ]

let pillars = lazy (is_ok "register" (Register.load root))
let tags = lazy (Register.scan_tests root)

let register_tests =
  [ case "P00-03-03 the register holds every pillar with at least twenty capabilities and pinned titles" (fun () ->
        let all = Register.all_capabilities (Lazy.force pillars) in
        equal_int "pillars" 19 (List.length (Lazy.force pillars));
        check "every pillar holds twenty or more"
          (List.for_all (fun p -> List.length (Register.pillar_caps p) >= 20) (Lazy.force pillars));
        equal_int "distinct identifiers" (List.length all) (List.length (List.sort_uniq compare (List.map (fun (c : Register.capability) -> c.Register.id) all)));
        equal_int "distinct titles" (List.length all) (List.length (List.sort_uniq compare (List.map (fun (c : Register.capability) -> c.Register.title) all)));
        equal_string "digest" (String.trim (Support.read "registry/titles.sha256")) (Register.titles_digest (Lazy.force pillars));
        (* A register with a pillar below the floor, or with identifiers out of order, is refused. *)
        let thin = List.map (fun p -> if p.Register.pillar_id = "P02" then { p with Register.groups = [ List.hd p.Register.groups ] } else p) (Lazy.force pillars) in
        check "a pillar below the floor is a problem"
          (List.exists (fun problem -> Text.contains problem "P02 Formalize holds") (Register.check ~min_per_pillar:20 root thin (Lazy.force tags)));
        let swapped =
          List.map
            (fun p ->
              if p.Register.pillar_id <> "P01" then p
              else { p with Register.groups = (match p.Register.groups with a :: b :: rest -> b :: a :: rest | groups -> groups) })
            (Lazy.force pillars)
        in
        check "identifiers out of order are a problem"
          (List.exists (fun problem -> Text.contains problem "identifiers must rise") (Register.check root swapped (Lazy.force tags))));
    case "P00-03-03 every register file is in canonical form" (fun () ->
        List.iter
          (fun p -> equal_int (Register.file_of p) 0 (compare (Register.emit p) (Support.read ("registry/capabilities/" ^ Register.file_of p))))
          (Lazy.force pillars));
    case "P00-03-03 the register and the test suites agree" (fun () ->
        equal_strings "problems" [] (Register.check root (Lazy.force pillars) (Lazy.force tags)));
    case "P00-03-03 tests are found by the tagging convention of each language" (fun () ->
        (* The identifiers are assembled here so that this file tags nothing by accident. *)
        let id n = "P01-0" ^ n in
        equal_strings "ocaml" [ id "1-02" ]
          (Register.tagged_ids [ "\"" ] (Printf.sprintf {|case "%s best-first order" (fun () -> ()); "%s"; "not %s"|} (id "1-02") (id "1-03") (id "1-04")));
        equal_strings "lean" [ id "6-01"; id "6-02" ]
          (Register.tagged_ids [ "-- cap: " ] (Printf.sprintf "-- cap: %s\nexample : True := trivial\n-- cap: %s\n-- see %s\n" (id "6-01") (id "6-02") (id "6-03")));
        equal_strings "julia" [ id "8-02" ] (Register.tagged_ids [ "@testset \"" ] (Printf.sprintf {|@testset "%s tuning" begin end|} (id "8-02")));
        equal_strings "r" [ id "9-02" ] (Register.tagged_ids [ "test_that(\"" ] (Printf.sprintf {|test_that("%s calibration", { })|} (id "9-02")));
        equal_strings "out of range" [] (Register.tagged_ids [ "\"" ] {|"P99-01-01 x" "P00-00-01 y"|}));
    case "P00-03-03 synchronizing marks tested capabilities done and untested ones planned" (fun () ->
        let all = Register.all_capabilities (Lazy.force pillars) in
        let first = (List.hd all).Register.id and last = (List.nth all (List.length all - 1)).Register.id in
        let synced = Register.sync (Lazy.force pillars) [ (first, "ocaml/test/a.ml"); (first, "ocaml/test/b.ml"); (last, "r/x/tests/t.R") ] in
        let find id = List.find (fun (c : Register.capability) -> c.Register.id = id) (Register.all_capabilities synced) in
        equal_string "done" "done" (find first).Register.status;
        equal_strings "files" [ "ocaml/test/a.ml"; "ocaml/test/b.ml" ] (find first).Register.tests;
        equal_string "last" "done" (find last).Register.status;
        equal_int "everything else is planned" (List.length all - 2)
          (List.length (List.filter (fun (c : Register.capability) -> c.Register.status = "planned") (Register.all_capabilities synced))));
    case "P00-03-03 the summary counts every capability once, under the language of its group" (fun () ->
        let all = Register.all_capabilities (Lazy.force pillars) in
        let by_language = List.map Register.capabilities_by_language (Lazy.force pillars) in
        equal_int "every capability is counted" (List.length all)
          (List.fold_left (fun n counts -> List.fold_left (fun n (_, k) -> n + k) n counts) 0 by_language);
        equal_bool "the four languages, in order" true (List.for_all (fun counts -> List.map fst counts = Register.languages) by_language);
        equal_bool "each language has capabilities" true
          (List.for_all (fun language -> List.exists (fun counts -> List.assoc language counts > 0) by_language) Register.languages);
        let p = List.hd (Lazy.force pillars) in
        let shell = { (List.hd p.Register.groups) with Register.language = "OCaml, shell" } in
        equal_string "a group in two languages counts under the first" "OCaml" (Register.primary_language shell);
        equal_string "the summary file is the one the register gives" (Support.read "registry/status.json")
          (Json.pretty (Register.status_json (Lazy.force pillars)) ^ "\n"));
    case "P00-03-03 the checker reports each kind of disagreement" (fun () ->
        (* The last capability of the register stands in for one that is not built: the test edits
           a copy of the register and never the files. *)
        let all = Register.all_capabilities (Lazy.force pillars) in
        let target = (List.nth all (List.length all - 1)).Register.id in
        let group_of_target = String.sub target 0 6 in
        let edit ?path f =
          List.map
            (fun (p : Register.pillar) ->
              { p with
                Register.groups =
                  List.map
                    (fun (g : Register.group) ->
                      { g with
                        Register.path = (match path with Some path when g.Register.group_id = group_of_target -> path | _ -> g.Register.path);
                        capabilities = List.map (fun (c : Register.capability) -> if c.Register.id = target then f c else c) g.Register.capabilities })
                    p.Register.groups })
            (Lazy.force pillars)
        in
        let other_tags = List.filter (fun (id, _) -> id <> target) (Lazy.force tags) in
        let planned = edit (fun c -> { c with Register.status = "planned"; tests = [] }) in
        contains "a test names a capability that is not marked done"
          (String.concat "\n" (Register.check root planned (other_tags @ [ (target, "ocaml/test/missing.ml") ])))
          (target ^ " is named by ocaml/test/missing.ml but its status is 'planned'");
        contains "a test names a capability that is not in the register"
          (String.concat "\n" (Register.check root (Lazy.force pillars) (Lazy.force tags @ [ ("P18-10-" ^ "24", "ocaml/test/missing.ml") ])))
          "ocaml/test/missing.ml names an unknown capability";
        let tampered =
          edit ~path:"no/such/directory/" (fun c -> { c with Register.status = "done"; title = "Changed title"; tests = [ "ocaml/test/missing.ml" ] })
        in
        let text = String.concat "\n" (Register.check root tampered other_tags) in
        contains "title" text "identifiers or titles differ from registry/titles.sha256";
        contains "missing test" text (target ^ " lists a test file that does not exist: ocaml/test/missing.ml");
        contains "path" text (group_of_target ^ " has done capabilities but its path does not exist"));
    case "group paths may list alternatives and patterns" (fun () ->
        equal_strings "alternatives" [ "sdk/"; "ocaml/lib/client/"; "r/maithic/" ] (Register.path_alternatives "sdk/, ocaml/lib/client/ and r/maithic/");
        check "directory" (Register.path_exists root "ocaml/lib/gateway/core/");
        check "pattern" (Register.path_exists root "ocaml/lib/gateway/core/gateway_*.ml");
        check "missing" (not (Register.path_exists root "ocaml/lib/nowhere/" || Register.path_exists root "ocaml/lib/gateway/core/zzz-*.R"))) ]

let lines_of findings = List.map (fun (f : Hygiene.finding) -> Printf.sprintf "%s:%d" f.Hygiene.check f.Hygiene.line) findings

let hygiene_tests =
  [ case "unfinished-work markers are found in code as whole words" (fun () ->
        let marker = "TO" ^ "DO" in
        equal_strings "found" [ "placeholder:2"; "placeholder:3" ]
          (lines_of (Hygiene.placeholder_findings "lib/a.ml" (Printf.sprintf "let x = 1\n(* %s: finish *)\nlet f () = failwith \"not imple%s\"\nlet %sS = 2\n" marker "mented" marker)));
        equal_strings "documents are not code" [] (lines_of (Hygiene.placeholder_findings "docs/roadmap.md" ("- " ^ marker ^ " later\n"))));
    case "Lean libraries may not contain an unfinished proof or a new axiom" (fun () ->
        let text = "theorem a : True := by\n  sorry\naxiom b : False\n-- sorry in a comment\ndef c := \"sorry in a string\"\ntheorem usesSorryName : True := trivial\n" in
        equal_strings "library" [ "placeholder:2"; "placeholder:3" ] (lines_of (Hygiene.lean_findings "lean/Maithic/X.lean" text));
        equal_strings "test fixtures are outside the rule" [] (lines_of (Hygiene.lean_findings "lean/MaithicTest/X.lean" text)));
    case "key-shaped strings and private keys are found in any text" (fun () ->
        let leaked = "token = \"" ^ Support.fake_key "sk" ^ "\"\n-----BEGIN PRIV" ^ "ATE KEY-----\nplain line\n" in
        equal_strings "found" [ "secret:1"; "secret:2" ] (lines_of (Hygiene.secret_findings "config/x.toml" leaked));
        equal_strings "ordinary text" [] (lines_of (Hygiene.secret_findings "a.md" "Set ANTHROPIC_API_KEY to your key, for example in ~/.profile.\n")));
    case "statements about how the repository was written are found" (fun () ->
        (* The verbs are assembled here so that no line of this file is itself such a statement. *)
        let verb stem ending = stem ^ ending in
        let flagged =
          [ Printf.sprintf "This module was %s by an AI assistant." (verb "writ" "ten");
            Printf.sprintf "Code %s with ChatGPT." (verb "gener" "ated");
            "Co-" ^ "Authored-By: someone";
            Printf.sprintf "Docs %s using a large language model" (verb "draf" "ted");
            Printf.sprintf "Largely %s by Claude." (verb "prod" "uced") ]
        in
        List.iter (fun line -> check line (Hygiene.neutrality_findings "docs/a.md" line <> [])) flagged);
    case "sentences about the product's own use of models are not flagged" (fun () ->
        let fine =
          [ "Candidates proposed by the model are checked by the kernel."; "Requests are built by the Anthropic adapter.";
            "The xAI API follows the OpenAI request shapes."; "Maithic supports Anthropic, OpenAI and xAI as model providers.";
            "A proof found with Claude is still re-checked in a fresh process."; "AI proposes. The kernel decides.";
            "Verified implementations of AI-generated algorithms."; "This table is generated by `maithic config reference`.";
            "The answer was produced by the provider's model." ]
        in
        List.iter (fun line -> equal_strings line [] (lines_of (Hygiene.neutrality_findings "docs/a.md" line))) fine);
    case "a path below the home directory of a user is found in any text" (fun () ->
        (* The paths are assembled here so that no line of this file holds one. *)
        let home = "/ho" ^ "me/" and users = "/Us" ^ "ers/" in
        let recorded = Printf.sprintf "bridge  ok  (%salice/src/maithic/lean/.lake/build/bin/maithic-bridge)
plain line
<text>%sbob/work/x.lean</text>
" home users in
        equal_strings "found" [ "path:1"; "path:3" ] (lines_of (Hygiene.machine_path_findings "assets/readme/term.svg" recorded));
        equal_strings "a home that is only named, and paths written with a tilde" []
          (lines_of (Hygiene.machine_path_findings "docs/a.md" (Printf.sprintf "HOME=%su
Keys are read from ~/.config/maithic.
The directory %s holds the homes.
" home home))));
    case "source files need the copyright notice" (fun () ->
        equal_strings "missing" [ "header:1" ] (lines_of (Hygiene.header_findings "ocaml/lib/a.ml" "let x = 1\n"));
        equal_strings "present" [] (lines_of (Hygiene.header_findings "ocaml/lib/a.ml" "(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)\n"));
        equal_strings "examples and fixtures are exempt" []
          (lines_of (Hygiene.header_findings "examples/a.lean" "theorem t : True := trivial\n" @ Hygiene.header_findings "fixtures/x/a.lean" "")));
    case "relative Markdown links must lead to a file of the repository" (fun () ->
        let text = "See [the plan](ROADMAP.md), [a section](docs/install.md#steps), [the web](https://example.org) and [nothing](docs/nowhere.md).\n" in
        equal_strings "broken" [ "link:1" ] (lines_of (Hygiene.link_findings root "README.md" text));
        equal_strings "relative to the file" [] (lines_of (Hygiene.link_findings root "docs/reference/gateway.md" "[errors](errors.md) and [config](../../config/models.toml)\n"));
        equal_strings "only Markdown" [] (lines_of (Hygiene.link_findings root "a.ml" "[x](missing.md)\n")));
    case "P00-03-01 every package carries the version of PROJECT.toml" (fun () ->
        let files = Repo.files root in
        equal_strings "findings" [] (lines_of (Hygiene.version_findings root files));
        let sources = Hygiene.version_sources root files in
        check "the OCaml, Lean, Julia and R packages are all covered"
          (List.for_all
             (fun prefix -> List.exists (fun (file, _) -> Text.starts_with ~prefix file) sources)
             [ "ocaml/dune-project"; "ocaml/lib/schema/version.ml"; "lean/lakefile.toml"; "lean/Maithic/Version.lean"; "julia/"; "r/" ]);
        check "all equal" (List.for_all (fun (_, v) -> v = Some Maithic_schema.Version.version) sources));
    case "the repository passes every hygiene check" (fun () -> equal_strings "findings" [] (lines_of (Hygiene.run root))) ]

let readme_tests =
  [ case "finds HTML and Markdown images with their alternative text" (fun () ->
        let text = "# Title\n<p><img src=\"assets/a.svg\" alt=\"Diagram of A\" width=\"400\"> <img src=\"assets/b.png\"></p>\n![Chart of C](assets/c.svg \"title\") and ![](assets/d.svg)\n" in
        equal_strings "images" [ "assets/a.svg|Diagram of A"; "assets/b.png|"; "assets/c.svg|Chart of C"; "assets/d.svg|" ]
          (List.map (fun (i : Readmecheck.image) -> i.Readmecheck.src ^ "|" ^ i.Readmecheck.alt) (Readmecheck.images text)));
    case "reports missing files, missing alternative text, remote images and too few images" (fun () ->
        let dir = temp_dir "maithic-readme" in
        ignore (is_ok "w" (Fs.write Fs.(dir / "assets" / "a.svg") "<svg/>"));
        ignore (is_ok "w" (Fs.write Fs.(dir / "README.md") "![A](assets/a.svg)\n![](assets/a.svg)\n![Gone](assets/gone.svg)\n![Badge](https://img.example/badge.svg)\n"));
        let report = Readmecheck.check ~min_images:3 dir in
        let text = String.concat "\n" report.Readmecheck.problems in
        contains "missing file" text "README.md:3: image file assets/gone.svg does not exist";
        contains "no alt" text "README.md:2: image assets/a.svg has no alternative text";
        contains "remote" text "README.md:4: image https://img.example/badge.svg is not a file of the repository";
        contains "count" text "shows 2 distinct images; PROJECT.toml asks for at least 3";
        ignore (is_ok "w" (Fs.write Fs.(dir / "README.md") "![A](assets/a.svg)\n"));
        equal_strings "clean" [] (Readmecheck.check ~min_images:1 dir).Readmecheck.problems;
        Fs.remove_tree dir);
    case "the README of the repository passes its image check" (fun () ->
        let min_images = Readmecheck.min_images_of_project (is_ok "project" (Repo.project root)) in
        equal_strings "problems" [] (Readmecheck.check ~min_images root).Readmecheck.problems) ]

let contract_tests =
  [ case "a schema name maps to one file under schemas" (fun () ->
        let file name = Option.value (Contracts.schema_file name) ~default:"-" in
        equal_strings "files"
          [ "schemas/prove/run.schema.json"; "schemas/oracle/certificate.schema.json"; "schemas/bridge/tactic-outcome.schema.json";
            "schemas/prove/run.v2.schema.json"; "-"; "-"; "-"; "-"; "-" ]
          (List.map file
             [ "maithic.prove.run/1"; "maithic.oracle.certificate/1"; "maithic.bridge.tactic.outcome/1"; "maithic.prove.run/2"; "prove.run/1";
               "maithic.prove/1"; "maithic.prove.run"; "maithic.prove.run/0"; "maithic..run/1" ]));
    case "a document is held to the schema it names" (fun () ->
        let dir = temp_dir "contracts" in
        let write rel text = ignore (is_ok rel (Fs.write Fs.(dir / rel) text)) in
        write "PROJECT.toml" "[project]\nname = \"t\"\n";
        write "schemas/demo/thing.schema.json"
          {|{"title":"Thing","description":"A thing.","type":"object","properties":{"schema":{"const":"maithic.demo.thing/1"},"n":{"type":"integer","minimum":0}},"required":["schema","n"],"additionalProperties":false}|};
        write "fixtures/good.json" {|{"schema":"maithic.demo.thing/1","n":3}|};
        write "fixtures/unrelated.json" {|{"anything":"goes","schema":"someone.elses/1"}|};
        let clean = Contracts.run dir in
        equal_strings "no problem" [] clean.Contracts.problems;
        equal_int "schemas" 1 clean.Contracts.schema_files;
        equal_int "documents that name a schema" 2 clean.Contracts.documents;
        write "fixtures/bad.json" {|{"schema":"maithic.demo.thing/1","n":-1,"extra":true}|};
        write "fixtures/unpublished.json" {|{"schema":"maithic.demo.other/1"}|};
        write "fixtures/misnamed.json" {|{"schema":"maithic.demo/1"}|};
        write "fixtures/broken.json" "{not json";
        write "schemas/demo/notes.txt" "stray";
        write "schemas/demo/untitled.schema.json" {|{"type":"object","tipe":"x"}|};
        let problems = String.concat "\n" (Contracts.run dir).Contracts.problems in
        contains "a value out of range" problems "fixtures/bad.json: $.n: must be at least 0";
        contains "a member that is not allowed" problems {|fixtures/bad.json: $: the member "extra" is not allowed|};
        contains "an unpublished schema" problems "fixtures/unpublished.json: names the schema maithic.demo.other/1, which is not published";
        contains "a malformed name" problems "fixtures/misnamed.json: the schema name";
        contains "a file that is not JSON" problems "fixtures/broken.json:";
        contains "a stray file" problems "schemas/demo/notes.txt: only schema files";
        contains "a schema without a title" problems "schemas/demo/untitled.schema.json: a schema needs a title and a description";
        contains "a schema with a mistake" problems {|schemas/demo/untitled.schema.json: $: the keyword "tipe" is not known|});
    case "recorded Bridge responses are held to the protocol" (fun () ->
        let s = Contracts.schemas root in
        let line response = Printf.sprintf {|{"request":{"method":"runCommand"},"response":%s}|} response in
        equal_strings "a well-formed error" []
          (Contracts.transcript s "t.jsonl" (line {|{"jsonrpc":"2.0","id":1,"error":{"code":-32001,"message":"unknown environment 9","data":{"code":"MTH-BRG-0102"}}}|}));
        let problems text = String.concat "\n" (Contracts.transcript s "t.jsonl" text) in
        contains "neither result nor error" (problems (line {|{"jsonrpc":"2.0","id":1}|})) "t.jsonl:1: $: matches 0 of the 2 alternatives";
        contains "a malformed error code" (problems (line {|{"jsonrpc":"2.0","id":1,"error":{"code":-1,"message":"m","data":{"code":"E1"}}}|})) "$.error.data.code";
        contains "a report with a member missing"
          (problems (line {|{"jsonrpc":"2.0","id":1,"result":{"report":{"ok":true,"errors":0,"warnings":0,"diagnostics":[],"decls":[]}}}|}))
          {|t.jsonl:1 report: $: the member "elapsedMs" is missing|};
        contains "the line is counted" (problems ("\n" ^ line {|{"jsonrpc":"1.0","id":1,"result":{}}|})) "t.jsonl:2:";
        contains "an entry without a response" (problems {|{"request":{}}|}) "has no response");
    case "every document of the repository conforms to the schema it names" (fun () ->
        let summary = Contracts.run root in
        equal_strings "problems" [] summary.Contracts.problems;
        check "the schemas are there" (summary.Contracts.schema_files >= 19);
        check "documents were checked" (summary.Contracts.documents >= 90 && summary.Contracts.transcripts >= 8)) ]

(* ---- pictures of terminal sessions ---- *)

let picture_tests =
  let text line = String.concat "" (List.map (fun (s : Term_card.span) -> s.Term_card.text) line) in
  let plain text = [ { Term_card.text; fill = Term_card.mist; bold = false } ] in
  [ case "a line that fits a terminal is left as it is, and a longer one goes on between words" (fun () ->
        equal_strings "fits" [ "one two three" ] (List.map text (Term_card.wrap 60 (plain "one two three")));
        let long = "error: " ^ String.concat " " (List.init 20 (fun i -> Printf.sprintf "word%02d" i)) in
        let pieces = List.map text (Term_card.wrap 60 (plain long)) in
        equal_int "pieces" 3 (List.length pieces);
        check "no piece is wider than the terminal" (List.for_all (fun piece -> Text.utf8_length piece <= 60) pieces);
        check "later pieces are set in by four characters" (List.for_all (fun piece -> Text.starts_with ~prefix:"    word" piece) (List.tl pieces));
        (* nothing is lost and no word is cut: the words of the pieces are the words of the line *)
        let words s = List.filter (fun w -> w <> "") (String.split_on_char ' ' s) in
        equal_strings "the words" (words long) (List.concat_map words pieces));
    case "a long line with columns goes on under its last column" (fun () ->
        let row = "  ! traps      flagged  " ^ String.concat " " (List.init 16 (fun i -> Printf.sprintf "reason%02d" i)) in
        let pieces = List.map text (Term_card.wrap 80 (plain row)) in
        check "more than one piece" (List.length pieces > 1);
        check "under the last column" (List.for_all (fun piece -> Text.starts_with ~prefix:(String.make 24 ' ' ^ "reason") piece) (List.tl pieces));
        check "no piece is wider than the terminal" (List.for_all (fun piece -> Text.utf8_length piece <= 80) pieces));
    case "a line without a space is cut at the width, and colours are kept" (fun () ->
        let pieces = List.map text (Term_card.wrap 60 (plain (String.make 150 'x'))) in
        equal_strings "pieces" [ String.make 60 'x'; "    " ^ String.make 56 'x'; "    " ^ String.make 34 'x' ] pieces;
        let coloured = Term_card.lines_of_ansi ("\027[32mok\027[0m " ^ String.concat " " (List.init 30 (fun _ -> "word"))) in
        let wrapped = List.concat_map (Term_card.wrap 60) coloured in
        check "the first span keeps its colour" (match wrapped with (first :: _) :: _ -> first.Term_card.text = "ok" && first.Term_card.fill = Term_card.green | _ -> false);
        (* characters of more than one byte count as one, and are never cut in two *)
        let arrows = String.concat " " (List.init 40 (fun _ -> "\xE2\x86\x92x")) in
        check "arrows" (List.for_all (fun piece -> Text.utf8_length piece <= 60) (List.map text (Term_card.wrap 60 (plain arrows)))));
    case "the picture holds every line of the session and names itself" (fun () ->
        let svg =
          Term_card.render ~columns:60 ~title:"a title" ~label:"A terminal session: one command."
            [ { Term_card.command = "maithic version"; output = "maithic 0.1.0\n" ^ String.concat " " (List.init 30 (fun _ -> "word")) ^ "\n" } ]
        in
        contains "the label" svg "aria-label=\"A terminal session: one command.\"";
        contains "the command" svg "maithic version";
        contains "the output" svg "maithic 0.1.0";
        (* the prompt and the command are two runs, each at its column and with its length *)
        contains "the prompt" svg "<text x=\"22.00\" y=\"58\" textLength=\"7.83\" lengthAdjust=\"spacingAndGlyphs\" fill=\"#2CC4B6\" font-weight=\"bold\">$</text>";
        contains "the command at the third column" svg "<text x=\"37.66\" y=\"58\" textLength=\"117.45\" lengthAdjust=\"spacingAndGlyphs\" fill=\"#E8E9F3\" font-weight=\"bold\">maithic version</text>";
        let count sub text = List.length (Text.split_on sub text) - 1 in
        equal_int "every run states its length" (count "<text x=" svg - 1) (count " textLength=" svg);
        check "no layout is left to how a browser treats spaces" (not (Text.contains svg "xml:space") && not (Text.contains svg "  </text>"));
        check "no mark of a cut line" (not (Text.contains svg "\xE2\x80\xA6")));
    case "a line is drawn as runs that begin at their columns" (fun () ->
        let runs line = List.map (fun (column, (s : Term_card.span)) -> (column, s.Term_card.text)) (Term_card.runs line) in
        check "a table row: every cell is a run at its column, and a single space stays inside a run"
          (runs (plain "name    status  the last column") = [ (0, "name"); (8, "status"); (16, "the last column") ]);
        check "spaces at the ends are not drawn, and are counted" (runs (plain "    set in  ") = [ (4, "set in") ]);
        check "an empty line has no run" (runs (plain "   ") = [] && runs [] = []);
        (* a change of colour begins a run, and the column goes on counting through it *)
        let coloured = [ { Term_card.text = "ok "; fill = Term_card.green; bold = false }; { Term_card.text = "\xE2\x8A\xA2 x"; fill = Term_card.mist; bold = false } ] in
        check "colours" (runs coloured = [ (0, "ok"); (3, "\xE2\x8A\xA2 x") ]);
        check "a character of several bytes is one column"
          (runs (plain "\xE2\x88\x80 n  : Nat") = [ (0, "\xE2\x88\x80 n"); (5, ": Nat") ])) ]

let () =
  run "tools"
    [ ("language share", langshare_tests); ("capability register", register_tests); ("hygiene", hygiene_tests); ("readme", readme_tests);
      ("contracts", contract_tests); ("pictures", picture_tests) ]
