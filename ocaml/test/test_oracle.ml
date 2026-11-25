(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the oracle core. Julia is not needed: the process that answers a goal is played by a
   function, and the goals, certificates and outcomes are the fixtures that the Julia package
   writes. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_oracle
open Testkit

let fixture rel = Json.parse_exn (Support.read ("fixtures/oracle/" ^ rel))
let files dir = List.sort compare (List.filter (fun f -> Fs.extension f = ".json") (Fs.list_dir (Support.path ("fixtures/oracle/" ^ dir))))
let schema rel = is_ok rel (Json_schema.load (Support.path ("schemas/oracle/" ^ rel)))
let conforms what s j = match Json_schema.conforms s j with Ok () -> () | Error e -> fail (what ^ ":\n" ^ e)

let goal ?(kind = Oracle_goal.Linear) ?(domain = Oracle_goal.Integers) hypotheses target =
  { Oracle_goal.kind; domain; variables = [ "x"; "y" ];
    hypotheses = List.mapi (fun i statement -> { Oracle_goal.name = Printf.sprintf "h%d" (i + 1); statement }) hypotheses; goal = target }

(* An oracle made for a test. *)
let fake name kinds domains answer : (module Oracle_registry.ORACLE) =
  (module struct
    let name = name
    let description = "a test oracle"
    let kinds = kinds
    let domains = domains
    let solve ?timeout:_ _ = answer
  end)

let registry_tests =
  let open Oracle_goal in
  let linear = fake "linear" [ Linear ] [ Integers; Rationals ] (Ok (Oracle_outcome.Unknown "first"))
  and both = fake "general" [ Linear; Polynomial ] [ Rationals ] (Ok (Oracle_outcome.Unknown "second")) in
  [ case "P13-01-01 an oracle is a name, the goals it takes and a function that answers" (fun () ->
        let (module O : Oracle_registry.ORACLE) = linear in
        equal_string "name" "linear" O.name;
        check "kinds and number types" (O.kinds = [ Linear ] && O.domains = [ Integers; Rationals ]);
        check "it takes a linear goal over the integers" (Oracle_registry.accepts linear (goal [] "false"));
        check "and no polynomial one" (not (Oracle_registry.accepts linear (goal ~kind:Polynomial [] "false")));
        check "nor one over the reals" (not (Oracle_registry.accepts linear (goal ~domain:Reals [] "false")));
        check "it answers" (O.solve (goal [] "false") = Ok (Oracle_outcome.Unknown "first")));
    case "P13-01-03 the registry finds oracles by name and by goal" (fun () ->
        let registry = Oracle_registry.create [ linear; both ] in
        equal_strings "names in the order of registration" [ "linear"; "general" ] (Oracle_registry.names registry);
        check "by name" (Option.is_some (Oracle_registry.find registry "general") && Oracle_registry.find registry "missing" = None);
        let names g = List.map (fun (module O : Oracle_registry.ORACLE) -> O.name) (Oracle_registry.for_goal registry g) in
        equal_strings "linear over the rationals: both" [ "linear"; "general" ] (names (goal ~domain:Rationals [] "false"));
        equal_strings "linear over the integers: one" [ "linear" ] (names (goal [] "false"));
        equal_strings "polynomial over the rationals: the other" [ "general" ] (names (goal ~kind:Polynomial ~domain:Rationals [] "false"));
        equal_strings "real: none" [] (names (goal ~kind:Real ~domain:Reals [] "false")));
    case "P13-01-03 a later oracle of the same name replaces the earlier" (fun () ->
        let replaced = Oracle_registry.create [ linear; both; fake "linear" [ Real ] [ Reals ] (Ok (Oracle_outcome.Unknown "new")) ] in
        equal_strings "names" [ "general"; "linear" ] (Oracle_registry.names replaced);
        check "the new one" (Oracle_registry.accepts (Option.get (Oracle_registry.find replaced "linear")) (goal ~kind:Real ~domain:Reals [] "false")));
    case "P13-01-03 the oracles that ship are linear, polynomial and real" (fun () ->
        let registry = Oracle_registry.builtin { Oracle_registry.julia = "julia"; project = "/nowhere" } in
        equal_strings "names" [ "linear"; "polynomial"; "real" ] (Oracle_registry.names registry);
        List.iter
          (fun file ->
            let g = is_ok file (Oracle_goal.of_json (fixture ("goals/" ^ file))) in
            equal_int ("exactly one oracle takes " ^ file) 1 (List.length (Oracle_registry.for_goal registry g)))
          (files "goals"));
    case "P13-01-07 asking records the oracle, the goal, the answer and the time" (fun () ->
        let registry = Oracle_registry.create [ linear; both ] in
        let asked = goal ~domain:Rationals [ "x >= 0" ] "x >= 1" in
        let result = is_ok "solve" (Oracle_registry.solve registry asked) in
        equal_string "the first oracle that takes the goal" "linear" result.Oracle_outcome.oracle;
        check "the goal" (result.Oracle_outcome.goal = asked);
        check "the answer" (result.Oracle_outcome.outcome = Oracle_outcome.Unknown "first");
        check "the time" (result.Oracle_outcome.seconds >= 0.0);
        contains "no oracle for a goal is an error"
          (is_error "none" (Oracle_registry.solve registry (goal ~kind:Real ~domain:Reals [] "false"))) "no oracle takes goals of kind real over real") ]

let hyp name ty = { Bridge_types.hyp_name = name; hyp_type = ty; hyp_value = None }
let lean_goal hypotheses target = { Bridge_types.goal_id = "g"; tag = ""; hypotheses; target; pretty = "" }

let export_tests =
  let ints = [ hyp "x" "Int"; hyp "y" "Int" ] in
  let translate = Oracle_export.translate ~domain:Oracle_goal.Integers ~variables:[ "x"; "y" ] in
  [ case "P13-01-04 comparisons are rewritten from Lean's notation" (fun () ->
        equal_string "at most" "2 * x + 4 * y <= 7" (is_ok "le" (translate "2 * x + 4 * y \xE2\x89\xA4 7"));
        equal_string "at least" "x + 2 * y >= 4" (is_ok "ge" (translate "x + 2 * y \xE2\x89\xA5 4"));
        equal_string "unequal" "x != y" (is_ok "ne" (translate "x \xE2\x89\xA0 y"));
        equal_string "powers, brackets and signs" "( x + y ) ^ 2 = x ^ 2 + 2 * x * y + y ^ 2 - - 1"
          (is_ok "poly" (translate "(x + y) ^ 2 = x ^ 2 + 2 * x * y + y ^ 2 - -1")));
    case "P13-01-04 what is not arithmetic over the variables is refused, with the reason" (fun () ->
        contains "a connective" (is_error "or" (translate "x = y \xE2\x88\xA8 x < y")) "\"\xE2\x88\xA8\", which is not arithmetic";
        contains "a cast" (is_error "cast" (translate "\xE2\x86\x91x < 3")) "which is not arithmetic";
        contains "a function" (is_error "name" (translate "f x < 3")) "the name f is not one of the variables";
        contains "no comparison" (is_error "term" (translate "x + y")) "it is not a comparison";
        contains "two comparisons" (is_error "chain" (translate "x < y = y")) "more than one comparison";
        contains "integer division" (is_error "div" (translate "x / 2 < 3")) "divides integers";
        equal_string "division is fine over the rationals" "x / 2 < 3"
          (is_ok "rat" (Oracle_export.translate ~domain:Oracle_goal.Rationals ~variables:[ "x" ] "x / 2 < 3")));
    case "P13-01-04 a goal of Lean becomes a goal for an oracle, with its names kept" (fun () ->
        let exported =
          is_ok "export"
            (Oracle_export.export
               (lean_goal (ints @ [ hyp "h\xE2\x82\x81" "2 * x + 4 * y < 7"; hyp "h\xE2\x82\x82" "x + 2 * y \xE2\x89\xA5 4" ]) "False"))
        in
        let g = exported.Oracle_export.goal in
        check "linear over the integers" (g.Oracle_goal.kind = Oracle_goal.Linear && g.Oracle_goal.domain = Oracle_goal.Integers);
        equal_strings "variables" [ "x"; "y" ] g.Oracle_goal.variables;
        equal_strings "hypothesis names as Lean has them" [ "h\xE2\x82\x81"; "h\xE2\x82\x82" ]
          (List.map (fun (h : Oracle_goal.hypothesis) -> h.Oracle_goal.name) g.Oracle_goal.hypotheses);
        equal_strings "statements" [ "2 * x + 4 * y < 7"; "x + 2 * y >= 4" ]
          (List.map (fun (h : Oracle_goal.hypothesis) -> h.Oracle_goal.statement) g.Oracle_goal.hypotheses);
        equal_string "False asks for a contradiction" "false" g.Oracle_goal.goal;
        check "nothing was left out" (exported.Oracle_export.skipped = []);
        conforms "the document" (schema "goal.schema.json") (Oracle_goal.to_json g));
    case "P13-01-04 hypotheses that say something else are left out, and said to be" (fun () ->
        let exported =
          is_ok "export"
            (Oracle_export.export
               (lean_goal (ints @ [ hyp "h" "x < y"; hyp "hp" "x = x \xE2\x88\xA8 True"; hyp "a\xE2\x9C\x9D" "y < 3"; hyp "p" "Prop" ]) "x + 1 \xE2\x89\xA4 y"))
        in
        equal_strings "kept" [ "h" ]
          (List.map (fun (h : Oracle_goal.hypothesis) -> h.Oracle_goal.name) exported.Oracle_export.goal.Oracle_goal.hypotheses);
        equal_strings "left out" [ "hp"; "a\xE2\x9C\x9D"; "p" ] (List.map fst exported.Oracle_export.skipped);
        contains "why" (List.assoc "a\xE2\x9C\x9D" exported.Oracle_export.skipped) "no name that a proof can refer to";
        equal_string "the target" "x + 1 <= y" exported.Oracle_export.goal.Oracle_goal.goal);
    case "P13-01-04 the kind follows from the statements" (fun () ->
        let kind hypotheses target = (is_ok "export" (Oracle_export.export (lean_goal (ints @ hypotheses) target))).Oracle_export.goal.Oracle_goal.kind in
        check "linear" (kind [ hyp "h" "2 * x + 3 \xE2\x89\xA4 y * 4" ] "x < y" = Oracle_goal.Linear);
        check "a product of variables" (kind [ hyp "h" "x * y = 2" ] "x < y" = Oracle_goal.Polynomial);
        check "a power" (kind [] "x ^ 2 \xE2\x89\xA5 0" = Oracle_goal.Polynomial);
        check "a number times a bracket is linear" (Oracle_export.is_linear "2 * ( x + 1 ) <= y");
        check "a bracket times a bracket is not" (not (Oracle_export.is_linear "( x + 1 ) * ( y + 2 ) <= 3")));
    case "P13-01-04 goals outside the fragment are refused as a whole" (fun () ->
        let refused hypotheses target = is_error "export" (Oracle_export.export (lean_goal hypotheses target)) in
        contains "naturals" (refused [ hyp "n" "Nat" ] "n + 1 > n") "variables over the natural numbers are not exported: their subtraction stops at zero";
        contains "no variables" (refused [ hyp "p" "Prop" ] "p") "no integer or rational variable";
        contains "mixed" (refused [ hyp "x" "Int"; hyp "q" "Rat" ] "x < 1") "mixes integer and rational";
        contains "the target" (refused ints "x < y \xE2\x88\xA7 y < x") "the target cannot be exported");
    case "P13-01-04 rational variables give a goal over the rationals" (fun () ->
        let exported = is_ok "export" (Oracle_export.export (lean_goal [ hyp "x" "Rat"; hyp "h" "x / 2 \xE2\x89\xA4 1" ] "x \xE2\x89\xA4 2")) in
        check "domain" (exported.Oracle_export.goal.Oracle_goal.domain = Oracle_goal.Rationals);
        equal_strings "division kept" [ "x / 2 <= 1" ]
          (List.map (fun (h : Oracle_goal.hypothesis) -> h.Oracle_goal.statement) exported.Oracle_export.goal.Oracle_goal.hypotheses)) ]

let run_with output = Oracle_registry.run ~runner:(fun ?timeout:_ ~stdin:_ () -> output)
let answered text = Ok { Proc.status = 0; stdout = text; stderr = "" }

let dispatch_tests =
  [ case "P13-01-05 a goal goes to the engine as a document and the answer comes back as an outcome" (fun () ->
        let sent = ref "" and limit = ref 0.0 in
        let runner ?timeout ~stdin () =
          sent := stdin;
          limit := Option.value ~default:0.0 timeout;
          answered (Support.read "fixtures/oracle/outcomes/linear-refuted.json")
        in
        let asked = goal ~domain:Oracle_goal.Rationals [ "2 * x + 3 * y <= 6" ] "x + y <= 2" in
        (match is_ok "run" (Oracle_registry.run ~timeout:7.0 ~runner asked) with
         | Oracle_outcome.Refuted model -> equal_strings "the point" [ "x = 3"; "y = 0" ] (List.map (fun (v, value) -> v ^ " = " ^ value) model)
         | _ -> fail "the fixture is a refutation");
        check "the goal that was sent is the goal that was asked" (Oracle_goal.of_json (Json.parse_exn !sent) = Ok asked);
        conforms "what was sent" (schema "goal.schema.json") (Json.parse_exn !sent);
        check "the time limit is passed on" (!limit = 7.0));
    case "P13-01-05 an engine that fails, runs out of time or writes nonsense is an error about the question" (fun () ->
        let asked = goal [] "false" in
        contains "status" (is_error "status" (run_with (Ok { Proc.status = 2; stdout = ""; stderr = "the goal could not be read: no goal\n" }) asked))
          "stopped with status 2: the goal could not be read";
        contains "time" (is_error "timeout" (run_with (Error "timed out after 60 s") asked)) "did not answer: timed out";
        contains "not JSON" (is_error "garbage" (run_with (answered "Segmentation fault") asked)) "not JSON";
        contains "not an outcome" (is_error "schema" (run_with (answered "{\"schema\": \"something/1\"}") asked)) "does not name the schema";
        contains "an outcome with no certificate" (is_error "empty" (run_with (answered "{\"schema\": \"maithic.oracle.outcome/1\", \"outcome\": \"proved\"}") asked))
          "no certificate");
    case "P13-01-05 the engine is the entry point of the Julia package, run without a registry" (fun () ->
        let engine = { Oracle_registry.julia = "julia"; project = "/src/julia/MaithicOracle" } in
        check "compiled"
          (Oracle_registry.command engine
           = ("julia", [ "--startup-file=no"; "--project=/src/julia/MaithicOracle"; "/src/julia/MaithicOracle/bin/solve.jl" ]));
        check "quick"
          (Oracle_registry.command ~quick:true engine
           = ("julia", [ "--startup-file=no"; "-O0"; "--compile=min"; "--project=/src/julia/MaithicOracle"; "/src/julia/MaithicOracle/bin/solve.jl" ]));
        check "the quick try is shorter than the default limit" (Oracle_registry.quick_seconds < Oracle_registry.default_timeout));
    case "P13-01-05 the package is found in the source tree, and its absence is said plainly" (fun () ->
        let here = Lazy.force Support.root in
        (match Proc.find_in_path "sh" with
         | None -> skip "no shell on the path to stand in for Julia"
         | Some _ ->
           let engine = is_ok "locate" (Oracle_registry.locate ~julia:"sh" ~cwd:Fs.(here / "ocaml" / "test") ()) in
           equal_string "project" Fs.(here / "julia" / "MaithicOracle") engine.Oracle_registry.project;
           contains "another directory" (is_error "nowhere" (Oracle_registry.locate ~julia:"sh" ~cwd:"/" ())) "was not found in the source tree";
           contains "a wrong project" (is_error "wrong" (Oracle_registry.locate ~julia:"sh" ~project:"/nowhere" ~cwd:here ())) "is not a Julia package directory");
        contains "no Julia" (is_error "julia" (Oracle_registry.locate ~julia:"no-such-julia" ~cwd:here ())) "is not on the path");
    case "P13-01-05 a package that was never used is set up once, without the network" (fun () ->
        match Proc.find_in_path "sh" with
        | None -> skip "no shell on the path to stand in for Julia"
        | Some _ ->
          let dir = Fs.temp_dir "maithic-engine" in
          (* A stand-in for Julia: it writes down how it was called and leaves a manifest behind. *)
          let stand_in body =
            let file = Fs.(dir / "julia-stand-in") in
            ignore (is_ok "write" (Fs.write file ("#!/bin/sh\nfor a in \"$@\"; do case \"$a\" in --project=*) p=\"${a#--project=}\";; esac; done\n" ^ body ^ "\n")));
            Unix.chmod file 0o755;
            file
          in
          let julia = stand_in "echo \"$@\" > \"$p/called\"; echo \"$JULIA_PKG_OFFLINE\" > \"$p/offline\"; : > \"$p/Manifest.toml\"" in
          let engine = { Oracle_registry.julia; project = dir } in
          check "the command resolves and installs the dependencies of the package"
            (Oracle_registry.setup_command engine
             = (julia, [ "--startup-file=no"; "--project=" ^ dir; "-e"; "using Pkg; Pkg.resolve(io = devnull); Pkg.instantiate(io = devnull)" ]));
          check "set up" (Oracle_registry.prepare engine = Ok ());
          contains "Julia was asked to resolve" (is_ok "called" (Fs.read Fs.(dir / "called"))) "Pkg.resolve(io = devnull); Pkg.instantiate(io = devnull)";
          equal_string "with the network switched off" "true" (String.trim (is_ok "offline" (Fs.read Fs.(dir / "offline"))));
          check "a manifest is there now" (Fs.exists Fs.(dir / "Manifest.toml"));
          Fs.remove Fs.(dir / "called");
          check "ready" (Oracle_registry.prepare engine = Ok ());
          check "with a manifest in place nothing is run" (not (Fs.exists Fs.(dir / "called")));
          Fs.remove Fs.(dir / "Manifest.toml");
          let broken = { engine with Oracle_registry.julia = stand_in "echo no registry >&2; exit 3" } in
          let why = is_error "a setup that fails" (Oracle_registry.prepare broken) in
          contains "what failed" why "could not be set up: Julia stopped with status 3: no registry";
          contains "what to do" why "make build-julia";
          Fs.remove_tree dir) ]

let record_tests =
  let certificate file = Option.get (Json.member "certificate" (fixture ("outcomes/" ^ file))) in
  [ case "P13-01-02 every certificate the engines write follows the schema of its kind" (fun () ->
        let s = schema "certificate.schema.json" in
        let kinds = ref [] in
        List.iter
          (fun file ->
            let j = fixture ("certificates/" ^ file) in
            conforms file s j;
            kinds := Json.string_or "" "kind" j :: !kinds)
          (files "certificates");
        List.iter
          (fun file ->
            match Json.member "certificate" (fixture ("outcomes/" ^ file)) with
            | Some j ->
              conforms file s j;
              kinds := Json.string_or "" "kind" j :: !kinds
            | None -> ())
          (files "outcomes");
        equal_int "kinds of certificate among the fixtures" 13 (List.length (List.sort_uniq compare !kinds)));
    case "P13-01-02 a certificate with a member changed is caught by the schema" (fun () ->
        let s = schema "certificate.schema.json" in
        let good = certificate "linear-integer-gap.json" in
        let without name = match good with Json.Obj members -> Json.Obj (List.filter (fun (k, _) -> k <> name) members) | j -> j in
        check "the fixture conforms" (Json_schema.conforms s good = Ok ());
        check "no claim" (Json_schema.conforms s (without "claim") <> Ok ());
        check "no data" (Json_schema.conforms s (without "data") <> Ok ());
        check "an unknown kind"
          (Json_schema.conforms s (match good with Json.Obj m -> Json.Obj (("kind", Json.String "linear.magic") :: List.remove_assoc "kind" m) | j -> j) <> Ok ()));
    case "P13-01-08 every goal fixture is read, and written back as it was read" (fun () ->
        equal_int "goal fixtures" 21 (List.length (files "goals"));
        List.iter
          (fun file ->
            let j = fixture ("goals/" ^ file) in
            conforms file (schema "goal.schema.json") j;
            let g = is_ok file (Oracle_goal.of_json j) in
            check (file ^ " survives the round trip") (Oracle_goal.of_json (Oracle_goal.to_json g) = Ok g);
            check (file ^ " keeps its statements") (Json.member "goal" j = Some (Json.String g.Oracle_goal.goal)))
          (files "goals"));
    case "P13-01-08 every outcome fixture is read as what its name says" (fun () ->
        List.iter
          (fun file ->
            let expected = if Text.contains file "refuted" then "refuted" else if Text.contains file "unknown" then "unknown" else "proved" in
            equal_string file expected (Oracle_outcome.label (is_ok file (Oracle_outcome.of_json (fixture ("outcomes/" ^ file))))))
          (files "outcomes"));
    case "P13-01-08 a goal document may leave out names, variables and the number type" (fun () ->
        let g = is_ok "short" (Oracle_goal.of_json (Json.parse_exn {|{"schema": "maithic.oracle.goal/1", "kind": "linear", "hypotheses": ["x >= 0", {"statement": "y >= x"}, {"name": "top", "statement": "y <= 9"}], "goal": "x <= 9"}|})) in
        equal_strings "names by position" [ "h1"; "h2"; "top" ] (List.map (fun (h : Oracle_goal.hypothesis) -> h.Oracle_goal.name) g.Oracle_goal.hypotheses);
        check "rationals by default" (g.Oracle_goal.domain = Oracle_goal.Rationals);
        let real = is_ok "real" (Oracle_goal.of_json (Json.parse_exn {|{"schema": "maithic.oracle.goal/1", "kind": "real", "goal": "x <= 9"}|})) in
        check "reals for a real goal" (real.Oracle_goal.domain = Oracle_goal.Reals);
        contains "another schema" (is_error "schema" (Oracle_goal.of_json (Json.parse_exn {|{"schema": "x/1", "kind": "linear", "goal": "false"}|}))) "does not name the schema";
        contains "an unknown kind" (is_error "kind" (Oracle_goal.of_json (Json.parse_exn {|{"schema": "maithic.oracle.goal/1", "kind": "magic", "goal": "false"}|}))) "linear, polynomial or real";
        contains "no goal" (is_error "goal" (Oracle_goal.of_json (Json.parse_exn {|{"schema": "maithic.oracle.goal/1", "kind": "linear"}|}))) "comparison or \"false\"");
    case "P13-01-07 a certificate is named by its content, not by who wrote it or in which order" (fun () ->
        let c = certificate "linear-integer-gap.json" in
        let id = Oracle_outcome.certificate_id c in
        equal_int "a SHA-256 in hexadecimal" 64 (String.length id);
        let reordered = match c with Json.Obj members -> Json.Obj (List.rev members) | j -> j in
        equal_string "member order does not matter" id (Oracle_outcome.certificate_id reordered);
        let other_engine =
          match c with Json.Obj m -> Json.Obj (("engine", Json.Obj [ ("name", Json.String "another") ]) :: List.remove_assoc "engine" m) | j -> j
        in
        equal_string "the engine record does not matter" id (Oracle_outcome.certificate_id other_engine);
        check "another certificate has another name" (id <> Oracle_outcome.certificate_id (certificate "linear-integer-bound.json")));
    case "P13-01-07 the record of a question follows its schema, whatever the answer" (fun () ->
        let s = schema "result.schema.json" in
        let asked = is_ok "goal" (Oracle_goal.of_json (fixture "goals/linear-integer-gap.json")) in
        let record outcome = Oracle_outcome.result_to_json { Oracle_outcome.oracle = "linear"; goal = asked; outcome; seconds = 0.25 } in
        let proved =
          match is_ok "outcome" (Oracle_outcome.of_json (fixture "outcomes/linear-integer-gap.json")) with
          | Oracle_outcome.Proved p -> Oracle_outcome.Proved { p with lean = Some { Oracle_outcome.check = None; replay = None; scripts = [ "omega" ] } }
          | _ -> fail "the fixture is a proof"
        in
        List.iter (fun j -> conforms "record" s j) [ record proved; record (Oracle_outcome.Refuted [ ("x", "-7/2") ]); record (Oracle_outcome.Unknown "why") ];
        let j = record proved in
        equal_string "outcome" "proved" (Json.string_or "" "outcome" j);
        equal_string "kind" "linear.farkas" (Json.string_or "" "kind" j);
        equal_int "time in milliseconds" 250 (Json.int_or 0 "elapsed_ms" j);
        check "the scripts for Lean" (Json.member "scripts" j = Some (Json.List [ Json.String "omega" ]));
        check "a record with no reason for an unknown answer does not conform"
          (Json_schema.conforms s (match record (Oracle_outcome.Unknown "why") with Json.Obj m -> Json.Obj (List.remove_assoc "reason" m) | j -> j) <> Ok ()));
    case "P13-01-06 a proved answer carries the scripts that hand its certificate to Lean" (fun () ->
        let answer =
          {|{"schema": "maithic.oracle.outcome/1", "outcome": "proved", "certificate": {"schema": "maithic.oracle.certificate/1", "kind": "linear.farkas", "claim": "c", "data": {}},
             "lean": {"check": "example : True := by trivial\n", "replay": null, "scripts": ["maithic_linear_int [x] [(1, ⟨[0], .le, -1⟩)] using [h]", "omega"]}}|}
        in
        (match is_ok "outcome" (Oracle_outcome.of_json (Json.parse_exn answer)) with
         | Oracle_outcome.Proved { kind; lean = Some lean; _ } ->
           equal_string "kind" "linear.farkas" kind;
           equal_int "two scripts, the replay of the certificate first" 2 (List.length lean.Oracle_outcome.scripts);
           contains "the checker's tactic" (List.hd lean.Oracle_outcome.scripts) "maithic_linear_int";
           check "no replay theorem" (lean.Oracle_outcome.replay = None);
           check "the statement for a decision procedure" (lean.Oracle_outcome.check = Some "example : True := by trivial\n")
         | _ -> fail "a proved outcome with members for Lean");
        (match is_ok "fixture" (Oracle_outcome.of_json (fixture "outcomes/linear-integer-gap.json")) with
         | Oracle_outcome.Proved { lean = None; _ } -> ()
         | _ -> fail "the fixtures carry no members for Lean")) ]

let () =
  run "oracle" [ ("registry", registry_tests); ("goal export", export_tests); ("dispatch", dispatch_tests); ("documents", record_tests) ]
