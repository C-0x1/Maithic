(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge
open Testkit

let ok what r = match r with Ok v -> v | Error e -> fail (what ^ ": " ^ Bridge_error.to_string e)
let err what r = match r with Error e -> e | Ok _ -> fail (what ^ ": expected an error")
let with_stub ?options ?timeout transcript f = ok "stub" (Bridge_client.with_bridge ?timeout (Support.stub ?options transcript) (fun b -> Ok (f b)))

let codec_tests =
  [ case "P09-04-01 a recorded report decodes into typed values" (fun () ->
        let line = List.nth (Text.lines (Support.read "fixtures/bridge/transcripts/whole-file.jsonl")) 0 in
        let report = Option.get (Json.path [ "response"; "result"; "report" ] (Json.parse_exn line)) in
        let r = is_ok "decode" (Bridge_types.report_of_json report) in
        check "not ok" (not r.Bridge_types.ok);
        equal_int "errors" 1 r.Bridge_types.errors;
        let d = List.hd r.Bridge_types.diagnostics in
        equal_string "severity" "error" d.Bridge_types.severity;
        equal_int "line" 7 d.Bridge_types.line;
        check "end position" (d.Bridge_types.end_line = Some 7);
        equal_strings "declarations" [ "t"; "u" ] (List.map (fun (d : Bridge_types.decl) -> d.Bridge_types.name) r.Bridge_types.decls);
        check "u is unfinished" (List.nth r.Bridge_types.decls 1).Bridge_types.uses_sorry;
        contains "error text" (Bridge_types.error_text r) "7:2: ");
    case "P09-04-01 reports encode and decode without loss" (fun () ->
        List.iter
          (fun name ->
            List.iter
              (fun line ->
                match Json.path [ "response"; "result"; "report" ] (Json.parse_exn line) with
                | None -> ()
                | Some j ->
                  let r = is_ok "decode" (Bridge_types.report_of_json j) in
                  check "round trip" (is_ok "again" (Bridge_types.report_of_json (Bridge_types.report_to_json r)) = r))
              (Text.lines (Support.read ("fixtures/bridge/transcripts/" ^ name ^ ".jsonl"))))
          [ "whole-file"; "environments"; "limits"; "escaping" ]);
    case "P09-04-01 a malformed report is a decoding error that names the member" (fun () ->
        contains "member" (is_error "decode" (Bridge_types.report_of_json (Json.parse_exn {|{"ok":true,"errors":0}|}))) "warnings") ]

let client_tests =
  [ case "P09-04-08 the client performs the handshake with a stub server" (fun () ->
        with_stub "environments" (fun b ->
            let h = Bridge_client.handshake b in
            equal_string "bridge" "stub" h.Bridge_types.bridge;
            equal_int "protocol" Maithic_schema.Version.bridge_protocol h.Bridge_types.protocol;
            check "open" (Bridge_client.is_open b)));
    case "P09-04-05 each request has a typed function" (fun () ->
        with_stub "environments" (fun b ->
            let env, cached = ok "loadHeader" (Bridge_client.load_header b [ "Init" ]) in
            equal_int "env" 1 env;
            check "first load is not cached" (not cached);
            let _, cached = ok "loadHeader" (Bridge_client.load_header b []) in
            check "second load is cached" cached;
            let report, derived = ok "runCommand" (Bridge_client.run_command ~keep:true b ~env:1 "def double (n : Nat) : Nat := n + n") in
            check "ok" report.Bridge_types.ok;
            check "derived handle" (derived = Some 2);
            equal_string "type" "Nat \xE2\x86\x92 Nat" (List.hd report.Bridge_types.decls).Bridge_types.type_;
            check "released" (ok "releaseEnv" (Bridge_client.release_env b 3))));
    case "P09-04-05 candidates come back as typed reports" (fun () ->
        with_stub "prove-even" (fun b ->
            ignore (ok "loadHeader" (Bridge_client.load_header b []));
            let statement = "theorem even_add_even (a b : Nat) (ha : a % 2 = 0) (hb : b % 2 = 0) : (a + b) % 2 = 0 := " in
            let candidates = List.map (fun p -> { Bridge_types.source = statement ^ p; untrusted = p }) [ "by rfl"; "by sorry"; "by omega"; "by simp" ] in
            let results = ok "tryCandidates" (Bridge_client.try_candidates ~names:[ "even_add_even" ] ~stop_on_first:true b ~env:1 candidates) in
            equal_int "stops at the first success" 3 (List.length results);
            let third = List.nth results 2 and second = List.nth results 1 in
            check "guard rejected sorry" (not second.Bridge_types.admitted && Text.contains second.Bridge_types.rejection "sorry");
            let report = Option.get third.Bridge_types.report in
            check "omega accepted" report.Bridge_types.ok;
            equal_strings "axioms" [ "propext"; "Quot.sound" ] (List.hd report.Bridge_types.decls).Bridge_types.axioms));
    case "P09-04-05 proof states are opened and read through typed functions" (fun () ->
        with_stub "proof-states" (fun b ->
            ignore (ok "loadHeader" (Bridge_client.load_header b []));
            let report, opened = ok "startProof" (Bridge_client.start_proof b ~env:1 (`Statement "theorem add_zero' (n : Nat) : n + 0 = n")) in
            check "the statement elaborates" report.Bridge_types.ok;
            (match opened with
             | [ { Bridge_types.state; at_line; at_column; goals = [ goal ] } ] ->
               equal_int "handle" 2 state;
               equal_int "line of the sorry" 2 at_line;
               equal_int "column of the sorry" 2 at_column;
               equal_string "target" "n + 0 = n" goal.Bridge_types.target;
               equal_string "as Lean prints it" "n : Nat\n\xE2\x8A\xA2 n + 0 = n" (Bridge_types.goal_text goal);
               check "hypothesis" (goal.Bridge_types.hypotheses = [ { Bridge_types.hyp_name = "n"; hyp_type = "Nat"; hyp_value = None } ])
             | _ -> fail "expected one state with one goal");
            let both = "theorem both (p q : Prop) (hp : p) (hq : q) : p \xE2\x88\xA7 q := by\n  constructor\n  \xC2\xB7 sorry\n  \xC2\xB7 sorry" in
            let _, branches = ok "startProof" (Bridge_client.start_proof b ~env:1 (`Source both)) in
            equal_strings "one state per sorry, with its case" [ "left"; "right" ]
              (List.concat_map (fun (o : Bridge_types.opened_proof) -> List.map (fun g -> g.Bridge_types.tag) o.goals) branches);
            let root = ok "getState" (Bridge_client.get_state b 4) and child = ok "getState" (Bridge_client.get_state b 6) in
            check "a root has no parent" (root.Bridge_types.parent = None && root.depth = 0 && not root.solved);
            check "a derived state names its parent" (child.Bridge_types.parent = Some 4 && child.depth = 1 && child.solved);
            let with_let = "theorem with_let : \xE2\x88\x80 n : Nat, n = n := by\n  intro n\n  let m := n + 1\n  sorry" in
            let _, lets = ok "startProof" (Bridge_client.start_proof b ~env:1 (`Source with_let)) in
            let hypotheses = (List.hd (List.hd lets).Bridge_types.goals).Bridge_types.hypotheses in
            check "a let shows its value" ((List.nth hypotheses 1).Bridge_types.hyp_value = Some "n + 1");
            let broken, none = ok "startProof" (Bridge_client.start_proof b ~env:1 (`Statement "theorem broken : (1 : Nat) = \"a\"")) in
            check "a statement with an error opens nothing" ((not broken.Bridge_types.ok) && none = []);
            check "released once" (ok "release" (Bridge_client.release_state b 2));
            check "and only once" (not (ok "release" (Bridge_client.release_state b 2)))));
    case "P09-04-05 a tactic gives a new state or says where it was stopped" (fun () ->
        with_stub "proof-states" (fun b ->
            (match ok "rfl" (Bridge_client.run_tactic b ~state:2 "rfl") with
             | Bridge_types.Stepped { state; goals; solved; _ } -> check "closed" (state = 3 && goals = [] && solved)
             | Bridge_types.Stopped { error; _ } -> fail error);
            (match ok "wrong" (Bridge_client.run_tactic b ~state:4 "exact hq") with
             | Bridge_types.Stopped { kind = Bridge_types.Tactic_failure; error; _ } -> contains "Lean's error" error "Type mismatch"
             | _ -> fail "expected a tactic failure");
            (match ok "focus" (Bridge_client.run_tactic ~goal:5 b ~state:8 "omega") with
             | Bridge_types.Stopped { kind = Bridge_types.Tactic_failure; error; _ } -> equal_string "error" "there is no goal 5; the state has 2 goal(s)" error
             | _ -> fail "expected a failure for a goal that is not there");
            (match ok "budget" (Bridge_client.run_tactic ~limits:{ Bridge_types.no_limits with Bridge_types.max_heartbeats = 1 } b ~state:13 "grind") with
             | Bridge_types.Stopped { error; _ } -> contains "budget" error "maximum number of heartbeats"
             | _ -> fail "expected the step budget to stop grind");
            let outcomes = ok "batch" (Bridge_client.try_tactics ~goal:0 b ~state:8 [ "rfl"; "#eval 1"; "exact ("; "omega"; "sorry" ]) in
            let shape = function
              | Bridge_types.Stepped { solved; goals; _ } -> Printf.sprintf "stepped:%b:%d" solved (List.length goals)
              | Bridge_types.Stopped { kind; _ } -> "stopped:" ^ Bridge_types.tactic_failure_kind_to_string kind
            in
            equal_strings "one outcome per tactic, in order"
              [ "stepped:false:1"; "stopped:guard"; "stopped:parse"; "stepped:false:1"; "stopped:guard" ] (List.map shape outcomes);
            (match outcomes with
             | Bridge_types.Stepped { goals = [ goal ]; _ } :: _ ->
               equal_string "the goal left over" "succ" goal.Bridge_types.tag;
               equal_strings "names as Lean shows them" [ "n\xE2\x9C\x9D"; "a\xE2\x9C\x9D" ]
                 (List.map (fun h -> h.Bridge_types.hyp_name) goal.Bridge_types.hypotheses)
             | _ -> fail "expected the first tactic to leave one goal");
            let stopped_early = ok "batch" (Bridge_client.try_tactics ~stop_on_solved:true b ~state:8 [ "all_goals omega"; "rfl" ]) in
            equal_strings "the batch ends at the first solution" [ "stepped:true:0" ] (List.map shape stopped_early);
            match err "unknown" (Bridge_client.run_tactic b ~state:999 "rfl") with
            | Bridge_error.Remote { code; tag; _ } ->
              equal_int "code" (-32003) code;
              equal_string "tag" "MTH-BRG-0104" tag
            | other -> fail (Bridge_error.to_string other)));
    case "P09-04-02 what the codecs write conforms to the published schemas" (fun () ->
        let schema rel = is_ok rel (Maithic_schema.Json_schema.load (Support.path ("schemas/bridge/" ^ rel))) in
        let conforms rel j = is_ok rel (Maithic_schema.Json_schema.conforms (schema rel) j) in
        let diagnostic = { Bridge_types.severity = "error"; line = 3; column = 0; end_line = Some 3; end_column = None; text = "unsolved goals" } in
        let decl = { Bridge_types.name = "t"; found = true; kind = "theorem"; type_ = "True"; axioms = [ "propext" ]; uses_sorry = false } in
        let report = { Bridge_types.ok = false; errors = 1; warnings = 0; diagnostics = [ diagnostic ]; decls = [ decl ]; elapsed_ms = 12 } in
        conforms "report.schema.json" (Bridge_types.report_to_json report);
        conforms "report.schema.json" (Bridge_types.report_to_json { report with Bridge_types.diagnostics = []; decls = [] });
        let goal = { Bridge_types.goal_id = "_uniq.7"; tag = ""; target = "p"; pretty = "\xE2\x8A\xA2 p";
                     hypotheses = [ { Bridge_types.hyp_name = "m"; hyp_type = "Nat"; hyp_value = Some "n + 1" } ] } in
        conforms "goal.schema.json" (Bridge_types.goal_to_json goal);
        (* The schema is strict where the codec is: a report without its timing is refused by both. *)
        let incomplete = Json.parse_exn {|{"ok":true,"errors":0,"warnings":0,"diagnostics":[],"decls":[]}|} in
        check "the schema refuses it" (Result.is_error (Maithic_schema.Json_schema.conforms (schema "report.schema.json") incomplete));
        let wrong_severity = Json.parse_exn {|{"severity":"fatal","line":1,"column":0,"text":"x"}|} in
        contains "an unknown severity"
          (is_error "diagnostic"
             (Maithic_schema.Json_schema.conforms
                (Maithic_schema.Json_schema.of_json ~dir:(Support.path "schemas/bridge") (Json.parse_exn {|{"$ref":"report.schema.json#/$defs/diagnostic"}|}))
                wrong_severity))
          "is not one of");
    case "P09-02-05 every recorded answer of the Bridge conforms to the published schemas" (fun () ->
        let schemas = Maithic_devtools.Contracts.schemas (Lazy.force Support.root) in
        let transcripts = [ "handshake"; "prove-even"; "environments"; "whole-file"; "limits"; "escaping"; "self-test"; "proof-states" ] in
        List.iter
          (fun name ->
            let rel = "fixtures/bridge/transcripts/" ^ name ^ ".jsonl" in
            equal_strings name [] (Maithic_devtools.Contracts.transcript schemas rel (Support.read rel)))
          transcripts;
        (* The typed decoders accept what the schemas accept: every recorded report and outcome decodes. *)
        List.iter
          (fun name ->
            List.iter
              (fun line ->
                if not (Text.is_blank line) then begin
                  let result = Option.value (Json.path [ "response"; "result" ] (Json.parse_exn line)) ~default:(Json.Obj []) in
                  (match Json.member "report" result with Some r -> ignore (is_ok "report" (Bridge_types.report_of_json r)) | None -> ());
                  if Json.member "ok" result <> None && Json.member "diagnostics" result <> None then
                    ignore (is_ok "outcome" (Bridge_types.tactic_outcome_of_json result));
                  List.iter (fun g -> ignore (is_ok "goal" (Bridge_types.goal_of_json g))) (Json.list_or_empty "goals" result)
                end)
              (Text.lines (Support.read ("fixtures/bridge/transcripts/" ^ name ^ ".jsonl"))))
          transcripts);
    case "P09-04-01 goals and tactic outcomes decode strictly" (fun () ->
        let goal = { Bridge_types.goal_id = "_uniq.7"; tag = "succ"; target = "m = n + 1"; pretty = "";
                     hypotheses = [ { Bridge_types.hyp_name = "n"; hyp_type = "Nat"; hyp_value = None };
                                    { Bridge_types.hyp_name = "m"; hyp_type = "Nat"; hyp_value = Some "n + 1" } ] } in
        check "round trip" (Bridge_types.goal_of_json (Bridge_types.goal_to_json goal) = Ok goal);
        equal_string "without Lean's rendering, the target stands in" "\xE2\x8A\xA2 m = n + 1" (Bridge_types.goal_text goal);
        contains "a goal without a target" (is_error "goal" (Bridge_types.goal_of_json (Json.parse_exn {|{"id":"g"}|}))) "target";
        contains "a success without a state" (is_error "outcome" (Bridge_types.tactic_outcome_of_json (Json.parse_exn {|{"ok":true,"goals":[]}|}))) "state";
        contains "an unknown kind of failure"
          (is_error "outcome" (Bridge_types.tactic_outcome_of_json (Json.parse_exn {|{"ok":false,"kind":"mystery","error":"x"}|}))) "mystery";
        check "a failure needs no state"
          (Bridge_types.tactic_outcome_of_json (Json.parse_exn {|{"ok":false,"kind":"parse","error":"x"}|})
           = Ok (Bridge_types.Stopped { kind = Bridge_types.Parse_failure; error = "x"; diagnostics = [] })));
    case "P09-04-07 a server error arrives as a typed remote error" (fun () ->
        with_stub "environments" (fun b ->
            match err "unknown method" (Bridge_client.call b "noSuchMethod" (Json.Obj [])) with
            | Bridge_error.Remote { code; tag; _ } ->
              equal_int "code" (-32601) code;
              equal_string "tag" "MTH-BRG-0003" tag;
              check "connection survives a remote error" (Bridge_client.is_open b)
            | other -> fail (Bridge_error.to_string other)));
    case "P09-04-07 a missing executable and an incompatible server are distinct errors" (fun () ->
        (match err "missing" (Bridge_client.start [ "/nonexistent/maithic-bridge" ]) with
         | Bridge_error.Not_found _ -> ()
         | other -> fail (Bridge_error.to_string other));
        (match err "protocol" (Bridge_client.start (Support.stub ~options:[ "--protocol"; "99" ] "environments")) with
         | Bridge_error.Incompatible detail -> contains "detail" detail "protocol 99"
         | other -> fail (Bridge_error.to_string other));
        List.iter
          (fun e -> check "fault code" (Maithic_schema.Fault.is_code (Bridge_error.to_fault e).Maithic_schema.Fault.code))
          [ Bridge_error.Not_found "x"; Bridge_error.Process "x"; Bridge_error.Protocol "x"; Bridge_error.Timeout 1.0;
            Bridge_error.Incompatible "x"; Bridge_error.Remote { code = -32000; tag = "MTH-BRG-0101"; message = "x" } ]);
    case "P09-04-04 a response with another identifier is a protocol error" (fun () ->
        let b = ok "start" (Bridge_client.start (Support.stub ~options:[ "--wrong-id-on"; "stats" ] "environments")) in
        (match err "stats" (Bridge_client.stats b) with
         | Bridge_error.Protocol detail -> contains "detail" detail "request identifier"
         | other -> fail (Bridge_error.to_string other));
        check "the connection is closed after a protocol error" (not (Bridge_client.is_open b)));
    case "P09-04-04 text that is not JSON is a protocol error" (fun () ->
        let b = ok "start" (Bridge_client.start (Support.stub ~options:[ "--garbage-on"; "stats" ] "environments")) in
        (match err "stats" (Bridge_client.stats b) with Bridge_error.Protocol _ -> () | other -> fail (Bridge_error.to_string other));
        (match err "after" (Bridge_client.stats b) with Bridge_error.Process _ -> () | other -> fail (Bridge_error.to_string other)));
    case "P09-04-06 a call that gets no answer ends at its deadline" (fun () ->
        let b = ok "start" (Bridge_client.start (Support.stub ~options:[ "--hang-on"; "stats" ] "environments")) in
        let started = Unix.gettimeofday () in
        (match err "stats" (Bridge_client.stats ~timeout:0.3 b) with
         | Bridge_error.Timeout seconds -> check "deadline" (seconds = 0.3)
         | other -> fail (Bridge_error.to_string other));
        check "returned promptly" (Unix.gettimeofday () -. started < 5.0));
    case "P09-05-01 a worker that exceeds its time limit is killed" (fun () ->
        let b = ok "start" (Bridge_client.start (Support.stub ~options:[ "--hang-on"; "runCommand" ] "environments")) in
        ignore (err "runCommand" (Bridge_client.run_command ~timeout:0.2 b ~env:1 "example : True := trivial"));
        check "closed" (not (Bridge_client.is_open b));
        Bridge_client.shutdown b) ]

let sandbox_tests =
  [ case "P09-05-02 only allowlisted variables reach a worker, and never a credential" (fun () ->
        let kept =
          Screen.scrub_env
            [| "PATH=/usr/bin"; "HOME=/home/u"; "LANG=C.UTF-8"; "LC_ALL=C"; "LEAN_PATH=/x"; "ELAN_HOME=/e";
               "ANTHROPIC_API_KEY=secret"; "OPENAI_API_KEY=secret"; "XAI_API_KEY=secret"; "GITHUB_TOKEN=secret"; "LEAN_AUTH_TOKEN=secret";
               "AWS_SECRET_ACCESS_KEY=secret"; "EDITOR=vi"; "SSH_AUTH_SOCK=/s"; "malformed" |]
        in
        equal_strings "kept" [ "PATH"; "HOME"; "LANG"; "LC_ALL"; "LEAN_PATH"; "ELAN_HOME" ] (List.map fst kept));
    case "P09-05-02 a started worker does not see the caller's keys" (fun () ->
        Unix.putenv "MAITHIC_TEST_API_KEY" "should-not-leak";
        Unix.putenv "ANTHROPIC_API_KEY" "should-not-leak";
        with_stub "environments" (fun b ->
            let names = Json.strings (Option.get (Json.member "names" (ok "env" (Bridge_client.call b "stubEnvironment" (Json.Obj []))))) in
            check "PATH is passed" (List.mem "PATH" names);
            check "no key variable" (not (List.exists Screen.is_secret_name names))));
    case "P09-05-05 ordinary proofs pass the screen" (fun () ->
        List.iter
          (fun proof -> check proof (Screen.admits proof))
          [ "by omega"; "by intro a b h; simp [Nat.add_comm] at h \xE2\x8A\xA2; exact h.symm"; "fun h => h.elim Or.inr Or.inl";
            "by\n  induction xs with\n  | nil => rfl\n  | cons x xs ih => simp [ih]"; "by exact?"; "by simp [h\xE2\x82\x81]" ]);
    case "P09-05-05 constructs that could run code or change later text are flagged" (fun () ->
        List.iter
          (fun proof -> check proof (not (Screen.admits proof)))
          [ "by sorry"; "by admit"; "by native_decide"; "by run_tac (pure ())"; "by first | omega | (#eval 1)"; "by exact IO.println \"x\"";
            "by exact _root_.IO.println \"x\""; "rfl\naxiom cheat : False"; "rfl\ntheorem other : True := trivial";
            "by set_option maxHeartbeats 0 in simp"; "by open Classical in exact em _"; "by exact \xC2\xABsorryAx\xC2\xBB _";
            "by exact Lean.Elab.Command.elabCommand"; "by \xE2\x88\x98IO.getStdout"; String.make (Screen.max_candidate_chars + 1) 'a' ];
        equal_string "reason" "forbidden keyword 'sorry'" (List.hd (Screen.violations "by sorry")).Screen.reason;
        equal_string "reason" "forbidden name 'IO.println' (matches 'IO.')" (List.hd (Screen.violations "by exact IO.println 1")).Screen.reason);
    case "P09-05-05 the screen agrees with the guard of the Bridge on the recorded cases" (fun () ->
        List.iter
          (fun line ->
            let j = Json.parse_exn line in
            match Json.path [ "request"; "method" ] j, Json.path [ "request"; "params"; "candidate" ] j, Json.path [ "response"; "result"; "admitted" ] j with
            | Some (Json.String "guard"), Some (Json.String candidate), Some (Json.Bool admitted) ->
              equal_bool candidate admitted (Screen.admits candidate)
            | _ -> ())
          (Text.lines (Support.read "fixtures/bridge/transcripts/self-test.jsonl") @ Text.lines (Support.read "fixtures/bridge/transcripts/escaping.jsonl")));
    case "P09-06-01 the Lake project root is found from inside the project" (fun () ->
        let lean = Support.path "lean" in
        check "from a subdirectory" (Bridge_locate.project_root Fs.(lean / "Maithic" / "Tactic") = Some lean);
        check "toolchain" (Bridge_locate.toolchain_of_project lean = Some Maithic_schema.Version.lean_toolchain);
        check "none" (Bridge_locate.project_root "/" = None)) ]

let () = run "bridge" [ ("wire types", codec_tests); ("client", client_tests); ("sandbox and projects", sandbox_tests) ]
