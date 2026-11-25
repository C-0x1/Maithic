(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the regression gate: the store of baselines and the rule that holds a run against one. *)

open Maithic_base
open Maithic_schema
open Maithic_bench
open Testkit

let problem ?(status = "verified") ?(attempts = 4) ?(ms = 100) id = { Bench_baseline.id; status; attempts; elapsed_ms = ms }
let baseline problems = { Bench_baseline.suite = "demo"; mode = "replay"; use_ai = false; policy = "default"; note = "a test"; problems }
let judge ?thresholds base run = Bench_gate.judge ?thresholds (baseline base) run
let committed_run () = Support.json "bench/results/basics-portfolio.json"

let store_tests =
  [ case "P15-04-01 a baseline holds what a suite gave, problem by problem, and is stored in a file of its own" (fun () ->
        let run = committed_run () in
        equal_strings "the suites of the committed run" [ "basics" ] (Bench_baseline.suites_of_run run);
        let b = is_ok "a baseline" (Bench_baseline.of_run ~note:"a release" ~suite:"basics" run) in
        equal_int "24 problems" 24 (List.length b.Bench_baseline.problems);
        equal_int "all verified" 24 (Bench_baseline.verified b);
        equal_string "its first problem" "basics-001" (List.hd b.Bench_baseline.problems).Bench_baseline.id;
        check "made without a model" (not b.Bench_baseline.use_ai);
        contains "a suite the run does not have" (is_error "refused" (Bench_baseline.of_run ~suite:"hard" run)) "no problem of the suite 'hard'";
        let json = Bench_baseline.to_json b in
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/bench/baseline.schema.json")) json));
        check "the same baseline back" (Bench_baseline.of_json json = Ok b);
        contains "another document is not a baseline" (is_error "refused" (Bench_baseline.of_json run)) "not a baseline";
        let dir = temp_dir "baselines" in
        check "nothing is stored yet" (Bench_baseline.stored ~dir:Fs.(dir / "none") = []);
        contains "no baseline yet" (is_error "missing" (Bench_baseline.load ~dir "basics")) "there is no baseline for the suite 'basics'";
        is_ok "stored" (Bench_baseline.save ~dir:Fs.(dir / "deep" / "store") b);
        check "and read back" (Bench_baseline.load ~dir:Fs.(dir / "deep" / "store") "basics" = Ok b);
        equal_strings "one suite is stored" [ "basics" ] (Bench_baseline.stored ~dir:Fs.(dir / "deep" / "store"));
        contains "a name that could leave the folder" (is_error "refused" (Bench_baseline.load ~dir "../basics")) "cannot be the name of a suite";
        is_ok "another file" (Fs.write Fs.(dir / "other.json") (Json.to_string json));
        contains "a file under the wrong name" (is_error "refused" (Bench_baseline.load ~dir "other")) "is the baseline of the suite 'basics'";
        check "the baseline the repository keeps is the committed run"
          (Result.map (fun (k : Bench_baseline.t) -> k.Bench_baseline.problems) (Bench_baseline.load ~dir:(Support.path "bench/baselines") "basics") = Ok b.Bench_baseline.problems);
        Fs.remove_tree dir) ]

let rule_tests =
  [ case "P15-04-02 a run that loses nothing passes, and one that loses a verified problem fails" (fun () ->
        let base = [ problem "a"; problem "b"; problem ~status:"failed" "c" ] in
        let same = judge base base in
        check "the same run passes" (same.Bench_gate.passed && same.Bench_gate.reasons = []);
        check "the time is the same" (same.Bench_gate.time_ratio = Some 1.0 && same.Bench_gate.attempts_ratio = Some 1.0);
        let worse = judge base [ problem "a"; problem ~status:"failed" "b"; problem ~status:"failed" "c" ] in
        check "a lost problem fails the gate" (not worse.Bench_gate.passed);
        equal_strings "and is named" [ "b" ] worse.Bench_gate.regressions;
        contains "with the reason" (List.hd worse.Bench_gate.reasons) "1 problem(s) that the baseline verified are not verified: b (at most 0 may be lost)";
        let tolerant = judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.max_regressions = 1 } base [ problem "a"; problem ~status:"failed" "b"; problem ~status:"failed" "c" ] in
        check "unless one may be lost" tolerant.Bench_gate.passed;
        let better = judge base [ problem "a"; problem "b"; problem "c"; problem "d" ] in
        check "a better run passes" better.Bench_gate.passed;
        equal_strings "what is newly verified" [ "c" ] better.Bench_gate.improvements;
        equal_strings "what the baseline did not have" [ "d" ] better.Bench_gate.added;
        equal_int "four problems" 4 better.Bench_gate.run_total;
        equal_int "of which the baseline verified two" 2 better.Bench_gate.baseline_verified);
    case "P15-04-02 absent problems, a share of verified problems, time and candidates each have a threshold" (fun () ->
        let base = [ problem ~ms:100 ~attempts:4 "a"; problem ~ms:300 ~attempts:6 "b"; problem ~status:"failed" ~ms:900 "c" ] in
        let partial = judge base [ problem "a" ] in
        equal_strings "what is absent" [ "b"; "c" ] partial.Bench_gate.missing;
        check "fails by default" (not partial.Bench_gate.passed);
        check "and passes when absence is allowed" (judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.allow_missing = true } base [ problem "a" ]).Bench_gate.passed;
        let slow = [ problem ~ms:200 ~attempts:4 "a"; problem ~ms:600 ~attempts:14 "b"; problem ~status:"failed" ~ms:10 "c" ] in
        let unjudged = judge base slow in
        check "time is reported" (unjudged.Bench_gate.time_ratio = Some 2.0);
        check "and so are candidates" (unjudged.Bench_gate.attempts_ratio = Some 1.8);
        check "and neither is judged unless asked" unjudged.Bench_gate.passed;
        let timed = judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.max_slowdown = 1.5 } base slow in
        check "twice the time is too slow for a limit of 1.5" (not timed.Bench_gate.passed);
        contains "the reason" (List.hd timed.Bench_gate.reasons) "took 2.00 times the time of the baseline, and at most 1.50 times is allowed";
        check "and fine for a limit of 2" (judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.max_slowdown = 2.0 } base slow).Bench_gate.passed;
        let tried = judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.max_attempts_ratio = 1.5 } base slow in
        contains "candidates" (List.hd tried.Bench_gate.reasons) "1.80 times the candidates";
        let rate = judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.min_rate = 0.9 } base base in
        contains "two of three is less than nine in ten" (List.hd rate.Bench_gate.reasons) "66.7% of the problems are verified, and 90.0% are asked for";
        (* only the problems both runs verified are timed *)
        let none = judge [ problem ~status:"failed" "a" ] [ problem "a" ] in
        check "nothing to compare" (none.Bench_gate.time_ratio = None && none.Bench_gate.passed);
        let several = judge ~thresholds:{ Bench_gate.default_thresholds with Bench_gate.max_slowdown = 1.1; min_rate = 1.0 } base [ problem ~ms:500 "a"; problem ~status:"failed" "b" ] in
        equal_int "every broken threshold is a reason" 4 (List.length several.Bench_gate.reasons));
    case "P15-04-03 the verdicts are a document and a few lines" (fun () ->
        let base = [ problem "a"; problem "b" ] in
        let failed = judge base [ problem "a"; problem ~status:"failed" "b"; problem "new" ] in
        let json = Bench_gate.to_json ~thresholds:Bench_gate.default_thresholds [ failed; judge base base ] in
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/bench/gate.schema.json")) json));
        check "one failed suite fails the gate" (Json.bool_or true "passed" json = false);
        let text = Bench_gate.render failed in
        contains "the line of a suite" text "demo  FAILED  verified 2 of 3 (baseline 2 of 2)  time x1.00  candidates x1.00";
        contains "what is new" text "  not in the baseline: new";
        contains "why it failed" text "  1 problem(s) that the baseline verified are not verified: b";
        contains "a passed suite" (Bench_gate.render (judge base base)) "demo  passed  verified 2 of 2 (baseline 2 of 2)";
        contains "nothing to time" (Bench_gate.render (judge [ problem ~status:"failed" "a" ] [ problem ~status:"failed" "a" ])) "time x-  candidates x-") ]

let () = run "bench" [ ("baselines", store_tests); ("gate rule", rule_tests) ]
