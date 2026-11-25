(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [bench gate] command: a benchmark run held against the baseline of
    its suite, with an exit status a pipeline can act on. *)

open Maithic_base
open Maithic_schema
open Maithic_bench
open Cli_args
open Commands

let bench_fault number ~exit message = Fault.make ~area:"BENCH" ~number ~exit message

let ratio parsed name ~default =
  match get parsed name with
  | None -> Ok default
  | Some text ->
    (match float_of_string_opt text with
     | Some x when x >= 0.0 -> Ok x
     | _ -> Error (usage_fault (Printf.sprintf "--%s needs a number that is not negative, and got %s" name text)))

let gate_cmd =
  { path = [ "bench"; "gate" ]; summary = "Hold a benchmark run against its baseline and fail when it is worse."; synopsis = "RUN";
    details =
      "RUN is a file that `maithic bench run --out` wrote. Each suite of the run is held against the baseline stored for it, a file \
       of the folder given with --baselines. By default the gate fails when a problem that the baseline verified is not verified \
       any more, or when a problem of the baseline is absent from the run; times are reported and not judged, because they differ \
       from machine to machine. --max-slowdown and --max-attempts set a limit on the time and on the candidates tried, as multiples \
       of the baseline's, over the problems both verified, and --min-rate asks for a share of verified problems. The exit status \
       is 13 when a suite fails. --update stores the run as the new baseline of its suites instead of judging it.";
    options =
      [ value "baselines" "DIR" "The folder of baselines, one file for each suite (bench/baselines).";
        value "suite" "NAME" "Judge only this suite of the run.";
        value "max-regressions" "N" "Verified problems that may be lost (0).";
        value "min-rate" "X" "The share of problems that must be verified, between 0 and 1 (no demand).";
        value "max-slowdown" "X" "The most the time may grow, as a multiple of the baseline's (no demand).";
        value "max-attempts" "X" "The most the candidates tried may grow, as a multiple of the baseline's (no demand).";
        switch "allow-missing" "Let problems of the baseline be absent from the run.";
        switch "update" "Store the run as the new baseline of its suites.";
        value "note" "TEXT" "With --update: why this baseline is set, such as a release or a commit.";
        value "out" "FILE" "Write the verdicts as JSON to FILE as well." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "bench gate needs exactly one RUN argument") in
            let* text = read_source file in
            let* run = Result.map_error (fun e -> bench_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "%s is not a benchmark run: %s" file e)) (Json.parse text) in
            let* () =
              if Json.string_or "" "schema" run = "maithic.bench.run/1" then Ok ()
              else Error (bench_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "%s is not a benchmark run: its schema is not maithic.bench.run/1" file))
            in
            let dir = Option.value ~default:"bench/baselines" (get parsed "baselines") in
            let* suites =
              match (get parsed "suite", Bench_baseline.suites_of_run run) with
              | _, [] -> Error (bench_fault 2 ~exit:Exit_code.Usage (file ^ " reports no problem"))
              | Some suite, all -> if List.mem suite all then Ok [ suite ] else Error (usage_fault (Printf.sprintf "the run has no suite '%s'; it has %s" suite (String.concat ", " all)))
              | None, all -> Ok all
            in
            let* max_regressions =
              match get parsed "max-regressions" with
              | None -> Ok Bench_gate.default_thresholds.Bench_gate.max_regressions
              | Some t -> (match int_of_string_opt t with Some n when n >= 0 -> Ok n | _ -> Error (usage_fault ("--max-regressions needs a whole number, 0 or more, and got " ^ t)))
            in
            let* min_rate = ratio parsed "min-rate" ~default:0.0 in
            let* () = if min_rate > 1.0 then Error (usage_fault "--min-rate is a share between 0 and 1") else Ok () in
            let* max_slowdown = ratio parsed "max-slowdown" ~default:0.0 in
            let* max_attempts_ratio = ratio parsed "max-attempts" ~default:0.0 in
            let thresholds = { Bench_gate.max_regressions; min_rate; max_slowdown; max_attempts_ratio; allow_missing = has parsed "allow-missing" } in
            if has parsed "update" then begin
              let* stored =
                List.fold_left
                  (fun acc suite ->
                    let* acc = acc in
                    let* baseline = Result.map_error usage_fault (Bench_baseline.of_run ?note:(get parsed "note") ~suite run) in
                    let* () = Result.map_error usage_fault (Bench_baseline.save ~dir baseline) in
                    Ok (baseline :: acc))
                  (Ok []) suites
              in
              let stored = List.rev stored in
              if ctx.json then Term.print_json (Json.Obj [ ("updated", Json.List (List.map Bench_baseline.to_json stored)) ])
              else
                List.iter
                  (fun (b : Bench_baseline.t) ->
                    Printf.printf "stored the baseline of %s: %d of %d verified (%s)\n" b.Bench_baseline.suite (Bench_baseline.verified b)
                      (List.length b.Bench_baseline.problems)
                      (Bench_baseline.path ~dir b.Bench_baseline.suite))
                  stored;
              Ok 0
            end
            else begin
              let* verdicts =
                List.fold_left
                  (fun acc suite ->
                    let* acc = acc in
                    let* baseline =
                      Result.map_error
                        (fun why -> bench_fault 3 ~exit:Exit_code.Usage (why ^ "; `maithic bench gate RUN --update` stores one"))
                        (Bench_baseline.load ~dir suite)
                    in
                    Ok (Bench_gate.judge ~thresholds baseline (Bench_baseline.problems_of_run ~suite run) :: acc))
                  (Ok []) suites
              in
              let verdicts = List.rev verdicts in
              let document = Bench_gate.to_json ~thresholds verdicts in
              let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
              let passed = List.for_all (fun (v : Bench_gate.verdict) -> v.Bench_gate.passed) verdicts in
              if ctx.json then Term.print_json document
              else begin
                List.iter (fun v -> print_endline (Bench_gate.render v)) verdicts;
                if not ctx.quiet then print_endline (if passed then "the gate is passed" else "the gate is not passed")
              end;
              Ok (if passed then 0 else Exit_code.to_int Exit_code.Regression)
            end)) }

let all = [ gate_cmd ]
