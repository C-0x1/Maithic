(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The regression gate: a run held against the baseline of its suite. *)

open Maithic_base

type thresholds = {
  max_regressions : int;  (** Problems the baseline verified and the run did not, at most. *)
  min_rate : float;  (** The share of problems verified, at least; 0 for no demand. *)
  max_slowdown : float;  (** The total time of the problems both verified, as a multiple of the baseline's; 0 for no demand. *)
  max_attempts_ratio : float;  (** The same for the candidates tried; 0 for no demand. *)
  allow_missing : bool;  (** Whether a problem of the baseline may be absent from the run. *)
}

(** By default nothing may be lost and time is not judged: times differ from
    machine to machine, and a gate that fails on a slow day teaches people
    to ignore it. *)
let default_thresholds = { max_regressions = 0; min_rate = 0.0; max_slowdown = 0.0; max_attempts_ratio = 0.0; allow_missing = false }

type verdict = {
  suite : string;
  passed : bool;
  reasons : string list;  (** Why it failed; empty when it passed. *)
  regressions : string list;  (** Verified in the baseline, not in the run. *)
  improvements : string list;  (** Verified in the run, not in the baseline. *)
  missing : string list;  (** In the baseline, absent from the run. *)
  added : string list;  (** In the run, absent from the baseline. *)
  baseline_verified : int;
  baseline_total : int;
  run_verified : int;
  run_total : int;
  time_ratio : float option;  (** Over the problems both verified; [None] when there is none or the baseline took no time. *)
  attempts_ratio : float option;
}

let rate verified total = if total = 0 then 0.0 else float_of_int verified /. float_of_int total

(** Holds the problems of a run against a baseline. *)
let judge ?(thresholds = default_thresholds) (baseline : Bench_baseline.t) (run : Bench_baseline.problem list) =
  let is_verified (p : Bench_baseline.problem) = p.Bench_baseline.status = "verified" in
  let in_run id = List.find_opt (fun (p : Bench_baseline.problem) -> p.Bench_baseline.id = id) run in
  let in_baseline id = List.find_opt (fun (p : Bench_baseline.problem) -> p.Bench_baseline.id = id) baseline.Bench_baseline.problems in
  let ids = List.map (fun (p : Bench_baseline.problem) -> p.Bench_baseline.id) in
  let regressions =
    ids (List.filter (fun (b : Bench_baseline.problem) -> is_verified b && match in_run b.Bench_baseline.id with Some r -> not (is_verified r) | None -> false) baseline.Bench_baseline.problems)
  in
  let improvements =
    ids (List.filter (fun (r : Bench_baseline.problem) -> is_verified r && match in_baseline r.Bench_baseline.id with Some b -> not (is_verified b) | None -> false) run)
  in
  let missing = ids (List.filter (fun (b : Bench_baseline.problem) -> in_run b.Bench_baseline.id = None) baseline.Bench_baseline.problems) in
  let added = ids (List.filter (fun (r : Bench_baseline.problem) -> in_baseline r.Bench_baseline.id = None) run) in
  let both =
    List.filter_map
      (fun (b : Bench_baseline.problem) -> match in_run b.Bench_baseline.id with Some r when is_verified b && is_verified r -> Some (b, r) | _ -> None)
      baseline.Bench_baseline.problems
  in
  let ratio measure =
    let before = List.fold_left (fun acc (b, _) -> acc + measure b) 0 both and after = List.fold_left (fun acc (_, r) -> acc + measure r) 0 both in
    if before <= 0 then None else Some (float_of_int after /. float_of_int before)
  in
  let time_ratio = ratio (fun (p : Bench_baseline.problem) -> p.Bench_baseline.elapsed_ms) in
  let attempts_ratio = ratio (fun (p : Bench_baseline.problem) -> p.Bench_baseline.attempts) in
  let run_verified = List.length (List.filter is_verified run) and run_total = List.length run in
  let reasons =
    (if List.length regressions > thresholds.max_regressions then
       [ Printf.sprintf "%d problem(s) that the baseline verified are not verified: %s (at most %d may be lost)" (List.length regressions) (String.concat ", " regressions)
           thresholds.max_regressions ]
     else [])
    @ (if missing <> [] && not thresholds.allow_missing then
         [ Printf.sprintf "%d problem(s) of the baseline are absent from the run: %s" (List.length missing) (String.concat ", " missing) ]
       else [])
    @ (if thresholds.min_rate > 0.0 && rate run_verified run_total < thresholds.min_rate then
         [ Printf.sprintf "%.1f%% of the problems are verified, and %.1f%% are asked for" (100.0 *. rate run_verified run_total) (100.0 *. thresholds.min_rate) ]
       else [])
    @ (match time_ratio with
       | Some r when thresholds.max_slowdown > 0.0 && r > thresholds.max_slowdown ->
         [ Printf.sprintf "the problems both runs verified took %.2f times the time of the baseline, and at most %.2f times is allowed" r thresholds.max_slowdown ]
       | _ -> [])
    @ match attempts_ratio with
      | Some r when thresholds.max_attempts_ratio > 0.0 && r > thresholds.max_attempts_ratio ->
        [ Printf.sprintf "the problems both runs verified took %.2f times the candidates of the baseline, and at most %.2f times is allowed" r thresholds.max_attempts_ratio ]
      | _ -> []
  in
  { suite = baseline.Bench_baseline.suite; passed = reasons = []; reasons; regressions; improvements; missing; added;
    baseline_verified = Bench_baseline.verified baseline; baseline_total = List.length baseline.Bench_baseline.problems; run_verified; run_total; time_ratio; attempts_ratio }

(* ---- output ---- *)

let schema = "maithic.bench.gate/1"
let strings items = Json.List (List.map (fun s -> Json.String s) items)

let thresholds_to_json t =
  Json.Obj
    [ ("max_regressions", Json.Int t.max_regressions); ("min_rate", Json.Float t.min_rate); ("max_slowdown", Json.Float t.max_slowdown);
      ("max_attempts_ratio", Json.Float t.max_attempts_ratio); ("allow_missing", Json.Bool t.allow_missing) ]

let verdict_to_json v =
  Json.Obj
    ([ ("suite", Json.String v.suite); ("passed", Json.Bool v.passed); ("reasons", strings v.reasons); ("regressions", strings v.regressions);
       ("improvements", strings v.improvements); ("missing", strings v.missing); ("added", strings v.added);
       ("baseline", Json.Obj [ ("verified", Json.Int v.baseline_verified); ("total", Json.Int v.baseline_total) ]);
       ("run", Json.Obj [ ("verified", Json.Int v.run_verified); ("total", Json.Int v.run_total) ]) ]
    @ (match v.time_ratio with Some r -> [ ("time_ratio", Json.Float r) ] | None -> [])
    @ match v.attempts_ratio with Some r -> [ ("attempts_ratio", Json.Float r) ] | None -> [])

let to_json ~thresholds verdicts =
  Json.Obj
    [ ("schema", Json.String schema); ("passed", Json.Bool (List.for_all (fun v -> v.passed) verdicts)); ("thresholds", thresholds_to_json thresholds);
      ("suites", Json.List (List.map verdict_to_json verdicts)) ]

(** A verdict in words: one line, and below it what changed and why it failed. *)
let render v =
  let ratio = function Some r -> Printf.sprintf "%.2f" r | None -> "-" in
  [ Printf.sprintf "%s  %s  verified %d of %d (baseline %d of %d)  time x%s  candidates x%s" v.suite (if v.passed then "passed" else "FAILED") v.run_verified v.run_total
      v.baseline_verified v.baseline_total (ratio v.time_ratio) (ratio v.attempts_ratio) ]
  @ (if v.improvements = [] then [] else [ "  newly verified: " ^ String.concat ", " v.improvements ])
  @ (if v.added = [] then [] else [ "  not in the baseline: " ^ String.concat ", " v.added ])
  @ List.map (fun reason -> "  " ^ reason) v.reasons
  |> String.concat "\n"
