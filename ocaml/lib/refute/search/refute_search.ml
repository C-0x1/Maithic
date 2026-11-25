(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema

type method_ = Test | Oracle | Vacuity

let methods = [ Test; Oracle; Vacuity ]
let method_to_string = function Test -> "test" | Oracle -> "oracle" | Vacuity -> "vacuity"
let method_of_string = function "test" -> Some Test | "oracle" -> Some Oracle | "vacuity" -> Some Vacuity | _ -> None

let methods_of_string text =
  let names = List.filter (fun s -> s <> "") (List.map String.trim (String.split_on_char ',' text)) in
  if names = [] then Error "no method named; the methods are test, oracle and vacuity, or all"
  else if names = [ "all" ] then Ok methods
  else
    match List.find_opt (fun name -> method_of_string name = None) names with
    | Some unknown -> Error (Printf.sprintf "there is no method '%s'; the methods are test, oracle and vacuity, or all" unknown)
    | None -> Ok (List.filter (fun m -> List.mem (method_to_string m) names) methods)

type budget = { cases : int; size : int; seed : int; rounds : int; seconds : float }

let default_budget = { cases = 100; size = 30; seed = 0; rounds = 3; seconds = 60.0 }

type oracle_answer = Point of Refute_witness.assignment | Proved of string | No_answer of string

type tools = {
  test : cases:int -> size:int -> seed:int -> (Refute_probe.test_report, string) result;
  oracle : unit -> (oracle_answer, string) result;
  vacuity : unit -> (Refute_probe.vacuity, string) result;
  confirm : Refute_witness.assignment -> (Refute_witness.confirmation, string) result;
  clock : unit -> float;
}

type step = {
  method_ : method_;
  outcome : string;
  detail : string;
  candidate : Refute_witness.assignment;
  confirmation : Refute_witness.confirmation option;
  rejection : string;
  seconds : float;
}

type verdict =
  | Refuted of { counterexample : Refute_witness.assignment; by : method_; confirmation : Refute_witness.confirmation }
  | Vacuous of string
  | Unconfirmed of { counterexample : Refute_witness.assignment; by : method_; why : string }
  | Not_refuted

let verdict_to_string = function Refuted _ -> "refuted" | Vacuous _ -> "vacuous" | Unconfirmed _ -> "unconfirmed" | Not_refuted -> "not-refuted"

type result = {
  file : string;
  theorem : string;
  line : int;
  statement : string;
  methods : method_ list;
  budget : budget;
  steps : step list;
  verdict : verdict;
  seconds : float;
}

let plural n one many = Printf.sprintf "%d %s" n (if n = 1 then one else many)
let assignment_text a = if a = [] then "the statement has no variables" else String.concat ", " (List.map (fun (name, value) -> name ^ " := " ^ value) a)

let search ?(methods = methods) ?(budget = default_budget) tools =
  let started = tools.clock () in
  let steps = ref [] in
  let refuted = ref None and vacuous = ref None and unconfirmed = ref None in
  let spent () = tools.clock () -. started in
  let out_of_time () = spent () >= budget.seconds in
  let record step = steps := step :: !steps in
  let plain method_ ~since outcome detail =
    record { method_; outcome; detail; candidate = []; confirmation = None; rejection = ""; seconds = tools.clock () -. since }
  in
  let skipped method_ = plain method_ ~since:(tools.clock ()) "skipped" (Printf.sprintf "the budget of %g s was used up" budget.seconds) in
  (* A candidate is put before Lean; what Lean says decides. *)
  let candidate method_ ~since detail assignment =
    match tools.confirm assignment with
    | Ok confirmation ->
      record { method_; outcome = "counterexample"; detail; candidate = assignment; confirmation = Some confirmation; rejection = ""; seconds = tools.clock () -. since };
      refuted := Some (assignment, method_, confirmation)
    | Error why ->
      record { method_; outcome = "counterexample"; detail; candidate = assignment; confirmation = None; rejection = why; seconds = tools.clock () -. since };
      if !unconfirmed = None then unconfirmed := Some (assignment, method_, why)
  in
  let test () =
    let rec round i =
      if i >= budget.rounds || !refuted <> None then ()
      else if out_of_time () then skipped Test
      else begin
        let since = tools.clock () in
        let seed = budget.seed + i in
        match tools.test ~cases:budget.cases ~size:budget.size ~seed with
        | Error why -> plain Test ~since "not-applicable" why
        | Ok (r : Refute_probe.test_report) ->
          (match r.Refute_probe.verdict with
           | Refute_probe.Falsified ->
             let found =
               Printf.sprintf "falsified at case %d with seed %d%s" r.Refute_probe.cases seed
                 (if r.Refute_probe.shrinks = 0 then "" else Printf.sprintf ", made smaller %s" (plural r.Refute_probe.shrinks "time" "times"))
             in
             candidate Test ~since found r.Refute_probe.counterexample;
             round (i + 1)
           | Refute_probe.Passed ->
             plain Test ~since "passed"
               (Printf.sprintf "passed %s with seed %d%s" (plural r.Refute_probe.cases "case" "cases") seed
                  (if r.Refute_probe.discarded = 0 then "" else Printf.sprintf ", %d left out by the hypotheses" r.Refute_probe.discarded));
             round (i + 1)
           | Refute_probe.Gave_up ->
             (* Sampling hardly ever meets the hypotheses: another seed will not either. *)
             plain Test ~since "gave-up"
               (Printf.sprintf "gave up with seed %d: %s left out by the hypotheses, %d passed" seed
                  (plural r.Refute_probe.discarded "case" "cases") r.Refute_probe.cases))
      end
    in
    round 0
  in
  let oracle () =
    if out_of_time () then skipped Oracle
    else begin
      let since = tools.clock () in
      match tools.oracle () with
      | Error why -> plain Oracle ~since "unavailable" why
      | Ok (Point point) -> candidate Oracle ~since "a solver found a point where the statement fails" point
      | Ok (Proved claim) -> plain Oracle ~since "proved" ("a solver proved it: " ^ claim)
      | Ok (No_answer why) -> plain Oracle ~since "unknown" why
    end
  in
  let vacuity () =
    if out_of_time () then skipped Vacuity
    else begin
      let since = tools.clock () in
      match tools.vacuity () with
      | Error why -> plain Vacuity ~since "not-applicable" why
      | Ok v ->
        let said = Refute_probe.describe_vacuity v in
        if v.Refute_probe.vacuous then begin
          plain Vacuity ~since "vacuous" said;
          vacuous := Some said
        end
        else if said <> "" then plain Vacuity ~since "trivial" said
        else plain Vacuity ~since "nothing" "no contradiction among the hypotheses, no empty domain"
    end
  in
  List.iter
    (fun method_ ->
      if !refuted = None && List.mem method_ methods then match method_ with Test -> test () | Oracle -> oracle () | Vacuity -> vacuity ())
    [ Test; Oracle; Vacuity ];
  let verdict =
    match (!refuted, !vacuous, !unconfirmed) with
    | Some (counterexample, by, confirmation), _, _ -> Refuted { counterexample; by; confirmation }
    | None, Some reason, _ -> Vacuous reason
    | None, None, Some (counterexample, by, why) -> Unconfirmed { counterexample; by; why }
    | None, None, None -> Not_refuted
  in
  (verdict, List.rev !steps)

let run ?(methods = methods) ?(budget = default_budget) ~file ~theorem ~line ~statement tools =
  let started = tools.clock () in
  let verdict, steps = search ~methods ~budget tools in
  { file; theorem; line; statement; methods; budget; steps; verdict; seconds = tools.clock () -. started }

let schema = "maithic.refute.report/1"

let assignment_json a = Json.List (List.map (fun (name, value) -> Json.Obj [ ("name", Json.String name); ("value", Json.String value) ]) a)
let seconds_json s = Json.Float (Float.round (s *. 1000.0) /. 1000.0)

let result_to_json r =
  let step s =
    Json.Obj
      ([ ("method", Json.String (method_to_string s.method_)); ("outcome", Json.String s.outcome); ("detail", Json.String s.detail) ]
       @ (if s.outcome = "counterexample" then
            [ ("candidate", assignment_json s.candidate); ("confirmed", Json.Bool (s.confirmation <> None)) ]
            @ (if s.rejection = "" then [] else [ ("rejection", Json.String s.rejection) ])
          else [])
       @ [ ("seconds", seconds_json s.seconds) ])
  in
  let findings =
    match r.verdict with
    | Refuted { counterexample; by; confirmation } ->
      [ ("counterexample", assignment_json counterexample); ("foundBy", Json.String (method_to_string by));
        ("refutation", Refute_witness.to_json confirmation) ]
    | Vacuous reason -> [ ("vacuity", Json.String reason) ]
    | Unconfirmed { counterexample; by; why } ->
      [ ("counterexample", assignment_json counterexample); ("foundBy", Json.String (method_to_string by)); ("unconfirmed", Json.String why) ]
    | Not_refuted -> []
  in
  Json.Obj
    ([ ("theorem", Json.String r.theorem); ("line", Json.Int r.line); ("statement", Json.String r.statement);
       ("verdict", Json.String (verdict_to_string r.verdict)) ]
     @ findings
     @ [ ("methods", Json.List (List.map (fun m -> Json.String (method_to_string m)) r.methods));
         ( "budget",
           Json.Obj
             [ ("cases", Json.Int r.budget.cases); ("size", Json.Int r.budget.size); ("seed", Json.Int r.budget.seed);
               ("rounds", Json.Int r.budget.rounds); ("seconds", Json.Float r.budget.seconds) ] );
         ("steps", Json.List (List.map step r.steps)); ("seconds", seconds_json r.seconds) ])

let render ?(color = false) r =
  let paint code text = if color then Printf.sprintf "\027[%sm%s\027[0m" code text else text in
  let coral = paint "31" and gold = paint "33" and teal = paint "36" and dim = paint "2" and bold = paint "1" in
  let b = Buffer.create 512 in
  let line fmt = Printf.ksprintf (fun s -> Buffer.add_string b s; Buffer.add_char b '\n') fmt in
  let indented prefix text = List.iter (fun l -> line "%s%s" prefix l) (String.split_on_char '\n' text) in
  line "%s  %s" (bold r.theorem) (dim (Printf.sprintf "%s:%d" r.file r.line));
  line "  %s" r.statement;
  List.iter
    (fun s ->
      let label = Printf.sprintf "%-8s" (method_to_string s.method_) in
      if s.outcome = "counterexample" then begin
        line "  %s  %s: %s" label s.detail (assignment_text s.candidate);
        match s.confirmation with
        | Some c ->
          line "  %-8s  accepts the refutation, closed by %s; %s" "Lean" c.Refute_witness.closer
            (if c.Refute_witness.axioms = [] then "it rests on no axiom" else "it rests on " ^ String.concat ", " c.Refute_witness.axioms)
        | None -> line "  %-8s  does not confirm it (%s)" "Lean" s.rejection
      end
      else line "  %s  %s" label s.detail)
    r.steps;
  (match r.verdict with
   | Refuted { confirmation; _ } ->
     line "  %s  the statement is false" (coral (Printf.sprintf "%-8s" "refuted"));
     line "";
     indented "    " confirmation.Refute_witness.lean
   | Vacuous reason -> line "  %s  the statement is true and says nothing: %s" (gold (Printf.sprintf "%-8s" "vacuous")) reason
   | Unconfirmed _ ->
     line "  %s  a counterexample was found, and Lean did not confirm it: the statement is not shown false" (gold (Printf.sprintf "%-8s" "open"))
   | Not_refuted -> line "  %s  nothing was found, which proves nothing" (teal (Printf.sprintf "%-8s" "standing")));
  Buffer.contents b

let count results name = List.length (List.filter (fun r -> verdict_to_string r.verdict = name) results)

let to_json ~file results =
  Json.Obj
    [ ("schema", Json.String schema); ("file", Json.String file);
      ( "summary",
        Json.Obj
          [ ("statements", Json.Int (List.length results)); ("refuted", Json.Int (count results "refuted"));
            ("vacuous", Json.Int (count results "vacuous")); ("unconfirmed", Json.Int (count results "unconfirmed"));
            ("standing", Json.Int (count results "not-refuted")) ] );
      ("results", Json.List (List.map result_to_json results)) ]

let exit_code results =
  if List.exists (fun r -> match r.verdict with Refuted _ -> true | _ -> false) results then Exit_code.Refuted
  else if List.exists (fun r -> match r.verdict with Vacuous _ | Unconfirmed _ -> true | _ -> false) results then Exit_code.Not_verified
  else Exit_code.Success

let summary results =
  Printf.sprintf "%s: %d refuted, %d vacuous, %d with a counterexample Lean did not confirm, %d standing"
    (plural (List.length results) "statement" "statements")
    (count results "refuted") (count results "vacuous") (count results "unconfirmed") (count results "not-refuted")
