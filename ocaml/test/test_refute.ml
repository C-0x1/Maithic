(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the search for counterexamples. Lean and the solvers are stood in for by functions,
   so that every path of the search can be taken; the real ones are met in the integration tests. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_prover
open Maithic_refute
open Testkit

let one_zero = [ ("a", "1"); ("b", "0") ]
let accepted = { Refute_witness.accepted = true; uses_sorry = false; axioms = []; error = "" }
let rejected error = { Refute_witness.accepted = false; uses_sorry = false; axioms = []; error }
let ends_with suffix text = Text.ends_with ~suffix text

let witness_tests =
  [ case "P14-02-01 a refutation puts the values into the statement and proves it false there" (fun () ->
        equal_string "the text"
          "theorem _root_.maithic_refutation_of_sum_le_product : \xC2\xAC type_of% @sum_le_product := by\n  intro h\n  have h' := h (a := 1) (b := 0)\n  revert h'\n  decide"
          (Refute_witness.text ~theorem:"sum_le_product" ~closer:"decide" one_zero);
        (* A statement without variables is refuted as it stands. *)
        contains "no values" (Refute_witness.text ~theorem:"two_is_three" ~closer:"decide" []) "\n  have h' := h\n  revert h'";
        (* The name is one Lean reads whatever the theorem is called, and it is put at the root. *)
        equal_string "a name in a namespace, with a prime" "maithic_refutation_of_Demo_lt_irrefl_" (Refute_witness.name_of "Demo.lt_irrefl'");
        equal_string "a name with letters outside ASCII, byte for byte" "maithic_refutation_of____comm" (Refute_witness.name_of "\xCE\xB1_comm");
        contains "at the root" (Refute_witness.text ~theorem:"Demo.t" ~closer:"simp" []) "theorem _root_.maithic_refutation_of_Demo_t : \xC2\xAC type_of% @Demo.t";
        equal_string "what came from outside" "a := 1\nb := 0" (Refute_witness.values one_zero);
        equal_strings "the tactics that are tried" [ "decide"; "decide +kernel"; "omega"; "grind"; "simp" ] Refute_witness.closers);
    case "P14-02-01 a counterexample is confirmed by the first refutation Lean accepts" (fun () ->
        let tried = ref [] in
        let stand_in ~untrusted lean =
          tried := (untrusted, lean) :: !tried;
          if ends_with "omega" lean then { accepted with Refute_witness.axioms = [ "propext"; "Quot.sound"; "propext" ] } else rejected "Tactic `decide` failed\nand more"
        in
        let c = is_ok "confirm" (Refute_witness.confirm ~check:stand_in ~theorem:"t" one_zero) in
        equal_string "the tactic that closed it" "omega" c.Refute_witness.closer;
        equal_string "the text Lean accepted" (Refute_witness.text ~theorem:"t" ~closer:"omega" one_zero) c.Refute_witness.lean;
        equal_strings "the axioms it rests on, each once" [ "Quot.sound"; "propext" ] c.Refute_witness.axioms;
        equal_int "nothing is tried after the first that is accepted" 3 (List.length !tried);
        check "the values go to the guard, not the text Maithic wrote" (List.for_all (fun (untrusted, _) -> untrusted = "a := 1\nb := 0") !tried);
        check "as a document"
          (Json.equal (Refute_witness.to_json c)
             (Json.Obj [ ("lean", Json.String c.Refute_witness.lean); ("closedBy", Json.String "omega"); ("axioms", Json.List [ Json.String "Quot.sound"; Json.String "propext" ]) ])));
    case "P14-02-01 a counterexample Lean does not confirm is not a refutation" (fun () ->
        let none ~untrusted:_ _ = rejected "unsolved goals\n\xE2\x8A\xA2 False" in
        equal_string "what Lean said about the last attempt" "simp: unsolved goals" (is_error "none" (Refute_witness.confirm ~check:none ~theorem:"t" one_zero));
        let silent ~untrusted:_ _ = rejected "" in
        equal_string "or that it said nothing" "simp: not accepted" (is_error "silent" (Refute_witness.confirm ~check:silent ~theorem:"t" one_zero));
        (* With the statement declared and unproved, a tactic that uses it proves anything: such a proof confirms nothing. *)
        let through_sorry ~untrusted:_ lean = if ends_with "simp" lean then { accepted with Refute_witness.uses_sorry = true } else rejected "failed" in
        equal_string "a proof that rests on sorry" "with simp the proof rests on sorry" (is_error "sorry" (Refute_witness.confirm ~check:through_sorry ~theorem:"t" one_zero));
        let later ~untrusted:_ lean = if ends_with "decide" lean then { accepted with Refute_witness.uses_sorry = true } else if ends_with "grind" lean then accepted else rejected "failed" in
        equal_string "is passed over for one that does not" "grind" (is_ok "later" (Refute_witness.confirm ~check:later ~theorem:"t" one_zero)).Refute_witness.closer) ]

(* ---- the search ---- *)

let report ?(verdict = Refute_probe.Passed) ?(cases = 100) ?(discarded = 0) ?(shrinks = 0) ?(counterexample = []) seed =
  { Refute_probe.verdict; cases; discarded; shrinks; seed; counterexample }

let sound = { Refute_probe.vacuous = false; contradiction = None; empty_domains = []; trivial_by = None }
let confirmation = { Refute_witness.closer = "decide"; lean = "theorem r : \xC2\xAC type_of% @t := by decide"; axioms = [] }

(* Tools that count how often each was called, and a clock that the tests move. *)
type calls = { mutable tests : (int * int * int) list; mutable oracles : int; mutable vacuities : int; mutable confirms : int; mutable now : float }

let stub ?(test = fun ~seed -> Ok (report seed)) ?(oracle = fun () -> Ok (Refute_search.No_answer "the goal has no integer or rational variable"))
    ?(vacuity = fun () -> Ok sound) ?(confirm = fun _ -> Ok confirmation) ?(test_takes = 0.0) () =
  let calls = { tests = []; oracles = 0; vacuities = 0; confirms = 0; now = 1000.0 } in
  ( calls,
    { Refute_search.test =
        (fun ~cases ~size ~seed ->
          calls.tests <- calls.tests @ [ (cases, size, seed) ];
          calls.now <- calls.now +. test_takes;
          test ~seed);
      oracle = (fun () -> calls.oracles <- calls.oracles + 1; oracle ());
      vacuity = (fun () -> calls.vacuities <- calls.vacuities + 1; vacuity ());
      confirm = (fun a -> calls.confirms <- calls.confirms + 1; confirm a);
      clock = (fun () -> calls.now) } )

let outcomes steps = List.map (fun (s : Refute_search.step) -> Refute_search.method_to_string s.Refute_search.method_ ^ " " ^ s.Refute_search.outcome) steps
let falsified ~seed = Ok (report ~verdict:Refute_probe.Falsified ~cases:5 ~shrinks:2 ~counterexample:one_zero seed)

let search_tests =
  [ case "P14-02-02 the methods are chosen by name and tried in a fixed order" (fun () ->
        check "all" (Refute_search.methods_of_string "all" = Ok Refute_search.methods);
        check "the order of trying does not follow the order of naming"
          (Refute_search.methods_of_string "vacuity, test" = Ok [ Refute_search.Test; Refute_search.Vacuity ]);
        check "one" (Refute_search.methods_of_string "oracle" = Ok [ Refute_search.Oracle ]);
        contains "a method that does not exist" (is_error "magic" (Refute_search.methods_of_string "test,magic")) "there is no method 'magic'";
        contains "none" (is_error "none" (Refute_search.methods_of_string " , ")) "no method named";
        check "names" (List.map Refute_search.method_to_string Refute_search.methods = [ "test"; "oracle"; "vacuity" ]
                       && List.for_all (fun m -> Refute_search.method_of_string (Refute_search.method_to_string m) = Some m) Refute_search.methods);
        (* Only the chosen methods are run. *)
        let calls, tools = stub () in
        let verdict, steps = Refute_search.search ~methods:[ Refute_search.Vacuity ] tools in
        check "nothing found" (verdict = Refute_search.Not_refuted);
        equal_strings "one step" [ "vacuity nothing" ] (outcomes steps);
        check "the others were not called" (calls.tests = [] && calls.oracles = 0 && calls.vacuities = 1));
    case "P14-02-02 a counterexample that Lean confirms refutes the statement and ends the search" (fun () ->
        let calls, tools = stub ~test:falsified () in
        let verdict, steps = Refute_search.search tools in
        (match verdict with
         | Refute_search.Refuted { counterexample; by; confirmation = c } ->
           check "the counterexample" (counterexample = one_zero);
           check "found by testing" (by = Refute_search.Test);
           equal_string "with the proof Lean accepted" confirmation.Refute_witness.lean c.Refute_witness.lean
         | _ -> fail "not refuted");
        equal_strings "one step" [ "test counterexample" ] (outcomes steps);
        equal_string "which says how it was found" "falsified at case 5 with seed 0, made smaller 2 times" (List.hd steps).Refute_search.detail;
        check "no further round, no solver, no vacuity check" (List.length calls.tests = 1 && calls.oracles = 0 && calls.vacuities = 0);
        (* A point from a solver is a candidate like any other. *)
        let point = [ ("x", "3"); ("y", "0") ] in
        let calls, tools = stub ~oracle:(fun () -> Ok (Refute_search.Point point)) () in
        let verdict, steps = Refute_search.search tools in
        check "refuted by the solver's point" (match verdict with Refute_search.Refuted { by = Refute_search.Oracle; counterexample; _ } -> counterexample = point | _ -> false);
        equal_strings "after the rounds of testing" [ "test passed"; "test passed"; "test passed"; "oracle counterexample" ] (outcomes steps);
        check "the vacuity check was not needed" (calls.vacuities = 0 && calls.confirms = 1));
    case "P14-02-02 a counterexample that Lean does not confirm leaves the statement open" (fun () ->
        let calls, tools = stub ~test:falsified ~confirm:(fun _ -> Error "simp: unsolved goals") () in
        let verdict, steps = Refute_search.search tools in
        (match verdict with
         | Refute_search.Unconfirmed { counterexample; by; why } ->
           check "the candidate is reported" (counterexample = one_zero && by = Refute_search.Test);
           equal_string "with what Lean said" "simp: unsolved goals" why
         | _ -> fail "the candidate was taken for a refutation");
        (* Another seed may find a counterexample that Lean can confirm, so the rounds go on, and so do the other methods. *)
        equal_strings "every round and every method" [ "test counterexample"; "test counterexample"; "test counterexample"; "oracle unknown"; "vacuity nothing" ] (outcomes steps);
        check "each candidate was put before Lean" (calls.confirms = 3);
        check "none is marked confirmed" (List.for_all (fun (s : Refute_search.step) -> s.Refute_search.confirmation = None) steps);
        equal_string "the step keeps the reason" "simp: unsolved goals" (List.hd steps).Refute_search.rejection;
        (* A later candidate that is confirmed makes a refutation. *)
        let _, tools = stub ~test:falsified ~confirm:(let n = ref 0 in fun _ -> incr n; if !n < 2 then Error "not yet" else Ok confirmation) () in
        let verdict, steps = Refute_search.search tools in
        check "refuted in the second round" (match verdict with Refute_search.Refuted _ -> true | _ -> false);
        equal_int "two rounds" 2 (List.length steps));
    case "P14-02-02 the budget sets the cases, the sizes, the seeds, the rounds and the time" (fun () ->
        let budget = { Refute_search.cases = 250; size = 12; seed = 40; rounds = 4; seconds = 60.0 } in
        let calls, tools = stub () in
        let _ = Refute_search.search ~budget tools in
        check "each round takes the next seed" (calls.tests = [ (250, 12, 40); (250, 12, 41); (250, 12, 42); (250, 12, 43) ]);
        (* No method is started once the time is used up; what was not done is said. *)
        let calls, tools = stub ~test_takes:6.0 () in
        let verdict, steps = Refute_search.search ~budget:{ Refute_search.default_budget with Refute_search.seconds = 10.0 } tools in
        equal_strings "two rounds fit" [ "test passed"; "test passed"; "test skipped"; "oracle skipped"; "vacuity skipped" ] (outcomes steps);
        equal_string "and the rest says why it was left out" "the budget of 10 s was used up" (List.nth steps 2).Refute_search.detail;
        check "nothing was called after the time" (List.length calls.tests = 2 && calls.oracles = 0 && calls.vacuities = 0);
        check "a search that ran out of time found nothing, which is all it says" (verdict = Refute_search.Not_refuted);
        check "the defaults" (Refute_search.default_budget = { Refute_search.cases = 100; size = 30; seed = 0; rounds = 3; seconds = 60.0 }));
    case "P14-02-02 a statement that sampling cannot reach is handed to the other methods" (fun () ->
        (* The hypotheses are hardly ever met: one round says so, and the vacuity check says why. *)
        let calls, tools =
          stub
            ~test:(fun ~seed -> Ok (report ~verdict:Refute_probe.Gave_up ~cases:0 ~discarded:1000 seed))
            ~vacuity:(fun () -> Ok { sound with Refute_probe.vacuous = true; contradiction = Some ([ "h1"; "h2" ], "omega") })
            ()
        in
        let verdict, steps = Refute_search.search tools in
        equal_strings "steps" [ "test gave-up"; "oracle unknown"; "vacuity vacuous" ] (outcomes steps);
        equal_int "one round is enough to know" 1 (List.length calls.tests);
        check "vacuous, with the reason" (verdict = Refute_search.Vacuous "the hypotheses h1, h2 contradict each other (shown by omega)");
        (* A statement that cannot be sampled at all. *)
        let calls, tools = stub ~test:(fun ~seed:_ -> Error "the statement cannot be tested by sampling") ~oracle:(fun () -> Error "julia was not found") () in
        let verdict, steps = Refute_search.search tools in
        equal_strings "what could not be done is said, method by method" [ "test not-applicable"; "oracle unavailable"; "vacuity nothing" ] (outcomes steps);
        check "and is no verdict" (verdict = Refute_search.Not_refuted && List.length calls.tests = 1);
        (* A solver that proves the statement has not refuted it; the claim is noted and nothing more. *)
        let _, tools = stub ~oracle:(fun () -> Ok (Refute_search.Proved "\xE2\x88\x80 x : \xE2\x84\x9A, x \xE2\x89\xA4 x")) () in
        let verdict, steps = Refute_search.search tools in
        check "a proof by a solver" (verdict = Refute_search.Not_refuted && List.mem "oracle proved" (outcomes steps));
        (* A conclusion that holds without its hypotheses is worth saying, and is not vacuity. *)
        let _, tools = stub ~vacuity:(fun () -> Ok { sound with Refute_probe.trivial_by = Some "simp" }) () in
        let verdict, steps = Refute_search.search tools in
        check "a trivial conclusion" (verdict = Refute_search.Not_refuted && List.mem "vacuity trivial" (outcomes steps));
        (* A refutation outweighs everything; vacuity outweighs a candidate that was not confirmed. *)
        let _, tools =
          stub ~test:falsified ~confirm:(fun _ -> Error "no") ~vacuity:(fun () -> Ok { sound with Refute_probe.vacuous = true; empty_domains = [ ("i", "Fin 0", "simp") ] }) ()
        in
        check "vacuity before an open candidate" (fst (Refute_search.search tools) = Refute_search.Vacuous "i ranges over Fin 0, which is empty (shown by simp)"));
    case "P14-02-02 the report says what was found, how, and with which exit status" (fun () ->
        let run tools = Refute_search.run ~file:"Conjectures.lean" ~theorem:"t" ~line:7 ~statement:"theorem t (a b : Nat) : a + b \xE2\x89\xA4 a * b" tools in
        let refuted = run (snd (stub ~test:falsified ())) in
        let vacuous = run (snd (stub ~vacuity:(fun () -> Ok { sound with Refute_probe.vacuous = true; contradiction = Some ([ "h" ], "omega") }) ())) in
        let unconfirmed = run (snd (stub ~test:falsified ~confirm:(fun _ -> Error "simp: unsolved goals") ())) in
        let standing = run (snd (stub ())) in
        let all = [ refuted; vacuous; unconfirmed; standing ] in
        let document = Refute_search.to_json ~file:"Conjectures.lean" all in
        let schema = is_ok "schema" (Json_schema.load (Support.path "schemas/refute/report.schema.json")) in
        check "the report follows its schema" (Json_schema.conforms schema document = Ok ());
        let summary = Option.get (Json.member "summary" document) in
        equal_ints "counts by verdict" [ 4; 1; 1; 1; 1 ]
          (List.map (fun k -> Json.int_or (-1) k summary) [ "statements"; "refuted"; "vacuous"; "unconfirmed"; "standing" ]);
        equal_strings "verdicts" [ "refuted"; "vacuous"; "unconfirmed"; "not-refuted" ]
          (List.map (Json.string_or "" "verdict") (Json.list_or_empty "results" document));
        let first = List.hd (Json.list_or_empty "results" document) in
        equal_string "the refutation is in the report" confirmation.Refute_witness.lean (Json.string_or "" "lean" (Option.get (Json.member "refutation" first)));
        check "an unconfirmed candidate carries no refutation" (Json.member "refutation" (List.nth (Json.list_or_empty "results" document) 2) = None);
        equal_string "summary line" "4 statements: 1 refuted, 1 vacuous, 1 with a counterexample Lean did not confirm, 1 standing" (Refute_search.summary all);
        check "a refutation decides the exit status" (Refute_search.exit_code all = Exit_code.Refuted);
        check "then vacuity and open candidates" (Refute_search.exit_code [ vacuous; standing ] = Exit_code.Not_verified && Refute_search.exit_code [ unconfirmed ] = Exit_code.Not_verified);
        check "nothing found is a success, of the search" (Refute_search.exit_code [ standing ] = Exit_code.Success && Refute_search.exit_code [] = Exit_code.Success);
        let text = Refute_search.render refuted in
        contains "the place" text "t  Conjectures.lean:7";
        contains "the step" text "  test      falsified at case 5 with seed 0, made smaller 2 times: a := 1, b := 0";
        contains "what Lean said" text "  Lean      accepts the refutation, closed by decide; it rests on no axiom";
        contains "the verdict" text "  refuted   the statement is false";
        contains "and the proof" text ("\n    " ^ confirmation.Refute_witness.lean);
        contains "an open candidate" (Refute_search.render unconfirmed) "  Lean      does not confirm it (simp: unsolved goals)";
        contains "is not called a refutation" (Refute_search.render unconfirmed) "the statement is not shown false";
        contains "vacuous" (Refute_search.render vacuous) "the statement is true and says nothing: the hypothesis h cannot hold (shown by omega)";
        contains "standing" (Refute_search.render standing) "nothing was found, which proves nothing";
        check "colour is on request only" (not (Text.contains text "\027[") && Text.contains (Refute_search.render ~color:true refuted) "\027["));
    case "P14-02-02 the recorded report of the example file tells its conjectures apart" (fun () ->
        let document = Support.json "fixtures/refute/conjectures.json" in
        equal_strings "verdicts"
          [ "sum_le_product refuted"; "square_monotone refuted"; "within_budget refuted"; "pred_lt refuted"; "small_and_large vacuous";
            "reverse_append not-refuted" ]
          (List.map (fun r -> Json.string_or "" "theorem" r ^ " " ^ Json.string_or "" "verdict" r) (Json.list_or_empty "results" document));
        List.iter
          (fun r ->
            if Json.string_or "" "verdict" r = "refuted" then begin
              let refutation = Option.get (Json.member "refutation" r) in
              contains "every refutation names its statement" (Json.string_or "" "lean" refutation) ("type_of% @" ^ Json.string_or "" "theorem" r);
              check "and rests on no axiom beyond Lean's logic"
                (List.for_all (fun a -> List.mem a [ Json.String "propext"; Json.String "Classical.choice"; Json.String "Quot.sound" ]) (Json.list_or_empty "axioms" refutation))
            end)
          (Json.list_or_empty "results" document)) ]

(* ---- what is sent to Lean and what comes back ---- *)

let diagnostic severity text = { Bridge_types.severity; line = 1; column = 0; end_line = None; end_column = None; text }
let bridge_report diagnostics =
  let errors = List.length (List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error") diagnostics) in
  { Bridge_types.ok = errors = 0; errors; warnings = 0; diagnostics; decls = []; elapsed_ms = 1 }

let probe_tests =
  [ case "P14-02-02 a statement is examined where it stands, with the tests imported" (fun () ->
        let source = "import Std\n\nnamespace Demo\n\n/-- Not so. -/\ntheorem t (a : Nat) : a < 3 := by\n  omega\n\nend Demo\n" in
        let decl = is_ok "find" (Lean_source.find source "t") in
        equal_string "the import comes first" ("import Maithic.Refute.Vacuity.Command\nimport Std\n\nnamespace Demo\n\n") (Refute_probe.with_library decl.Lean_source.prelude);
        equal_string "the theorem is declared without its proof" "/-- Not so. -/\ntheorem t (a : Nat) : a < 3 := sorry\n" (Refute_probe.declaration decl);
        equal_string "a file of the module system keeps its first line first"
          "-- a note\nmodule\nimport Maithic.Refute.Vacuity.Command\nimport Std\n" (Refute_probe.with_library "-- a note\nmodule\nimport Std\n");
        equal_string "module alone" "module\nimport Maithic.Refute.Vacuity.Command\n" (Refute_probe.with_library "module");
        equal_string "a word that only begins like it" "import Maithic.Refute.Vacuity.Command\nmodules_are_fine\n" (Refute_probe.with_library "modules_are_fine\n");
        equal_string "an empty file" "import Maithic.Refute.Vacuity.Command\n" (Refute_probe.with_library "");
        equal_string "the test, with its settings" "#maithic_test json (cases := 50) (size := 9) (seed := 4) t" (Refute_probe.test_command ~cases:50 ~size:9 ~seed:4 "t");
        equal_string "the vacuity check" "#maithic_vacuous json t" (Refute_probe.vacuity_command "t"));
    case "P14-02-02 the reports of Lean are read, and what is not a report is an error" (fun () ->
        let parse = Json.parse_exn in
        let r = is_ok "falsified" (Refute_probe.test_report_of_json (parse {|{"verdict":"falsified","cases":5,"discarded":2,"shrinks":1,"seed":7,"counterexample":[{"name":"a","value":"1"},{"name":"b","value":"0"}]}|})) in
        check "a failing test" (r = { Refute_probe.verdict = Refute_probe.Falsified; cases = 5; discarded = 2; shrinks = 1; seed = 7; counterexample = one_zero });
        check "a passing one" ((is_ok "passed" (Refute_probe.test_report_of_json (parse {|{"verdict":"passed","cases":100,"counterexample":[]}|}))).Refute_probe.verdict = Refute_probe.Passed);
        check "one that gave up" ((is_ok "gave up" (Refute_probe.test_report_of_json (parse {|{"verdict":"gave-up","discarded":1000}|}))).Refute_probe.verdict = Refute_probe.Gave_up);
        contains "an unknown verdict" (is_error "odd" (Refute_probe.test_report_of_json (parse {|{"verdict":"perhaps"}|}))) "the verdict 'perhaps'";
        contains "a value without a name" (is_error "unnamed" (Refute_probe.test_report_of_json (parse {|{"verdict":"falsified","counterexample":[{"value":"1"}]}|}))) "not all named";
        let v =
          is_ok "vacuity"
            (Refute_probe.vacuity_of_json
               (parse {|{"vacuous":true,"contradiction":{"found_by":"omega","hypotheses":["h1","h2"]},"empty_domains":[{"variable":"i","type":"Fin 0","found_by":"simp"}],"trivial_conclusion":null}|}))
        in
        check "a contradiction and an empty domain" (v.Refute_probe.vacuous && v.Refute_probe.contradiction = Some ([ "h1"; "h2" ], "omega") && v.Refute_probe.empty_domains = [ ("i", "Fin 0", "simp") ]);
        equal_string "in a sentence" "the hypotheses h1, h2 contradict each other (shown by omega); i ranges over Fin 0, which is empty (shown by simp)" (Refute_probe.describe_vacuity v);
        equal_string "one hypothesis" "the hypothesis h cannot hold (shown by simp)" (Refute_probe.describe_vacuity { sound with Refute_probe.vacuous = true; contradiction = Some ([ "h" ], "simp") });
        let trivial = is_ok "trivial" (Refute_probe.vacuity_of_json (parse {|{"vacuous":false,"contradiction":null,"empty_domains":[],"trivial_conclusion":{"found_by":"simp"}}|})) in
        equal_string "a trivial conclusion" "the conclusion holds without the hypotheses (shown by simp)" (Refute_probe.describe_vacuity trivial);
        equal_string "nothing to say" "" (Refute_probe.describe_vacuity sound);
        contains "not a vacuity report" (is_error "odd" (Refute_probe.vacuity_of_json (parse {|{"verdict":"passed"}|}))) "does not say whether the statement is vacuous";
        (* The report is the last message that is a JSON object; a warning about sorry before it is not in the way. *)
        let printed = Refute_probe.printed (bridge_report [ diagnostic "warning" "declaration uses `sorry`"; diagnostic "info" "{\"verdict\": \"passed\",\n \"cases\": 3}" ]) in
        check "the printed report" (Json.int_or 0 "cases" (is_ok "printed" printed) = 3);
        equal_string "a command that failed says why" "1:0: this statement cannot be tested by sampling"
          (is_error "failed" (Refute_probe.printed (bridge_report [ diagnostic "error" "this statement cannot be tested by sampling" ])));
        equal_string "a command that printed nothing" "the command printed no report" (is_error "silent" (Refute_probe.printed (bridge_report [ diagnostic "info" "passed 3 cases" ])));
        check "a missing library is told from other errors"
          (Refute_probe.library_missing "1:0: unknown module prefix 'Maithic'\n\nNo directory 'Maithic' or file 'Maithic.olean' in the search path entries"
           && not (Refute_probe.library_missing "3:8: unknown identifier 'foo'"))) ]

let () = run "refute" [ ("witness", witness_tests); ("search", search_tests); ("probe", probe_tests) ]
