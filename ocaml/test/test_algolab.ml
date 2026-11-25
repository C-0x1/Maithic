(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the algorithm laboratory: the task, the screen, the prompts, the loop and its record.
   A model is played by scripted answers and Lean by a function, so that every way a run can go
   is taken. The real Lean judges real drafts in the integration tests. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_gateway
open Maithic_algolab
open Testkit

let task_file = "examples/algo/run-lengths.toml"
let demo () = is_ok "the task" (Algolab_task.read (Support.path task_file))
let recorded name = Support.json ("fixtures/algolab/" ^ name ^ ".json")

let small ?signature ?(examples = [ { Algolab_task.call = "f 2"; expect = "4" }; { Algolab_task.call = "f 0"; expect = "0" } ]) () =
  is_ok "a task" (Algolab_task.make ?signature ~examples ~name:"f" "Twice the number.")

let scripted answers =
  let handler, seen = Scripted.sequence (List.map (fun text -> Ok (Scripted.answer text)) answers) in
  match Gateway.create ~scripted:handler (Maithic_config.Config.of_defaults ()) with Ok g -> (g, seen) | Error f -> fail (Fault.to_string f)

let answer implementation = Json.to_string (Json.Obj [ ("implementation", Json.String implementation); ("notes", Json.String "A note.") ])

(* A Lean that knows a few definitions by their text. *)
let compiles = { Algolab_synth.errors = []; examples = [] }
let ran (task : Algolab_task.t) outcomes =
  { Algolab_synth.errors = [];
    examples = List.map2 (fun example (got, passed) -> { Algolab_synth.example; got; passed; error = "" }) task.Algolab_task.examples outcomes }

let good = "def f (n : Nat) : Nat := n + n"
let wrong = "def f (n : Nat) : Nat := n * n"
let broken = "def f (n : Nat) : Nat := n +"

let lean_of ?(asked = ref []) task implementation =
  asked := !asked @ [ implementation ];
  if implementation = good then Ok (ran task [ ("4", true); ("0", true) ])
  else if implementation = wrong then Ok (ran task [ ("4", true); ("0", true) ])
  else if implementation = broken then Ok { compiles with Algolab_synth.errors = [ "line 1: unexpected end of input" ] }
  else Ok (ran task [ ("5", false); ("0", true) ])

(* Tools whose model gives the listed implementations in order, and records what it was told. *)
let tools ?(asked = ref []) ?(told = ref []) ?(lean = lean_of) task answers =
  let left = ref answers in
  let next origin =
    match !left with
    | [] -> Error "the model has no more to say"
    | text :: rest ->
      left := rest;
      Ok (text, { Algolab_draft.origin; provider = "scripted"; model = "m"; prompt = "algolab." ^ origin })
  in
  { Algolab_synth.draft = (fun () -> next "draft");
    repair =
      (fun ~implementation ~report ~earlier ->
        told := !told @ [ (implementation, report, earlier) ];
        next "repair");
    lean = lean ~asked task }

let origins (s : Algolab_synth.t) = List.map (fun (a : Algolab_synth.attempt) -> a.Algolab_synth.provenance.Algolab_draft.origin) s.Algolab_synth.attempts

let task_tests =
  [ case "P05-04-01 a task is read from a file: a name, what is asked, and the signature, constraints and examples when there are any" (fun () ->
        let t = demo () in
        equal_string "the name" "runLengths" t.Algolab_task.name;
        contains "what is asked" t.Algolab_task.description "Run-length encoding";
        check "the signature" (t.Algolab_task.signature = Some "(l : List Nat) : List (Nat × Nat)");
        equal_int "the constraints" 2 (List.length t.Algolab_task.constraints);
        equal_strings "the examples, in order"
          [ "runLengths [1, 1, 2, 3, 3, 3]"; "runLengths []"; "runLengths [7]"; "runLengths [4, 4, 4, 4]"; "(runLengths [5, 5, 6, 5]).length" ]
          (List.map (fun (e : Algolab_task.example) -> e.Algolab_task.call) t.Algolab_task.examples);
        equal_string "the line the definition starts with" "def runLengths (l : List Nat) : List (Nat × Nat)" (Algolab_task.header t);
        let bare = is_ok "words are enough" (Algolab_task.of_document (Json.Obj [ ("name", Json.String " twice "); ("description", Json.String "Twice the number.\n") ])) in
        equal_string "the name is trimmed" "twice" bare.Algolab_task.name;
        check "nothing else is needed" (bare.Algolab_task.signature = None && bare.Algolab_task.constraints = [] && bare.Algolab_task.examples = []);
        contains "and the model is told to choose" (Algolab_task.header bare) "choose one";
        let again = is_ok "reads back" (Algolab_task.of_document (match Algolab_task.to_json t with Json.Obj m -> Json.Obj (List.map (fun (k, v) -> ((if k = "examples" then "example" else k), v)) m) | j -> j)) in
        check "a task written as JSON is the same task" (again = t));
    case "P05-04-01 a task that cannot be worked on is refused, with the reason" (fun () ->
        let refused ?(examples = []) name description = is_error "refused" (Algolab_task.make ~examples ~name description) in
        contains "a name Lean cannot take" (refused "run lengths" "x") "cannot be the name of a function";
        contains "a name with a dot" (refused "List.f" "x") "cannot be the name";
        contains "no name" (refused "" "x") "cannot be the name";
        contains "nothing asked" (refused "f" "  \n ") "says nothing";
        contains "half an example" (refused ~examples:[ { Algolab_task.call = "f 1"; expect = " " } ] "f" "x") "needs a call and what the call should give";
        contains "an example that is a command" (refused ~examples:[ { Algolab_task.call = "f 1"; expect = "2\n#eval IO.println 1" } ] "f" "x") "not a plain expression";
        contains "an example that reaches outside" (refused ~examples:[ { Algolab_task.call = "IO.FS.readFile \"x\""; expect = "1" } ] "f" "x") "not a plain expression";
        contains "a file that is not TOML" (is_error "refused" (Algolab_task.read (Support.path "README.md"))) "README.md");
    case "P05-04-01 a draft is screened before Lean sees it: one definition of the function that was asked for, and nothing that acts" (fun () ->
        let violations = Algolab_screen.violations ~name:"f" in
        equal_strings "a plain definition passes" [] (violations good);
        equal_strings "with a comment, a where clause and a termination clause" []
          (violations "/-- Twice. -/\ndef f (n : Nat) : Nat := go n 0\nwhere\n  go : Nat → Nat → Nat\n    | 0, acc => acc\n    | k + 1, acc => go k (acc + 2)\ntermination_by structural n => n");
        check "the function has to be the one asked for" (List.exists (fun r -> Text.contains r "does not define f") (violations "def g (n : Nat) : Nat := n"));
        check "a longer name is another function" (List.exists (fun r -> Text.contains r "does not define f") (violations "def fast (n : Nat) : Nat := n"));
        check "one definition at the top" (List.exists (fun r -> Text.contains r "more than one definition") (violations "def h (n : Nat) : Nat := n\ndef f (n : Nat) : Nat := h n"));
        List.iter
          (fun (what, draft) -> check what (violations draft <> []))
          [ ("a partial definition", "partial def f (n : Nat) : Nat := f n"); ("an unsafe one", "unsafe def f (n : Nat) : Nat := n");
            ("a command that evaluates", good ^ "\n#eval f 1"); ("an action", "def f (n : Nat) : IO Nat := do\n  IO.println n\n  pure n");
            ("a hole", "def f (n : Nat) : Nat := sorry"); ("an axiom", "axiom cheat : False\n" ^ good);
            ("a compiler hook", "@[implemented_by g] " ^ good) ]) ]

let prompt_tests =
  [ case "P05-04-02 the drafting prompt asks for one total function in the style of the laboratory, and shows the task as data" (fun () ->
        let t = demo () in
        let prompt = is_ok "the prompt is built in" (Prompt.load Algolab_draft.draft_prompt) in
        let system, user = is_ok "renders" (Prompt.render prompt (Algolab_draft.draft_bindings t)) in
        contains "the task" system "write one Lean 4 function";
        contains "the style" system "No `partial`, no `unsafe`, no `IO`, no `sorry`";
        contains "the libraries" system "Only the Lean core library, without Mathlib";
        contains "no proof is asked for" system "You are not asked to prove anything";
        contains "what is asked is data" user ("<data>\n" ^ t.Algolab_task.description);
        contains "the signature" user "def runLengths (l : List Nat) : List (Nat × Nat)";
        contains "the constraints, one to a line" user "- Nothing beyond the Lean core library.";
        contains "the examples" user "runLengths [7]  should give  [(7, 1)]";
        let none = is_ok "a task of words" (Algolab_task.make ~name:"f" "Twice.") in
        let _, bare = is_ok "renders" (Prompt.render prompt (Algolab_draft.draft_bindings none)) in
        contains "nothing is made up for what the task leaves out" bare "Constraints:\n<data>\n(none)";
        let gateway, seen = scripted [ answer ("```lean\n" ^ good ^ "\n```") ] in
        let implementation, provenance = is_ok "a draft" (Algolab_draft.draft gateway ~max_tokens:512 (small ())) in
        equal_string "the fence around the answer is taken off" good implementation;
        equal_string "who wrote it" "draft" provenance.Algolab_draft.origin;
        equal_string "with which prompt" "algolab.draft" provenance.Algolab_draft.prompt;
        equal_int "one question was put" 1 (List.length (seen ()));
        let silent, _ = scripted [ "Here is the function you wanted." ] in
        contains "an answer that is not JSON is no draft" (is_error "refused" (Algolab_draft.draft silent ~max_tokens:512 (small ()))) "not JSON";
        let empty, _ = scripted [ "{\"implementation\": \"  \"}" ] in
        contains "nor is an empty one" (is_error "refused" (Algolab_draft.draft empty ~max_tokens:512 (small ()))) "no implementation");
    case "P05-04-02 the correcting prompt shows the last version, what happened to it, and the versions not to repeat" (fun () ->
        let prompt = is_ok "the prompt is built in" (Prompt.load Algolab_draft.repair_prompt) in
        let bindings = Algolab_draft.repair_bindings (small ()) ~implementation:wrong ~report:"Lean reports:\nline 1: an error" ~earlier:[ broken ] in
        let system, user = is_ok "renders" (Prompt.render prompt bindings) in
        contains "the task" system "correct one Lean 4 function";
        contains "the last version" user ("The last version:\n<data>\n" ^ wrong);
        contains "what happened" user "What happened to it:\n<data>\nLean reports:\nline 1: an error";
        contains "what was tried" user ("not to be repeated:\n<data>\n" ^ broken);
        let _, first = is_ok "renders" (Prompt.render prompt (Algolab_draft.repair_bindings (small ()) ~implementation:wrong ~report:"r" ~earlier:[])) in
        contains "nothing tried before" first "not to be repeated:\n<data>\n(none)") ]

let loop_tests =
  [ case "P05-04-03 a draft that does not compile goes back to the model with what Lean said, until one is accepted" (fun () ->
        let task = small () and told = ref [] and asked = ref [] in
        let s = Algolab_synth.run (tools ~asked ~told task [ broken; good ]) task in
        check "accepted" (Algolab_synth.accepted s);
        equal_strings "a draft, then a correction" [ "draft"; "repair" ] (origins s);
        equal_strings "Lean saw both" [ broken; good ] !asked;
        (match !told with
         | [ (implementation, report, earlier) ] ->
           equal_string "the model is shown the version that failed" broken implementation;
           contains "and what Lean said" report "Lean reports:\nline 1: unexpected end of input";
           equal_strings "nothing was tried before it" [] earlier
         | _ -> fail "the model was asked for one correction");
        equal_string "the final implementation" good (Option.get (Algolab_synth.final s)).Algolab_synth.implementation;
        equal_string "in a line" "accepted: f compiles and gets 2 example(s) right, after 2 attempt(s)" (Algolab_synth.summary s));
    case "P05-04-03 the loop is bounded, and ends early when the model repeats itself or cannot be asked" (fun () ->
        let task = small () in
        let other n = Printf.sprintf "def f (n : Nat) : Nat := n + %d" n in
        let s = Algolab_synth.run ~rounds:2 (tools task [ other 1; other 2; other 3; good ]) task in
        check "not accepted" (not (Algolab_synth.accepted s));
        equal_int "a draft and two corrections" 3 (List.length s.Algolab_synth.attempts);
        check "the reason" (s.Algolab_synth.status = Algolab_synth.Failed "no implementation was accepted after 2 correction(s)");
        check "there is no final implementation" (Algolab_synth.final s = None && Algolab_synth.lean_text s = None);
        let told = ref [] in
        let s = Algolab_synth.run ~rounds:5 (tools ~told task [ other 1; other 2; other 1; good ]) task in
        check "a version that was tried is not tried again" (s.Algolab_synth.status = Algolab_synth.Failed "the model returned an implementation it had returned before");
        equal_int "and Lean is not asked about it" 2 (List.length s.Algolab_synth.attempts);
        (match List.rev !told with
         | (_, _, earlier) :: _ -> equal_strings "the model was told what had been tried" [ other 1 ] earlier
         | [] -> fail "the model was asked");
        let s = Algolab_synth.run (tools task [ other 1 ]) task in
        check "a model that falls silent ends the run" (s.Algolab_synth.status = Algolab_synth.Failed "the model was not asked for a correction: the model has no more to say");
        let s = Algolab_synth.run (tools task []) task in
        check "and one that never spoke leaves no attempt" (s.Algolab_synth.attempts = [] && s.Algolab_synth.status = Algolab_synth.Failed "no implementation was drafted: the model has no more to say");
        let s = Algolab_synth.run ~rounds:0 ~given:(other 1) (tools task [ good ]) task in
        check "with no rounds a given implementation is judged once" (origins s = [ "given" ] && s.Algolab_synth.status = Algolab_synth.Failed "the implementation was not accepted");
        let s = Algolab_synth.run ~given:(other 1) (tools task [ good ]) task in
        equal_strings "a given implementation that fails is corrected like a draft" [ "given"; "repair" ] (origins s);
        let down = { (tools task [ good ]) with Algolab_synth.lean = (fun _ -> Error "the Bridge is gone") } in
        check "a Lean that cannot be asked ends the run" ((Algolab_synth.run down task).Algolab_synth.status = Algolab_synth.Failed "Lean could not be asked: the Bridge is gone"));
    case "P05-04-03 a draft that may not be given to Lean is refused unseen, and the model is told why" (fun () ->
        let task = small () and asked = ref [] and told = ref [] in
        let s = Algolab_synth.run (tools ~asked ~told task [ "partial def f (n : Nat) : Nat := f n"; good ]) task in
        check "accepted in the end" (Algolab_synth.accepted s);
        equal_strings "Lean saw only the second draft" [ good ] !asked;
        let first = List.hd s.Algolab_synth.attempts in
        check "the first has a reason and no verdict" (first.Algolab_synth.refused <> [] && first.Algolab_synth.verdict = None && not first.Algolab_synth.accepted);
        (match !told with (_, report, _) :: _ -> contains "the report" report "The definition was not given to Lean:\n- " | [] -> fail "the model was asked");
        contains "in the terminal" (Algolab_synth.render s) "attempt 1  (draft)  refused: ") ]

let example_tests =
  let example = { Algolab_task.call = "f 2"; expect = "4" } in
  let diagnostic ?(severity = "info") line text = { Bridge_types.severity; line; column = 0; end_line = None; end_column = None; text } in
  let report diagnostics =
    let errors = List.length (List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error") diagnostics) in
    { Bridge_types.ok = errors = 0; errors; warnings = 0; diagnostics; decls = []; elapsed_ms = 1 }
  in
  [ case "P05-04-04 an example is run through #eval twice: for the value, and for the comparison with what is expected" (fun () ->
        equal_string "the commands" "#eval (f 2)\n#eval ((f 2) == (4))" (Algolab_lean.example_commands example);
        let right = Algolab_lean.example_result example (report [ diagnostic 1 "4\n"; diagnostic 2 "true" ]) in
        check "a right answer" (right.Algolab_synth.passed && right.Algolab_synth.got = "4" && right.Algolab_synth.error = "");
        let off = Algolab_lean.example_result example (report [ diagnostic 1 "5"; diagnostic 2 "false" ]) in
        check "a wrong one, with the value it gave" ((not off.Algolab_synth.passed) && off.Algolab_synth.got = "5" && off.Algolab_synth.error = "");
        let untyped = Algolab_lean.example_result example (report [ diagnostic 1 "5"; diagnostic ~severity:"error" 2 "type mismatch\n  4\nhas type String" ]) in
        check "a comparison Lean cannot make" ((not untyped.Algolab_synth.passed) && untyped.Algolab_synth.error = "type mismatch");
        let unprintable = Algolab_lean.example_result example (report [ diagnostic ~severity:"error" 1 "could not synthesize a 'Repr' instance"; diagnostic 2 "true" ]) in
        check "a value that cannot be printed can still be right" (unprintable.Algolab_synth.passed && unprintable.Algolab_synth.got = "");
        let nothing = Algolab_lean.example_result example (report []) in
        check "silence is not a pass" ((not nothing.Algolab_synth.passed) && nothing.Algolab_synth.error = "Lean gave no answer");
        let failed = Algolab_lean.example_result example (report [ diagnostic ~severity:"error" 1 "unknown identifier 'f'" ]) in
        check "nor is an error" ((not failed.Algolab_synth.passed) && failed.Algolab_synth.error = "unknown identifier 'f'"));
    case "P05-04-04 a function is accepted only when every example gives what was expected, and a wrong answer goes back to the model" (fun () ->
        let task = small () and told = ref [] in
        let off = "def f (n : Nat) : Nat := n + 3" in
        let s = Algolab_synth.run (tools ~told task [ off; good ]) task in
        check "accepted on the second attempt" (Algolab_synth.accepted s && List.length s.Algolab_synth.attempts = 2);
        let first = List.hd s.Algolab_synth.attempts in
        check "the first compiled" ((Option.get first.Algolab_synth.verdict).Algolab_synth.errors = []);
        check "and one example was wrong" (not (Algolab_synth.passed (Option.get first.Algolab_synth.verdict)));
        (match !told with
         | (_, report, _) :: _ ->
           contains "the report names the example" report "It gets these examples wrong:\n- f 2  should give  4  and gives  5";
           check "and not the one that was right" (not (Text.contains report "f 0"))
         | [] -> fail "the model was asked");
        let shown = Algolab_synth.render s in
        contains "the first attempt" shown "attempt 1  (draft)  compiles; 1 of 2 example(s) right\n    f 2  should give  4  and gives  5";
        contains "the second" shown "attempt 2  (repair)  compiles; 2 of 2 example(s) right";
        let none = small ~examples:[] () in
        let s = Algolab_synth.run (tools ~lean:(fun ?asked:_ _ _ -> Ok compiles) none [ good ]) none in
        check "a task without examples asks only that the function compiles" (Algolab_synth.accepted s);
        let crashed = { Algolab_synth.errors = []; examples = [ { Algolab_synth.example; got = ""; passed = false; error = "not run: Lean had stopped" } ] } in
        let report = Algolab_synth.report { first with Algolab_synth.verdict = Some crashed } in
        contains "an example that could not be run is reported as such" report "- f 2  could not be run: not run: Lean had stopped") ]

let record_tests =
  let schema = Json_schema.of_json (Support.json "schemas/algolab/synthesis.schema.json") in
  let valid what j = equal_strings what [] (List.map Json_schema.error_to_string (Json_schema.validate schema j)) in
  [ case "P05-04-05 the record keeps every draft, what Lean said about each, and the implementation that was accepted" (fun () ->
        let task = small () in
        let s = Algolab_synth.run (tools task [ "unsafe def f (n : Nat) : Nat := n"; broken; "def f (n : Nat) : Nat := n + 3"; good ]) task in
        let j = Algolab_synth.to_json s in
        valid "the record" j;
        equal_string "its status" "accepted" (Json.string_or "" "status" j);
        equal_string "the final implementation" good (Json.string_or "" "implementation" j);
        let attempts = Json.list_or_empty "attempts" j in
        equal_ints "the rounds" [ 0; 1; 2; 3 ] (List.map (Json.int_or (-1) "round") attempts);
        equal_strings "every draft is kept" [ "unsafe def f (n : Nat) : Nat := n"; broken; "def f (n : Nat) : Nat := n + 3"; good ] (List.map (Json.string_or "" "implementation") attempts);
        (match attempts with
         | [ refused; uncompiled; off; accepted ] ->
           check "a refused draft has its reasons and nothing from Lean" (Json.list_or_empty "refused" refused <> [] && Json.member "errors" refused = None && Json.member "examples" refused = None);
           equal_strings "the errors of Lean" [ "line 1: unexpected end of input" ] (Json.strings (Json.List (Json.list_or_empty "errors" uncompiled)));
           equal_strings "what each example gave" [ "5"; "0" ] (List.map (Json.string_or "" "got") (Json.list_or_empty "examples" off));
           check "only the last is accepted" (List.map (Json.bool_or false "accepted") attempts = [ false; false; false; true ]);
           equal_string "who wrote it" "repair" (Json.string_or "" "origin" (Option.get (Json.member "provenance" accepted)))
         | _ -> fail "four attempts");
        let failed = Algolab_synth.run ~rounds:0 (tools task [ broken ]) task in
        let j = Algolab_synth.to_json failed in
        valid "the record of a run that failed" j;
        check "it has a reason and no implementation" (Json.string_or "" "status" j = "failed" && Json.member "implementation" j = None && Json.string_or "" "reason" j <> "");
        let given = Algolab_synth.to_json (Algolab_synth.run ~given:good (tools task []) task) in
        valid "the record of a given implementation" given;
        check "which names no model" (Json.member "model" (Option.get (Json.member "provenance" (List.hd (Json.list_or_empty "attempts" given)))) = None));
    case "P05-04-05 an accepted function is written as a Lean file whose examples Lean checks again" (fun () ->
        let task = small () in
        let s = Algolab_synth.run (tools task [ good ]) task in
        let text = Option.get (Algolab_synth.lean_text s) in
        contains "the definition" text ("\n" ^ good ^ "\n");
        contains "the examples as checks" text "#guard (f 2) == (4)\n#guard (f 0) == (0)\n";
        contains "and what it does not claim" text "nothing is proved about it";
        valid "the recorded run" (recorded "run-lengths");
        valid "the recorded run of a given implementation" (recorded "loses-a-run");
        let run = recorded "run-lengths" in
        equal_strings "the demonstration: no termination, a lost run, accepted" [ "draft"; "repair"; "repair" ]
          (List.map (fun a -> Json.string_or "" "origin" (Option.get (Json.member "provenance" a))) (Json.list_or_empty "attempts" run));
        contains "the file the repository keeps holds the accepted function" (Support.read "fixtures/algolab/lean/RunLengths.lean") (Json.string_or "" "implementation" run);
        let report = Support.json "fixtures/algolab/differential.json" in
        equal_strings "the recorded differential report" []
          (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/algolab/differential.schema.json")) report));
        check "in which every answer was accepted" (Algolab_differential.agrees report);
        equal_string "in a line" "agree: 190 case(s) over 19 algorithm(s), and every answer is accepted by its reference" (Algolab_differential.summary report);
        let differs =
          Json.Obj
            [ ("agrees", Json.Bool false); ("cases", Json.Int 4); ("mismatches", Json.Int 1); ("algorithms", Json.List [ Json.Obj [ ("name", Json.String "gcd"); ("cases", Json.Int 4); ("mismatches", Json.Int 1) ] ]);
              ("failures", Json.List [ Json.Obj [ ("algorithm", Json.String "gcd"); ("request", Json.String "gcd 4 6"); ("expected", Json.String "2"); ("answer", Json.String "3") ] ]) ]
        in
        check "a report with a mismatch does not agree" (not (Algolab_differential.agrees differs));
        equal_string "and says how many" "differ: 1 of 4 answer(s) are not accepted by the reference" (Algolab_differential.summary differs);
        equal_strings "and which" [ "gcd: the request `gcd 4 6` should give `2` and gives `3`" ] (Algolab_differential.failures differs);
        check "the rows" (Algolab_differential.rows differs = [ ("gcd", 4, 1) ])) ]

let () =
  run "algolab"
    [ ("tasks", task_tests); ("prompts", prompt_tests); ("the loop", loop_tests); ("examples", example_tests); ("records", record_tests) ]
