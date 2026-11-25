(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The pictures of terminal sessions in the documentation, and the commands
    they are made from. Each is run from the root of the repository with the
    built command, in replay mode, with nothing of the user's own
    configuration; what it prints is the picture. *)

open Maithic_base

type shot = {
  name : string;  (** The picture is [assets/readme/term-NAME.svg]. *)
  title : string;
  label : string;  (** What the picture shows, for readers who cannot see it. *)
  commands : (string list * int) list;  (** Arguments of [maithic] and the exit status that is expected. *)
  max_lines : int;
  columns : int;  (** The width of the terminal, in characters. *)
}

let shot ?(max_lines = 60) ?(columns = 120) name title label commands = { name; title; label; commands; max_lines; columns }

let all =
  [ shot "refute" "maithic refute"
      "A terminal session: maithic refute examines three unproved statements. One is refuted by a counterexample, with the Lean proof that confirms it; one is found vacuous because its hypotheses contradict each other; one stands after three rounds of tests."
      [ ([ "refute"; "examples/conjectures.lean"; "--theorem"; "square_monotone" ], 8);
        ([ "refute"; "examples/conjectures.lean"; "--theorem"; "small_and_large"; "--method"; "test,vacuity" ], 7);
        ([ "refute"; "examples/conjectures.lean"; "--theorem"; "reverse_append"; "--method"; "test" ], 0) ];
    shot "refute-file" "maithic refute examples/conjectures.lean"
      "A terminal session: maithic refute examines the six unproved statements of a file. Four are refuted, each with a counterexample and the Lean proof that confirms it, among them one that is false by a slip of the key; one is found vacuous; one stands."
      [ ([ "refute"; "examples/conjectures.lean" ], 8) ] ~max_lines:80;
    shot "refute-oracle" "maithic refute --method oracle"
      "A terminal session: a solver finds the exact point x = 3, y = 0 where a statement about rationals fails, and Lean accepts the refutation built from it."
      [ ([ "refute"; "examples/conjectures.lean"; "--theorem"; "within_budget"; "--method"; "oracle" ], 8) ];
    shot "review" "maithic review --suggest"
      "A terminal session: maithic review reports a proof that one call of omega replaces and a step that is not needed, each edit accepted by Lean, and a suppression comment without a reason."
      [ ([ "review"; "fixtures/review/lean/Simplify.lean"; "--suggest" ], 0) ];
    shot "review-errors" "maithic review"
      "A terminal session: maithic review of a file with unfinished proofs lists each finding with its place, severity, rule and the line of source, and ends with a summary."
      [ ([ "review"; "fixtures/intel/lean/Demo.lean" ], 7) ] ~max_lines:44;
    shot "intel-show" "maithic intel show, dependents"
      "A terminal session: maithic intel show prints what a definition uses and what uses it; intel dependents lists every declaration a change would reach, with its distance."
      [ ([ "intel"; "show"; "insertSorted"; "--dir"; "fixtures/intel" ], 0);
        ([ "intel"; "dependents"; "Maithic.Algo.insertSorted"; "--dir"; "fixtures/intel" ], 0) ];
    shot "intel-health" "maithic intel health"
      "A terminal session: maithic intel health counts declarations by kind, lists every unfinished proof with its open goal, and names the axioms the code relies on."
      [ ([ "intel"; "health"; "--dir"; "fixtures/intel" ], 0) ];
    shot "corpus" "maithic corpus harvest, verify"
      "A terminal session: maithic corpus harvest traces the Lean files of a library into theorem, step, premise and pair records, and corpus verify confirms the dataset against its manifest."
      [ ([ "corpus"; "harvest"; "lean/Maithic/Algo"; "--project"; "lean"; "--out"; ".maithic/dataset"; "--revision"; "v0.1.0" ], 0);
        ([ "corpus"; "verify"; ".maithic/dataset" ], 0) ];
    shot "prove" "maithic prove"
      "A terminal session: maithic prove closes a theorem about even numbers; the proof that the automation found is checked by Lean's kernel and the result is verified, with the axioms it rests on."
      [ ([ "prove"; "examples/even.lean" ], 0) ];
    shot "prove-search" "maithic prove --no-ai --search"
      "A terminal session: with models switched off, maithic prove finds a proof by step-wise search, by induction with rfl for the first case and simp and omega for the second, and the result is verified after 19 attempts."
      [ ([ "prove"; "examples/double.lean"; "--no-ai"; "--search" ], 0) ];
    shot "tree" "maithic tree --steps"
      "A terminal session: maithic tree prints a recorded proof search step by step: which goal was expanded, which tactics were tried, and what each led to."
      [ ([ "tree"; "fixtures/trees/found/double_eq.json"; "--steps" ], 0) ] ~max_lines:40;
    shot "oracle" "maithic oracle solve"
      "A terminal session: maithic oracle list prints the three oracles with the goals and number types each takes, and maithic oracle solve proves a goal of linear integer arithmetic and prints the claim, the name of its certificate and the Lean script that replays it."
      [ ([ "oracle"; "list" ], 0); ([ "oracle"; "solve"; "fixtures/oracle/goals/linear-integer-gap.json" ], 0) ];
    shot "guide" "maithic prove --guide"
      "A terminal session: the same theorem is proved twice by step-wise search, first without and then with a trained model that orders the tactics. Both proofs are verified by Lean; the guided one is a step shorter, and the last line says that the guide answered six of six questions."
      [ ([ "prove"; "examples/guided.lean"; "--no-ai"; "--search" ], 0); ([ "prove"; "examples/guided.lean"; "--no-ai"; "--guide"; "fixtures/guide/library/model.json" ], 0) ];
    shot "guide-rank" "maithic guide info, rank"
      "A terminal session: maithic guide info describes a trained tactic model, with its most frequent tactic templates and how often each closed a goal, and maithic guide rank lists the tactics the model would try first on the goal of a theorem, with the model's probability, the preference of the search and the two combined."
      [ ([ "guide"; "info"; "fixtures/guide/library/model.json"; "--top"; "5" ], 0);
        ([ "guide"; "rank"; "fixtures/guide/library/model.json"; "--file"; "examples/double.lean"; "--theorem"; "double_eq"; "--top"; "6" ], 0) ];
    shot "mend" "maithic mend scan, repair"
      "A terminal session: maithic mend scan lists five breakages of a Lean file after an upgrade, each with its class; maithic mend repair then repairs three by rule and two with a model, every repair compiled by Lean, and reports that Lean accepts the whole file."
      [ ([ "mend"; "scan"; "fixtures/mend/lean/Upgrade.lean" ], 7); ([ "mend"; "repair"; "fixtures/mend/lean/Upgrade.lean"; "--renames"; "fixtures/mend/renames.toml" ], 0) ];
    shot "mend-minimize" "maithic mend minimize --automation"
      "A terminal session: maithic mend minimize shortens the proofs of a file. Four of six proofs get shorter, each shorter proof compiled by Lean with the same statement and no new axiom, and one is left alone because the shorter proof would need axioms the original does without."
      [ ([ "mend"; "minimize"; "fixtures/mend/lean/Verbose.lean"; "--automation" ], 0) ];
    shot "formalize" "maithic formalize"
      "A terminal session: maithic formalize turns the sentence 'the square of a negative integer is positive' into four candidate Lean statements. Lean rejects one as vacuous and one with the counterexample n = 0, a third is repaired and found to be a duplicate, and the remaining statement passes every check and is chosen."
      [ ([ "formalize"; "--file"; "fixtures/formalize/problems/negative-square.txt"; "--candidates"; "4" ], 0) ] ~max_lines:64;
    shot "formalize-flagged" "maithic formalize, on a problem that is false as posed"
      "A terminal session: for the claim that (a - b) + b = a over the natural numbers, Lean refutes the literal statement at a = 0, b = 1. The statement that is chosen adds an assumption, and the report flags it: its subtraction is cut off at zero, and its reading differs from the problem."
      [ ([ "formalize"; "--file"; "fixtures/formalize/problems/subtraction.md" ], 7) ] ~max_lines:64;
    shot "formalize-prove" "maithic prove, on the statement that was formalized"
      "A terminal session: maithic prove takes the Lean file that maithic formalize wrote and proves its theorem. After 21 attempts a proof from the model is verified, and the statement, the proof and the one axiom it rests on are printed."
      [ ([ "prove"; "fixtures/formalize/lean/NegativeSquare.lean" ], 0) ];
    shot "council" "maithic council"
      "A terminal session: a council works on the theorem that the first n odd numbers add up to n squared. The verifier examines the statement; the researcher names lemmas, four of five of which exist; the prover's first two proofs are rejected by Lean; the critic explains why; the prover's next proof is accepted by Lean's kernel and the claim is verified."
      [ ([ "council"; "examples/council.lean" ], 0) ];
    shot "council-examine" "maithic council, when the examination settles it"
      "A terminal session: two sessions that end before any model is asked. A false statement is refuted in the examination with the counterexample a = 1, b = 0, and a statement that automation can prove is verified in the examination."
      [ ([ "council"; "examples/conjectures.lean"; "--theorem"; "sum_le_product" ], 8); ([ "council"; "examples/even.lean"; "--no-ai" ], 0) ];
    shot "blueprint" "maithic blueprint status, next"
      "A terminal session: maithic blueprint status lists the eight nodes of a blueprint with what Lean reports of each: three are done, one is proved on lemmas that are not, three are stated and one is planned; two of the stated ones are ready to prove. maithic blueprint next then names those two, the one that most rests on first."
      [ ([ "blueprint"; "status"; "examples/blueprint/triangles.md"; "--lean"; "examples/blueprint/triangles.lean" ], 0);
        ([ "blueprint"; "next"; "examples/blueprint/triangles.md"; "--lean"; "examples/blueprint/triangles.lean" ], 0) ];
    shot "blueprint-plan" "maithic blueprint plan, skeleton"
      "A terminal session: maithic blueprint plan takes an argument in words apart into two definitions, four lemmas, a theorem and a corollary, with what each rests on, and notes the two things it put in order in the answer. maithic blueprint skeleton then writes the eight declarations as Lean, each with sorry for a proof, and Lean compiles them."
      [ ([ "blueprint"; "plan"; "examples/blueprint/argument.md"; "--namespace"; "Triangles"; "--out"; ".maithic/shots/plan.blueprint.md" ], 0);
        ([ "blueprint"; "skeleton"; "examples/blueprint/triangles.md"; "--out"; ".maithic/shots/Triangles.lean"; "--force" ], 0) ];
    shot "find" "maithic search"
      "A terminal session: maithic search answers the words sorting keeps the elements with theorems about insertion sort, the first of them the theorem that insertion sort keeps the elements, each with its statement, its documentation and the words that matched. A second search, for add comm, is also put to Lean and returns the commutativity of addition for integers, natural numbers and rationals from Lean's own library."
      [ ([ "search"; "sorting"; "keeps"; "the"; "elements"; "--from"; "fixtures/intel/extractions"; "--limit"; "3" ], 0);
        ([ "search"; "add"; "comm"; "--in"; "lean/Maithic/Algo/Arith"; "--project"; "lean"; "--lean"; "--limit"; "4" ], 0) ];
    shot "gate" "maithic bench gate"
      "A terminal session: maithic bench gate holds a benchmark run against the stored baseline. The committed run passes, with 24 of 24 problems verified as in the baseline. A copy of the run in which one result was changed fails: problem basics-007, which the baseline verified, is not verified, and the command ends with status 13."
      [ ([ "bench"; "gate"; "bench/results/basics-portfolio.json" ], 0); ([ "bench"; "gate"; "fixtures/bench/regressed.json" ], 13) ];
    shot "nnv" "maithic nnv verify"
      "A terminal session: maithic nnv verify decides two properties of small neural networks. For the first the verifier claims that the outputs stay within the claimed box, and Lean's kernel accepts the theorem that says so. For the second the verifier claims a violation, the input 3 mapped to 3, outside the box, and Lean's kernel accepts the theorem that the property fails."
      [ ([ "nnv"; "verify"; "examples/nnv/small-classifier.json"; "examples/nnv/unit-square.json" ], 0);
        ([ "nnv"; "verify"; "examples/nnv/absolute-value.json"; "examples/nnv/too-tight.json" ], 8) ];
    shot "algo-synth" "maithic algo synth"
      "A terminal session: maithic algo synth asks a model for a run-length encoding function. The first draft does not compile because Lean cannot see that it terminates. The correction compiles and gets three of five examples right, and the two wrong answers are shown. The next correction gets all five right and is accepted."
      [ ([ "algo"; "synth"; "examples/algo/run-lengths.toml" ], 0) ] ~columns:124;
    shot "algo-differential" "maithic algo differential"
      "A terminal session: maithic algo differential sends ten generated requests for each of nineteen verified algorithms to the compiled Lean program and to reference implementations in Julia. No answer differs: 190 cases, and every answer is accepted by its reference."
      [ ([ "algo"; "differential" ], 0) ];
    shot "doctor" "maithic version, doctor"
      "A terminal session: maithic version prints the version of the command, of the Bridge protocol, of Lean and of OCaml. maithic doctor lists Lean, Lake, the Bridge with the result of its self-test, curl, Julia and R, each with its version and the status ok, and then the gateway mode, the provider and model, and that candidate screening is active and that Lean workers never see API keys."
      [ ([ "version" ], 0); ([ "doctor" ], 0) ];
    shot "check" "maithic check"
      "A terminal session: maithic check elaborates two files, each in a new process. In the first, ten theorems about Euclid's algorithm are verified, each with the axioms it rests on. In the second, six conjectures are reported as failed because each proof depends on sorry, and the command ends with status 7."
      [ ([ "check"; "lean/Maithic/Algo/Arith/Gcd.lean"; "--project"; "lean" ], 0); ([ "check"; "examples/conjectures.lean" ], 7) ];
    shot "models" "maithic models, auth status"
      "A terminal session: maithic models lists four providers, anthropic, openai, xai and local, with the API each speaks, the model of the fast, balanced and best tier, and the endpoint. maithic auth status shows for each provider whether a key is set and the environment variable it is read from, and prints no key."
      [ ([ "models" ], 0); ([ "auth"; "status" ], 0) ] ~columns:126;
    shot "config" "maithic config show"
      "A terminal session: maithic config show prints every setting with its effective value and the layer it came from. Most values come from the defaults; the gateway mode comes from the environment variable MAITHIC_GATEWAY_MODE."
      [ ([ "config"; "show" ], 0) ];
    shot "bench" "maithic bench run"
      "A terminal session: maithic bench run with AI switched off proves the 24 problems of the basics suite with the automation portfolio. Every row is a problem with the status verified, the proof that was found and the time it took, and the last line gives 24 of 24."
      [ ([ "bench"; "run"; "--no-ai" ], 0) ];
    shot "intel-scan" "maithic intel scan, stats"
      "A terminal session: maithic intel scan elaborates the 23 files of the algorithm library and counts 261 declarations, none resting on sorry. maithic intel stats then gives the size of the graph, and the declarations that most others use: Sorted, TotalPreorder and insertionSort among those of the scanned files, and List, Nat and Eq from anywhere."
      [ ([ "intel"; "scan"; "lean/Maithic/Algo"; "--project"; "lean"; "--dir"; ".maithic/shots/intel-algo" ], 0);
        ([ "intel"; "stats"; "--dir"; ".maithic/shots/intel-algo" ], 0) ];
    shot "bridge" "maithic bridge selftest"
      "A terminal session: maithic bridge selftest starts the Bridge and runs five checks, all ok: the kernel accepts a true statement, the kernel rejects a false statement, the axiom audit sees injected axioms, the guard blocks evaluation commands, and tactics step a proof state to its end."
      [ ([ "bridge"; "selftest" ], 0) ];
    shot "help" "maithic help status, exit-codes"
      "A terminal session: maithic help status prints the eight status words with the meaning of each, and maithic help exit-codes prints the fourteen exit codes from 0, success, to 13, regression gate failed."
      [ ([ "help"; "status" ], 0); ([ "help"; "exit-codes" ], 0) ];
    shot "errors" "what an error looks like"
      "A terminal session: three commands that end with an error, each with a code and its own exit status. A file that does not exist: MTH-CLI-0002, status 2. A setting with a value of the wrong type: MTH-CONFIG-0002, status 3. A model call in replay mode with no cassette for the request: MTH-FORMALIZE-0004, status 5, with the digest of the request."
      [ ([ "prove"; "examples/nothing.lean" ], 2); ([ "prove"; "examples/even.lean"; "--rounds"; "many" ], 3);
        ([ "formalize"; "Show that every prime greater than two is odd." ], 5) ];
    shot "prove-rounds" "maithic prove --verbose"
      "A terminal session: maithic prove with the option verbose lists every candidate proof of a theorem about triangular numbers. The eighteen candidates of the automation fail. In round 1 a model proposes two proofs and Lean rejects both, the first as a failed tactic and the second with unsolved goals. In round 2 the model's proof by induction is accepted, and the result is verified after 21 attempts."
      [ ([ "prove"; "examples/triangle.lean"; "--verbose" ], 0) ];
    shot "nnv-unknown" "maithic nnv verify, the third answer"
      "A terminal session: maithic nnv verify decides two more properties of the absolute-value network on the inputs from -2 to 3. That the output stays within 0 and 6 is verified, and Lean's kernel accepts the theorem. That it stays within 0 and 4 is answered with unknown: the bounds are too loose to show the property and no tested point leaves the box, so Lean is not asked and nothing is claimed."
      [ ([ "nnv"; "verify"; "examples/nnv/absolute-value.json"; "examples/nnv/bounded.json" ], 0);
        ([ "nnv"; "verify"; "examples/nnv/absolute-value.json"; "examples/nnv/beyond-the-bounds.json" ], 7) ];
    shot "oracle-answers" "maithic oracle solve, three more answers"
      "A terminal session: three goals sent to the oracles. A linear goal over the rationals is refuted with the exact point x = 3, y = 0. A linear goal over the integers is answered with unknown, and the reason: the hypotheses and the negated goal have rational solutions, and no integer one was found nearby. A polynomial goal is proved with a certificate, and the command says that Lean has no checker for this kind of certificate yet."
      [ ([ "oracle"; "solve"; "fixtures/oracle/goals/linear-refuted.json" ], 8);
        ([ "oracle"; "solve"; "fixtures/oracle/goals/linear-unknown.json" ], 7);
        ([ "oracle"; "solve"; "fixtures/oracle/goals/polynomial-symmetric.json" ], 0) ];
    shot "json" "maithic check --json"
      "A terminal session: maithic check with the option json prints its report as a JSON document that names its schema, maithic.check.report/1, with the file, the counts of errors, warnings and declarations, the theorem even_add_even with its statement, the status failed, the axiom sorryAx and the reason, and the diagnostic of Lean with its line and column."
      [ ([ "check"; "examples/even.lean"; "--json" ], 7) ];
    shot "intel-deps" "maithic intel deps"
      "A terminal session: maithic intel deps lists what the theorem sorted_insertionSort rests on, with the distance of each declaration: at distance 1 the definitions Sorted and insertionSort, the class TotalPreorder and two theorems, and at distance 2 what those use in turn, each with its kind and the file and line where it is."
      [ ([ "intel"; "deps"; "Maithic.Algo.sorted_insertionSort"; "--dir"; "fixtures/intel" ], 0) ];
    shot "guide-train" "maithic corpus harvest, guide train, guide rank"
      "A terminal session: from proofs to a tactic model in three commands. maithic corpus harvest traces the algorithm library into pairs of a goal and the tactic used on it. maithic guide train trains a small model on them and reports its accuracy on proofs it has not seen, beside a baseline that always answers with the most frequent tactics. maithic guide rank asks the new model about the goal double n = 2 * n, and it puts simp [double] first."
      [ ([ "corpus"; "harvest"; "lean/Maithic/Algo"; "--project"; "lean"; "--out"; ".maithic/shots/guide-data"; "--revision"; "v0.1.0" ], 0);
        ([ "guide"; "train"; ".maithic/shots/guide-data"; ".maithic/shots/guide-model"; "--seed"; "1" ], 0);
        ([ "guide"; "rank"; ".maithic/shots/guide-model/model.json"; "--file"; "examples/double.lean"; "--theorem"; "double_eq"; "--top"; "4" ], 0) ] ]

let environment = [ ("CLICOLOR_FORCE", "1"); ("MAITHIC_HOME", Fs.(Filename.get_temp_dir_name () / "maithic-no-home")) ]

(* Runs the commands of a shot and draws them. A command whose exit status is not the expected
   one is a problem: the picture would show something other than what its caption says. *)
let draw ~root ~maithic (s : shot) =
  let problems = ref [] in
  let sessions =
    List.map
      (fun (args, expected) ->
        let quoted arg = if String.contains arg ' ' then "\"" ^ arg ^ "\"" else arg in
        let shown = "maithic " ^ String.concat " " (List.map quoted args) in
        match Proc.run ~cwd:root ~env:environment ~timeout:600.0 maithic args with
        | Error e ->
          problems := Printf.sprintf "%s: %s" shown e :: !problems;
          { Term_card.command = shown; output = "" }
        | Ok out ->
          if out.Proc.status <> expected then
            problems := Printf.sprintf "%s: exit status %d, and %d was expected\n%s" shown out.Proc.status expected out.Proc.stderr :: !problems;
          (* A command that ends with an error says why on its other stream, and a reader of the picture
             wants to see that. A status that reports an outcome (7, 8 or 13) is not an error. *)
          let is_error = out.Proc.status <> 0 && not (List.mem out.Proc.status [ 7; 8; 13 ]) in
          let shown_error = if is_error || out.Proc.status <> expected then out.Proc.stderr else "" in
          (* Paths are shown as a reader would type them: from the root of the repository. *)
          let text = Text.replace_all ~sub:(root ^ "/") ~by:"" (out.Proc.stdout ^ shown_error) in
          { Term_card.command = shown; output = text })
      s.commands
  in
  (Term_card.render ~columns:s.columns ~max_lines:s.max_lines ~title:s.title ~label:s.label sessions, List.rev !problems)

let file_of (s : shot) = Fs.("assets" / "readme" / ("term-" ^ s.name ^ ".svg"))

(** Draws every picture, or the named ones, and returns what went wrong. *)
let write ?(only = []) ~root ~maithic () =
  List.concat_map
    (fun (s : shot) ->
      if only <> [] && not (List.mem s.name only) then []
      else begin
        let svg, problems = draw ~root ~maithic s in
        match Fs.write Fs.(root / file_of s) svg with
        | Ok () ->
          Printf.printf "wrote %s\n%!" (file_of s);
          problems
        | Error e -> e :: problems
      end)
    all
