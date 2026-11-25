(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the council. Lean is stood in for by functions that accept the proofs holding a given
   text, and the models by lists of answers, so that whole sessions can be held without either;
   the real ones meet in the integration tests. *)

open Maithic_base
open Maithic_schema
open Maithic_gateway
open Maithic_ledger
open Maithic_council
open Testkit

(* ---- a problem, a stand-in for Lean and stand-ins for the models ---- *)

let source =
  "def sumOdd : Nat → Nat\n  | 0 => 0\n  | n + 1 => sumOdd n + (2 * n + 1)\n\n/-- Odd numbers add up to squares. -/\ntheorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by\n  sorry\n"

let problem = is_ok "problem" (Council_problem.of_source ~file:"Sum.lean" source)
let formal = "∀ (n : Nat), sumOdd n = n * n"
let good_proof = "by\n  induction n with\n  | zero => rfl\n  | succ k ih =>\n    simp only [sumOdd, ih, Nat.succ_mul, Nat.mul_succ]\n    omega"

let accepted ?(statement = formal) ?(axioms = [ "propext" ]) ?(uses_sorry = false) ?(rechecked = true) () =
  { Council_evidence.accepted = true; statement; axioms; uses_sorry; rechecked; error = "" }

let library = [ ("sumOdd", "sumOdd : Nat → Nat"); ("Nat.succ_mul", "Nat.succ_mul (n m : Nat) : n.succ * m = n * m + m"); ("Nat.mul_succ", "Nat.mul_succ (n m : Nat) : n * m.succ = n * m + n");
                ("Nat.mul_comm", "Nat.mul_comm (n m : Nat) : n * m = m * n") ]

(* A Lean that accepts a proof exactly when it holds [good], and knows the names of [library]. *)
let lean ?(good = "Nat.succ_mul") ?(answer = accepted ()) ?(elaborates = true) ?prove ?refute ?(seen = ref []) () : Council_tools.raw =
  { Council_tools.compile =
      (fun ~untrusted text ->
        if Text.contains untrusted "theorem" then Error "the guard of the Bridge refused the text: forbidden keyword 'theorem'"
        else if Text.contains text "Even" then Ok [ { Council_tools.severity = "error"; line = 1; text = "Unknown identifier `Even`" }; { Council_tools.severity = "info"; line = 1; text = "a remark" } ]
        else Ok [ { Council_tools.severity = "info"; line = 1; text = "Nat : Type" } ]);
    statement = (fun () -> if elaborates then accepted ~axioms:[ "sorryAx" ] ~uses_sorry:true ~rechecked:false () else Council_evidence.rejected "Unknown identifier `sumOd`");
    proof =
      (fun text ->
        seen := !seen @ [ text ];
        if Text.contains text good then answer else Council_evidence.rejected ("unsolved goals\n⊢ sumOdd k + (2 * k + 1) = (k + 1) * (k + 1)"));
    lookup = (fun names -> List.filter (fun (name, _) -> List.mem name names) library); prove; refute }

(* Models that answer each prompt with the answers given for it, in turn, and note what they were asked. *)
let models ?(tokens = (0, 0)) answers =
  let left = ref answers and asked = ref [] in
  let model : Council_session.model =
   fun ~purpose ~schema:_ bindings ->
    asked := !asked @ [ (purpose, bindings) ];
    match List.assoc_opt purpose !left with
    | Some (answer :: rest) ->
      left := (purpose, rest) :: List.remove_assoc purpose !left;
      Result.map (fun text -> (text, fst tokens, snd tokens)) answer
    | _ -> Error ("no answer is left for " ^ purpose)
  in
  (model, asked)

let research = {|{"names": ["sumOdd", "Nat.succ_mul", "Nat.mul_succ", "Nat.add_mul_self_eq", "Nat.mul_comm"], "ideas": ["Induction on n, then compare both sides."]}|}
let first_proofs = {|{"candidates": ["by\n  induction n <;> simp [sumOdd]", "by omega"]}|}
let critique = {|{"diagnosis": "The product (k + 1) * (k + 1) is not linear, so omega cannot read it.", "suggestions": ["Rewrite it with Nat.succ_mul and Nat.mul_succ first."]}|}
let second_proofs = Json.to_string (Json.Obj [ ("candidates", Json.List [ Json.String good_proof ]) ])

let script () = models [ ("council.research", [ Ok research ]); ("council.prove", [ Ok first_proofs; Ok second_proofs ]); ("council.critique", [ Ok critique ]) ]

let session ?settings ?budget ?(raw = lean ()) ?(policy = Trust_policy.default) model =
  Council_session.run ?settings ?model ~tools:(Council_tools.make ?budget raw) ~policy problem

(* A board with the statement and the claim on it, and a context for a role to act in. *)
let setting ?(task = Council_plan.Verify) ?(round = 1) () =
  let board = Council_board.create ~clock:(fun () -> 0.0) () and transcript = Council_transcript.create ~clock:(fun () -> 0.0) () in
  let statement = Council_board.post board ~author:"coordinator" Council_board.Statement ~title:"sumOdd_eq_square" problem.Council_problem.statement in
  let claim = Council_board.post board ~author:"coordinator" Council_board.Claim ~title:"the statement holds" ~refers:[ statement.Council_board.id ] problem.Council_problem.statement in
  Council_board.set_round board round;
  Council_transcript.set_round transcript round;
  { Council_role.problem; board; transcript; statement = statement.Council_board.id; claim = claim.Council_board.id; round; task }

let elaborated = is_ok "elaborated" (Council_evidence.elaborated (accepted ~uses_sorry:true ()))
let proved = is_ok "proved" (Council_evidence.proved ~policy:Trust_policy.default ~statement:formal (accepted ()))
let formalize (c : Council_role.context) = is_ok "formalized" (Council_board.Verifier.formalized c.Council_role.board ~claim:c.Council_role.claim elaborated)
let post_proof (c : Council_role.context) ?(author = "prover") text = (Council_board.post c.Council_role.board ~author Council_board.Proof ~title:"a candidate" ~refers:[ c.Council_role.claim ] text).Council_board.id
let status_of (c : Council_role.context) = Option.map Council_board.status_name (Council_board.status c.Council_role.board c.Council_role.claim)
let said (c : Council_role.context) = List.map (fun (m : Council_message.t) -> Council_message.performative_name m.Council_message.performative ^ ": " ^ m.Council_message.subject) (Council_transcript.messages c.Council_role.transcript)

(* ---- the coordinator ---- *)

let coordinator_tests =
  [ case "P06-01-01 a problem is a theorem of a file, with what stands before it and what is asked in words" (fun () ->
        equal_string "the theorem whose proof is unfinished" "sumOdd_eq_square" problem.Council_problem.theorem;
        equal_string "its statement on one line, without its proof" "theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n" problem.Council_problem.statement;
        equal_int "its line" 6 problem.Council_problem.line;
        equal_string "its context is what stands before it" "def sumOdd : Nat → Nat\n  | 0 => 0\n  | n + 1 => sumOdd n + (2 * n + 1)\n\n" problem.Council_problem.context;
        equal_string "the file as it was named" "Sum.lean" problem.Council_problem.file;
        equal_string "nothing asked in words" "" problem.Council_problem.goal;
        let two = source ^ "\ntheorem other (n : Nat) : n = n := by\n  sorry\n\ntheorem done_ (n : Nat) : n + 0 = n := rfl\n" in
        contains "several unfinished theorems need a name" (is_error "two" (Council_problem.of_source ~file:"Two.lean" two))
          "Two.lean has 2 theorems with an unfinished proof (sumOdd_eq_square, other); name one with --theorem";
        let named = is_ok "named" (Council_problem.of_source ~theorem:"done_" ~goal:"  Is there a shorter proof?\n" ~file:"Two.lean" two) in
        equal_string "a theorem that is named is taken, finished or not" "done_" named.Council_problem.theorem;
        equal_string "what is asked, in words" "Is there a shorter proof?" named.Council_problem.goal;
        check "the context of a later theorem holds the earlier ones" (Text.contains named.Council_problem.context "theorem other");
        contains "nothing unfinished" (is_error "none" (Council_problem.of_source ~file:"Done.lean" "theorem t : 1 = 1 := rfl\n")) "Done.lean has no theorem with an unfinished proof";
        check "no such theorem" (Result.is_error (Council_problem.of_source ~theorem:"missing" ~file:"Sum.lean" source));
        (* A model is shown the end of a long context, from the start of a line. *)
        let long = is_ok "long" (Council_problem.of_source ~file:"Long.lean" (String.concat "" (List.init 400 (fun i -> Printf.sprintf "def d%d := %d\n" i i)) ^ source)) in
        let shown = Council_problem.excerpt ~limit:200 long in
        check "an excerpt within the limit" (String.length shown <= 200);
        check "that begins a line and ends with the definition" (Text.starts_with ~prefix:"def d" shown || Text.starts_with ~prefix:"  |" shown || Text.starts_with ~prefix:"def sumOdd" shown);
        check "and keeps what is nearest the theorem" (Text.ends_with ~suffix:"| n + 1 => sumOdd n + (2 * n + 1)" shown);
        equal_string "the file with a proof put in"
          (Text.replace_all ~sub:" := by\n  sorry\n" ~by:(" := " ^ good_proof ^ "\n") source)
          (Council_problem.with_proof problem good_proof);
        let j = Council_problem.to_json named in
        equal_string "in the record" "done_" (Json.string_or "" "theorem" j);
        equal_string "with what was asked" "Is there a shorter proof?" (Json.string_or "" "goal" j);
        check "and no goal where none was given" (Json.member "goal" (Council_problem.to_json problem) = None));
    case "P06-01-02 each type of subtask is assigned to the role it belongs to" (fun () ->
        equal_strings "the types of task" [ "examine"; "research"; "prove"; "verify"; "critique" ] (List.map Council_plan.task_name Council_plan.tasks);
        equal_strings "and whose each is" [ "verifier"; "researcher"; "prover"; "verifier"; "critic" ] (List.map Council_plan.role_for Council_plan.tasks);
        let assigned ~round ~last = List.map (fun (task, role) -> Council_plan.task_name task ^ " -> " ^ role) (Council_plan.assign ~round ~last) in
        equal_strings "before the first round, the examination" [ "examine -> verifier" ] (assigned ~round:0 ~last:false);
        equal_strings "the first round" [ "research -> researcher"; "prove -> prover"; "verify -> verifier"; "critique -> critic" ] (assigned ~round:1 ~last:false);
        equal_strings "research is done once" [ "prove -> prover"; "verify -> verifier"; "critique -> critic" ] (assigned ~round:2 ~last:false);
        equal_strings "nobody would act on a critique of the last round" [ "prove -> prover"; "verify -> verifier" ] (assigned ~round:3 ~last:true);
        List.iter
          (fun task ->
            equal_string "the role declares the task" (Council_plan.role_for task) (Council_roles.for_task task).Council_role.name;
            check "and the coordinator can say what it asks for" (String.length (Council_plan.describe task) > 20))
          Council_plan.tasks);
    case "P06-01-03 the coordinator collects from each turn its outcome, what was posted and what it cost" (fun () ->
        let model, asked = script () in
        let s = session ~raw:(lean ~prove:(fun () -> (Error "its automation found no proof in 18 attempts", 18)) ()) (Some model) in
        equal_strings "the turns that were taken, each with its outcome"
          [ "0 verifier examine done"; "1 researcher research done"; "1 prover prove done"; "1 verifier verify done"; "1 critic critique done"; "2 prover prove done"; "2 verifier verify done" ]
          (List.map
             (fun (r : Council_session.turn_result) ->
               Printf.sprintf "%d %s %s %s" r.Council_session.turn.Council_schedule.round r.Council_session.turn.Council_schedule.role
                 (Council_plan.task_name r.Council_session.turn.Council_schedule.task) r.Council_session.status)
             s.Council_session.turns);
        equal_strings "what each turn did, in a line"
          [ "the statement is well formed; automation found no proof"; "4 of 5 names exist in the library; 1 idea"; "2 candidate proofs"; "Lean rejected all 2 proofs";
            "a diagnosis of 2 rejected proofs, with 1 suggestion"; "1 candidate proof"; "Lean accepted a proof (1 examined)" ]
          (List.map (fun (r : Council_session.turn_result) -> r.Council_session.summary) s.Council_session.turns);
        equal_ints "the questions each turn put to a model" [ 0; 1; 1; 0; 1; 1; 0 ] (List.map (fun (r : Council_session.turn_result) -> r.Council_session.model_calls) s.Council_session.turns);
        equal_ints "the calls each turn made to Lean" [ 19; 1; 0; 2; 0; 0; 1 ] (List.map (fun (r : Council_session.turn_result) -> r.Council_session.lean_calls) s.Council_session.turns);
        equal_ints "the items each turn posted" [ 0; 5; 2; 0; 1; 1; 0 ] (List.map (fun (r : Council_session.turn_result) -> List.length r.Council_session.posted) s.Council_session.turns);
        check "the session ended because the claim was settled" (s.Council_session.ending = Council_session.Settled);
        check "the claim is verified" (Council_session.status s = Council_board.Verified);
        equal_strings "the models were asked in the order of the schedule" [ "council.research"; "council.prove"; "council.critique"; "council.prove" ] (List.map fst !asked);
        equal_string "in a line" "verified: proof 11, by the prover in round 2 (4 model calls, 23 calls to Lean)" (Council_session.summary s);
        check "the exit status is success" (Council_session.exit_code s = Exit_code.Success);
        (* The whole session is one document of its schema. *)
        let j = Council_session.to_json s in
        equal_string "the schema" "maithic.council.session/1" (Json.string_or "" "schema" j);
        let schema = Json_schema.of_json (Support.json "schemas/council/session.schema.json") in
        equal_strings "a document of its schema" [] (List.map Json_schema.error_to_string (Json_schema.validate schema j));
        equal_int "every turn is in it" 7 (List.length (Json.list_or_empty "turns" j));
        equal_int "and every item" 11 (List.length (Json.list_or_empty "board" j));
        equal_int "and every message" (Council_transcript.length s.Council_session.transcript) (List.length (Json.list_or_empty "messages" j));
        let text = Council_session.render s in
        contains "a line for each turn" text "  critic      critique  a diagnosis of 2 rejected proofs, with 1 suggestion";
        contains "the answer" text "verified  Lean's kernel accepted proof 11, posted by the prover in round 2. It rests on the axioms propext.";
        contains "the theorem with its proof" text "  theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by\n    induction n with";
        contains "what was set aside" text "set aside, not part of the answer: 1 definition, 2 proofs, 2 notes");
    case "P06-01-03 a session without a model is the examination, and a prover that cannot be asked ends a session" (fun () ->
        let alone = session None in
        equal_strings "one turn" [ "verifier" ] (List.map (fun (r : Council_session.turn_result) -> r.Council_session.turn.Council_schedule.role) alone.Council_session.turns);
        check "the claim is formalized and no more" (Council_session.status alone = Council_board.Formalized);
        check "the turns were all taken" (alone.Council_session.ending = Council_session.Rounds_over);
        contains "in a line" (Council_session.summary alone) "not proved: the claim is formalized; no model was asked, so no round followed the examination";
        check "the exit status says it is not settled" (Council_session.exit_code alone = Exit_code.Not_verified);
        let model, _ = models [ ("council.research", [ Ok research ]) ] in
        let stuck = session (Some model) in
        (match stuck.Council_session.ending with
         | Council_session.Stopped why -> contains "why it stopped" why "no answer is left for council.prove"
         | _ -> fail "the session should have stopped");
        equal_strings "nothing was asked after the prover failed" [ "examine"; "research"; "prove" ]
          (List.map (fun (r : Council_session.turn_result) -> Council_plan.task_name r.Council_session.turn.Council_schedule.task) stuck.Council_session.turns);
        let unreadable = session ~raw:(lean ~elaborates:false ()) (Some (fst (script ()))) in
        (match unreadable.Council_session.ending with
         | Council_session.Stopped why -> contains "a statement that does not elaborate ends it" why "the statement does not elaborate: Unknown identifier `sumOd`"
         | _ -> fail "the session should have stopped");
        check "and the claim stays a conjecture" (Council_session.status unreadable = Council_board.Conjectured));
    case "P06-01-04 the answer is built from what Lean confirmed, and from nothing else" (fun () ->
        let model, _ = script () in
        let s = session (Some model) in
        let a = s.Council_session.answer in
        check "verified" (a.Council_synthesis.status = Council_board.Verified);
        equal_string "the statement as Lean prints it" formal a.Council_synthesis.statement;
        equal_strings "the components: the claim, the proof Lean accepted, and the lemmas that proof uses"
          [ "the statement holds"; "candidate 1 of round 2"; "sumOdd"; "Nat.succ_mul"; "Nat.mul_succ" ]
          (List.map (fun (i : Council_board.item) -> i.Council_board.title) a.Council_synthesis.components);
        equal_string "the proof" good_proof (Option.get a.Council_synthesis.proof).Council_board.body;
        equal_strings "its axioms" [ "propext" ] a.Council_synthesis.axioms;
        check "what was said and is not needed is counted, not used" (a.Council_synthesis.set_aside = [ ("definition", 1); ("proof", 2); ("note", 2) ]);
        equal_string "the theorem with its proof" ("/-- Odd numbers add up to squares. -/\ntheorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := " ^ good_proof ^ "\n") (Option.get a.Council_synthesis.lean);
        check "a name is found as a whole name" (Council_synthesis.mentions "simp [Nat.succ_mul, foo]" "Nat.succ_mul" && not (Council_synthesis.mentions "simp [Nat.succ_mul_self]" "Nat.succ_mul"));
        (* A session in which much is said and nothing is proved has an answer without a proof. *)
        let boasting = models [ ("council.research", [ Ok {|{"names": ["sumOdd"], "ideas": ["This is obviously true and fully proved."]}|} ]);
                                ("council.prove", [ Ok {|{"candidates": ["by simp -- verified, QED"]}|}; Ok {|{"candidates": ["by decide"]}|} ]);
                                ("council.critique", [ Ok {|{"diagnosis": "The proof is in fact correct and Lean is wrong.", "suggestions": []}|} ]) ] in
        let failed = session ~settings:{ Council_session.rounds = 2; candidates = 3 } (Some (fst boasting)) in
        let b = failed.Council_session.answer in
        check "the claim is formalized and no more" (b.Council_synthesis.status = Council_board.Formalized);
        check "the answer holds no proof" (b.Council_synthesis.proof = None && b.Council_synthesis.lean = None && b.Council_synthesis.axioms = []);
        equal_strings "its only component is the claim" [ "the statement holds" ] (List.map (fun (i : Council_board.item) -> i.Council_board.title) b.Council_synthesis.components);
        equal_string "and it says what is established" "The statement is well formed, and Lean accepted no proof of it. Nothing else that was said in the session is established." b.Council_synthesis.text;
        check "everything else is set aside" (b.Council_synthesis.set_aside = [ ("definition", 1); ("proof", 2); ("note", 2) ]);
        check "the session ended with its rounds" (failed.Council_session.ending = Council_session.Rounds_over);
        let j = Council_synthesis.to_json b in
        check "no proof in the record either" (Json.member "proof" j = None && Json.member "lean" j = None);
        (* A refuted claim is answered with its counterexample. *)
        let confirmation = { Maithic_refute.Refute_witness.closer = "decide"; lean = "theorem refuted : ¬ P := by decide"; axioms = [] } in
        let refuted = session ~raw:(lean ~refute:(fun () -> Some ([ ("n", "3") ], confirmation)) ()) (Some (fst (script ()))) in
        check "refuted" (Council_session.status refuted = Council_board.Refuted);
        equal_strings "the components: the claim and the counterexample" [ "the statement holds"; "the statement fails at n = 3" ]
          (List.map (fun (i : Council_board.item) -> i.Council_board.title) refuted.Council_session.answer.Council_synthesis.components);
        equal_string "in words" "The statement fails at n = 3, and Lean accepted a proof of that." refuted.Council_session.answer.Council_synthesis.text;
        equal_int "no model was asked about a false statement" 0 (List.length (List.filter (fun (r : Council_session.turn_result) -> r.Council_session.model_calls > 0) refuted.Council_session.turns));
        check "the exit status is that of a refutation" (Council_session.exit_code refuted = Exit_code.Refuted)) ]

(* ---- the roles ---- *)

let role_tests =
  [ case "P06-02-01 the researcher posts the names Lean knows, with their signatures, and drops the rest" (fun () ->
        let c = setting ~task:Council_plan.Research () in
        let asked = ref [] and looked_up = ref [] in
        let ask ~purpose ~schema:_ bindings =
          asked := (purpose, bindings) :: !asked;
          Ok {|{"names": ["sumOdd", "Nat.succ_mul", "Nat.square_succ", "rm -rf", "2fast", "Nat..x", "sumOdd", "Nat.mul_comm"], "ideas": ["One.", "Two.", "Three.", "Four."]}|}
        in
        let lookup names =
          looked_up := names;
          List.filter (fun (name, _) -> List.mem name names) library
        in
        let work = Council_researcher.act ~ask ~lookup c in
        equal_strings "only what could be a name is looked up, each once" [ "sumOdd"; "Nat.succ_mul"; "Nat.square_succ"; "Nat.mul_comm" ] !looked_up;
        check "done" (work.Council_role.status = Council_role.Done);
        equal_string "in a line" "3 of 4 names exist in the library; 3 ideas" work.Council_role.summary;
        let definitions = Council_board.of_kind c.Council_role.board Council_board.Definition in
        equal_strings "the names Lean knows are posted" [ "sumOdd"; "Nat.succ_mul"; "Nat.mul_comm" ] (List.map (fun (i : Council_board.item) -> i.Council_board.title) definitions);
        equal_string "with the signature Lean gave" "Nat.succ_mul (n m : Nat) : n.succ * m = n * m + m" (List.nth definitions 1).Council_board.body;
        check "and marked as confirmed by Lean" (List.for_all (fun (i : Council_board.item) -> i.Council_board.checked && i.Council_board.author = "researcher") definitions);
        let notes = Council_board.of_kind c.Council_role.board Council_board.Note in
        equal_strings "at most three ideas, as notes" [ "One."; "Two."; "Three." ] (List.map (fun (i : Council_board.item) -> i.Council_board.body) notes);
        check "which nobody confirmed" (List.for_all (fun (i : Council_board.item) -> not i.Council_board.checked) notes);
        (match Council_transcript.messages c.Council_role.transcript with
         | [ m ] ->
           equal_strings "the prover is told" [ "prover"; "coordinator" ] m.Council_message.recipients;
           contains "what exists" m.Council_message.content "Nat.mul_comm (n m : Nat) : n * m = m * n";
           contains "and what does not" m.Council_message.content "Lean does not know: Nat.square_succ";
           equal_int "with the items it is about" 6 (List.length m.Council_message.refers)
         | _ -> fail "one message was expected");
        (match !asked with
         | [ (purpose, bindings) ] ->
           equal_string "the prompt" "council.research" purpose;
           equal_string "the statement" "theorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n" (List.assoc "statement" bindings);
           contains "the context" (List.assoc "context" bindings) "def sumOdd : Nat → Nat";
           equal_string "nothing asked in words" "(none given)" (List.assoc "goal" bindings)
         | _ -> fail "one question was expected");
        check "a name of parts, with dots between" (Council_researcher.well_formed "List.length_append" && Council_researcher.well_formed "Nat.succ'" && not (Council_researcher.well_formed "#eval 1"));
        (* A model that cannot be asked is a failure with its reason, said in the transcript. *)
        let c = setting ~task:Council_plan.Research () in
        let failed = Council_researcher.act ~ask:(fun ~purpose:_ ~schema:_ _ -> Error "the provider is down") ~lookup c in
        check "failed" (failed.Council_role.status = Council_role.Failed "the provider is down");
        equal_strings "and said" [ "decline: no research" ] (said c));
    case "P06-02-02 the prover proposes proofs from what the board holds, and posts them as candidates" (fun () ->
        let c = setting ~task:Council_plan.Prove ~round:2 () in
        formalize c;
        ignore (Council_board.post c.Council_role.board ~author:"researcher" Council_board.Definition ~title:"Nat.succ_mul" ~checked:true "Nat.succ_mul (n m : Nat) : n.succ * m = n * m + m");
        ignore (Council_board.post c.Council_role.board ~author:"researcher" Council_board.Note ~title:"an idea for the proof" "Induction on n.");
        let old = post_proof c "by omega" in
        is_ok "rejected" (Council_board.Verifier.rejected c.Council_role.board ~proof:old "omega could not prove the goal");
        ignore (Council_board.post c.Council_role.board ~author:"critic" Council_board.Note ~title:"why the proofs of round 1 failed" ~refers:[ old ] "The goal is not linear.");
        let asked = ref [] in
        let ask ~purpose ~schema:_ bindings =
          asked := (purpose, bindings) :: !asked;
          Ok {|{"candidates": ["by  omega", "by\n  induction n <;> simp [sumOdd]", "by\n  induction n <;>   simp [sumOdd]", "by rfl", "by decide", "by trivial"]}|}
        in
        let work = Council_prover.act ~ask ~candidates:3 c in
        equal_string "in a line" "3 candidate proofs" work.Council_role.summary;
        equal_strings "new proofs only, each once, as many as were asked for" [ "by\n  induction n <;> simp [sumOdd]"; "by rfl"; "by decide" ]
          (List.map (fun id -> (Option.get (Council_board.find c.Council_role.board id)).Council_board.body) work.Council_role.posted);
        List.iter
          (fun id ->
            let item = Option.get (Council_board.find c.Council_role.board id) in
            check "a candidate is a proof of the claim" (item.Council_board.kind = Council_board.Proof && item.Council_board.refers = [ c.Council_role.claim ]);
            check "that Lean has not seen" (Council_board.examined c.Council_role.board id = Council_board.Unexamined))
          work.Council_role.posted;
        equal_string "proposing changes nothing about the claim" "formalized" (Option.get (status_of c));
        equal_strings "the verifier is told" [ "propose: 3 candidate proofs" ] (said c);
        (match !asked with
         | [ (purpose, bindings) ] ->
           equal_string "the prompt" "council.prove" purpose;
           equal_string "how many" "3" (List.assoc "count" bindings);
           equal_string "the lemmas Lean confirmed" "Nat.succ_mul (n m : Nat) : n.succ * m = n * m + m" (List.assoc "lemmas" bindings);
           equal_string "the proofs Lean rejected, with what it said" "by omega\n-- Lean: omega could not prove the goal" (List.assoc "attempts" bindings);
           equal_string "the remarks of the others"
             "researcher (an idea for the proof): Induction on n.\n\ncritic (why the proofs of round 1 failed): The goal is not linear." (List.assoc "advice" bindings)
         | _ -> fail "one question was expected");
        (* Nothing new to propose is said, and is no failure. *)
        let again = Council_prover.act ~ask:(fun ~purpose:_ ~schema:_ _ -> Ok {|{"candidates": ["by omega", "by rfl"]}|}) ~candidates:3 c in
        check "idle" (again.Council_role.status = Council_role.Idle);
        equal_string "and why" "the model returned only proofs that were tried before" again.Council_role.summary;
        let empty = Council_prover.act ~ask:(fun ~purpose:_ ~schema:_ _ -> Ok "I cannot prove this.") ~candidates:3 c in
        equal_string "no proof at all" "the model returned no proof" empty.Council_role.summary;
        let first = setting ~task:Council_plan.Prove () in
        ignore (Council_prover.act ~ask:(fun ~purpose:_ ~schema:_ bindings -> asked := [ ("", bindings) ]; Ok "{}") ~candidates:3 first);
        equal_strings "an empty board is said to be empty" [ "(none)"; "(none)"; "(none)" ] (List.map (fun key -> List.assoc key (snd (List.hd !asked))) [ "lemmas"; "attempts"; "advice" ]));
    case "P06-02-03 the critic says why the proofs Lean rejected failed, once for each" (fun () ->
        let c = setting ~task:Council_plan.Critique () in
        formalize c;
        let calls = ref 0 in
        let ask ~purpose:_ ~schema:_ _ =
          incr calls;
          Ok critique
        in
        let idle = Council_critic.act ~ask c in
        check "nothing rejected, nothing to say" (idle.Council_role.status = Council_role.Idle);
        equal_int "and no model is asked" 0 !calls;
        let a = post_proof c "by simp" and b = post_proof c "by omega" and waiting = post_proof c "by rfl" in
        is_ok "a" (Council_board.Verifier.rejected c.Council_role.board ~proof:a "unsolved goals");
        is_ok "b" (Council_board.Verifier.rejected c.Council_role.board ~proof:b "omega could not prove the goal");
        let asked = ref [] in
        let work = Council_critic.act ~ask:(fun ~purpose ~schema:_ bindings -> asked := (purpose, bindings) :: !asked; Ok critique) c in
        equal_string "in a line" "a diagnosis of 2 rejected proofs, with 1 suggestion" work.Council_role.summary;
        (match List.map (fun id -> Option.get (Council_board.find c.Council_role.board id)) work.Council_role.posted with
         | [ note ] ->
           check "a note" (note.Council_board.kind = Council_board.Note && note.Council_board.author = "critic");
           equal_string "with the diagnosis and what to try"
             "The product (k + 1) * (k + 1) is not linear, so omega cannot read it.\nTry: Rewrite it with Nat.succ_mul and Nat.mul_succ first." note.Council_board.body;
           equal_ints "about the proofs Lean rejected, and not the one it has not seen" [ a; b ] note.Council_board.refers;
           check "an opinion, which nobody confirmed" (not note.Council_board.checked)
         | _ -> fail "one note was expected");
        equal_strings "the prover is told" [ "critique: a diagnosis of 2 rejected proofs, with 1 suggestion" ] (said c);
        (match !asked with
         | [ (purpose, bindings) ] ->
           equal_string "the prompt" "council.critique" purpose;
           equal_string "each proof with Lean's words" "by simp\n-- Lean: unsolved goals\n\nby omega\n-- Lean: omega could not prove the goal" (List.assoc "attempts" bindings)
         | _ -> fail "one question was expected");
        check "the proofs are commented on once" ((Council_critic.act ~ask c).Council_role.status = Council_role.Idle);
        is_ok "c" (Council_board.Verifier.rejected c.Council_role.board ~proof:waiting "The rfl tactic failed");
        equal_ints "a proof rejected later is the one that is open" [ waiting ] (List.map (fun ((i : Council_board.item), _) -> i.Council_board.id) (Council_critic.open_failures c));
        equal_string "the critic changes nothing about the claim" "formalized" (Option.get (status_of c)));
    case "P06-02-04 the verifier gives proofs to Lean, and only what Lean accepts verifies the claim" (fun () ->
        let seen = ref [] in
        let tools = Council_tools.make (lean ~seen ()) in
        let c = setting ~task:Council_plan.Examine ~round:0 () in
        let examined = Council_verifier.act ~tools ~policy:Trust_policy.default c in
        equal_string "the examination" "the statement is well formed" examined.Council_role.summary;
        equal_string "formalizes the claim" "formalized" (Option.get (status_of c));
        equal_string "as Lean prints the statement" formal (Council_role.formal_statement c);
        let c = { c with Council_role.task = Council_plan.Verify; round = 1 } in
        check "no proof, nothing to do" ((Council_verifier.act ~tools ~policy:Trust_policy.default c).Council_role.status = Council_role.Idle);
        let bad = post_proof c "by simp" and good = post_proof c good_proof and later = post_proof c "by rfl" in
        let work = Council_verifier.act ~tools ~policy:Trust_policy.default c in
        equal_string "in a line" "Lean accepted a proof (2 examined)" work.Council_role.summary;
        equal_strings "each proof went to Lean, in the order of posting, until one was accepted" [ "by simp"; good_proof ] !seen;
        equal_string "the claim is verified" "verified" (Option.get (status_of c));
        check "the proof Lean rejected keeps Lean's words"
          (Council_board.examined c.Council_role.board bad = Council_board.Rejected "unsolved goals\n⊢ sumOdd k + (2 * k + 1) = (k + 1) * (k + 1)");
        check "the proof Lean accepted is marked" (Council_board.examined c.Council_role.board good = Council_board.Accepted);
        check "and is the proof the claim was verified by" ((Option.get (Council_board.accepted_proof c.Council_role.board c.Council_role.claim)).Council_board.id = good);
        check "what came after it was not needed" (Council_board.examined c.Council_role.board later = Council_board.Unexamined);
        equal_strings "what was said"
          [ "verdict: the statement is well formed"; Printf.sprintf "verdict: proof %d is rejected" bad; Printf.sprintf "verdict: proof %d is accepted: the claim is verified" good ]
          (said c);
        check "the verifier asks no model" ((not Council_verifier.role.Council_role.uses_model) && Council_verifier.role.Council_role.prompt = "");
        (* What Lean accepts and the policy does not, or what proves something else, verifies nothing. *)
        let unverified answer policy =
          let c = setting () in
          formalize c;
          let proof = post_proof c good_proof in
          ignore (Council_verifier.act ~tools:(Council_tools.make (lean ~answer ())) ~policy c);
          equal_string "the claim stays formalized" "formalized" (Option.get (status_of c));
          match Council_board.examined c.Council_role.board proof with Council_board.Rejected why -> why | _ -> fail "the proof should not count"
        in
        equal_string "a proof that rests on sorry" "the proof rests on sorry" (unverified (accepted ~uses_sorry:true ()) Trust_policy.default);
        equal_string "a proof of another statement" "the theorem Lean accepted states something else: True" (unverified (accepted ~statement:"True" ()) Trust_policy.default);
        equal_string "an axiom the policy does not allow" "the proof rests on what the trust policy does not allow: Lean.ofReduceBool"
          (unverified (accepted ~axioms:[ "propext"; "Lean.ofReduceBool" ] ()) Trust_policy.default);
        contains "a proof that was not checked again, under a policy that asks for it" (unverified (accepted ~rechecked:false ()) Trust_policy.default) "the trust policy is not met";
        (* Under a policy that does not ask for a second check, the first is enough. *)
        let c = setting () in
        formalize c;
        ignore (post_proof c good_proof);
        ignore (Council_verifier.act ~tools:(Council_tools.make (lean ~answer:(accepted ~rechecked:false ()) ())) ~policy:{ Trust_policy.default with Trust_policy.require_recheck = false } c);
        equal_string "verified under the laxer policy" "verified" (Option.get (status_of c));
        (* A claim that was never formalized has nothing a proof could be a proof of. *)
        let c = setting () in
        ignore (post_proof c good_proof);
        check "no formal statement, no verdict" ((Council_verifier.act ~tools ~policy:Trust_policy.default c).Council_role.status = Council_role.Failed "the claim was never formalized"));
    case "P06-02-05 roles are looked up by name and handed what their declaration lists" (fun () ->
        equal_strings "the roles" [ "researcher"; "prover"; "critic"; "verifier" ] Council_roles.names;
        equal_string "a role by its name" "Generates candidate proofs." (is_ok "find" (Council_roles.find "prover")).Council_role.purpose;
        equal_string "a role that is not there" "there is no role 'judge'; the roles are researcher, prover, critic, verifier" (is_error "find" (Council_roles.find "judge"));
        equal_strings "who asks a model" [ "researcher"; "prover"; "critic" ]
          (List.filter_map (fun (r : Council_role.t) -> if r.Council_role.uses_model then Some r.Council_role.name else None) Council_roles.all);
        equal_strings "the tools each is handed" [ "researcher: lookup"; "prover: "; "critic: "; "verifier: lean_check, elaborate, check_proof, prove, refute" ]
          (List.map (fun (r : Council_role.t) -> r.Council_role.name ^ ": " ^ String.concat ", " r.Council_role.tools) Council_roles.all);
        check "only the verifier holds a tool that returns evidence"
          (List.for_all (fun (r : Council_role.t) -> r.Council_role.name = "verifier" || not (List.exists (fun tool -> List.mem tool [ "elaborate"; "check_proof"; "refute" ]) r.Council_role.tools)) Council_roles.all);
        List.iter
          (fun (r : Council_role.t) ->
            if r.Council_role.uses_model then begin
              check (r.Council_role.name ^ " asks with a prompt that ships") (List.mem r.Council_role.prompt Prompt.names);
              check (r.Council_role.name ^ " has a purpose") (String.length r.Council_role.purpose > 20)
            end)
          Council_roles.all;
        (* The prompts that ship take exactly the values the roles give them. *)
        let c = setting () in
        let renders prompt bindings = Result.is_ok (Result.bind (Prompt.load prompt) (fun p -> Prompt.render p bindings)) in
        check "research" (renders "council.research" (Council_researcher.bindings c));
        check "prove" (renders "council.prove" (Council_prover.bindings ~candidates:3 c));
        check "critique" (renders "council.critique" (Council_critic.bindings c []));
        let tools = Council_tools.make (lean ()) in
        let without = { Council_roles.ask = None; tools; policy = Trust_policy.default; candidates = 3 } in
        check "the verifier needs no model" (Result.is_ok (Council_roles.instantiate without "verifier"));
        equal_string "the others need one" "the role prover asks a model, and the session has none" (is_error "instantiate" (Council_roles.instantiate without "prover"));
        let with_model = { without with Council_roles.ask = Some (fun ~purpose:_ ~schema:_ _ -> Ok first_proofs) } in
        List.iter (fun name -> check (name ^ " can be put to work") (Result.is_ok (Council_roles.instantiate with_model name))) Council_roles.names;
        check "a role that is not there cannot" (Result.is_error (Council_roles.instantiate with_model "judge"));
        let c = setting ~task:Council_plan.Prove () in
        let work = (is_ok "prover" (Council_roles.instantiate with_model "prover")) c in
        equal_int "an instantiated role takes its turn" 2 (List.length work.Council_role.posted)) ]

(* ---- the blackboard ---- *)

let memory_tests =
  [ case "P06-03-01 the blackboard keeps every item with its author, its round and its moment" (fun () ->
        let now = ref 100.0 in
        let board = Council_board.create ~clock:(fun () -> !now) () in
        let first = Council_board.post board ~author:"coordinator" Council_board.Statement ~title:"t" "theorem t : 1 = 1" in
        now := 101.5;
        Council_board.set_round board 2;
        let second = Council_board.post board ~author:"prover" Council_board.Note ~title:"a thought" ~refers:[ first.Council_board.id ] "It is rfl." in
        equal_ints "numbered from one, in the order of posting" [ 1; 2 ] [ first.Council_board.id; second.Council_board.id ];
        equal_string "the author" "prover" second.Council_board.author;
        equal_int "the round" 2 second.Council_board.round;
        check "the moment, counted from when the board was set up" (first.Council_board.at = 0.0 && second.Council_board.at = 1.5);
        equal_ints "what it is about" [ 1 ] second.Council_board.refers;
        check "an item is found by its number" (Council_board.find board 2 = Some second && Council_board.find board 3 = None);
        equal_ints "the items of an author" [ 2 ] (List.map (fun (i : Council_board.item) -> i.Council_board.id) (Council_board.by board "prover"));
        equal_ints "the items about an item" [ 2 ] (List.map (fun (i : Council_board.item) -> i.Council_board.id) (Council_board.about board 1));
        equal_ints "every item, the first one first" [ 1; 2 ] (List.map (fun (i : Council_board.item) -> i.Council_board.id) (Council_board.items board));
        raises "an item about nothing that is there" (fun () -> Council_board.post board ~author:"prover" Council_board.Note ~title:"x" ~refers:[ 9 ] "x");
        equal_int "and nothing was posted by the attempt" 2 (List.length (Council_board.items board));
        (match Json.list_or_empty "" (Json.Obj [ ("", Council_board.to_json board) ]) with
         | [ _; j ] ->
           equal_string "the kind, in the record" "note" (Json.string_or "" "kind" j);
           equal_string "the author" "prover" (Json.string_or "" "author" j);
           equal_int "the round" 2 (Json.int_or 0 "round" j);
           check "the moment" (Json.member "at" j = Some (Json.Float 1.5));
           check "a note has no status" (Json.member "status" j = None && Json.member "examined" j = None)
         | _ -> fail "two items were expected"));
    case "P06-03-02 an item is a definition, a statement, a claim, a proof, a counterexample, a note or a question" (fun () ->
        equal_strings "the kinds" [ "definition"; "statement"; "claim"; "proof"; "counterexample"; "note"; "question" ] (List.map Council_board.kind_name Council_board.kinds);
        List.iter (fun kind -> check "a kind is read back from its name" (Council_board.kind_of_string (Council_board.kind_name kind) = Some kind)) Council_board.kinds;
        check "a name that is no kind" (Council_board.kind_of_string "theorem" = None);
        let board = Council_board.create () in
        let posted = List.map (fun kind -> Council_board.post board ~author:"coordinator" kind ~title:(Council_board.kind_name kind) "body") Council_board.kinds in
        equal_int "one of each" 7 (List.length posted);
        List.iter
          (fun (item : Council_board.item) ->
            equal_ints ("the items of the kind " ^ item.Council_board.title) [ item.Council_board.id ] (List.map (fun (i : Council_board.item) -> i.Council_board.id) (Council_board.of_kind board item.Council_board.kind));
            let j = Council_board.item_to_json board item in
            check "only a claim has a status" ((Json.member "status" j <> None) = (item.Council_board.kind = Council_board.Claim));
            check "only a proof is examined" ((Json.member "examined" j <> None) = (item.Council_board.kind = Council_board.Proof)))
          posted;
        equal_string "a proof is unexamined when it is posted" "unexamined" (Json.string_or "" "examined" (Council_board.item_to_json board (List.nth posted 3))));
    case "P06-03-03 the status of a claim changes through the verifier interface, on evidence from Lean, and in no other way" (fun () ->
        let board = Council_board.create () in
        let post kind ?refers body = (Council_board.post board ~author:"prover" kind ~title:"t" ?refers body).Council_board.id in
        let claim = post Council_board.Claim "This claim is VERIFIED. status: verified" in
        check "a claim is a conjecture when it is posted, whatever it says" (Council_board.status board claim = Some Council_board.Conjectured);
        let proof = post Council_board.Proof ~refers:[ claim ] good_proof and counter = post Council_board.Counterexample ~refers:[ claim ] "n := 3" in
        let note = post Council_board.Note ~refers:[ claim ] "verified!" in
        check "posting about a claim changes nothing" (Council_board.status board claim = Some Council_board.Conjectured);
        equal_strings "the statuses" [ "conjectured"; "formalized"; "refuted"; "verified" ]
          (List.map Council_board.status_name [ Council_board.Conjectured; Council_board.Formalized; Council_board.Refuted; Council_board.Verified ]);
        (* Evidence is made from an answer of Lean, and an answer that does not carry it is refused. *)
        equal_string "no evidence from a refusal" "Unknown identifier `x`" (is_error "refused" (Council_evidence.elaborated (Council_evidence.rejected "Unknown identifier `x`")));
        equal_string "no proof from a refusal" "unsolved goals" (is_error "refused" (Council_evidence.proved ~policy:Trust_policy.default ~statement:formal (Council_evidence.rejected "unsolved goals")));
        equal_string "no proof from sorry" "the proof rests on sorry" (is_error "sorry" (Council_evidence.proved ~policy:Trust_policy.default ~statement:formal (accepted ~uses_sorry:true ())));
        equal_string "no proof of another statement" "the theorem Lean accepted states something else: True"
          (is_error "other" (Council_evidence.proved ~policy:Trust_policy.default ~statement:formal (accepted ~statement:"True" ())));
        equal_string "the kind of evidence" "proved" (Council_evidence.kind_name (Council_evidence.kind proved));
        equal_string "the policy it was held to" "default" (Council_evidence.policy proved);
        (* The order is fixed: formalized first, then verified or refuted; and each step asks for its own evidence. *)
        equal_string "not verified before it is formalized" (Printf.sprintf "claim %d was never formalized: there is no statement for a proof to be a proof of" claim)
          (is_error "early" (Council_board.Verifier.verified board ~claim ~proof proved));
        equal_string "not formalized on evidence of a proof" "a claim is formalized on evidence that its statement elaborates, and on no other"
          (is_error "kind" (Council_board.Verifier.formalized board ~claim proved));
        is_ok "formalized" (Council_board.Verifier.formalized board ~claim elaborated);
        check "formalized" (Council_board.status board claim = Some Council_board.Formalized);
        equal_string "not twice" (Printf.sprintf "claim %d is already formalized" claim) (is_error "twice" (Council_board.Verifier.formalized board ~claim elaborated));
        equal_string "not verified on evidence that it elaborates" "a claim is verified on evidence of a proof, and on no other"
          (is_error "kind" (Council_board.Verifier.verified board ~claim ~proof elaborated));
        equal_string "not verified by an item that is no proof" (Printf.sprintf "item %d is not a proof" note) (is_error "note" (Council_board.Verifier.verified board ~claim ~proof:note proved));
        let other = is_ok "other" (Council_evidence.proved ~policy:Trust_policy.default ~statement:"True" (accepted ~statement:"True" ())) in
        equal_string "not verified by a proof of something else" "the proof is of another statement: True" (is_error "other" (Council_board.Verifier.verified board ~claim ~proof other));
        equal_string "a status is the status of a claim" (Printf.sprintf "item %d is not a claim" proof) (is_error "item" (Council_board.Verifier.verified board ~claim:proof ~proof proved));
        check "none of this changed anything" (Council_board.status board claim = Some Council_board.Formalized && Council_board.examined board proof = Council_board.Unexamined);
        is_ok "verified" (Council_board.Verifier.verified board ~claim ~proof proved);
        check "verified" (Council_board.status board claim = Some Council_board.Verified);
        equal_strings "the evidence the status rests on, in the order it was given" [ "elaborated"; "proved" ]
          (List.map (fun e -> Council_evidence.kind_name (Council_evidence.kind e)) (Council_board.evidence board claim));
        let disproved = Council_evidence.disproved ~statement:formal { Maithic_refute.Refute_witness.closer = "decide"; lean = "theorem r : ¬ P := by decide"; axioms = [] } in
        equal_string "a verified claim is not refuted" (Printf.sprintf "claim %d is verified" claim) (is_error "late" (Council_board.Verifier.refuted board ~claim ~counterexample:counter disproved));
        equal_string "an accepted proof is not rejected" (Printf.sprintf "proof %d was accepted" proof) (is_error "late" (Council_board.Verifier.rejected board ~proof "no"));
        (* Refutation, on a claim of its own. *)
        let second = post Council_board.Claim "another" in
        let point = post Council_board.Counterexample ~refers:[ second ] "n := 0" in
        equal_string "not refuted on evidence of a proof" "a claim is refuted on evidence of a refutation, and on no other"
          (is_error "kind" (Council_board.Verifier.refuted board ~claim:second ~counterexample:point proved));
        equal_string "not refuted by a counterexample to another claim" (Printf.sprintf "item %d is not a counterexample to claim %d" counter second)
          (is_error "other" (Council_board.Verifier.refuted board ~claim:second ~counterexample:counter disproved));
        is_ok "refuted" (Council_board.Verifier.refuted board ~claim:second ~counterexample:point disproved);
        check "refuted" (Council_board.status board second = Some Council_board.Refuted);
        let j = Council_board.item_to_json board (Option.get (Council_board.find board claim)) in
        equal_string "the status, in the record" "verified" (Json.string_or "" "status" j);
        equal_int "with its evidence" 2 (List.length (Json.list_or_empty "evidence" j))) ]

(* ---- messages and the transcript ---- *)

let message ?(seq = 1) ?(sender = "prover") ?(recipients = [ "verifier" ]) ?(subject = "a proof") () =
  { Council_message.seq; at = 1.25; round = 1; sender; recipients; performative = Council_message.Propose; subject; content = "by omega"; refers = [ 3; 4 ] }

let protocol_tests =
  [ case "P06-04-01 a message has a sender, recipients, a performative, a subject and a content" (fun () ->
        equal_strings "the performatives" [ "request"; "inform"; "propose"; "critique"; "verdict"; "decline" ] (List.map Council_message.performative_name Council_message.performatives);
        List.iter
          (fun p -> check "a performative is read back from its name" (Council_message.performative_of_string (Council_message.performative_name p) = Some p))
          Council_message.performatives;
        check "a well formed message" (Council_message.check (message ()) = Ok ());
        equal_string "no sender" "a message needs a sender" (is_error "sender" (Council_message.check (message ~sender:" " ())));
        equal_string "no recipient" "a message needs at least one recipient, and each needs a name" (is_error "recipients" (Council_message.check (message ~recipients:[] ())));
        equal_string "a recipient without a name" "a message needs at least one recipient, and each needs a name"
          (is_error "recipients" (Council_message.check (message ~recipients:[ "verifier"; "" ] ())));
        equal_string "no subject" "a message needs a subject" (is_error "subject" (Council_message.check (message ~subject:"" ())));
        equal_string "no place" "a message needs a place of one or more" (is_error "seq" (Council_message.check (message ~seq:0 ())));
        let j = Council_message.to_json (message ()) in
        equal_string "the performative, in the record" "propose" (Json.string_or "" "performative" j);
        equal_strings "the recipients" [ "verifier" ] (Json.strings (Json.List (Json.list_or_empty "recipients" j)));
        check "a message is read back as it was written" (Council_message.of_json j = Ok (message ()));
        check "one without references has no member for them" (Json.member "refers" (Council_message.to_json { (message ()) with Council_message.refers = [] }) = None);
        let odd = match j with Json.Obj members -> Json.Obj (List.map (fun (k, v) -> if k = "performative" then (k, Json.String "shout") else (k, v)) members) | other -> other in
        equal_string "an unknown performative" "a message with the unknown performative 'shout'" (is_error "odd" (Council_message.of_json odd)));
    case "P06-04-02 the transcript keeps every message of a session in the order it was said" (fun () ->
        let now = ref 50.0 in
        let transcript = Council_transcript.create ~clock:(fun () -> !now) () in
        let say sender recipients performative subject = Council_transcript.say transcript ~sender ~recipients performative ~subject "content" in
        let first = say "coordinator" [ "prover" ] Council_message.Request "prove" in
        now := 52.0;
        Council_transcript.set_round transcript 1;
        let second = say "prover" [ "verifier" ] Council_message.Propose "one proof" in
        let third = say "verifier" [ "coordinator"; "prover" ] Council_message.Verdict "rejected" in
        equal_ints "each message gets its place when it is said" [ 1; 2; 3 ] [ first.Council_message.seq; second.Council_message.seq; third.Council_message.seq ];
        equal_ints "and the round it was said in" [ 0; 1; 1 ] [ first.Council_message.round; second.Council_message.round; third.Council_message.round ];
        check "and its moment" (first.Council_message.at = 0.0 && third.Council_message.at = 2.0);
        equal_int "the length" 3 (Council_transcript.length transcript);
        equal_strings "in order" [ "prove"; "one proof"; "rejected" ] (List.map (fun (m : Council_message.t) -> m.Council_message.subject) (Council_transcript.messages transcript));
        equal_strings "what a member said" [ "one proof" ] (List.map (fun (m : Council_message.t) -> m.Council_message.subject) (Council_transcript.from transcript "prover"));
        equal_strings "what was said to a member" [ "prove"; "rejected" ] (List.map (fun (m : Council_message.t) -> m.Council_message.subject) (Council_transcript.to_ transcript "prover"));
        raises "a message that is not well formed is not logged" (fun () -> say "prover" [] Council_message.Inform "nobody listens");
        equal_int "and leaves the log as it was" 3 (Council_transcript.length transcript);
        (* A session's transcript begins with the problem and ends with the answer. *)
        let s = session (Some (fst (script ()))) in
        let messages = Council_transcript.messages s.Council_session.transcript in
        equal_ints "the places are 1, 2, 3, …" (List.init (List.length messages) (fun i -> i + 1)) (List.map (fun (m : Council_message.t) -> m.Council_message.seq) messages);
        equal_string "the first message" "the problem: sumOdd_eq_square" (List.hd messages).Council_message.subject;
        equal_string "the last message" "the answer: verified" (List.nth messages (List.length messages - 1)).Council_message.subject;
        check "only the verifier gives a verdict"
          (List.for_all (fun (m : Council_message.t) -> m.Council_message.performative <> Council_message.Verdict || m.Council_message.sender = "verifier") messages);
        check "the rounds never go back" (fst (List.fold_left (fun (ok, last) (m : Council_message.t) -> (ok && m.Council_message.round >= last, m.Council_message.round)) (true, 0) messages)));
    case "P06-04-03 a transcript is exported as JSON and as Markdown" (fun () ->
        let s = session (Some (fst (script ()))) in
        let transcript = s.Council_session.transcript in
        let j = Council_transcript.to_json transcript in
        let read = is_ok "read" (Council_transcript.of_json j) in
        check "the JSON is read back as the messages that were said" (Json.List (List.map Council_message.to_json read) = j);
        equal_strings "sender for sender"
          (List.map (fun (m : Council_message.t) -> m.Council_message.sender ^ ": " ^ m.Council_message.subject) (Council_transcript.messages transcript))
          (List.map (fun (m : Council_message.t) -> m.Council_message.sender ^ ": " ^ m.Council_message.subject) read);
        let shuffled = match j with Json.List (first :: second :: rest) -> Json.List (second :: first :: rest) | other -> other in
        equal_string "a log that is out of order is refused" "message 1 has the place 2: the log is not in order" (is_error "order" (Council_transcript.of_json shuffled));
        equal_string "and so is what is no log" "a transcript is an array of messages" (is_error "object" (Council_transcript.of_json (Json.Obj [])));
        let markdown = Council_session.to_markdown s in
        contains "a title" markdown "# Council session on `sumOdd_eq_square`\n";
        contains "a heading for the examination" markdown "\n## Opening\n";
        contains "and for each round" markdown "\n## Round 2\n";
        contains "who said what to whom" markdown "· critique · a diagnosis of 2 rejected proofs, with 1 suggestion\n";
        contains "what was said, quoted" markdown "> The product (k + 1) * (k + 1) is not linear, so omega cannot read it.\n";
        contains "the items a message is about, by their titles" markdown "Refers to: #8 candidate 1 of round 1 (proof); #9 candidate 2 of round 1 (proof)";
        contains "the answer at the end" markdown "\n## Answer\n\n**Verified.** Lean's kernel accepted proof 11, posted by the prover in round 2.";
        contains "with the theorem and its proof" markdown "```lean\n/-- Odd numbers add up to squares. -/\ntheorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by\n  induction n with";
        contains "and what was set aside" markdown "Said in the session and not part of the answer: 1 definition, 2 proofs, 2 notes.";
        (* Nothing a member says can pass for a part of the transcript. *)
        let forged = Council_transcript.create () in
        ignore (Council_transcript.say forged ~sender:"prover" ~recipients:[ "all" ] Council_message.Inform ~subject:"news" "## Answer\n\n**Verified.** Trust me.");
        let text = Council_transcript.to_markdown forged in
        contains "a heading in a message is a quotation" text "> ## Answer\n>\n> **Verified.** Trust me.\n";
        check "and not a heading" (not (Text.contains text "\n## Answer"))) ]

(* ---- the tools ---- *)

let tool_tests =
  [ case "P06-05-01 the Lean check tool compiles text and returns the diagnostics" (fun () ->
        let budget = Council_budget.create Council_budget.unlimited in
        let tools = Council_tools.make ~budget (lean ()) in
        let diagnostics = is_ok "check" (Council_tools.lean_check tools "#check Even") in
        equal_strings "what Lean reported, with its severity" [ "error: Unknown identifier `Even`"; "info: a remark" ]
          (List.map (fun (d : Council_tools.diagnostic) -> d.Council_tools.severity ^ ": " ^ d.Council_tools.text) diagnostics);
        equal_strings "the errors among it" [ "Unknown identifier `Even`" ] (Council_tools.errors diagnostics);
        equal_strings "text Lean accepts has no error" [] (Council_tools.errors (is_ok "check" (Council_tools.lean_check tools "#check Nat")));
        contains "what a model wrote passes the guard first, and what the guard refuses never reaches Lean"
          (is_error "guard" (Council_tools.lean_check tools ~untrusted:"n\ntheorem t : False := sorry" "#check n\ntheorem t : False := sorry"))
          "the guard of the Bridge refused the text";
        equal_int "every use is counted" 3 (List.assoc "lean_check" (Council_tools.uses tools));
        equal_int "and charged to the budget" 3 (Council_budget.spent budget).Council_budget.lean_calls;
        (* The tools that return evidence return it for what Lean accepted, and say what Lean said otherwise. *)
        check "the statement elaborates" (Result.is_ok (Council_tools.elaborate tools));
        check "a proof Lean accepts is evidence" (Result.is_ok (Council_tools.check_proof tools ~policy:Trust_policy.default ~statement:formal good_proof));
        check "a proof Lean rejects is what Lean said"
          (Council_tools.check_proof tools ~policy:Trust_policy.default ~statement:formal "by simp"
           = Error (Council_tools.Rejected "unsolved goals\n⊢ sumOdd k + (2 * k + 1) = (k + 1) * (k + 1)"));
        equal_strings "the names Lean knows" [ "Nat.mul_comm" ] (List.map fst (Council_tools.lookup tools [ "Nat.mul_comm"; "Nat.nonsense" ]));
        equal_strings "how often each tool was used" [ "lean_check 3"; "lookup 1"; "elaborate 1"; "check_proof 2"; "prove 0"; "refute 0" ]
          (List.map (fun (name, n) -> Printf.sprintf "%s %d" name n) (Council_tools.uses tools));
        (* A tool that is asked after the budget is used up answers with that, and does nothing. *)
        let seen = ref [] in
        let spent = Council_tools.make ~budget:(Council_budget.create { Council_budget.unlimited with Council_budget.lean_calls = 1 }) (lean ~seen ()) in
        check "the first call is made" (Result.is_ok (Council_tools.lean_check spent "#check Nat"));
        equal_string "the second is not" "the budget of the session is used up: the limit of 1 calls to Lean is reached" (is_error "budget" (Council_tools.lean_check spent "#check Nat"));
        check "nor a check of a proof, which is then not judged at all"
          (Council_tools.check_proof spent ~policy:Trust_policy.default ~statement:formal good_proof
           = Error (Council_tools.Unavailable "the budget of the session is used up: the limit of 1 calls to Lean is reached"));
        equal_strings "Lean never saw the proof" [] !seen;
        equal_int "nothing is looked up either" 0 (List.length (Council_tools.lookup spent [ "Nat.mul_comm" ])));
    case "P06-05-02 the prove tool runs one bounded attempt of the prover, and its proof is a candidate like any other" (fun () ->
        let budget = Council_budget.create Council_budget.unlimited in
        let found = Council_tools.make ~budget (lean ~prove:(fun () -> (Ok (good_proof, "portfolio"), 7)) ()) in
        check "the session has a prover" (Council_tools.can_prove found);
        check "a proof and who found it" (Council_tools.prove found = Ok (good_proof, "portfolio"));
        equal_int "the proofs it gave to Lean are charged" 7 (Council_budget.spent budget).Council_budget.lean_calls;
        let none = Council_tools.make (lean ~prove:(fun () -> (Error "its automation found no proof in 18 attempts", 18)) ()) in
        equal_string "no proof, and why" "its automation found no proof in 18 attempts" (is_error "prove" (Council_tools.prove none));
        equal_int "an attempt that fails is charged too" 18 (Council_budget.spent (Council_tools.budget none)).Council_budget.lean_calls;
        let without = Council_tools.make (lean ()) in
        check "a session without a prover" (not (Council_tools.can_prove without));
        equal_string "says so" "the session has no prover" (is_error "prove" (Council_tools.prove without));
        (* In the examination the verifier lets automation try, and what it finds goes to Lean like any proof. *)
        let seen = ref [] in
        let s = session ~raw:(lean ~seen ~prove:(fun () -> (Ok (good_proof, "portfolio"), 7)) ()) (Some (fst (script ()))) in
        check "verified in the examination" (Council_session.status s = Council_board.Verified);
        equal_strings "one turn, and no model" [ "verifier examine 0" ]
          (List.map
             (fun (r : Council_session.turn_result) ->
               Printf.sprintf "%s %s %d" r.Council_session.turn.Council_schedule.role (Council_plan.task_name r.Council_session.turn.Council_schedule.task) r.Council_session.model_calls)
             s.Council_session.turns);
        equal_strings "the proof of the prover was checked as a candidate" [ good_proof ] !seen;
        let proof = Option.get s.Council_session.answer.Council_synthesis.proof in
        equal_string "and is posted by the verifier, with where it came from" "verifier: from the prover's automation (portfolio)" (proof.Council_board.author ^ ": " ^ proof.Council_board.title);
        contains "in a line" (Council_session.summary s) "verified: proof 3, by the verifier in the examination (0 model calls";
        (* A proof of the prover that Lean does not accept here verifies nothing. *)
        let wrong = session ~raw:(lean ~prove:(fun () -> (Ok ("by simp", "portfolio"), 3)) ()) None in
        check "the claim stays formalized" (Council_session.status wrong = Council_board.Formalized);
        equal_string "and the turn says so" "automation proposed a proof that was not accepted" (List.hd wrong.Council_session.turns).Council_session.summary) ]

(* ---- budget and schedule ---- *)

let control_tests =
  [ case "P06-06-01 a session has limits on tokens, cost, time and calls to Lean, and stops when one is reached" (fun () ->
        let now = ref 0.0 in
        let limits = { Council_budget.tokens = 1000; cost = 0.05; seconds = 60.0; lean_calls = 5; model_calls = 3 } in
        let fresh ?(limits = limits) () = Council_budget.create ~clock:(fun () -> !now) ~prices:{ Council_budget.input = 3.0; output = 15.0 } limits in
        let budget = fresh () in
        check "nothing is reached at the start" (Council_budget.exhausted budget = None);
        Council_budget.charge_model budget ~input_tokens:400 ~output_tokens:100;
        Council_budget.charge_lean budget;
        Council_budget.charge_lean ~calls:2 budget;
        now := 12.5;
        let spent = Council_budget.spent budget in
        equal_ints "tokens read and written, calls to Lean and to a model" [ 400; 100; 3; 1 ]
          [ spent.Council_budget.input_tokens; spent.Council_budget.output_tokens; spent.Council_budget.lean_calls; spent.Council_budget.model_calls ];
        check "the cost, at the prices given" (abs_float (spent.Council_budget.cost -. ((400.0 *. 3.0 +. 100.0 *. 15.0) /. 1e6)) < 1e-12);
        check "the time" (spent.Council_budget.seconds = 12.5);
        check "still within every limit" (Council_budget.exhausted budget = None);
        let reached charge = let b = fresh () in charge b; Option.get (Council_budget.exhausted b) in
        equal_string "tokens" "the limit of 1000 tokens is reached" (reached (fun b -> Council_budget.charge_model b ~input_tokens:900 ~output_tokens:100));
        equal_string "calls to Lean" "the limit of 5 calls to Lean is reached" (reached (fun b -> Council_budget.charge_lean ~calls:5 b));
        equal_string "calls to a model" "the limit of 3 calls to a model is reached" (reached (fun b -> for _ = 1 to 3 do Council_budget.charge_model b ~input_tokens:1 ~output_tokens:1 done));
        let costly = Council_budget.create ~prices:{ Council_budget.input = 3.0; output = 15.0 } { Council_budget.unlimited with Council_budget.cost = 0.05 } in
        Council_budget.charge_model costly ~input_tokens:10_000 ~output_tokens:2_000;
        equal_string "cost" "the limit of 0.05 dollars is reached" (Option.get (Council_budget.exhausted costly));
        now := 60.0;
        equal_string "time" "the limit of 60 seconds is reached" (Option.get (Council_budget.exhausted (let b = fresh () in now := !now +. 60.0; b)));
        let free = Council_budget.create Council_budget.unlimited in
        Council_budget.charge_model free ~input_tokens:10_000_000 ~output_tokens:10_000_000;
        Council_budget.charge_lean ~calls:100_000 free;
        check "a limit of zero is no limit" (Council_budget.exhausted free = None);
        check "and without prices nothing costs anything" ((Council_budget.spent free).Council_budget.cost = 0.0);
        equal_ints "the limits when none are given" [ 200_000; 200; 24 ]
          [ Council_budget.default_limits.Council_budget.tokens; Council_budget.default_limits.Council_budget.lean_calls; Council_budget.default_limits.Council_budget.model_calls ];
        let j = Council_budget.to_json costly in
        check "the limit that was reached is in the record" (Json.string_or "" "exhausted" j = "the limit of 0.05 dollars is reached");
        check "with the prices" (Json.member "prices" j <> None);
        (* A session stops between two calls when a limit is reached, and keeps what it had established. *)
        let short = session ~budget:(Council_budget.create { Council_budget.unlimited with Council_budget.model_calls = 2 }) (Some (fst (script ()))) in
        (match short.Council_session.ending with
         | Council_session.Out_of_budget why -> equal_string "the limit that ended it" "the limit of 2 calls to a model is reached" why
         | _ -> fail "the budget should have ended the session");
        equal_strings "the turns up to that point" [ "examine"; "research"; "prove"; "verify" ]
          (List.map (fun (r : Council_session.turn_result) -> Council_plan.task_name r.Council_session.turn.Council_schedule.task) short.Council_session.turns);
        check "the claim keeps the status it had reached" (Council_session.status short = Council_board.Formalized);
        contains "in a line" (Council_session.summary short) "not proved: the claim is formalized; the budget ran out: the limit of 2 calls to a model is reached";
        let model, asked = models ~tokens:(700, 300) [ ("council.research", [ Ok research ]); ("council.prove", [ Ok first_proofs; Ok second_proofs ]); ("council.critique", [ Ok critique ]) ] in
        let frugal = session ~budget:(Council_budget.create { Council_budget.unlimited with Council_budget.tokens = 1500 }) (Some model) in
        equal_strings "tokens are counted from what the models report" [ "council.research"; "council.prove" ] (List.map fst !asked);
        check "and end a session too" (frugal.Council_session.ending = Council_session.Out_of_budget "the limit of 1500 tokens is reached");
        let lean_bound = session ~budget:(Council_budget.create { Council_budget.unlimited with Council_budget.lean_calls = 3 }) (Some (fst (script ()))) in
        check "and so do calls to Lean" (lean_bound.Council_session.ending = Council_session.Out_of_budget "the limit of 3 calls to Lean is reached");
        check "a proof that was never given to Lean verifies nothing" (Council_session.status lean_bound = Council_board.Formalized);
        let proofs = Council_board.of_kind lean_bound.Council_session.board Council_board.Proof in
        equal_strings "and is not said to be rejected either" [ "rejected"; "unexamined" ]
          (List.map
             (fun (i : Council_board.item) ->
               match Council_board.examined lean_bound.Council_session.board i.Council_board.id with
               | Council_board.Rejected _ -> "rejected"
               | Council_board.Accepted -> "accepted"
               | Council_board.Unexamined -> "unexamined")
             proofs);
        equal_string "the turn says what was left" "Lean could not be asked about 1 of 2 proofs"
          (List.nth lean_bound.Council_session.turns (List.length lean_bound.Council_session.turns - 1)).Council_session.summary;
        (* When only the models are spent, what is on the board still goes to Lean. *)
        let late = session ~budget:(Council_budget.create { Council_budget.unlimited with Council_budget.model_calls = 2 }) (Some (fst (models [ ("council.research", [ Ok research ]); ("council.prove", [ Ok second_proofs ]) ]))) in
        check "a proof proposed with the last call is still verified" (Council_session.status late = Council_board.Verified));
    case "P06-06-02 the scheduler gives each role its turn in a fixed order" (fun () ->
        equal_strings "the order within a round" [ "researcher"; "prover"; "verifier"; "critic" ] Council_schedule.order;
        let shown turns = List.map (fun (t : Council_schedule.turn) -> Printf.sprintf "%d:%d %s %s" t.Council_schedule.number t.Council_schedule.round t.Council_schedule.role (Council_plan.task_name t.Council_schedule.task)) turns in
        equal_strings "the turns of a session of two rounds"
          [ "1:0 verifier examine"; "2:1 researcher research"; "3:1 prover prove"; "4:1 verifier verify"; "5:1 critic critique"; "6:2 prover prove"; "7:2 verifier verify" ]
          (shown (Council_schedule.turns ~rounds:2));
        equal_strings "no round, only the examination" [ "1:0 verifier examine" ] (shown (Council_schedule.turns ~rounds:0));
        equal_int "three rounds" 10 (List.length (Council_schedule.turns ~rounds:3));
        equal_string "in a line" "examine: verifier | round 1: researcher, prover, verifier, critic | round 2: prover, verifier" (Council_schedule.describe ~rounds:2);
        let turns = Council_schedule.turns ~rounds:2 in
        check "the turn after a turn" ((Option.get (Council_schedule.after turns (List.nth turns 2))).Council_schedule.role = "verifier");
        check "nothing after the last" (Council_schedule.after turns (List.nth turns 6) = None);
        (* A session follows the schedule, turn for turn, for as long as it lasts. *)
        let s = session ~settings:{ Council_session.rounds = 2; candidates = 3 } (Some (fst (models [ ("council.research", [ Ok research ]); ("council.prove", [ Ok first_proofs; Ok {|{"candidates": ["by rfl"]}|} ]); ("council.critique", [ Ok critique ]) ]))) in
        equal_strings "every turn of the schedule was taken, in its order" (shown turns) (shown (List.map (fun (r : Council_session.turn_result) -> r.Council_session.turn) s.Council_session.turns));
        check "and the rounds were over" (s.Council_session.ending = Council_session.Rounds_over);
        contains "in a line" (Council_session.summary s) "not proved: the claim is formalized; the 2 rounds are over";
        let early = session (Some (fst (script ()))) in
        equal_strings "a session that is settled early is a beginning of the schedule"
          (List.filteri (fun i _ -> i < 7) (shown (Council_schedule.turns ~rounds:3)))
          (shown (List.map (fun (r : Council_session.turn_result) -> r.Council_session.turn) early.Council_session.turns))) ]

let () =
  run "council"
    [ ("coordinator", coordinator_tests); ("roles", role_tests); ("blackboard", memory_tests); ("protocol", protocol_tests); ("tools", tool_tests);
      ("control", control_tests) ]
