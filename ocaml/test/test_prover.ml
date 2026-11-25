(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_prover
open Testkit

let file =
  {|import Std

/-- A doc comment that mentions theorem fake : True := trivial. -/
def double (n : Nat) : Nat := n + n -- theorem also_fake := 1

namespace Demo

@[simp] theorem double_eq (n : Nat)
    (h : n = n := rfl) : double n = 2 * n := by
  unfold double
  omega

/- a block comment with := and theorem inside /- nested -/ still comment -/
private theorem hidden (xs : List Nat) : xs ++ [] = xs := by
  sorry

lemma str : "theorem s := sorry" = "theorem s := sorry" := rfl

end Demo
|}

let source_tests =
  [ case "masks comments and string contents but keeps every offset" (fun () ->
        let masked = Lean_source.mask file in
        equal_int "length" (String.length file) (String.length masked);
        check "line breaks kept" (List.length (String.split_on_char '\n' masked) = List.length (String.split_on_char '\n' file));
        check "comment text removed" (not (Text.contains masked "fake"));
        check "string text removed" (not (Text.contains masked "theorem s"));
        check "code kept" (Text.contains masked "def double (n : Nat) : Nat := n + n"));
    case "finds the theorems of a file and nothing inside comments or strings" (fun () ->
        equal_strings "names" [ "double_eq"; "hidden"; "str" ]
          (List.map (fun (d : Lean_source.decl) -> d.Lean_source.name) (Lean_source.declarations file)));
    case "splits a declaration at the := that ends its statement" (fun () ->
        let d = is_ok "find" (Lean_source.find file "double_eq") in
        equal_string "header" "@[simp] theorem double_eq (n : Nat)\n    (h : n = n := rfl) : double n = 2 * n" d.Lean_source.header;
        equal_string "proof" "by\n  unfold double\n  omega" d.Lean_source.proof;
        equal_int "line" 8 d.Lean_source.line;
        check "prelude ends before the declaration" (Text.ends_with ~suffix:"namespace Demo\n\n" d.Lean_source.prelude);
        check "comments after the proof belong to what follows" (Text.starts_with ~prefix:"\n/- a block comment" d.Lean_source.rest);
        equal_string "statement" "@[simp] theorem double_eq (n : Nat) (h : n = n := rfl) : double n = 2 * n" (Lean_source.statement d));
    case "keeps modifiers on the line of the keyword in the header" (fun () ->
        let d = is_ok "find" (Lean_source.find file "hidden") in
        equal_string "header" "private theorem hidden (xs : List Nat) : xs ++ [] = xs" d.Lean_source.header;
        check "unfinished" (Lean_source.has_sorry d);
        check "finished" (not (Lean_source.has_sorry (is_ok "find" (Lean_source.find file "double_eq"))));
        check "sorry inside a string is not code" (not (Lean_source.has_sorry (is_ok "find" (Lean_source.find file "str")))));
    case "explains what is in the file when a theorem is not found" (fun () ->
        contains "listing" (is_error "missing" (Lean_source.find file "nope")) "found: double_eq, hidden, str";
        contains "empty" (is_error "empty" (Lean_source.find "def x := 1\n" "x")) "no theorem";
        ignore (is_ok "by short name" (Lean_source.find file "Demo.hidden")));
    case "replacing a proof leaves the rest of the file untouched" (fun () ->
        let d = is_ok "find" (Lean_source.find file "hidden") in
        let updated = Lean_source.with_proof d "by simp" in
        contains "new proof" updated "private theorem hidden (xs : List Nat) : xs ++ [] = xs := by simp\n";
        check "no sorry left" (not (Lean_source.has_sorry (is_ok "find" (Lean_source.find updated "hidden"))));
        check "before unchanged" (Text.starts_with ~prefix:d.Lean_source.prelude updated);
        check "after unchanged" (Text.ends_with ~suffix:d.Lean_source.rest updated);
        equal_strings "same theorems" [ "double_eq"; "hidden"; "str" ]
          (List.map (fun (d : Lean_source.decl) -> d.Lean_source.name) (Lean_source.declarations updated)));
    case "a documentation comment and attributes on lines of their own belong to the declaration" (fun () ->
        let source =
          "import Lib\n\ndef f := 1\n\n-- a note\n/-- The sum of `a` and `b`,\n    /- with a comment inside -/ in either order. -/\n@[simp]\nprivate\ntheorem \
           swap (a b : Nat) : a + b = b + a := by\n  sorry\n\n/-- Another. -/\ntheorem next : 1 = 1 := rfl\n"
        in
        let d = is_ok "find" (Lean_source.find source "swap") in
        equal_string "what comes before ends before the comment" "import Lib\n\ndef f := 1\n\n-- a note\n" d.Lean_source.prelude;
        equal_string "the comment" "/-- The sum of `a` and `b`,\n    /- with a comment inside -/ in either order. -/\n" d.Lean_source.doc;
        equal_string "the statement with its attributes and modifiers" "@[simp]\nprivate\ntheorem swap (a b : Nat) : a + b = b + a" d.Lean_source.header;
        equal_int "the line of the keyword" 10 d.Lean_source.line;
        equal_string "on one line" "@[simp] private theorem swap (a b : Nat) : a + b = b + a" (Lean_source.statement d);
        let updated = Lean_source.with_proof d "by omega" in
        equal_string "the file with the proof filled in"
          (Text.replace_all ~sub:" := by\n  sorry\n" ~by:" := by omega\n" source) updated;
        let after = is_ok "find" (Lean_source.find source "next") in
        equal_string "the comment of the next theorem is its own" "/-- Another. -/\n" after.Lean_source.doc;
        check "and is not part of what follows the first" (Text.starts_with ~prefix:"\n/-- Another. -/" d.Lean_source.rest);
        (* What looks like the end of a comment but is not one changes nothing. *)
        let plain = is_ok "find" (Lean_source.find "def g := 2 -/ 1\ntheorem t : 1 = 1 := rfl\n" "t") in
        equal_string "no comment" "" plain.Lean_source.doc;
        equal_string "prelude" "def g := 2 -/ 1\n" plain.Lean_source.prelude;
        let first = is_ok "find" (Lean_source.find "/-- First thing in the file. -/\ntheorem t : 1 = 1 := rfl\n" "t") in
        equal_string "a comment at the very start" "/-- First thing in the file. -/\n" first.Lean_source.doc;
        equal_string "nothing before it" "" first.Lean_source.prelude);
    case "a command that ends in `in` belongs to the declaration it stands before" (fun () ->
        let source =
          "def f := 1\n\nset_option maxHeartbeats 400000 in\nopen Nat in\n/-- Doubling. -/\n@[simp]\ntheorem twice (n : Nat) : n + n = 2 * n := by\n  sorry\n\n"
          ^ "set_option autoImplicit false in\ntheorem next : 1 = 1 := by\n  sorry\n"
        in
        let d = is_ok "find" (Lean_source.find source "twice") in
        equal_string "what comes before is whole commands" "def f := 1\n\n" d.Lean_source.prelude;
        equal_string "the commands and the comment" "set_option maxHeartbeats 400000 in\nopen Nat in\n/-- Doubling. -/\n" d.Lean_source.doc;
        equal_string "the header" "@[simp]\ntheorem twice (n : Nat) : n + n = 2 * n" d.Lean_source.header;
        equal_string "the declaration with a proof keeps its commands"
          "set_option maxHeartbeats 400000 in\nopen Nat in\n/-- Doubling. -/\n@[simp]\ntheorem twice (n : Nat) : n + n = 2 * n := by omega\n"
          (Lean_source.assemble d "by omega");
        check "what follows begins with the command of the next theorem"
          (Text.starts_with ~prefix:"\nset_option autoImplicit false in\ntheorem next" d.Lean_source.rest);
        let after = is_ok "find" (Lean_source.find source "next") in
        equal_string "the next theorem has its own" "set_option autoImplicit false in\n" after.Lean_source.doc;
        check "and what comes before it ends with the first theorem" (Text.ends_with ~suffix:"  sorry\n\n" after.Lean_source.prelude);
        equal_string "the file with both proofs filled in"
          (Text.replace_all ~sub:" := by\n  sorry\n" ~by:" := by omega\n" source)
          (let once = Lean_source.with_proof d "by omega" in
           Lean_source.with_proof (is_ok "find" (Lean_source.find once "next")) "by omega");
        (* A line that only happens to end in the word is left where it is. *)
        let plain = is_ok "find" (Lean_source.find "def within := 1\n-- as said in\ntheorem t : 1 = 1 := rfl\n" "t") in
        equal_string "a comment is not a command" "def within := 1\n-- as said in\n" plain.Lean_source.prelude);
    case "a long or multi-line proof is laid out on its own lines" (fun () ->
        let d = is_ok "find" (Lean_source.find file "hidden") in
        equal_string "layout" "private theorem hidden (xs : List Nat) : xs ++ [] = xs :=\n  by\n    induction xs with\n    | nil => rfl\n    | cons x xs ih => simp\n"
          (Lean_source.assemble d "by\n  induction xs with\n  | nil => rfl\n  | cons x xs ih => simp")) ]

let proposer_tests =
  [ case "P01-02-01 the portfolio enumerates core automation in a fixed order" (fun () ->
        let candidates = Portfolio.candidates "theorem t (a b : Nat) : a + b = b + a" in
        equal_strings "first steps" [ "by rfl"; "by trivial"; "by simp"; "by omega"; "by decide"; "by simp_all"; "by grind" ]
          (List.filteri (fun i _ -> i < 7) candidates);
        check "then the same after introducing hypotheses" (List.nth candidates 7 = "by intros; rfl");
        equal_int "no duplicates" (List.length candidates) (List.length (List.sort_uniq compare candidates));
        check "deterministic" (candidates = Portfolio.candidates "theorem t (a b : Nat) : a + b = b + a");
        check "every candidate passes the screen" (List.for_all Maithic_bridge.Screen.admits candidates));
    case "P01-02-01 a statement that is a named proposition is unfolded first" (fun () ->
        let candidates = Portfolio.candidates "theorem maithic_solution : MaithicBench.Basics.add_comm'" in
        equal_string "first" "by unfold MaithicBench.Basics.add_comm'; intros; rfl" (List.hd candidates);
        check "plain steps follow" (List.mem "by omega" candidates);
        check "not for True" (not (List.exists (fun c -> Text.contains c "unfold") (Portfolio.candidates "theorem t : True")));
        check "not for a compound statement" (not (List.exists (fun c -> Text.contains c "unfold") (Portfolio.candidates "theorem t (p : Prop) : p \xE2\x86\x92 p"))));
    case "reads candidates from the requested JSON object" (fun () ->
        equal_strings "plain" [ "by omega"; "by simp" ] (Model_proposer.parse_candidates {|{"candidates": ["by omega", " by simp ", ""]}|});
        equal_strings "fenced" [ "by omega" ] (Model_proposer.parse_candidates "Here you go:\n```json\n{\"candidates\": [\"by omega\"]}\n```\n");
        equal_strings "surrounded by prose" [ "by grind" ] (Model_proposer.parse_candidates "Sure. {\"candidates\": [\"by grind\"]} Hope it helps.");
        equal_strings "a single fenced proof" [ "by\n  intro h\n  exact h" ] (Model_proposer.parse_candidates "```lean\nby\n  intro h\n  exact h\n```");
        equal_strings "nothing usable" [] (Model_proposer.parse_candidates "I am not sure.");
        equal_strings "wrong shape" [] (Model_proposer.parse_candidates {|{"proofs": ["by omega"]}|}));
    case "keeps the end of a long context" (fun () ->
        let long = String.concat "\n" (List.init 2000 (fun i -> Printf.sprintf "def d%d := %d" i i)) in
        let excerpt = Model_proposer.context_excerpt ~max_chars:500 long in
        check "bounded" (String.length excerpt < 600);
        contains "the last definition is kept" excerpt "def d1999 := 1999";
        contains "the cut is announced" excerpt "omitted";
        equal_string "short context unchanged" "def x := 1" (Model_proposer.context_excerpt "def x := 1")) ]

let attempt ?(accepted = false) ?(round = 1) ?error_class ?(error = "") proof =
  { Attempts.round; proposer = "model"; proof; accepted; error_class; error; elapsed_ms = 1 }

let reflect_tests =
  [ case "P01-03-01 Lean failures are sorted into classes" (fun () ->
        let cls text = Attempts.class_to_string (Attempts.classify text) in
        equal_string "unknown" "unknown-identifier" (cls "unknown identifier 'Nat.foo'");
        equal_string "constant" "unknown-identifier" (cls "Unknown constant `Nat.foo_bar`");
        equal_string "mismatch" "type-mismatch" (cls "Type mismatch\n  h\nhas type\n  a = b");
        equal_string "unsolved" "unsolved-goals" (cls "unsolved goals\nn : Nat\n\xE2\x8A\xA2 n + 0 = n");
        equal_string "timeout" "timeout" (cls "(deterministic) timeout at `isDefEq`, maximum number of heartbeats (200000) has been reached");
        equal_string "tactic" "tactic-failed" (cls "omega could not prove the goal");
        equal_string "tactic" "tactic-failed" (cls "Tactic `rfl` failed: The left-hand side");
        equal_string "decide" "tactic-failed" (cls "Tactic `decide` proved that the proposition\n  1 = 2\nis false");
        equal_string "syntax" "syntax-error" (cls "unexpected token 'at'; expected term");
        equal_string "other" "other" (cls "something else entirely"));
    case "P01-03-06 every error class has a deterministic hint" (fun () ->
        let classes =
          [ Attempts.Unknown_identifier; Attempts.Type_mismatch; Attempts.Unsolved_goals; Attempts.Tactic_failed; Attempts.Syntax_error;
            Attempts.Timeout; Attempts.Not_closed; Attempts.Guard_rejected; Attempts.Other_error ]
        in
        let hints = List.map Attempts.hint classes in
        check "non-empty" (List.for_all (fun h -> String.length h > 20) hints);
        equal_int "distinct" (List.length classes) (List.length (List.sort_uniq compare hints));
        contains "unknown identifier" (Attempts.hint Attempts.Unknown_identifier) "names from the context");
    case "P01-03-03 a proof that was tried is never tried again" (fun () ->
        let memory = Attempts.create () in
        Attempts.record memory (attempt "by simp\n  [foo]");
        check "same text" (Attempts.already_tried memory "by simp\n  [foo]");
        check "same up to blank space" (Attempts.already_tried memory "by   simp [foo]");
        check "different proof" (not (Attempts.already_tried memory "by simp [bar]"));
        equal_strings "fresh keeps order and drops repeats" [ "by omega"; "by grind" ]
          (Attempts.fresh memory [ "by simp [foo]"; "by omega"; "  by   omega "; ""; "by grind" ]));
    case "P01-03-02 the repair text shows each failed proof with Lean's exact error" (fun () ->
        let memory = Attempts.create () in
        Attempts.record memory (attempt ~error_class:Attempts.Tactic_failed ~error:"omega could not prove the goal\nn : Nat\n\xE2\x8A\xA2 2 * tri n = n * (n + 1)" "by omega");
        Attempts.record memory (attempt ~accepted:true "by exact h");
        Attempts.record memory (attempt ~round:2 ~error_class:Attempts.Unknown_identifier ~error:"unknown identifier 'tri_succ'" "by rw [tri_succ]");
        let text = Attempts.render_failures memory in
        contains "first proof" text "by omega";
        contains "goal state from Lean" text "\xE2\x8A\xA2 2 * tri n = n * (n + 1)";
        contains "class" text "error (unknown-identifier):";
        contains "hint" text "hint: A name does not exist.";
        check "accepted proofs are not failures" (not (Text.contains text "by exact h"));
        check "oldest first" (Option.get (Text.find_sub text "by omega") < Option.get (Text.find_sub text "tri_succ"));
        equal_int "limit" 1 (List.length (Text.split_on "proof:\n" (Attempts.render_failures ~limit:1 memory)) - 1));
    property "normalization is idempotent and ignores blank space" (Gen.string_of ~max:30 "ab \n\t;") (fun s ->
        Attempts.normalize (Attempts.normalize s) = Attempts.normalize s
        && Attempts.normalize (" " ^ s ^ "\n") = Attempts.normalize s) ]

let () = run "prover" [ ("lean source", source_tests); ("proposers", proposer_tests); ("attempts and errors", reflect_tests) ]
