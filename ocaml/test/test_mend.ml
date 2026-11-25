(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the repair engine. Lean is stood in for by functions that object to given pieces of
   text, and the model by lists of answers, so that every path can be taken; the real ones meet
   in the integration tests. *)

open Maithic_base
open Maithic_bridge
open Maithic_gateway
open Maithic_prover
open Maithic_mend
open Testkit

(* ---- a stand-in for Lean on whole files ---- *)

let diagnostic ?(severity = "error") ~line ~column ?(width = 1) text =
  { Bridge_types.severity; line; column; end_line = Some line; end_column = Some (column + width); text }

let report diagnostics =
  let count s = List.length (List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = s) diagnostics) in
  { Bridge_types.ok = count "error" = 0; errors = count "error"; warnings = count "warning"; diagnostics; decls = []; elapsed_ms = 0 }

(* Where a text occurs in a source, as lines and columns. The sources of these tests are ASCII
   where it matters, so bytes are columns. *)
let occurrences source needle =
  let rec go from acc =
    match Text.find_sub ~from source needle with
    | None -> List.rev acc
    | Some at ->
      let before = String.sub source 0 at in
      let line = List.length (String.split_on_char '\n' before) in
      let column = match String.rindex_opt before '\n' with Some i -> at - i - 1 | None -> at in
      go (at + 1) ((line, column) :: acc)
  in
  go 0 []

(* A Lean that objects to pieces of text: wherever one stands, it says what is given for it. *)
let objecting objections : Mend_detect.check =
 fun source ->
  Ok
    (report
       (List.concat_map
          (fun (needle, severity, text) ->
            List.map (fun (line, column) -> diagnostic ~severity ~line ~column ~width:(String.length needle) text) (occurrences source needle))
          objections))

let upgrade =
  String.concat "\n"
    [ "def twice (n : Nat) : Nat := n + n"; ""; "/-- Commutes. -/"; "theorem sum_comm (a b : Nat) : a + b = b + a := by"; "  exact Nat.add_commutes a b"; "";
      "theorem uses_old (n : Nat) : twice n = 2 * n := by"; "  exact twice_is_double n"; ""; "theorem explores (n : Nat) : n <= n + 1 := by"; "  exact?"; "";
      "theorem fine (n : Nat) : n = n := rfl"; "" ]

let lean_of_upgrade =
  objecting
    [ ("Nat.add_commutes", "error", "Unknown constant `Nat.add_commutes`");
      ("twice_is_double", "warning", "`twice_is_double` has been deprecated: Use `twice_eq` instead\n\nHint: Replace the deprecated name");
      ("exact?", "info", "Try this:\n  [apply] exact Nat.le_add_right n 1") ]

let breakages_of ?(file = "Upgrade.lean") check source = fst (is_ok "collect" (Mend_detect.collect ~check ~file source))
let class_names breakages = List.map (fun (b : Mend_breakage.t) -> Mend_class.name b.Mend_breakage.class_) breakages
let class_of text ?(severity = "error") () = Mend_class.of_diagnostic (diagnostic ~severity ~line:1 ~column:0 text)

let detect_tests =
  [ case "P17-01-01 what Lean reports is collected by theorem, in the order of the file" (fun () ->
        let found = breakages_of lean_of_upgrade upgrade in
        equal_strings "the theorems that are broken" [ "sum_comm"; "uses_old"; "explores" ] (List.map (fun (b : Mend_breakage.t) -> b.Mend_breakage.declaration) found);
        equal_strings "their classes" [ "unknown-identifier"; "deprecated"; "suggestion" ] (class_names found);
        equal_ints "the lines of the diagnostics" [ 5; 8; 11 ] (List.map (fun (b : Mend_breakage.t) -> b.Mend_breakage.line) found);
        equal_strings "their severities" [ "error"; "warning"; "info" ] (List.map (fun (b : Mend_breakage.t) -> b.Mend_breakage.severity) found);
        check "a theorem Lean has nothing against is no breakage" (not (List.exists (fun (b : Mend_breakage.t) -> b.Mend_breakage.declaration = "fine") found));
        (* A warning that is no deprecation, and an information that is no suggestion, report no breakage. *)
        let noisy = objecting [ ("n = n", "warning", "unused variable `n`"); ("rfl", "info", "Nat"); ("twice n", "warning", "declaration uses `sorry`") ] in
        check "other warnings are not breakages" (breakages_of noisy upgrade = []);
        (* A diagnostic outside every theorem is a breakage of its own, with the line it stands on. *)
        let outside = breakages_of (objecting [ ("n + n", "error", "Unknown identifier `m`") ]) upgrade in
        (match outside with
         | [ b ] ->
           equal_string "no theorem" "" b.Mend_breakage.declaration;
           equal_string "the line as context" "def twice (n : Nat) : Nat := n + n" b.Mend_breakage.context;
           equal_int "its line" 1 b.Mend_breakage.first_line
         | _ -> fail "one breakage outside a theorem was expected");
        equal ~show:(fun l -> String.concat "," (List.map (fun (k, n) -> Printf.sprintf "%s=%d" k n) l)) "counted by class"
          [ ("unknown-identifier", 1); ("deprecated", 1); ("unsolved-goals", 0); ("type-mismatch", 0); ("suggestion", 1); ("other", 0) ]
          (Mend_detect.count_by_class found);
        equal_string "a check that fails is passed on" "the Bridge is gone"
          (is_error "failed" (Mend_detect.collect ~check:(fun _ -> Error "the Bridge is gone") ~file:"f" upgrade)));
    case "P17-01-01 the diagnostics of one theorem are one breakage, classed by its first error" (fun () ->
        let lean =
          objecting
            [ ("twice_is_double", "warning", "`twice_is_double` has been deprecated: Use `twice_eq` instead"); ("exact twice", "error", "unsolved goals\n\xE2\x8A\xA2 False") ]
        in
        match breakages_of lean upgrade with
        | [ b ] ->
          equal_string "the class of the error" "unsolved-goals" (Mend_class.name b.Mend_breakage.class_);
          equal_string "its severity" "error" b.Mend_breakage.severity;
          equal_strings "errors first, then the rest" [ "error"; "warning" ] (List.map (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity) b.Mend_breakage.diagnostics);
          equal_strings "all its classes" [ "unsolved-goals"; "deprecated" ] (List.map Mend_class.name (Mend_breakage.classes b));
          check "it has an error" (Mend_breakage.has_error b)
        | _ -> fail "one breakage was expected");
    case "P17-01-02 a breakage records the declaration, the error, the class and the text it is in" (fun () ->
        let b = List.hd (breakages_of lean_of_upgrade upgrade) in
        equal_string "file" "Upgrade.lean" b.Mend_breakage.file;
        equal_string "declaration" "sum_comm" b.Mend_breakage.declaration;
        equal_string "kind" "theorem" b.Mend_breakage.kind;
        equal_int "line" 5 b.Mend_breakage.line;
        equal_int "column" 8 b.Mend_breakage.column;
        equal_string "what Lean said" "Unknown constant `Nat.add_commutes`" b.Mend_breakage.error;
        equal_string "the context is the theorem with its documentation"
          "/-- Commutes. -/\ntheorem sum_comm (a b : Nat) : a + b = b + a := by\n  exact Nat.add_commutes a b" b.Mend_breakage.context;
        equal_int "and begins on the line of the documentation" 3 b.Mend_breakage.first_line;
        equal_string "the key" "sum_comm|unknown-identifier|Nat.add_commutes" (Mend_breakage.key b);
        (* The key does not move when lines are added above. *)
        let moved = List.hd (breakages_of lean_of_upgrade ("-- a new first line\n" ^ upgrade)) in
        equal_string "the same breakage after the file moved" (Mend_breakage.key b) (Mend_breakage.key moved);
        check "on another line" (moved.Mend_breakage.line = 6);
        let j = Mend_breakage.to_json b in
        equal_string "class in the record" "unknown-identifier" (Json.string_or "" "class" j);
        equal_string "detail" "Nat.add_commutes" (Json.string_or "" "detail" j);
        equal_int "line in the record" 5 (Json.int_or 0 "line" j);
        equal_int "the diagnostics" 1 (List.length (Json.list_or_empty "diagnostics" j));
        equal_int "where the context begins" 3 (Json.int_or 0 "first_line" j);
        equal_string "in a line" "Upgrade.lean:5  sum_comm  unknown-identifier: Unknown constant `Nat.add_commutes`" (Mend_breakage.describe b);
        (* The line that says most: the suggestion itself, and the name when Lean's first line does not hold it. *)
        let suggested = List.nth (breakages_of lean_of_upgrade upgrade) 2 in
        equal_string "a suggestion" "Try this: exact Nat.le_add_right n 1" suggested.Mend_breakage.error;
        let hinted =
          breakages_of (objecting [ ("twice n = 2", "error", "Function expected at\n  twoce\n\nHint: The identifier `twoce` is unknown, and Lean's `autoImplicit` option") ]) upgrade
        in
        equal_string "a name Lean took for a variable" "Unknown identifier `twoce`: Function expected at" (List.hd hinted).Mend_breakage.error);
    case "P17-01-03 a name that no longer exists is an unknown identifier, however Lean words it" (fun () ->
        let unknown text = match class_of text () with Some (Mend_class.Unknown_identifier name) -> name | _ -> fail ("not an unknown identifier: " ^ text) in
        equal_string "an identifier" "foo" (unknown "Unknown identifier `foo`");
        equal_string "a constant with its namespace" "Nat.add_commutes" (unknown "Unknown constant `Nat.add_commutes`");
        equal_string "as older versions of Lean wrote it" "List.foo" (unknown "unknown identifier 'List.foo'");
        equal_string "as older versions wrote a constant" "bar" (unknown "unknown constant 'bar'");
        equal_string "a name in a statement, which Lean took for a variable" "twoce"
          (unknown "Function expected at\n  twoce\nbut this term has type\n  ?m.1\n\nHint: The identifier `twoce` is unknown, and Lean's `autoImplicit` option causes");
        equal_string "the name of the class" "unknown-identifier" (Mend_class.name (Mend_class.Unknown_identifier "foo"));
        equal_string "its detail" "foo" (Mend_class.detail (Mend_class.Unknown_identifier "foo"));
        contains "its treatment" (Mend_class.treatment (Mend_class.Unknown_identifier "foo")) "table of renames";
        equal_strings "names in a message" [ "a"; "b.c" ] (Mend_class.quoted "`a` was replaced by `b.c`");
        check "a quote that does not close names nothing" (Mend_class.quoted "an odd ` in the text" = []));
    case "P17-01-04 a tactic that no longer closes its goal is the class of unsolved goals" (fun () ->
        let unsolved text = class_of text () = Some Mend_class.Unsolved_goals in
        check "goals left at the end" (unsolved "unsolved goals\nn : Nat\n\xE2\x8A\xA2 n + 0 = n");
        check "simp that does nothing" (unsolved "`simp` made no progress");
        check "omega that gives up" (unsolved "omega could not prove the goal:\na possible counterexample may satisfy the constraints");
        check "a tactic that says it failed" (unsolved "Tactic `rfl` failed: The left-hand side\n  n + 0");
        check "a search that found nothing" (unsolved "`exact?` could not close the goal.");
        check "grind" (unsolved "`grind` failed\ncase grind");
        (* Other errors have classes of their own. *)
        check "a term of the wrong type" (class_of "Type mismatch\n  Nat.zero_add n\nhas type\n  0 + n = n\nbut is expected to have type\n  n + 0 = n" () = Some Mend_class.Type_mismatch);
        check "as older versions wrote it" (class_of "type mismatch\n  h\nhas type\n  a\nbut is expected to have type\n  b" () = Some Mend_class.Type_mismatch);
        check "anything else" (class_of "failed to synthesize\n  Decidable p" () = Some Mend_class.Other);
        (* Warnings and information. *)
        check "a deprecation, with the successor Lean names"
          (class_of ~severity:"warning" "`twice_is_double` has been deprecated: Use `twice_eq` instead\n\nHint: Replace `x` by `y`" ()
           = Some (Mend_class.Deprecated { old_name = "twice_is_double"; new_name = "twice_eq" }));
        check "a deprecation without a successor"
          (class_of ~severity:"warning" "`old` has been deprecated" () = Some (Mend_class.Deprecated { old_name = "old"; new_name = "" }));
        check "another warning" (class_of ~severity:"warning" "unused variable `h`" () = None);
        check "a suggestion" (class_of ~severity:"info" "Try this:\n  [apply] exact Nat.le_refl n" () = Some (Mend_class.Suggestion [ "exact Nat.le_refl n" ]));
        check "as older versions wrote it" (class_of ~severity:"info" "Try this: simp only [Nat.add_zero]" () = Some (Mend_class.Suggestion [ "simp only [Nat.add_zero]" ]));
        check "a choice of suggestions"
          (class_of ~severity:"info" "Try these:\n  [apply] exact h\n  [apply] exact h.symm" () = Some (Mend_class.Suggestion [ "exact h"; "exact h.symm" ]));
        check "a suggestion of several lines"
          (class_of ~severity:"info" "Try this:\n  [apply] refine \xE2\x9F\xA8?_, ?_\xE2\x9F\xA9\n    \xC2\xB7 rfl" () = Some (Mend_class.Suggestion [ "refine \xE2\x9F\xA8?_, ?_\xE2\x9F\xA9\n  \xC2\xB7 rfl" ]));
        check "other information" (class_of ~severity:"info" "Nat : Type" () = None);
        equal_strings "the classes" [ "unknown-identifier"; "deprecated"; "unsolved-goals"; "type-mismatch"; "suggestion"; "other" ] Mend_class.names;
        contains "the treatment of a proof that no longer closes" (Mend_class.treatment Mend_class.Unsolved_goals) "ask a model for the smallest change") ]

(* ---- rules ---- *)

let context ?(renames = []) source = { Mend_rule.source; renames }
let table = [ ("Nat.add_commutes", "Nat.add_comm") ]

let rule_tests =
  [ case "P17-02-01 a rule looks at a breakage and proposes edits, or none" (fun () ->
        (* A rule of the test: it comments a broken line out. *)
        let silence =
          { Mend_rule.name = "silence"; priority = 5; summary = "Comments out the line of the first diagnostic.";
            propose =
              (fun (c : Mend_rule.context) (b : Mend_breakage.t) ->
                match Mend_edit.offset c.Mend_rule.source ~line:b.Mend_breakage.line ~column:0 with
                | Some start -> [ { Mend_edit.rule = "silence"; description = "comment the line out"; replacements = [ { Mend_edit.start; stop = start; text = "-- " } ]; untrusted = "-- " } ]
                | None -> []) }
        in
        let b = List.hd (breakages_of lean_of_upgrade upgrade) in
        (match silence.Mend_rule.propose (context upgrade) b with
         | [ edit ] ->
           let changed = is_ok "apply" (Mend_edit.apply upgrade edit.Mend_edit.replacements) in
           contains "the edit" changed "\n--   exact Nat.add_commutes a b\n";
           equal_int "one line differs, before and after" 2 (Mend_edit.changed_lines upgrade changed)
         | _ -> fail "one edit was expected");
        check "the rename rule has nothing for a breakage without a name it knows" (Mend_rename.rule.Mend_rule.propose (context upgrade) b = []));
    case "P17-02-01 edits are replacements of stretches of text, and positions are Lean's" (fun () ->
        let text = "ab\n\xCE\xB1\xCE\xB2 cd\nlast" in
        check "a line and a column" (Mend_edit.offset text ~line:1 ~column:1 = Some 1);
        check "columns count code points, not bytes" (Mend_edit.offset text ~line:2 ~column:3 = Some 8);
        check "the end of the text" (Mend_edit.offset text ~line:3 ~column:4 = Some (String.length text));
        check "a column past the end of its line" (Mend_edit.offset text ~line:1 ~column:5 = None);
        check "a line that is not there" (Mend_edit.offset text ~line:4 ~column:0 = None);
        check "the stretch of a diagnostic" (Mend_edit.span text (diagnostic ~line:2 ~column:3 ~width:2 "x") = Some (8, 10));
        check "a diagnostic without an end has none" (Mend_edit.span text { (diagnostic ~line:2 ~column:3 "x") with Bridge_types.end_line = None } = None);
        equal_string "replacements are made together, whatever their order" "AB\n\xCE\xB1\xCE\xB2 CD\nlast"
          (is_ok "apply" (Mend_edit.apply text [ { Mend_edit.start = 8; stop = 10; text = "CD" }; { Mend_edit.start = 0; stop = 2; text = "AB" } ]));
        equal_string "an insertion" "ab!\n\xCE\xB1\xCE\xB2 cd\nlast" (is_ok "insert" (Mend_edit.apply text [ { Mend_edit.start = 2; stop = 2; text = "!" } ]));
        contains "overlapping replacements are refused"
          (is_error "overlap" (Mend_edit.apply text [ { Mend_edit.start = 0; stop = 2; text = "x" }; { Mend_edit.start = 1; stop = 3; text = "y" } ])) "overlap";
        contains "and so is one outside the text" (is_error "outside" (Mend_edit.apply text [ { Mend_edit.start = 0; stop = 99; text = "" } ])) "outside";
        (* A name is found as a whole name only. *)
        let line = "exact foo_bar (foo x) Nat.foo foo." in
        check "not inside a longer name" (Mend_edit.find_word line "foo" ~from:0 ~upto:(String.length line) = Some 15);
        check "not after a namespace" (Mend_edit.find_word line "foo" ~from:16 ~upto:(String.length line) = Some 30);
        check "within its stretch" (Mend_edit.find_word line "foo" ~from:0 ~upto:14 = None);
        equal_int "lines that differ, counted on both sides" 2 (Mend_edit.changed_lines "by\n  simp\n  omega" "by\n  simp\n  grind");
        equal_int "a line taken out" 1 (Mend_edit.changed_lines "by\n  simp\n  omega" "by\n  omega");
        equal_int "the same text" 0 (Mend_edit.changed_lines "by\n  omega" "by\n    omega"));
    case "P17-02-02 a name that is gone is replaced from the table of renames" (fun () ->
        let b = List.hd (breakages_of lean_of_upgrade upgrade) in
        (match Mend_rename.rule.Mend_rule.propose (context ~renames:table upgrade) b with
         | [ edit ] ->
           equal_string "the rule" "rename" edit.Mend_edit.rule;
           equal_string "what it does" "rename Nat.add_commutes to Nat.add_comm" edit.Mend_edit.description;
           equal_string "what the guard is shown" "Nat.add_comm" edit.Mend_edit.untrusted;
           contains "the edit" (is_ok "apply" (Mend_edit.apply upgrade edit.Mend_edit.replacements)) "  exact Nat.add_comm a b\n"
         | _ -> fail "one edit was expected");
        check "the rule has priority 10" (Mend_rename.rule.Mend_rule.priority = 10);
        check "a name the table does not hold is left" (Mend_rename.rule.Mend_rule.propose (context ~renames:[ ("Nat.other", "Nat.x") ] upgrade) b = []);
        (* Lean prints a name in full; the file may write it short inside its namespace. *)
        let short = "namespace Nat\ntheorem t (a b : Nat) : a + b = b + a := by\n  exact add_commutes a b\nend Nat\n" in
        let found = breakages_of (objecting [ ("add_commutes", "error", "Unknown constant `Nat.add_commutes`") ]) short in
        (match Mend_rename.rule.Mend_rule.propose (context ~renames:table short) (List.hd found) with
         | [ edit ] -> contains "the short name is replaced" (is_ok "apply" (Mend_edit.apply short edit.Mend_edit.replacements)) "  exact Nat.add_comm a b\n"
         | _ -> fail "one edit was expected");
        (* In a statement Lean points at the application, and the name is found inside it. *)
        let statement = "theorem t (n : Nat) : twoce n = n + n := rfl\n" in
        let found = breakages_of (objecting [ ("twoce n", "error", "Function expected at\n  twoce\n\nHint: The identifier `twoce` is unknown, and") ]) statement in
        (match Mend_rename.rule.Mend_rule.propose (context ~renames:[ ("twoce", "twice") ] statement) (List.hd found) with
         | [ edit ] -> equal_string "the name in the statement" "theorem t (n : Nat) : twice n = n + n := rfl\n" (is_ok "apply" (Mend_edit.apply statement edit.Mend_edit.replacements))
         | _ -> fail "one edit was expected"));
    case "P17-02-02 a deprecated name is replaced by the successor Lean names" (fun () ->
        let b = List.nth (breakages_of lean_of_upgrade upgrade) 1 in
        (match Mend_rename.rule.Mend_rule.propose (context upgrade) b with
         | [ edit ] ->
           equal_string "what it does" "rename twice_is_double to twice_eq" edit.Mend_edit.description;
           contains "the edit" (is_ok "apply" (Mend_edit.apply upgrade edit.Mend_edit.replacements)) "  exact twice_eq n\n"
         | _ -> fail "one edit was expected");
        check "Lean's successor" (Mend_rename.replacement [] (Mend_class.Deprecated { old_name = "a"; new_name = "b" }) = Some "b");
        check "the table's, when Lean names none" (Mend_rename.replacement [ ("a", "c") ] (Mend_class.Deprecated { old_name = "a"; new_name = "" }) = Some "c");
        check "nothing for another class" (Mend_rename.replacement [ ("a", "c") ] Mend_class.Unsolved_goals = None);
        (* Two names of one theorem are replaced in one edit. *)
        let two = "theorem t (a b : Nat) : a + b = b + a := by\n  rw [Nat.old_one, Nat.old_two]\n" in
        let lean = objecting [ ("Nat.old_one", "error", "Unknown constant `Nat.old_one`"); ("Nat.old_two", "error", "Unknown constant `Nat.old_two`") ] in
        (match Mend_rename.rule.Mend_rule.propose (context ~renames:[ ("Nat.old_one", "Nat.one"); ("Nat.old_two", "Nat.two") ] two) (List.hd (breakages_of lean two)) with
         | [ edit ] ->
           equal_int "two replacements" 2 (List.length edit.Mend_edit.replacements);
           contains "both made" (is_ok "apply" (Mend_edit.apply two edit.Mend_edit.replacements)) "rw [Nat.one, Nat.two]"
         | _ -> fail "one edit was expected"));
    case "P17-02-02 a table of renames is read from TOML, and a table that contradicts itself is refused" (fun () ->
        check "old names and new" (Mend_rename.parse_table "[renames]\n\"Nat.add_commutes\" = \"Nat.add_comm\"\ntwoce = \"twice\"\n" = Ok [ ("Nat.add_commutes", "Nat.add_comm"); ("twoce", "twice") ]);
        contains "no table" (is_error "none" (Mend_rename.parse_table "[other]\na = \"b\"\n")) "no table [renames]";
        contains "to itself" (is_error "self" (Mend_rename.parse_table "[renames]\na = \"a\"\n")) "renames a to itself";
        contains "to nothing" (is_error "empty" (Mend_rename.parse_table "[renames]\na = \"\"\n")) "renames a to nothing";
        contains "not a text" (is_error "number" (Mend_rename.parse_table "[renames]\na = 3\n")) "must be a text";
        contains "not TOML" (is_error "broken" (Mend_rename.parse_table "[renames\n")) "is not TOML";
        let kept = is_ok "the table the repository keeps" (Mend_rename.load_table (Support.path "fixtures/mend/renames.toml")) in
        check "it renames the name of the demonstration" (List.assoc_opt "Nat.add_commutes" kept = Some "Nat.add_comm");
        contains "a file that is not there" (is_error "absent" (Mend_rename.load_table "/nowhere/renames.toml")) "nowhere");
    case "P17-02-04 what Lean suggests is written in place of the call that asked" (fun () ->
        let b = List.nth (breakages_of lean_of_upgrade upgrade) 2 in
        (match Mend_suggest.rule.Mend_rule.propose (context upgrade) b with
         | [ edit ] ->
           equal_string "the rule" "suggestion" edit.Mend_edit.rule;
           equal_string "what it does" "write what Lean suggests: exact Nat.le_add_right n 1 for exact?" edit.Mend_edit.description;
           contains "the edit" (is_ok "apply" (Mend_edit.apply upgrade edit.Mend_edit.replacements)) "theorem explores (n : Nat) : n <= n + 1 := by\n  exact Nat.le_add_right n 1\n"
         | _ -> fail "one edit was expected");
        check "the rule has priority 20" (Mend_suggest.rule.Mend_rule.priority = 20);
        check "nothing for a breakage without a suggestion" (Mend_suggest.rule.Mend_rule.propose (context upgrade) (List.hd (breakages_of lean_of_upgrade upgrade)) = []);
        (* A suggestion of several lines keeps its shape under the column of the call. *)
        let deep = "theorem t : True \xE2\x88\xA7 True := by\n    constructor?\n" in
        let lean = objecting [ ("constructor?", "info", "Try this:\n  [apply] refine \xE2\x9F\xA8?_, ?_\xE2\x9F\xA9\n    \xC2\xB7 trivial\n    \xC2\xB7 trivial") ] in
        (match Mend_suggest.rule.Mend_rule.propose (context deep) (List.hd (breakages_of lean deep)) with
         | edit :: _ ->
           equal_string "moved in to the depth of the call"
             "theorem t : True \xE2\x88\xA7 True := by\n    refine \xE2\x9F\xA8?_, ?_\xE2\x9F\xA9\n      \xC2\xB7 trivial\n      \xC2\xB7 trivial\n"
             (is_ok "apply" (Mend_edit.apply deep edit.Mend_edit.replacements))
         | [] -> fail "an edit was expected");
        equal_string "later lines are moved in" "a\n    b" (Mend_suggest.indent_to 2 "a\n  b");
        (* When Lean gives a choice, each is an edit of its own, the first first. *)
        let choice = "theorem t (h : a = b) : b = a := by\n  exact?\n" in
        let lean = objecting [ ("exact?", "info", "Try these:\n  [apply] exact h\n  [apply] exact h.symm") ] in
        let edits = Mend_suggest.rule.Mend_rule.propose (context choice) (List.hd (breakages_of lean choice)) in
        equal_strings "one edit for each choice"
          [ "theorem t (h : a = b) : b = a := by\n  exact h\n"; "theorem t (h : a = b) : b = a := by\n  exact h.symm\n" ]
          (List.map (fun (e : Mend_edit.t) -> is_ok "apply" (Mend_edit.apply choice e.Mend_edit.replacements)) edits));
    case "P17-02-05 the registry holds the rules in the order they are tried" (fun () ->
        let rule name priority = { Mend_rule.name; priority; summary = ""; propose = (fun _ _ -> [ { Mend_edit.rule = name; description = name; replacements = []; untrusted = "" } ]) } in
        let registry = Mend_rule.registry [ rule "late" 30; rule "early" 5; rule "also-early" 5; rule "middle" 10 ] in
        equal_strings "by priority, and by name within one" [ "also-early"; "early"; "middle"; "late" ] (Mend_rule.names registry);
        check "found by name" (Option.map (fun (r : Mend_rule.t) -> r.Mend_rule.priority) (Mend_rule.find registry "middle") = Some 10);
        check "a name that is not there" (Mend_rule.find registry "absent" = None);
        (* A name is held once: a later rule of the same name replaces the earlier. *)
        let replaced = Mend_rule.registry [ rule "a" 10; rule "b" 20; rule "a" 30 ] in
        equal_strings "replaced, and in its new place" [ "b"; "a" ] (Mend_rule.names replaced);
        equal_strings "one more rule, in its place" [ "first"; "b"; "a" ] (Mend_rule.names (Mend_rule.add replaced (rule "first" 1)));
        equal_strings "one rule less" [ "a" ] (Mend_rule.names (Mend_rule.without replaced "b"));
        equal_strings "the rules of a repair" [ "rename"; "suggestion" ] (Mend_rule.names Mend_apply.default_rules);
        let b = List.hd (breakages_of lean_of_upgrade upgrade) in
        equal_strings "every edit, in the order of the rules" [ "also-early"; "early"; "middle"; "late" ]
          (List.map (fun (e : Mend_edit.t) -> e.Mend_edit.rule) (Mend_rule.proposals registry (context upgrade) b));
        check "an empty registry proposes nothing" (Mend_rule.proposals (Mend_rule.registry []) (context upgrade) b = []));
    case "P17-02-03 an edit is kept only if Lean no longer reports the breakage, and the loop goes on from there" (fun () ->
        let outcome = is_ok "run" (Mend_apply.run ~renames:table ~check:lean_of_upgrade ~file:"Upgrade.lean" upgrade) in
        equal_strings "three repairs, in the order of the file" [ "sum_comm"; "uses_old"; "explores" ]
          (List.map (fun (r : Mend_apply.repair) -> r.Mend_apply.breakage.Mend_breakage.declaration) outcome.Mend_apply.repairs);
        equal_strings "each by its rule" [ "rename"; "rename"; "suggestion" ] (List.map (fun (r : Mend_apply.repair) -> r.Mend_apply.edit.Mend_edit.rule) outcome.Mend_apply.repairs);
        check "nothing is left" (outcome.Mend_apply.remaining = []);
        contains "the file after" outcome.Mend_apply.source "exact Nat.add_comm a b";
        contains "all of it" outcome.Mend_apply.source "exact twice_eq n";
        contains "to the last" outcome.Mend_apply.source "exact Nat.le_add_right n 1";
        equal_int "one check to begin with and one for each edit" 4 outcome.Mend_apply.checks;
        let first = List.hd outcome.Mend_apply.repairs in
        equal_string "the theorem after its repair" "/-- Commutes. -/\ntheorem sum_comm (a b : Nat) : a + b = b + a := by\n  exact Nat.add_comm a b" first.Mend_apply.after;
        equal_int "one edit was enough" 1 first.Mend_apply.tried;
        (* Without the table the name stays broken, and the rest is repaired all the same. *)
        let partial = is_ok "run" (Mend_apply.run ~check:lean_of_upgrade ~file:"Upgrade.lean" upgrade) in
        equal_int "two repairs" 2 (List.length partial.Mend_apply.repairs);
        equal_strings "one breakage left" [ "sum_comm" ] (List.map (fun (b : Mend_breakage.t) -> b.Mend_breakage.declaration) partial.Mend_apply.remaining);
        contains "its line untouched" partial.Mend_apply.source "exact Nat.add_commutes a b");
    case "P17-02-03 an edit Lean does not accept is undone, and the next is tried" (fun () ->
        (* The table renames to a name that does not exist either; a second rule knows the right one. *)
        let lean = objecting [ ("Nat.add_commutes", "error", "Unknown constant `Nat.add_commutes`"); ("Nat.add_swap", "error", "Unknown constant `Nat.add_swap`") ] in
        let fallback =
          { Mend_rule.name = "second-opinion"; priority = 15; summary = "";
            propose =
              (fun (c : Mend_rule.context) _ ->
                match Text.find_sub c.Mend_rule.source "Nat.add_commutes" with
                | Some start -> [ { Mend_edit.rule = "second-opinion"; description = "use Nat.add_comm"; replacements = [ { Mend_edit.start; stop = start + 16; text = "Nat.add_comm" } ]; untrusted = "Nat.add_comm" } ]
                | None -> []) }
        in
        let outcome =
          is_ok "run"
            (Mend_apply.run ~rules:(Mend_rule.add Mend_apply.default_rules fallback) ~renames:[ ("Nat.add_commutes", "Nat.add_swap") ] ~check:lean ~file:"f" upgrade)
        in
        (match outcome.Mend_apply.repairs with
         | [ repair ] ->
           equal_string "the edit that compiled" "second-opinion" repair.Mend_apply.edit.Mend_edit.rule;
           equal_int "after one that did not" 2 repair.Mend_apply.tried
         | _ -> fail "one repair was expected");
        check "the name that did not exist is not in the file" (not (Text.contains outcome.Mend_apply.source "Nat.add_swap"));
        (* An edit that removes one error and brings two is not progress. *)
        let worse = objecting [ ("Nat.add_commutes", "error", "Unknown constant `Nat.add_commutes`"); ("Nat.add_comm", "error", "Type mismatch"); ("a b\n", "error", "unsolved goals") ] in
        let stuck = is_ok "run" (Mend_apply.run ~renames:table ~check:(fun source -> if Text.contains source "Nat.add_commutes" then objecting [ ("Nat.add_commutes", "error", "Unknown constant `Nat.add_commutes`") ] source else worse source) ~file:"f" upgrade) in
        check "it is not kept" (stuck.Mend_apply.repairs = [] && stuck.Mend_apply.source = upgrade);
        (* Text that the guard refuses never reaches Lean. *)
        let hostile = is_ok "run" (Mend_apply.run ~renames:[ ("Nat.add_commutes", "(by run_cmd IO.println 1)") ] ~check:lean_of_upgrade ~file:"f" upgrade) in
        check "a new name that is a program is not written" (not (Text.contains hostile.Mend_apply.source "run_cmd"));
        check "the guard lets a name through" (Mend_apply.admits "Nat.add_comm");
        check "and stops a command" (not (Mend_apply.admits "#eval 1"));
        (* The loop is bounded. *)
        let bounded = is_ok "run" (Mend_apply.run ~max_steps:1 ~renames:table ~check:lean_of_upgrade ~file:"f" upgrade) in
        equal_int "one step, one repair" 1 (List.length bounded.Mend_apply.repairs);
        equal_int "the rest is left" 2 (List.length bounded.Mend_apply.remaining);
        equal_int "fifty steps at the most" 50 Mend_apply.max_steps) ]

(* ---- the agent ---- *)

let theorem_source = "def twice (n : Nat) : Nat := n + n\n\ntheorem twice_add (m n : Nat) : twice (m + n) = twice m + twice n := by\n  simp [twice]\n  simp\n"
let twice_add = match Lean_source.find theorem_source "twice_add" with Ok d -> d | Error e -> failwith e
let stated = "\xE2\x88\x80 (m n : Nat), twice (m + n) = twice m + twice n"

let verdict ?(accepted = true) ?(statement = stated) ?(axioms = [ "propext" ]) ?(uses_sorry = false) ?(error = "") () =
  { Mend_check.accepted; rejection = ""; error; statement; axioms; uses_sorry }

(* A Lean for one theorem: the open theorem states what it states; a proof is accepted when it
   ends in omega. Every text it is given is written down. *)
let judging ?(accepts = fun text -> Text.contains text "omega") seen : Mend_check.t =
 fun ~untrusted text ->
  seen := (untrusted, text) :: !seen;
  if Text.contains text ":= sorry" then verdict ~uses_sorry:true ~axioms:[ "sorryAx" ] ()
  else if accepts text then verdict ()
  else verdict ~accepted:false ~error:"3:2: unsolved goals\n\xE2\x8A\xA2 m + n + (m + n) = m + m + (n + n)" ()

(* A model that gives its answers in order, and writes down what it was shown. *)
let answering answers shown : Mend_agent.ask =
 fun ~round ~failures ->
  shown := (round, failures) :: !shown;
  match List.nth_opt answers (round - 1) with Some (Ok proofs) -> Ok proofs | Some (Error e) -> Error e | None -> Ok []

let one_step = "by\n  simp [twice]\n  omega"
let rewritten = "by\n  unfold twice\n  omega"

let agent_tests =
  [ case "P17-03-01 the prompt shows the broken proof, what Lean said and the class, and asks for the smallest change" (fun () ->
        let lean = objecting [ ("  simp\n", "error", "`simp` made no progress") ] in
        let b = List.hd (breakages_of ~file:"T.lean" lean theorem_source) in
        let bindings = Mend_prompt.bindings ~count:3 ~decl:twice_add ~breakage:b ~failures:[] in
        equal_string "the theorem as it stands" "theorem twice_add (m n : Nat) : twice (m + n) = twice m + twice n := by\n  simp [twice]\n  simp" (List.assoc "theorem" bindings);
        equal_string "the class" "unsolved-goals" (List.assoc "class" bindings);
        equal_string "the error, with its line counted from the theorem" "line 3: `simp` made no progress" (List.assoc "errors" bindings);
        equal_string "what stands before" "def twice (n : Nat) : Nat := n + n" (List.assoc "context" bindings);
        equal_string "no attempt yet" "(none)" (List.assoc "attempts" bindings);
        equal_string "how many" "3" (List.assoc "count" bindings);
        let later = Mend_prompt.bindings ~count:3 ~decl:twice_add ~breakage:b ~failures:[ ("by simp", "unsolved goals"); ("by rfl", "The rfl tactic failed") ] in
        equal_string "attempts, each with what Lean said" "by simp\n-- Lean: unsolved goals\n\nby rfl\n-- Lean: The rfl tactic failed" (List.assoc "attempts" later);
        (* The prompt that ships takes exactly these values. *)
        let prompt = is_ok "load" (Prompt.load Mend_prompt.name) in
        let system, user = is_ok "render" (Prompt.render prompt later) in
        contains "the task" system "with the smallest change that makes it compile again";
        contains "the statement is not the model's to change" system "The statement is not yours to change";
        contains "what the model may not write" system "no `sorry`";
        contains "the answer" system "\"candidates\"";
        contains "the proof in the data" user "<data>\ntheorem twice_add";
        contains "the class in the request" user "Kind of breakage: unsolved-goals";
        contains "the attempts" user "by rfl\n-- Lean: The rfl tactic failed";
        equal_string "the name of the prompt" "mend.repair" Mend_prompt.name;
        check "it is one of the prompts that ship" (List.mem "mend.repair" Prompt.names));
    case "P17-03-01 the model is asked through the gateway, and its answer is read as proofs" (fun () ->
        let handler, seen = Scripted.sequence [ Ok (Scripted.answer {|{"candidates": ["by\n  simp [twice]\n  omega", "by omega", "by simp", "by rfl"]}|}) ] in
        let gateway = match Gateway.create ~scripted:handler (Maithic_config.Config.of_defaults ()) with Ok g -> g | Error f -> fail (Maithic_schema.Fault.to_string f) in
        let b = List.hd (breakages_of (objecting [ ("  simp\n", "error", "`simp` made no progress") ]) theorem_source) in
        let proofs = is_ok "ask" (Mend_prompt.ask gateway ~max_tokens:512 ~count:3 ~decl:twice_add ~breakage:b ~failures:[]) in
        equal_strings "at most as many proofs as were asked for" [ one_step; "by omega"; "by simp" ] proofs;
        (match seen () with
         | [ (request : Gateway_types.request) ] ->
           equal_string "the purpose of the call" "mend.repair" request.Gateway_types.purpose;
           equal_int "the limit on the answer" 512 request.Gateway_types.max_tokens
         | _ -> fail "one request was expected"));
    case "P17-03-02 each proof a model proposes is compiled, and the model is asked again with what Lean said" (fun () ->
        let seen = ref [] and shown = ref [] in
        let ask = answering [ Ok [ "by simp"; "by rfl" ]; Ok [ one_step ] ] shown in
        (match Mend_agent.repair ~ask ~check:(judging seen) Mend_agent.default_settings twice_add with
         | Mend_agent.Fixed { stage; proof; round; changed_lines; attempts; axioms } ->
           check "by the model" (stage = Mend_agent.Model);
           equal_string "the proof" one_step proof;
           equal_int "from the second round" 2 round;
           equal_int "one line of the proof changed" 2 changed_lines;
           equal_strings "the axioms of the repaired theorem" [ "propext" ] axioms;
           equal_strings "every proof that was tried" [ "by simp"; "by rfl"; one_step ] (List.map (fun (a : Mend_agent.attempt) -> a.Mend_agent.proof) attempts);
           equal_ints "with its round" [ 1; 1; 2 ] (List.map (fun (a : Mend_agent.attempt) -> a.Mend_agent.round) attempts);
           check "and whether Lean accepted it" (List.map (fun (a : Mend_agent.attempt) -> a.Mend_agent.accepted) attempts = [ false; false; true ])
         | Mend_agent.Unfixed { reason; _ } -> fail reason);
        (* The second round was shown the proofs of the first with Lean's words. *)
        (match List.rev !shown with
         | [ (1, []); (2, failures) ] ->
           equal_strings "the proofs that failed" [ "by simp"; "by rfl" ] (List.map fst failures);
           contains "what Lean said" (snd (List.hd failures)) "unsolved goals"
         | _ -> fail "two rounds were expected");
        (* Lean saw the open theorem once, and every proof in the place of the old one. *)
        let texts = List.rev_map snd !seen in
        equal_int "four elaborations" 4 (List.length texts);
        contains "the theorem with the proof in it" (List.nth texts 3) "theorem twice_add (m n : Nat) : twice (m + n) = twice m + twice n :=\n  by\n    simp [twice]\n    omega";
        check "the proofs are what the guard is shown" (List.rev_map fst !seen = [ ""; "by simp"; "by rfl"; one_step ]));
    case "P17-03-02 of the proofs Lean accepts, the smallest change is kept" (fun () ->
        let seen = ref [] and shown = ref [] in
        (match Mend_agent.repair ~ask:(answering [ Ok [ rewritten; one_step; "by omega" ] ] shown) ~check:(judging seen) Mend_agent.default_settings twice_add with
         | Mend_agent.Fixed { proof; changed_lines; round; _ } ->
           equal_string "the proof with one step changed, though it came second" one_step proof;
           equal_int "two lines differ" 2 changed_lines;
           equal_int "in the first round" 1 round
         | Mend_agent.Unfixed { reason; _ } -> fail reason);
        (* A proof that comes twice is compiled once, and no more are taken than were asked for. *)
        let seen = ref [] in
        let settings = { Mend_agent.rounds = 1; candidates = 2 } in
        (match Mend_agent.repair ~ask:(answering [ Ok [ "by simp"; "by simp"; "  by simp  "; one_step ] ] (ref [])) ~check:(judging seen) settings twice_add with
         | Mend_agent.Unfixed { attempts; _ } -> equal_int "one attempt" 1 (List.length attempts)
         | Mend_agent.Fixed _ -> fail "the proof that works was the fourth, and two were asked for");
        equal_int "the open theorem and one proof" 2 (List.length !seen));
    case "P17-03-02 the loop is bounded, and a model that cannot be asked ends it" (fun () ->
        let shown = ref [] in
        (match Mend_agent.repair ~ask:(answering [ Ok [ "by simp" ]; Ok [ "by rfl" ]; Ok [ one_step ] ] shown) ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; attempts } ->
           equal_string "why" "no repair after 2 round(s) with the model" reason;
           equal_int "two attempts" 2 (List.length attempts)
         | Mend_agent.Fixed _ -> fail "the third round should never be asked for");
        equal_int "two rounds" 2 (List.length !shown);
        (match Mend_agent.repair ~ask:(answering [ Error "replay mode has no cassette for this request" ] (ref [])) ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; attempts } ->
           equal_string "why" "the model was not asked: replay mode has no cassette for this request" reason;
           check "nothing was tried" (attempts = [])
         | Mend_agent.Fixed _ -> fail "no model, no repair");
        (match Mend_agent.repair ~ask:(answering [ Ok [] ] (ref [])) ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; _ } -> contains "a model with nothing to propose" reason "had no repair to propose in round 1"
         | Mend_agent.Fixed _ -> fail "nothing was proposed");
        (* A proof the guard refuses is an attempt that Lean never sees. *)
        let seen = ref [] in
        (match Mend_agent.repair ~ask:(answering [ Ok [ "by\n  #eval 1\n  omega" ] ] (ref [])) ~check:(judging seen) { Mend_agent.rounds = 1; candidates = 3 } twice_add with
         | Mend_agent.Unfixed { attempts = [ attempt ]; _ } -> contains "refused" attempt.Mend_agent.said "refused by the guard"
         | _ -> fail "one refused attempt was expected");
        equal_int "only the open theorem reached Lean" 1 (List.length !seen);
        check "two rounds and three proofs by default" (Mend_agent.default_settings = { Mend_agent.rounds = 2; candidates = 3 }));
    case "P17-03-03 an edit that changes what the theorem states is rejected" (fun () ->
        (* A Lean that accepts the proof and reports another statement: the proof proves something else. *)
        let other : Mend_check.t =
         fun ~untrusted:_ text -> if Text.contains text ":= sorry" then verdict ~uses_sorry:true () else verdict ~statement:"\xE2\x88\x80 (m n : Nat), True" ()
        in
        (match Mend_agent.repair ~ask:(answering [ Ok [ one_step ] ] (ref [])) ~check:other { Mend_agent.rounds = 1; candidates = 3 } twice_add with
         | Mend_agent.Unfixed { attempts = [ attempt ]; _ } ->
           check "not accepted" (not attempt.Mend_agent.accepted);
           contains "why" attempt.Mend_agent.said "the edit changes the statement, from \xE2\x88\x80 (m n : Nat), twice (m + n) = twice m + twice n to \xE2\x88\x80 (m n : Nat), True"
         | _ -> fail "one rejected attempt was expected");
        (* A proof that rests on sorry is no proof. *)
        let lazy_ : Mend_check.t = fun ~untrusted:_ _ -> verdict ~uses_sorry:true () in
        (match Mend_agent.repair ~ask:(answering [ Ok [ one_step ] ] (ref [])) ~check:lazy_ { Mend_agent.rounds = 1; candidates = 3 } twice_add with
         | Mend_agent.Unfixed { attempts = [ attempt ]; _ } -> equal_string "why" "the proof rests on sorry" attempt.Mend_agent.said
         | _ -> fail "one rejected attempt was expected");
        (* A statement that does not elaborate cannot be repaired through its proof, and the model is not asked. *)
        let shown = ref [] in
        let broken : Mend_check.t = fun ~untrusted:_ _ -> Mend_check.failed "2:10: Unknown identifier `twoce`" in
        (match Mend_agent.repair ~ask:(answering [ Ok [ one_step ] ] shown) ~check:broken Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; attempts } ->
           contains "why" reason "the statement does not elaborate, and a change to the proof cannot repair that: 2:10: Unknown identifier `twoce`";
           check "nothing was tried" (attempts = [])
         | Mend_agent.Fixed _ -> fail "a broken statement is not repaired by a proof");
        check "the model was not asked" (!shown = []);
        check "what the theorem states" ((is_ok "reference" (Mend_agent.reference ~check:(judging (ref [])) twice_add)).Mend_check.statement = stated);
        (* The judgement itself. *)
        let reference = verdict () in
        check "the same statement, accepted, without sorry" (Mend_check.proves ~reference (verdict ~axioms:[] ()));
        check "another statement" (not (Mend_check.proves ~reference (verdict ~statement:"True" ())));
        check "not accepted" (not (Mend_check.proves ~reference (verdict ~accepted:false ())));
        equal_string "the guard's word" "refused by the guard: forbidden keyword 'axiom'"
          (Mend_check.why_not ~reference { (Mend_check.failed "") with Mend_check.rejection = "forbidden keyword 'axiom'" });
        equal_string "Lean's word" "3:2: unsolved goals" (Mend_check.why_not ~reference (Mend_check.failed "3:2: unsolved goals")));
    case "P17-03-04 when the rounds are spent, the statement is handed to the prover" (fun () ->
        let asked = ref 0 in
        let reprove () =
          incr asked;
          Ok ("by\n  unfold twice\n  omega", "portfolio")
        in
        (match Mend_agent.mend ~ask:(answering [ Ok [ "by simp" ]; Ok [ "by rfl" ] ] (ref [])) ~reprove ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Fixed { stage; proof; round; attempts; _ } ->
           check "by the prover, with the proposer that found the proof" (stage = Mend_agent.Prove "portfolio");
           equal_string "its proof" "by\n  unfold twice\n  omega" proof;
           equal_int "which belongs to no round" 0 round;
           equal_int "after the two attempts of the model" 3 (List.length attempts)
         | Mend_agent.Unfixed { reason; _ } -> fail reason);
        equal_int "the prover was asked once" 1 !asked;
        (* A model that repairs the proof leaves the prover alone. *)
        asked := 0;
        (match Mend_agent.mend ~ask:(answering [ Ok [ one_step ] ] (ref [])) ~reprove ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Fixed { stage; _ } -> check "by the model" (stage = Mend_agent.Model)
         | Mend_agent.Unfixed { reason; _ } -> fail reason);
        equal_int "the prover was not asked" 0 !asked;
        (* Without a model the prover is the first remedy. *)
        (match Mend_agent.mend ~reprove ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Fixed { stage; attempts; _ } ->
           check "by the prover" (stage = Mend_agent.Prove "portfolio");
           equal_int "one proof was tried" 1 (List.length attempts)
         | Mend_agent.Unfixed { reason; _ } -> fail reason);
        (* What the prover returns is checked like any proof. *)
        (match Mend_agent.mend ~reprove:(fun () -> Ok ("by simp", "search")) ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; _ } -> contains "why" reason "the proof of the prover was not accepted in the file"
         | Mend_agent.Fixed _ -> fail "a proof Lean does not accept is no repair");
        (match Mend_agent.mend ~ask:(answering [ Ok [ "by simp" ]; Ok [] ] (ref [])) ~reprove:(fun () -> Error "its automation found no proof") ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; attempts } ->
           contains "both reasons" reason "the prover did not prove the statement either: its automation found no proof";
           equal_int "the model's attempt is kept" 1 (List.length attempts)
         | Mend_agent.Fixed _ -> fail "nothing proved it");
        (match Mend_agent.mend ~check:(judging (ref [])) Mend_agent.default_settings twice_add with
         | Mend_agent.Unfixed { reason; _ } -> equal_string "with neither" "no model was asked" reason
         | Mend_agent.Fixed _ -> fail "nothing was asked");
        equal_string "the stage of a model" "model" (Mend_agent.stage_name Mend_agent.Model);
        equal_string "the stage of the prover" "prove" (Mend_agent.stage_name (Mend_agent.Prove "search"))) ]

(* ---- the whole repair ---- *)

let repair_tests =
  let broken =
    upgrade ^ "\ntheorem closes (a b : Nat) (h : a <= b) : a < b + 1 := by\n  simp\n\ntheorem hopeless (n : Nat) : n < n := by\n  simp\n\ndef uses (n : Nat) : Nat := Nat.gone n\n"
  in
  let lean =
    objecting
      [ ("Nat.add_commutes", "error", "Unknown constant `Nat.add_commutes`"); ("twice_is_double", "warning", "`twice_is_double` has been deprecated: Use `twice_eq` instead");
        ("exact?", "info", "Try this:\n  [apply] exact Nat.le_add_right n 1"); (":= by\n  simp\n", "error", "`simp` made no progress"); ("Nat.gone", "error", "Unknown constant `Nat.gone`") ]
  in
  let tools ?(ask = true) ?(reprove = true) () =
    { Mend_repair.check_file = lean;
      check_theorem =
        (fun _ decl ~untrusted:_ text ->
          if Text.contains text ":= sorry" then verdict ~statement:decl.Lean_source.name ~uses_sorry:true ()
          else if Text.contains text "omega" && decl.Lean_source.name = "closes" then verdict ~statement:decl.Lean_source.name ()
          else verdict ~statement:decl.Lean_source.name ~accepted:false ~error:"unsolved goals" ());
      ask = (if ask then Some (fun ~decl:_ ~breakage:_ ~round:_ ~failures:_ -> Ok [ "by\n  omega" ]) else None);
      reprove = (if reprove then Some (fun ~source:_ (decl : Lean_source.decl) () -> if decl.Lean_source.name = "closes" then Ok ("by omega", "portfolio") else Error "its automation found no proof") else None) }
  in
  [ case "P17-03-04 a file is repaired from the cheapest remedy to the dearest, and the last check is Lean's" (fun () ->
        let outcome = is_ok "run" (Mend_repair.run ~renames:table (tools ()) ~file:"Upgrade.lean" broken) in
        let by = List.map (fun (e : Mend_repair.entry) -> (e.Mend_repair.breakage.Mend_breakage.declaration, Mend_repair.outcome_stage e.Mend_repair.outcome)) outcome.Mend_repair.entries in
        check "each breakage with the remedy that worked"
          (by = [ ("sum_comm", "rule"); ("uses_old", "rule"); ("explores", "rule"); ("closes", "model"); ("hopeless", "none"); ("", "none") ]);
        contains "the model's proof is in the file, in the file's own style" outcome.Mend_repair.source "theorem closes (a b : Nat) (h : a <= b) : a < b + 1 := by\n  omega\n";
        check "what cannot be repaired is left as it was" (Text.contains outcome.Mend_repair.source "theorem hopeless (n : Nat) : n < n := by\n  simp\n");
        equal_int "two breakages remain" 2 (List.length outcome.Mend_repair.remaining);
        check "the file is not clean" (not (Mend_repair.clean outcome));
        (match List.nth outcome.Mend_repair.entries 4, List.nth outcome.Mend_repair.entries 5 with
         | { Mend_repair.outcome = Mend_repair.Unrepaired why; tried; _ }, { Mend_repair.outcome = Mend_repair.Unrepaired outside; _ } ->
           contains "why the theorem was left" why "no repair after 2 round(s) with the model";
           contains "and that the prover was tried" why "the prover did not prove the statement either";
           equal_int "the proof the model offered was tried once" 1 tried;
           contains "why the definition was left" outside "outside every theorem"
         | _ -> fail "two breakages should be left");
        (* Without a model the prover is tried, and without either the proofs are left. *)
        let no_model = is_ok "run" (Mend_repair.run ~renames:table (tools ~ask:false ()) ~file:"f" broken) in
        check "the prover's proof" (List.exists (fun (e : Mend_repair.entry) -> e.Mend_repair.outcome = Mend_repair.By_prover "portfolio") no_model.Mend_repair.entries);
        let rules_only = is_ok "run" (Mend_repair.run ~renames:table (tools ~ask:false ~reprove:false ()) ~file:"f" broken) in
        equal_int "three repairs by rules" 3 (List.length (List.filter Mend_repair.repaired rules_only.Mend_repair.entries));
        equal_int "three breakages left" 3 (List.length rules_only.Mend_repair.remaining));
    case "P17-03-04 a repair the last check does not bear out is not counted" (fun () ->
        (* The check of the theorem accepts a proof that the check of the file still objects to. *)
        let source = "theorem closes (a b : Nat) (h : a <= b) : a < b + 1 := by\n  simp\n" in
        let stubborn = objecting [ ("theorem closes", "error", "unsolved goals") ] in
        let tools =
          { Mend_repair.check_file = stubborn; check_theorem = (fun _ _ ~untrusted:_ text -> if Text.contains text ":= sorry" then verdict ~uses_sorry:true () else verdict ());
            ask = Some (fun ~decl:_ ~breakage:_ ~round:_ ~failures:_ -> Ok [ "by omega" ]); reprove = None }
        in
        let outcome = is_ok "run" (Mend_repair.run tools ~file:"f" source) in
        (match outcome.Mend_repair.entries with
         | [ { Mend_repair.outcome = Mend_repair.Unrepaired why; _ } ] -> contains "why" why "Lean still reports the breakage when the whole file is checked"
         | _ -> fail "the repair should not be counted");
        check "not clean" (not (Mend_repair.clean outcome)));
    case "P17-03-04 a theorem is checked with the broken proofs around it left open" (fun () ->
        let source = "theorem a : 1 = 1 := by\n  broken\n\ntheorem b : 2 = 2 := by\n  broken too\n\ntheorem c : 3 = 3 := rfl\n" in
        equal_string "every broken proof but the one in hand" "theorem a : 1 = 1 := sorry\n\ntheorem b : 2 = 2 := by\n  broken too\n\ntheorem c : 3 = 3 := rfl\n"
          (Mend_repair.sanitize source ~except:"b" [ "a"; "b" ]);
        equal_string "a name that is not in the file changes nothing" source (Mend_repair.sanitize source ~except:"" [ "z" ]);
        (* The check of a theorem is given the file with its neighbours open. *)
        let given = ref "" in
        let lean = objecting [ ("broken", "error", "unknown tactic") ] in
        let tools =
          { Mend_repair.check_file = lean;
            check_theorem =
              (fun around _ ~untrusted:_ text ->
                given := around;
                if Text.contains text ":= sorry" then verdict ~uses_sorry:true () else verdict ~accepted:false ~error:"no" ());
            ask = Some (fun ~decl:_ ~breakage:_ ~round:_ ~failures:_ -> Ok [ "by rfl" ]); reprove = None }
        in
        ignore (is_ok "run" (Mend_repair.run ~settings:{ Mend_agent.rounds = 1; candidates = 1 } tools ~file:"f" source));
        contains "the last theorem was checked with the first one open" !given "theorem a : 1 = 1 := sorry");
    case "P17-03-04 the report says what was done about each breakage" (fun () ->
        let outcome = is_ok "run" (Mend_repair.run ~renames:table (tools ()) ~file:"Upgrade.lean" broken) in
        let j = Mend_repair.to_json outcome in
        equal_string "the schema" "maithic.mend.report/1" (Json.string_or "" "schema" j);
        let summary = Option.get (Json.member "summary" j) in
        equal_ints "found, repaired, left, by a rule, by a model, by the prover" [ 6; 4; 2; 3; 1; 0 ]
          (List.map (fun k -> Json.int_or (-1) k summary) [ "breakages"; "repaired"; "unrepaired"; "by_rule"; "by_model"; "by_prover" ]);
        check "not clean" (Json.member "clean" summary = Some (Json.Bool false));
        let entries = Json.list_or_empty "repairs" j in
        let first = List.hd entries in
        equal_string "the theorem" "sum_comm" (Json.string_or "" "declaration" first);
        equal_string "its class" "unknown-identifier" (Json.string_or "" "class" first);
        equal_string "what happened" "repaired" (Json.string_or "" "outcome" first);
        equal_string "at which stage" "rule" (Json.string_or "" "stage" first);
        equal_string "by which rule" "rename" (Json.string_or "" "rule" first);
        contains "the text before" (Json.string_or "" "before" first) "Nat.add_commutes";
        contains "and after" (Json.string_or "" "after" first) "Nat.add_comm a b";
        let model = List.nth entries 3 in
        equal_string "a model's repair" "model" (Json.string_or "" "stage" model);
        equal_int "with its round" 1 (Json.int_or 0 "round" model);
        let left = List.nth entries 4 in
        equal_string "what was left" "unrepaired" (Json.string_or "" "outcome" left);
        check "with the reason, and no text after" (Json.member "reason" left <> None && Json.member "after" left = None);
        equal_int "what Lean still reports" 2 (List.length (Json.list_or_empty "remaining" j));
        let text = Mend_repair.render outcome in
        contains "a repair by a rule" text "sum_comm      unknown-identifier  repaired  by the rule rename";
        contains "what Lean said" text "    Lean: Unknown constant `Nat.add_commutes`";
        contains "what was done" text "    done: rename Nat.add_commutes to Nat.add_comm";
        contains "a repair by a model" text "repaired  by a model, round 1";
        contains "the sum" text "Upgrade.lean: 6 breakage(s), 4 repaired (3 by a rule, 1 by a model, 0 by the prover), 2 left; Lean still reports 2 problem(s)";
        check "no colour unless asked" (not (Text.contains text "\027["));
        check "colour when asked" (Text.contains (Mend_repair.render ~color:true outcome) "\027[32mrepaired\027[0m");
        let healthy = is_ok "run" (Mend_repair.run (tools ()) ~file:"Fine.lean" "theorem fine (n : Nat) : n = n := rfl\n") in
        equal_string "a file with nothing broken" "Fine.lean: nothing is broken\n" (Mend_repair.render healthy);
        check "is clean" (Mend_repair.clean healthy)) ]

(* ---- shorter proofs ---- *)

let verbose =
  "def twice (n : Nat) : Nat := n + n\n\ntheorem twice_eq (n : Nat) : twice n = 2 * n := by\n  unfold twice\n  have bound : n <= n + n := by\n    omega\n  simp only [Nat.two_mul, Nat.add_zero]\n  done\n"

let twice_eq = match Lean_source.find verbose "twice_eq" with Ok d -> d | Error e -> failwith e

(* A Lean for shorter proofs: a proof stands when it unfolds the definition and uses the lemma
   that matters. Every proof it is given is written down. *)
let needing ?(needs = [ "unfold twice"; "Nat.two_mul" ]) ?(axioms = fun _ -> [ "propext" ]) given : Mend_check.t =
 fun ~untrusted:_ text ->
  given := text :: !given;
  if List.for_all (Text.contains text) needs then verdict ~statement:"\xE2\x88\x80 (n : Nat), twice n = 2 * n" ~axioms:(axioms text) ()
  else verdict ~accepted:false ~statement:"\xE2\x88\x80 (n : Nat), twice n = 2 * n" ~error:"unsolved goals" ()

let minimize_tests =
  [ case "P17-04-01 steps are taken out one at a time, and a removal is kept when the proof still compiles" (fun () ->
        let result = Mend_minimize.minimize ~check:(needing (ref [])) twice_eq in
        equal_string "the proof that is left" "by\n  unfold twice\n  simp only [Nat.two_mul]" result.Mend_minimize.after;
        equal_string "how" "deletion" result.Mend_minimize.how;
        equal_strings "what went, in the order it went"
          [ "done"; "have bound : n <= n + n := by"; "Nat.add_zero in `simp only [Nat.two_mul, Nat.add_zero]`" ]
          result.Mend_minimize.removed;
        check "it changed" (Mend_minimize.changed result);
        equal_string "the proof as it stood is kept in the result" "by\n  unfold twice\n  have bound : n <= n + n := by\n    omega\n  simp only [Nat.two_mul, Nat.add_zero]\n  done" result.Mend_minimize.before;
        (* A proof with nothing to spare stays as it is. *)
        let tight = match Lean_source.find "theorem t (n : Nat) : twice n = 2 * n := by\n  unfold twice\n  simp only [Nat.two_mul]\n" "t" with Ok d -> d | Error e -> failwith e in
        let same = Mend_minimize.minimize ~check:(needing (ref [])) tight in
        check "unchanged" ((not (Mend_minimize.changed same)) && same.Mend_minimize.after = same.Mend_minimize.before && same.Mend_minimize.how = "unchanged"));
    case "P17-04-01 the proofs one removal away: steps with what belongs to them, and arguments of lemma lists" (fun () ->
        let proof = "by\n  induction n with\n  | zero => rfl\n  | succ k ih =>\n    simp [twice, Nat.add_comm] at *\n    omega\n  done" in
        let what = List.map snd (Mend_minimize.candidates proof) in
        equal_strings "steps from the last to the first, then arguments from the last to the first"
          [ "done"; "omega"; "simp [twice, Nat.add_comm] at *"; "induction n with"; "Nat.add_comm in `simp [twice, Nat.add_comm] at *`"; "twice in `simp [twice, Nat.add_comm] at *`" ]
          what;
        (* A step goes with the deeper lines under it; an alternative of a case split is never taken out alone. *)
        let without_induction = List.assoc "induction n with" (List.map (fun (p, w) -> (w, p)) (Mend_minimize.candidates proof)) in
        equal_string "the induction with its cases" "by\n  done" without_induction;
        check "no candidate removes one case" (not (List.exists (fun w -> Text.starts_with ~prefix:"|" w) what));
        let without_argument = List.assoc "twice in `simp [twice, Nat.add_comm] at *`" (List.map (fun (p, w) -> (w, p)) (Mend_minimize.candidates proof)) in
        contains "an argument less" without_argument "    simp [Nat.add_comm] at *\n";
        (* The last lemma of a simp call goes with its brackets; a rewrite is not left without a rule. *)
        equal_strings "simp with one lemma, and a rewrite with one rule" [ "simp [twice]"; "rw [h]"; "twice in `simp [twice]`" ]
          (List.map snd (Mend_minimize.candidates "by\n  rw [h]\n  simp [twice]"));
        contains "simp without its last lemma" (fst (List.nth (Mend_minimize.candidates "by\n  rw [h]\n  simp [twice]") 2)) "\n  simp";
        equal_strings "brackets inside an argument do not split it" [ "h in `simp only [f (g [1, 2]), h]`"; "f (g [1, 2]) in `simp only [f (g [1, 2]), h]`" ]
          (List.map snd (Mend_minimize.candidates "by\n  simp only [f (g [1, 2]), h]"));
        check "a term proof has no steps to take out" (Mend_minimize.candidates "Nat.add_comm a b" = []);
        check "and a proof of one step keeps it" (Mend_minimize.candidates "by\n  omega" = []));
    case "P17-04-02 every shorter proof is compiled before it is accepted" (fun () ->
        let given = ref [] in
        let result = Mend_minimize.minimize ~check:(needing given) twice_eq in
        equal_int "the checks are counted" (List.length !given) result.Mend_minimize.checks;
        (* The proof that is left is one Lean was given and accepted, not one that was put together afterwards. *)
        check "the result was compiled" (List.exists (fun text -> Text.contains text "by\n    unfold twice\n    simp only [Nat.two_mul]\n") !given);
        check "the first check is of the proof as it stood" (Text.contains (List.nth (List.rev !given) 0) "have bound");
        (* A removal Lean rejects is not kept. *)
        let strict = Mend_minimize.minimize ~check:(needing ~needs:[ "unfold twice"; "Nat.two_mul"; "have bound"; "omega" ] (ref [])) twice_eq in
        equal_string "what Lean needs stays" "by\n  unfold twice\n  have bound : n <= n + n := by\n    omega\n  simp only [Nat.two_mul]" strict.Mend_minimize.after;
        (* The number of proofs given to Lean is bounded. *)
        let bounded = Mend_minimize.minimize ~max_checks:2 ~check:(needing (ref [])) twice_eq in
        equal_int "two checks" 2 bounded.Mend_minimize.checks;
        equal_strings "one removal" [ "done" ] bounded.Mend_minimize.removed);
    case "P17-04-03 the result gives the proof that is left, what went, and the size before and after" (fun () ->
        let result = Mend_minimize.minimize ~check:(needing (ref [])) twice_eq in
        check "lines, steps and characters before" (result.Mend_minimize.size_before = { Mend_minimize.lines = 6; steps = 5; chars = 106 });
        check "and after" (result.Mend_minimize.size_after = { Mend_minimize.lines = 3; steps = 2; chars = 43 });
        check "a by on a line of its own is a line and no step" (Mend_minimize.measure "by\n  omega" = { Mend_minimize.lines = 2; steps = 1; chars = 10 });
        check "characters are counted as written, not as stored" ((Mend_minimize.measure "by exact \xE2\x9F\xA8h\xE2\x9F\xA9").Mend_minimize.chars = 12);
        let j = Mend_minimize.to_json ~file:"Verbose.lean" [ result ] in
        equal_string "the schema" "maithic.mend.minimization/1" (Json.string_or "" "schema" j);
        let summary = Option.get (Json.member "summary" j) in
        equal_ints "theorems, shortened, lines before and after" [ 1; 1; 6; 3 ] (List.map (fun k -> Json.int_or (-1) k summary) [ "theorems"; "shortened"; "lines_before"; "lines_after" ]);
        equal_int "the checks of the run" result.Mend_minimize.checks (Json.int_or (-1) "checks" summary);
        let entry = List.hd (Json.list_or_empty "results" j) in
        equal_string "the theorem" "twice_eq" (Json.string_or "" "theorem" entry);
        equal_string "the proof that is left" result.Mend_minimize.after (Json.string_or "" "after" entry);
        equal_int "what went" 3 (List.length (Json.list_or_empty "removed" entry));
        equal_int "the size after" 43 (Json.int_or 0 "chars" (Option.get (Json.member "size_after" entry)));
        let text = Mend_minimize.render ~file:"Verbose.lean" [ result ] in
        contains "the theorem and its sizes" text "twice_eq  shorter  6 to 3 line(s), 106 to 43 character(s)  (deletion,";
        contains "what went" text "  - have bound : n <= n + n := by\n";
        contains "the proof that is left" text "    by\n      unfold twice\n      simp only [Nat.two_mul]\n";
        contains "the sum" text "Verbose.lean: 1 of 1 proof(s) shortened, 6 line(s) to 3, every shorter proof checked by Lean";
        check "colour when asked" (Text.contains (Mend_minimize.render ~color:true ~file:"f" [ result ]) "\027[32mshorter\027[0m"));
    case "P17-04-05 a shorter proof that states something else, or needs a new axiom, is refused" (fun () ->
        let reference = verdict ~axioms:[ "propext" ] () in
        check "the same statement on the same axioms" (Mend_minimize.keeps ~reference (verdict ~axioms:[ "propext" ] ()));
        check "fewer axioms are welcome" (Mend_minimize.keeps ~reference (verdict ~axioms:[] ()));
        check "a new axiom is not" (not (Mend_minimize.keeps ~reference (verdict ~axioms:[ "propext"; "Classical.choice" ] ())));
        check "another statement is not" (not (Mend_minimize.keeps ~reference (verdict ~statement:"True" ())));
        check "a proof that rests on sorry is not" (not (Mend_minimize.keeps ~reference (verdict ~uses_sorry:true ())));
        check "a proof Lean rejects is not" (not (Mend_minimize.keeps ~reference (verdict ~accepted:false ())));
        (* A removal that brings an axiom in is undone, though Lean accepts the proof. *)
        let classical text = if Text.contains text "have bound" then [ "propext" ] else [ "propext"; "Classical.choice" ] in
        let kept = Mend_minimize.minimize ~check:(needing ~axioms:classical (ref [])) twice_eq in
        check "the step whose removal needs the axiom stays" (Text.contains kept.Mend_minimize.after "have bound");
        equal_strings "what could go without it went" [ "done"; "Nat.add_zero in `simp only [Nat.two_mul, Nat.add_zero]`" ] kept.Mend_minimize.removed;
        equal_strings "the axioms are those of the proof as it stood" [ "propext" ] kept.Mend_minimize.axioms;
        (* One call of automation is tried on request, under the same conditions. *)
        let automatic text = Text.contains text ":=\n  by omega" || Text.contains text ":= by omega" || Text.contains text "unfold twice" in
        let by_call = Mend_minimize.minimize ~automation:true ~check:(fun ~untrusted:_ text -> if automatic text then verdict () else verdict ~accepted:false ()) twice_eq in
        equal_string "the whole proof, for one call" "by omega" by_call.Mend_minimize.after;
        equal_string "how" "automation" by_call.Mend_minimize.how;
        check "the calls that are tried" (List.mem "by omega" Mend_minimize.automation && List.mem "by simp" Mend_minimize.automation);
        let without = Mend_minimize.minimize ~check:(fun ~untrusted:_ text -> if automatic text then verdict () else verdict ~accepted:false ()) twice_eq in
        check "not unless asked" (without.Mend_minimize.how <> "automation");
        (* A call that needs axioms the proof does without is refused, and the result says so. *)
        let plain = match Lean_source.find "theorem s (a b : Nat) (h : a <= b) : a + a <= b + b := by\n  have t := h\n  exact Nat.add_le_add t t\n" "s" with Ok d -> d | Error e -> failwith e in
        let costly ~untrusted:_ text =
          if Text.contains text "by omega" then verdict ~axioms:[ "propext"; "Quot.sound" ] ()
          else if Text.contains text "have t" && Text.contains text "exact" then verdict ~axioms:[] ()
          else verdict ~accepted:false ()
        in
        let refused = Mend_minimize.minimize ~automation:true ~check:costly plain in
        check "unchanged" (not (Mend_minimize.changed refused));
        equal_string "and why" "Lean accepts a shorter proof, and it needs what this proof does without: Quot.sound, propext" refused.Mend_minimize.note;
        (* A proof that does not compile, or is not finished, is not a proof to shorten. *)
        let broken = Mend_minimize.minimize ~check:(fun ~untrusted:_ _ -> Mend_check.failed "4:2: unsolved goals") twice_eq in
        contains "a proof that does not compile" broken.Mend_minimize.note "the proof does not compile as it stands: 4:2: unsolved goals";
        equal_int "one check, and no more" 1 broken.Mend_minimize.checks;
        let unfinished = Mend_minimize.minimize ~check:(fun ~untrusted:_ _ -> verdict ~uses_sorry:true ()) twice_eq in
        equal_string "a proof that rests on sorry" "the proof is not finished: it rests on sorry" unfinished.Mend_minimize.note) ]

let () =
  run "mend"
    [ ("detect", detect_tests); ("rules", rule_tests); ("agent", agent_tests); ("repair", repair_tests); ("minimize", minimize_tests) ]
