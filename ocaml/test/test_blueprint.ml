(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of blueprints: the document model and its file format, the planner on scripted answers,
   the skeleton with a stand-in for Lean, and the status computed from what Lean would report. The
   real Lean meets them in the integration tests. *)

open Maithic_base
open Maithic_schema
open Maithic_gateway
open Maithic_blueprint
open Testkit

let node = Blueprint_node.make
let lemma ?proof ?lean ?formal ?uses ?proof_uses id = node ?proof ?lean ?formal ?uses ?proof_uses ~id ~kind:Blueprint_node.Lemma ~title:("About " ^ id) ("The claim " ^ id ^ ".")
let definition ?lean ?formal ?uses id = node ?lean ?formal ?uses ~id ~kind:Blueprint_node.Definition ~title:("The notion " ^ id) ("What " ^ id ^ " is.")
let doc ?namespace nodes = Blueprint_doc.make ?namespace ~title:"A test" nodes
let example () = is_ok "the example parses" (Blueprint_file.parse (Support.read "examples/blueprint/triangles.md"))
let ids nodes = List.map (fun (n : Blueprint_node.t) -> n.Blueprint_node.id) nodes
let problems d = List.map Blueprint_check.describe (Blueprint_check.validate d)
let ordered_ids d = match Blueprint_check.order d with Ok nodes -> ids nodes | Error cycle -> fail ("a circle: " ^ String.concat ", " cycle)

let model_tests =
  [ case "P16-01-01 a node holds what is said and what is stated, and survives JSON" (fun () ->
        let n = lemma ~proof:"By hand." ~lean:"foo_bar" ~formal:"(n : Nat) : n = n" ~uses:[ "a" ] ~proof_uses:[ "b" ] "foo" in
        equal_string "identifier" "foo" n.Blueprint_node.id;
        equal_string "statement" "The claim foo." n.Blueprint_node.statement;
        equal_strings "what it rests on, the statement first" [ "a"; "b" ] (Blueprint_node.dependencies n);
        check "the same node back" (is_ok "from JSON" (Blueprint_node.of_json (Blueprint_node.to_json n)) = n);
        check "plain identifiers" (List.for_all Blueprint_node.valid_id [ "a"; "tri-step"; "def:sum_odd"; "L2.1" ]);
        check "others are refused" (not (List.exists Blueprint_node.valid_id [ ""; "-a"; "two words"; "a`b"; String.make 65 'a' ]));
        equal_string "any text becomes an identifier" "odd-square" (Blueprint_node.slug "Odd Square!");
        equal_string "even an empty one" "node" (Blueprint_node.slug "?!"));
    case "P16-01-02 there are five kinds, and only a definition needs no proof" (fun () ->
        equal_strings "kinds" [ "definition"; "lemma"; "proposition"; "theorem"; "corollary" ] (List.map Blueprint_node.kind_name Blueprint_node.kinds);
        List.iter
          (fun kind -> check "a name leads back" (Blueprint_node.kind_of_name (String.uppercase_ascii (Blueprint_node.kind_name kind)) = Some kind))
          Blueprint_node.kinds;
        check "a conjecture is not a kind" (Blueprint_node.kind_of_name "conjecture" = None);
        equal_strings "claims" [ "lemma"; "proposition"; "theorem"; "corollary" ]
          (List.filter_map (fun kind -> if Blueprint_node.is_claim (node ~id:"x" ~kind ~title:"" "s") then Some (Blueprint_node.kind_name kind) else None) Blueprint_node.kinds);
        contains "an unknown kind in JSON is an error" (is_error "kind" (Blueprint_node.of_json (Json.Obj [ ("id", Json.String "x"); ("kind", Json.String "axiom") ]))) "not a kind");
    case "P16-01-03 an edge says whether the statement or only the proof uses a node" (fun () ->
        let d = doc [ definition "a"; definition "b"; lemma ~uses:[ "a" ] ~proof_uses:[ "a"; "b" ] "c" ] in
        let shown = List.map (fun (e : Blueprint_doc.edge) -> Printf.sprintf "%s->%s:%s" e.Blueprint_doc.source e.Blueprint_doc.target (Blueprint_doc.role_name e.Blueprint_doc.role)) (Blueprint_doc.edges d) in
        equal_strings "what the statement uses is not listed again for the proof" [ "c->a:statement"; "c->b:proof" ] shown;
        equal_strings "who uses b" [ "c" ] (ids (Blueprint_doc.dependents d "b"));
        equal_strings "nobody uses c" [] (ids (Blueprint_doc.dependents d "c"));
        equal_int "the example has thirteen edges" 13 (List.length (Blueprint_doc.edges (example ()))));
    case "P16-01-04 a blueprint is Markdown with front matter and one node for each section" (fun () ->
        let d = example () in
        equal_string "title" "Odd numbers, squares and triangles" d.Blueprint_doc.title;
        check "namespace" (d.Blueprint_doc.namespace = Some "Triangles");
        contains "the text before the first node" d.Blueprint_doc.intro "Two classical facts";
        equal_strings "nodes in the order of the file"
          [ "tri"; "sum-odd"; "tri-step"; "tri-formula"; "odd-step"; "odd-square"; "two-triangles"; "odd-triangles" ]
          (Blueprint_doc.ids d);
        let n = Option.get (Blueprint_doc.find d "two-triangles") in
        check "kind" (n.Blueprint_node.kind = Blueprint_node.Lemma);
        equal_string "title of the node" "Two triangles make a square" n.Blueprint_node.title;
        check "Lean name" (n.Blueprint_node.lean = Some "tri_add_tri_succ");
        check "Lean statement" (n.Blueprint_node.formal = Some "(n : Nat) : tri n + tri (n + 1) = (n + 1) * (n + 1)");
        equal_strings "uses" [ "tri" ] n.Blueprint_node.uses;
        equal_strings "proof uses" [ "tri-formula"; "tri-step" ] n.Blueprint_node.proof_uses;
        contains "the proof stands after its marker" n.Blueprint_node.proof "twice the n-th triangular number";
        check "the full Lean name carries the namespace" (Blueprint_doc.lean_name d n = Some "Triangles.tri_add_tri_succ");
        let tri = Option.get (Blueprint_doc.find d "tri") in
        check "a definition keeps its whole text" (tri.Blueprint_node.formal = Some "def tri : Nat → Nat\n  | 0 => 0\n  | n + 1 => tri n + (n + 1)"));
    case "P16-01-04 an error in a blueprint names the line" (fun () ->
        let head = "---\nblueprint: 1\ntitle: T\n---\n\n" in
        let refused text = is_error "refused" (Blueprint_file.parse text) in
        contains "no front matter" (refused "## Lemma `a`: A\n") "line 1";
        contains "another version" (refused "---\nblueprint: 2\ntitle: T\n---\n") "version 2";
        contains "no title" (refused "---\nblueprint: 1\n---\n") "no title";
        contains "a heading without an identifier" (refused (head ^ "## Lemma a: A\n")) "line 6";
        contains "an unknown kind" (refused (head ^ "## Conjecture `a`: A\n")) "not a kind of node";
        contains "an identifier that is not plain" (refused (head ^ "## Lemma `two words`: A\n")) "cannot be an identifier";
        contains "an unknown attribute" (refused (head ^ "## Lemma `a`: A\n\n- author: me\n")) "line 8";
        contains "a code block left open" (refused (head ^ "## Lemma `a`: A\n\nText.\n\n```lean\n(n : Nat) : n = n\n")) "not closed";
        (* a heading inside a code block is text, and a second Lean block belongs to the statement *)
        let d = is_ok "parses" (Blueprint_file.parse (head ^ "## Lemma `a`: A\n\nText.\n\n```\n## not a node\n```\n\n```lean\nP\n```\n\n```lean\nQ\n```\n")) in
        equal_int "one node" 1 (List.length d.Blueprint_doc.nodes);
        let a = List.hd d.Blueprint_doc.nodes in
        check "the first Lean block is the statement" (a.Blueprint_node.formal = Some "P");
        contains "what looks like a heading stays text" a.Blueprint_node.statement "## not a node";
        contains "and so does the second block" a.Blueprint_node.statement "Q");
    case "P16-01-05 what the printer writes, the parser reads back" (fun () ->
        let text = Support.read "fixtures/blueprint/plan.blueprint.md" in
        equal_string "a file in canonical form is printed as it stands" text (Blueprint_file.print (is_ok "parses" (Blueprint_file.parse text)));
        let d = example () in
        check "the example survives printing" (Blueprint_file.parse (Blueprint_file.print d) = Ok d);
        let bare = doc [ lemma "only" ] in
        check "a node with nothing but a statement" (Blueprint_file.parse (Blueprint_file.print bare) = Ok bare));
    (let open Gen in
     let word = string_of ~max:8 "abcdefgh" in
     let text = map (fun words -> String.concat " " (List.filter (fun w -> w <> "") words)) (list ~max:6 word) in
     let ident = map (fun (c, rest) -> String.make 1 c ^ rest) (pair (char_of "abcxyz") (string_of ~max:5 "abc-_.019")) in
     let some gen = choose [ return None; map Option.some gen ] in
     let gen_node =
       map
         (fun ((id, kind), ((title, statement), ((proof, lean), (formal, (uses, proof_uses))))) ->
           { Blueprint_node.id; kind; title; statement; proof = (if proof = "" then "" else proof); lean; formal; uses; proof_uses })
         (pair (pair ident (one_of Blueprint_node.kinds))
            (pair (pair text text)
               (pair (pair text (some ident)) (pair (some (map (fun t -> if t = "" then "x" else t) text)) (pair (list ~max:3 ident) (list ~max:3 ident))))))
     in
     let gen_doc =
       map
         (fun ((title, intro), nodes) ->
           { Blueprint_doc.title = (if title = "" then "t" else title); namespace = None; imports = []; meta = []; intro; nodes })
         (pair (pair text text) (list ~max:5 gen_node))
     in
     property ~count:300 ~show:Blueprint_file.print "P16-01-05 printing and parsing are inverse on any blueprint" gen_doc (fun d ->
         Blueprint_file.parse (Blueprint_file.print d) = Ok d));
    case "P16-01-06 a blueprint is checked before anything is built on it" (fun () ->
        equal_strings "the example is sound" [] (problems (example ()));
        let found d needle = check needle (List.exists (fun p -> Text.contains p needle) (problems d)) in
        found (doc [ lemma "a"; lemma "a" ]) "'a' is used by more than one node";
        found (doc [ lemma ~uses:[ "ghost" ] "a" ]) "'a' refers to 'ghost'";
        found (doc [ lemma ~proof_uses:[ "a" ] "a" ]) "'a' refers to itself";
        found (doc [ lemma ~lean:"same" "a"; lemma ~lean:"same" "b" ]) "the Lean name 'same' is given to more than one node: a, b";
        found (doc [ node ~id:"d" ~kind:Blueprint_node.Definition ~title:"D" ~proof:"Because." "A notion." ]) "a definition is not proved";
        found (doc [ node ~id:"e" ~kind:Blueprint_node.Lemma ~title:"E" "" ]) "'e' has no statement";
        found (doc [ lemma ~formal:"(n : Nat) : n = n" "f" ]) "no Lean name to state it under";
        let circle = doc [ lemma ~uses:[ "c" ] "a"; lemma ~uses:[ "a" ] "b"; lemma ~proof_uses:[ "b" ] "c"; lemma "free" ] in
        equal_strings "a circle is reported once, with its way round" [ "the nodes a -> c -> b -> a depend on each other in a circle" ] (problems circle);
        check "and it has no order" (Blueprint_check.order circle = Error [ "a"; "c"; "b" ]));
    case "P16-01-07 the graph is exported as JSON and in the DOT language" (fun () ->
        let d = example () in
        let graph = Blueprint_graph.to_json d in
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/blueprint/graph.schema.json")) graph));
        check "the committed graph is this one" (Json.equal graph (Support.json "fixtures/blueprint/graph.json"));
        equal_int "nodes" 8 (List.length (Json.list_or_empty "nodes" graph));
        let edges = Json.list_or_empty "edges" graph in
        equal_int "edges" 13 (List.length edges);
        equal_int "of which the proofs alone need six" 6 (List.length (List.filter (fun e -> Json.string_or "" "role" e = "proof") edges));
        let first = List.hd (Json.list_or_empty "nodes" graph) in
        equal_string "the Lean name is the full one" "Triangles.tri" (Json.string_or "" "lean" first);
        let dot = Blueprint_graph.to_dot d in
        contains "a definition is a box" dot "\"tri\" [label=\"tri\\nTriangular numbers\", shape=box];";
        contains "an edge of a statement" dot "\"tri-step\" -> \"tri\";";
        contains "an edge of a proof is dashed" dot "\"tri-formula\" -> \"tri-step\" [style=dashed];") ]

(* ---- the planner ---- *)

let scripted answers =
  let handler, seen = Scripted.sequence (List.map (fun text -> Ok (Scripted.answer text)) answers) in
  match Gateway.create ~scripted:handler (Maithic_config.Config.of_defaults ()) with Ok g -> (g, seen) | Error f -> fail (Fault.to_string f)

let plan nodes = Json.to_string (Json.Obj [ ("title", Json.String "A plan"); ("nodes", Json.List nodes) ])

let planned ?(extra = []) id kind =
  Json.Obj ([ ("id", Json.String id); ("kind", Json.String kind); ("title", Json.String ("About " ^ id)); ("statement", Json.String ("The claim " ^ id ^ ".")) ] @ extra)

let names items = Json.List (List.map (fun s -> Json.String s) items)

let plan_tests =
  [ case "P16-02-01 a model is asked to take an argument apart, and its answer becomes a blueprint" (fun () ->
        let argument = Support.read "examples/blueprint/argument.md" in
        let prompt = is_ok "the prompt is built in" (Prompt.load "blueprint.decompose") in
        let system, user = is_ok "renders" (Prompt.render prompt (Blueprint_plan.bindings ~imports:[ "Mathlib.Tactic" ] ~namespace:(Some "Triangles") argument)) in
        contains "the task" system "take one informal mathematical argument apart";
        contains "the namespace" system "relative to the namespace Triangles";
        contains "the libraries" system "the Lean core library and the modules Mathlib.Tactic";
        contains "the kernel has the last word" system "Lean decides";
        contains "the argument is data" user ("<data>\n" ^ Text.trim argument);
        let gateway, seen = scripted [ plan [ planned "a" "definition"; planned ~extra:[ ("uses", names [ "a" ]) ] "b" "theorem" ] ] in
        let outcome = is_ok "a plan" (Blueprint_plan.ask gateway ~max_tokens:512 ~namespace:"N" "Some argument.") in
        equal_strings "its nodes" [ "a"; "b" ] (Blueprint_doc.ids outcome.Blueprint_plan.doc);
        equal_string "its title" "A plan" outcome.Blueprint_plan.doc.Blueprint_doc.title;
        check "the namespace that was asked for" (outcome.Blueprint_plan.doc.Blueprint_doc.namespace = Some "N");
        equal_int "one question was put" 1 (List.length (seen ()));
        let empty, _ = scripted [ "{\"nodes\": []}" ] in
        contains "an answer without nodes is no plan" (is_error "refused" (Blueprint_plan.ask empty ~max_tokens:512 "x")) "names no node");
    case "P16-02-02 an answer is put in order before it is written as a blueprint" (fun () ->
        let answer =
          plan
            [ planned ~extra:[ ("proof", Json.String "Clearly."); ("proof_uses", names [ "Main Claim" ]) ] "Base Notion" "definition";
              planned ~extra:[ ("uses", names [ "Base Notion"; "nowhere"; "Main Claim" ]); ("proof_uses", names [ "Base Notion" ]) ] "Main Claim" "theorem";
              planned "Main Claim" "lemma"; planned "x" "riddle" ]
        in
        let outcome = is_ok "a blueprint" (Blueprint_plan.of_answer answer) in
        let d = outcome.Blueprint_plan.doc in
        equal_strings "identifiers are plain and unique" [ "base-notion"; "main-claim"; "main-claim-2" ] (Blueprint_doc.ids d);
        let main = Option.get (Blueprint_doc.find d "main-claim") in
        equal_strings "references follow the new names; the ones to nothing and to itself are gone" [ "base-notion" ] main.Blueprint_node.uses;
        equal_strings "what the statement uses is not repeated for the proof" [] main.Blueprint_node.proof_uses;
        let base = Option.get (Blueprint_doc.find d "base-notion") in
        equal_string "a definition keeps no proof" "" base.Blueprint_node.proof;
        equal_strings "nor what the proof referred to" [] (Blueprint_node.dependencies base);
        List.iter
          (fun needle -> check needle (List.exists (fun note -> Text.contains note needle) outcome.Blueprint_plan.notes))
          [ "'Base Notion' became 'base-notion'"; "'main-claim' referred to 'nowhere'"; "referred to itself"; "its proof was dropped"; "'riddle' is not a kind" ]);
    case "P16-02-02 a plan in a circle is refused, and a sound one is written as a file that reads back" (fun () ->
        contains "not JSON" (is_error "refused" (Blueprint_plan.of_answer "Here is my plan.")) "not JSON";
        let circle = plan [ planned ~extra:[ ("uses", names [ "b" ]) ] "a" "lemma"; planned ~extra:[ ("uses", names [ "a" ]) ] "b" "lemma" ] in
        contains "a circle" (is_error "refused" (Blueprint_plan.of_answer circle)) "in a circle";
        let fenced = "```json\n" ^ plan [ planned "a" "definition" ] ^ "\n```" in
        let outcome = is_ok "an answer in a code fence" (Blueprint_plan.of_answer ~title:"Mine" fenced) in
        equal_string "the title that was given wins" "Mine" outcome.Blueprint_plan.doc.Blueprint_doc.title;
        let dir = temp_dir "blueprint" in
        let path = Fs.(dir / "plan.md") in
        is_ok "written" (Blueprint_plan.write ~path outcome);
        check "and read back" (Blueprint_file.read path = Ok outcome.Blueprint_plan.doc);
        Fs.remove_tree dir) ]

(* ---- the skeleton ---- *)

let skeleton_tests =
  [ case "P16-03-01 the skeleton states every node and proves none, each under its identifier" (fun () ->
        let s = is_ok "a skeleton" (Blueprint_skeleton.emit (example ())) in
        equal_string "what the command wrote" (Support.read "fixtures/blueprint/lean/Stage1.lean") s.Blueprint_skeleton.text;
        contains "a theorem with sorry for a proof" s.Blueprint_skeleton.text
          "-- blueprint: odd-square\n/-- Odd numbers add up to a square.\n\nThe first n odd numbers add up to n squared. -/\ntheorem sumOdd_eq_square (n : Nat) : sumOdd n = n * n := by\n  sorry\n";
        contains "a definition as it stands" s.Blueprint_skeleton.text "-- blueprint: tri\n/-- Triangular numbers.";
        contains "inside the namespace" s.Blueprint_skeleton.text "namespace Triangles\n";
        equal_int "eight declarations" 8 (List.length s.Blueprint_skeleton.entries);
        equal_strings "the tags, read back from the text"
          [ "tri"; "sum-odd"; "tri-step"; "tri-formula"; "odd-step"; "odd-square"; "two-triangles"; "odd-triangles" ]
          (List.map fst (Blueprint_skeleton.tags s.Blueprint_skeleton.text));
        (* what is not stated yet is named and nothing more; a signature becomes a definition; a comment cannot end early *)
        let d =
          Blueprint_doc.make ~imports:[ "Init.Omega" ] ~title:"T"
            [ definition ~lean:"size" ~formal:"(n : Nat) : Nat" "size"; lemma ~lean:"later" "later";
              node ~id:"odd" ~kind:Blueprint_node.Lemma ~title:"Tricky -/ title" ~lean:"odd" ~formal:"theorem odd : True := trivial" "Text." ]
        in
        let s = is_ok "a skeleton" (Blueprint_skeleton.emit d) in
        contains "the imports" s.Blueprint_skeleton.text "import Init.Omega\n";
        contains "a signature alone" s.Blueprint_skeleton.text "def size (n : Nat) : Nat := sorry";
        contains "a whole declaration is kept" s.Blueprint_skeleton.text "theorem odd : True := trivial";
        contains "the comment stays closed" s.Blueprint_skeleton.text "/-- Tricky - / title.";
        equal_strings "not stated" [ "later" ] s.Blueprint_skeleton.unstated;
        contains "and named" s.Blueprint_skeleton.text "-- blueprint: later (not stated yet)");
    case "P16-03-02 the skeleton is given to Lean, and an error is laid at the door of its node" (fun () ->
        let s = is_ok "a skeleton" (Blueprint_skeleton.emit (example ())) in
        let line_of id = (List.find (fun (e : Blueprint_skeleton.entry) -> e.Blueprint_skeleton.id = id) s.Blueprint_skeleton.entries).Blueprint_skeleton.last in
        let warning line = { Blueprint_skeleton.severity = "warning"; line; message = "declaration uses 'sorry'" } in
        let fine = is_ok "compiled" (Blueprint_skeleton.compile ~elaborate:(fun _ -> Ok [ warning (line_of "tri-step"); warning (line_of "odd-square") ]) s) in
        check "warnings about sorry are what a skeleton is made of" fine.Blueprint_skeleton.compiles;
        let broken =
          is_ok "compiled"
            (Blueprint_skeleton.compile
               ~elaborate:(fun text ->
                 check "Lean is given the text of the skeleton" (text = s.Blueprint_skeleton.text);
                 Ok [ { Blueprint_skeleton.severity = "error"; line = line_of "two-triangles" - 1; message = "Unknown identifier `tri`" };
                      { Blueprint_skeleton.severity = "error"; line = 1; message = "unexpected token" } ])
               s)
        in
        check "it does not compile" (not broken.Blueprint_skeleton.compiles);
        check "the error and its node" (List.mem (Some "two-triangles", line_of "two-triangles" - 1, "Unknown identifier `tri`") broken.Blueprint_skeleton.problems);
        check "an error outside every node" (List.mem (None, 1, "unexpected token") broken.Blueprint_skeleton.problems);
        contains "Lean out of reach" (is_error "no verdict" (Blueprint_skeleton.compile ~elaborate:(fun _ -> Error "the Bridge is gone") s)) "gone");
    case "P16-03-03 declarations come in an order in which everything used stands before its use" (fun () ->
        let d = example () in
        let reversed = { d with Blueprint_doc.nodes = List.rev d.Blueprint_doc.nodes } in
        let ordered = ordered_ids reversed in
        let position id = Option.get (List.find_index (fun x -> x = id) ordered) in
        List.iter
          (fun (e : Blueprint_doc.edge) -> check (e.Blueprint_doc.target ^ " before " ^ e.Blueprint_doc.source) (position e.Blueprint_doc.target < position e.Blueprint_doc.source))
          (Blueprint_doc.edges d);
        equal_int "every node, once" 8 (List.length (List.sort_uniq compare ordered));
        equal_strings "a file that is in order already is left as it is" (Blueprint_doc.ids d) (ordered_ids d);
        let s = is_ok "a skeleton" (Blueprint_skeleton.emit reversed) in
        equal_strings "the skeleton follows that order" ordered (List.map (fun (e : Blueprint_skeleton.entry) -> e.Blueprint_skeleton.id) s.Blueprint_skeleton.entries);
        equal_strings "what a node rests on" [ "tri"; "tri-formula"; "tri-step" ] (List.sort compare (Blueprint_check.ancestors d "two-triangles"));
        equal_strings "what rests on a node" [ "odd-triangles"; "two-triangles" ] (List.sort compare (Blueprint_check.descendants d "tri-formula"))) ]

(* ---- progress ---- *)

(* What Lean reports about the example with the work half done. *)
let half_done =
  let decl ?(own = false) ?(rests = false) name deps =
    { Blueprint_status.name = "Triangles." ^ name; own_sorry = own; uses_sorry = own || rests; deps = List.map (fun d -> "Triangles." ^ d) deps @ [ "Nat"; "HAdd.hAdd" ] }
  in
  [ decl "tri" []; decl "sumOdd" []; decl "tri_succ" [ "tri" ]; decl ~own:true "two_mul_tri" [ "tri" ]; decl ~own:true "sumOdd_succ" [ "sumOdd" ];
    decl ~rests:true "sumOdd_eq_square" [ "sumOdd"; "sumOdd_succ" ]; decl ~own:true "tri_add_tri_succ" [ "tri" ] ]

let status ?label () = Blueprint_status.compute ~lean_file:"examples/blueprint/triangles.lean" ?label (example ()) half_done
let state_of s id = Blueprint_status.state_name (Option.get (Blueprint_status.find_entry s id)).Blueprint_status.state
let entry_ids entries = List.map (fun (e : Blueprint_status.entry) -> e.Blueprint_status.node.Blueprint_node.id) entries

let progress_tests =
  [ case "P16-04-01 the status of a node comes from what Lean reports about its declaration" (fun () ->
        let s = status () in
        equal_strings "node by node" [ "done"; "done"; "done"; "stated"; "stated"; "proved"; "stated"; "planned" ] (List.map (state_of s) (Blueprint_doc.ids (example ())));
        let none = Blueprint_status.compute (example ()) [] in
        check "without Lean, everything is planned" (List.for_all (fun (e : Blueprint_status.entry) -> e.Blueprint_status.state = Blueprint_status.Planned) none.Blueprint_status.entries);
        (* a name without the namespace is found too, and a finished proof is compared with the blueprint *)
        let d = doc ~namespace:"N" [ definition ~lean:"base" "base"; definition ~lean:"other" "other"; lemma ~lean:"claim" ~uses:[ "base" ] "claim" ] in
        let lean = [ { Blueprint_status.name = "base"; own_sorry = false; uses_sorry = false; deps = [] }; { Blueprint_status.name = "N.other"; own_sorry = false; uses_sorry = false; deps = [] };
                     { Blueprint_status.name = "N.claim"; own_sorry = false; uses_sorry = false; deps = [ "N.other" ] } ] in
        let s = Blueprint_status.compute d lean in
        equal_string "found by its short name" "done" (state_of s "base");
        let notes = (Option.get (Blueprint_status.find_entry s "claim")).Blueprint_status.notes in
        equal_strings "where the blueprint and the Lean text disagree"
          [ "the blueprint lists base, and the Lean text does not refer to it"; "the Lean text refers to other, which the blueprint does not list" ]
          notes;
        check "an unfinished proof is not compared" ((Option.get (Blueprint_status.find_entry (status ()) "tri-formula")).Blueprint_status.notes = []));
    case "P16-04-02 nodes are counted by status and by kind" (fun () ->
        let s = status ~label:"half done" () in
        equal_ints "planned, stated, proved, done" [ 1; 3; 1; 3 ] (List.map (Blueprint_status.count s) Blueprint_status.states);
        equal_string "in a line" "8 node(s): 3 done, 1 proved, 3 stated, 1 planned" (Blueprint_status.summary s);
        let lemmas = List.assoc Blueprint_node.Lemma (Blueprint_status.by_kind s) in
        equal_ints "the lemmas" [ 0; 3; 0; 1 ] (List.map snd lemmas);
        check "a kind without a node is left out" (not (List.mem_assoc Blueprint_node.Proposition (Blueprint_status.by_kind s)));
        check "not complete" (not (Blueprint_status.complete s));
        let json = Blueprint_status.to_json s in
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/blueprint/status.schema.json")) json));
        check "the recorded snapshot is this one" (Json.equal json (Support.json "fixtures/blueprint/snapshots/2-half-done.json"));
        List.iter
          (fun (file, expected) ->
            let recorded = Support.json ("fixtures/blueprint/snapshots/" ^ file) in
            equal_ints file expected (List.map (fun state -> Json.int_or (-1) state (Option.get (Json.path [ "summary"; "by_status" ] recorded))) [ "planned"; "stated"; "proved"; "done" ]))
          [ ("1-skeleton.json", [ 0; 6; 0; 2 ]); ("3-one-proof-left.json", [ 0; 1; 2; 5 ]); ("4-finished.json", [ 0; 0; 0; 8 ]) ]);
    case "P16-04-03 the frontier is what is not settled and waits for nothing" (fun () ->
        let s = status () in
        equal_strings "the node that most others rest on comes first" [ "tri-formula"; "odd-step" ] (entry_ids (Blueprint_status.frontier s));
        equal_ints "how many rest on each" [ 2; 1 ] (List.map (fun (e : Blueprint_status.entry) -> e.Blueprint_status.unblocks) (Blueprint_status.frontier s));
        (* among equals a stated node comes before a planned one, and then the file decides *)
        let d = doc [ lemma ~lean:"p" "planned-first"; lemma ~lean:"s" "stated-second"; lemma ~lean:"t" "stated-third" ] in
        let lean = List.map (fun name -> { Blueprint_status.name; own_sorry = true; uses_sorry = true; deps = [] }) [ "s"; "t" ] in
        equal_strings "stated before planned" [ "stated-second"; "stated-third"; "planned-first" ] (entry_ids (Blueprint_status.frontier (Blueprint_status.compute d lean)));
        let finished = Blueprint_status.compute (doc [ definition ~lean:"a" "a" ]) [ { Blueprint_status.name = "a"; own_sorry = false; uses_sorry = false; deps = [] } ] in
        check "nothing is left when all is done" (Blueprint_status.frontier finished = [] && Blueprint_status.complete finished));
    case "P16-04-04 a node that waits is listed with what it waits for" (fun () ->
        let s = status () in
        let waiting = List.map (fun (e : Blueprint_status.entry) -> (e.Blueprint_status.node.Blueprint_node.id, e.Blueprint_status.blockers)) (Blueprint_status.blocked s) in
        check "two nodes wait" (waiting = [ ("two-triangles", [ "tri-formula" ]); ("odd-triangles", [ "two-triangles" ]) ]);
        check "a proved node waits for nothing, though it rests on an open one" ((Option.get (Blueprint_status.find_entry s "odd-square")).Blueprint_status.blockers = []);
        check "a waiting node is not ready" (not (Option.get (Blueprint_status.find_entry s "two-triangles")).Blueprint_status.ready));
    case "P16-04-05 the status is shown as a table with the counts below it" (fun () ->
        let text = Blueprint_status.render (status ()) in
        contains "a node that can be proved now" text "tri-formula    lemma       stated   ready to prove; 2 node(s) rest on it";
        contains "a node that waits" text "odd-triangles  corollary   planned  waits for two-triangles";
        contains "the summary" text "8 node(s): 3 done, 1 proved, 3 stated, 1 planned";
        contains "by kind" text "lemma      : 1 done, 3 stated";
        check "no colour unless asked" (not (Text.contains text "\027["));
        contains "colour when asked" (Blueprint_status.render ~color:true (status ())) "\027[32mdone";
        let broken = Blueprint_status.compute ~lean_file:"x.lean" ~lean_errors:2 (example ()) half_done in
        contains "errors of the Lean file are said" (Blueprint_status.render broken) "Lean reports 2 error(s) in the file");
    case "P16-04-06 what to do next is a numbered list, the most useful first" (fun () ->
        let text = Blueprint_status.render_next (status ()) in
        equal_strings "two lines"
          [ "1. tri-formula  lemma  to prove  Twice a triangular number  (2 node(s) rest on it)"; "2. odd-step  lemma  to prove  The next odd number  (1 node(s) rest on it)" ]
          (String.split_on_char '\n' text);
        let all_done = Blueprint_status.compute (doc [ definition ~lean:"a" "a" ]) [ { Blueprint_status.name = "a"; own_sorry = false; uses_sorry = false; deps = [] } ] in
        equal_string "when all is done" "nothing is left: every node is done" (Blueprint_status.render_next all_done);
        let fresh = Blueprint_status.render_next (Blueprint_status.compute (example ()) []) in
        contains "before anything is in Lean, the definitions come first" fresh "1. tri  definition  to state";
        let svg = Blueprint_svg.render (status ()) in
        contains "the picture names every node" svg ">odd-triangles</text>";
        contains "and says what is ready" svg "lemma · stated · ready";
        equal_int "an arrow that follows from others is left out" 7 (List.length (Blueprint_svg.drawn_edges (example ())));
        (* A title is shown whole, in two lines when one is too short for it; only what does not fit in two is cut. *)
        equal_strings "a short title" [ "Triangular numbers" ] (Blueprint_svg.title_lines "Triangular numbers");
        equal_strings "a title of two lines" [ "The sum of the first odd"; "numbers" ] (Blueprint_svg.title_lines "The sum of the first odd numbers");
        check "no line is longer than a box is wide"
          (List.for_all (fun line -> Text.utf8_length line <= Blueprint_svg.title_chars)
             (Blueprint_svg.title_lines "A very long title that goes on and on and on, far beyond what two lines of a box can hold"));
        equal_int "and there are never more than two" 2
          (List.length (Blueprint_svg.title_lines "A very long title that goes on and on and on, far beyond what two lines of a box can hold"));
        contains "the picture shows the second line" svg ">triangles</text>";
        (* No two arrows end in the same point: every arrowhead of the picture has a place of its own. *)
        let heads =
          List.filter_map
            (fun line -> if Text.contains line "l5 -8 h-10 z" then Some (String.trim line) else None)
            (String.split_on_char '\n' svg)
        in
        equal_int "one arrowhead for each arrow" 7 (List.length heads);
        equal_int "each at its own point" 7 (List.length (List.sort_uniq compare heads))) ]

let () =
  run "blueprint" [ ("model", model_tests); ("planner", plan_tests); ("skeleton", skeleton_tests); ("progress", progress_tests) ]
