(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the code graph, its queries and the health report. They read what the Bridge
   extracted from four files, three of the library and one written to have gaps, as it is kept
   under fixtures/intel; no Lean is involved. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_intel
open Testkit

let demo_file = "fixtures/intel/lean/Demo.lean"

let extractions =
  lazy
    (List.map
       (fun name -> is_ok name (Extraction.of_json (Support.json ("fixtures/intel/extractions/" ^ name ^ ".json"))))
       [ "Maithic.Algo.Sort.Insertion"; "Maithic.Algo.Sort.Linear"; "Maithic.Algo.Spec.Sorted"; "fixtures.intel.lean.Demo" ])

let graph = lazy (Intel_graph.build (Lazy.force extractions))
let names nodes = List.map (fun (n : Intel_graph.node) -> n.Intel_graph.name) nodes
let node id = match Intel_graph.node (Lazy.force graph) id with Some n -> n | None -> fail ("no node " ^ id)
let has_edge source kind target = List.exists (fun (e : Intel_graph.edge) -> e.Intel_graph.source = source && e.Intel_graph.kind = kind && e.Intel_graph.target = target) (Intel_graph.edges (Lazy.force graph))
let via source target = (List.find (fun (e : Intel_graph.edge) -> e.Intel_graph.source = source && e.Intel_graph.target = target && e.Intel_graph.kind = Intel_graph.Uses) (Intel_graph.edges (Lazy.force graph))).Intel_graph.via
let schema name = is_ok name (Json_schema.load (Support.path ("schemas/intel/" ^ name ^ ".schema.json")))

let graph_tests =
  [ case "P07-02-01 the graph has four kinds of node and five kinds of edge, and is one document" (fun () ->
        let g = Lazy.force graph in
        equal_strings "kinds of node" [ "declaration"; "file"; "module"; "namespace" ]
          (List.sort_uniq compare (List.map (fun (n : Intel_graph.node) -> Intel_graph.node_kind_to_string n.Intel_graph.kind) (Intel_graph.nodes g)));
        equal_strings "kinds of edge" [ "contains"; "extends"; "imports"; "instance-of"; "uses" ]
          (List.sort_uniq compare (List.map (fun (e : Intel_graph.edge) -> Intel_graph.edge_kind_to_string e.Intel_graph.kind) (Intel_graph.edges g)));
        let json = Intel_graph.to_json g in
        check "it conforms to its schema" (Json_schema.conforms (schema "graph") json = Ok ());
        let back = is_ok "read back" (Intel_graph.of_json json) in
        check "the document reads back as the graph" (Json.equal (Intel_graph.to_json back) json);
        check "an identifier says the kind" (List.for_all (fun (n : Intel_graph.node) -> String.length n.Intel_graph.id > 2 && n.Intel_graph.id.[1] = ':') (Intel_graph.nodes g));
        let broken = match json with Json.Obj fields -> Json.Obj (List.map (fun (k, v) -> if k = "edges" then (k, Json.List [ Json.Obj [ ("source", Json.String "d:nowhere"); ("target", Json.String "d:double"); ("kind", Json.String "uses") ] ]) else (k, v)) fields) | j -> j in
        contains "an edge from nowhere is refused" (is_error "broken" (Intel_graph.of_json broken)) "an edge starts at a node that is not there");
    case "P07-02-02 the graph is built from what the Bridge extracted" (fun () ->
        let g = Lazy.force graph in
        let declared = List.fold_left (fun acc (e : Extraction.t) -> acc + List.length e.Extraction.declarations) 0 (Lazy.force extractions) in
        equal_int "a node for every declaration of the files" declared
          (List.length (List.filter (fun (n : Intel_graph.node) -> n.Intel_graph.kind = Intel_graph.Declaration && n.Intel_graph.internal) (Intel_graph.nodes g)));
        let double_eq = node "d:double_eq" in
        equal_string "the facts of a declaration" "theorem fixtures/intel/lean/Demo.lean 8-13"
          (Printf.sprintf "%s %s %d-%d" double_eq.Intel_graph.decl_kind double_eq.Intel_graph.file double_eq.Intel_graph.line double_eq.Intel_graph.end_line);
        check "documentation is noted" ((node "d:double").Intel_graph.documented && not double_eq.Intel_graph.documented);
        check "a lemma of the library is a node from outside" (not (node "d:Nat.add_comm").Intel_graph.internal);
        check "a proof uses the lemma it applies" (has_edge "d:Bad_Name" Intel_graph.Uses "d:Nat.add_comm");
        equal_string "used in the proof" "value" (via "d:uses_open" "d:open_one");
        equal_string "used in the statement and in the proof" "both" (via "d:double_eq" "d:double");
        check "an instance and its class" (has_edge "d:instInhabitedPoint" Intel_graph.Instance_of "d:Inhabited");
        check "a structure and what it extends" (has_edge "d:Point3" Intel_graph.Extends "d:Point");
        check "a file contains its module, and the module its declarations"
          (has_edge ("f:" ^ demo_file) Intel_graph.Contains "m:fixtures.intel.lean.Demo" && has_edge "m:fixtures.intel.lean.Demo" Intel_graph.Contains "d:double");
        check "namespaces nest down to the declaration"
          (has_edge "n:Maithic" Intel_graph.Contains "n:Maithic.Algo" && has_edge "n:Maithic.Algo" Intel_graph.Contains "d:Maithic.Algo.insertionSort");
        check "a module imports another of the files"
          (has_edge "m:Maithic.Algo.Sort.Insertion" Intel_graph.Imports "m:Maithic.Algo.Spec.Sorted" && (node "m:Maithic.Algo.Spec.Sorted").Intel_graph.internal);
        check "and modules from outside" (has_edge "m:fixtures.intel.lean.Demo" Intel_graph.Imports "m:Init" && not (node "m:Init").Intel_graph.internal);
        check "no declaration uses itself" (not (List.exists (fun (e : Intel_graph.edge) -> e.Intel_graph.kind = Intel_graph.Uses && e.Intel_graph.source = e.Intel_graph.target) (Intel_graph.edges g))));
    case "P07-02-03 a node is found by name, by file and by position" (fun () ->
        let g = Lazy.force graph in
        equal_string "by full name" "Maithic.Algo.insertionSort" (is_ok "full" (Intel_graph.find g "Maithic.Algo.insertionSort")).Intel_graph.name;
        equal_string "by the end of the name" "Maithic.Algo.insertionSort" (is_ok "short" (Intel_graph.find g "insertionSort")).Intel_graph.name;
        contains "a name that is not there" (is_error "missing" (Intel_graph.find g "no_such_thing")) "no declaration called 'no_such_thing'";
        let twins =
          Intel_graph.build
            [ { (List.hd (Lazy.force extractions)) with
                Extraction.declarations =
                  List.map
                    (fun name -> { (List.hd (List.hd (Lazy.force extractions)).Extraction.declarations) with Extraction.name; type_deps = []; value_deps = [] })
                    [ "A.same"; "B.same" ] } ]
        in
        contains "an end that fits two names" (is_error "two" (Intel_graph.find twins "same")) "several declarations end in 'same': A.same, B.same";
        equal_strings "the declarations of a file, in its order"
          [ "double"; "double_eq"; "Bad_Name"; "open_one"; "uses_open"; "Point"; "instInhabitedPoint"; "shortcut"; "by_shortcut"; "Point3"; "term_gap" ]
          (names (Intel_graph.in_file g demo_file));
        equal_string "the declaration at a line" "double_eq" (Option.get (Intel_graph.at g ~file:demo_file ~line:11)).Intel_graph.name;
        check "no declaration on an empty line" (Intel_graph.at g ~file:demo_file ~line:14 = None);
        (* A declaration begins with its documentation; its name stands where a reader is sent. *)
        let double = is_ok "double" (Intel_graph.find g "double") in
        equal_string "the documentation belongs to the declaration" "double" (Option.get (Intel_graph.at g ~file:demo_file ~line:3)).Intel_graph.name;
        check "where it begins, where its name stands, where it ends"
          ((double.Intel_graph.line, double.Intel_graph.name_line, double.Intel_graph.end_line) = (3, 4, 6));
        let reread = is_ok "reread" (Intel_graph.of_json (Intel_graph.to_json g)) in
        equal_int "the place of the name is kept in the document" 4 (is_ok "double" (Intel_graph.find reread "double")).Intel_graph.name_line;
        check "nothing in a file that was not scanned" (Intel_graph.in_file g "elsewhere.lean" = []));
    case "P07-02-04 direct dependencies and direct dependents" (fun () ->
        let g = Lazy.force graph in
        let uses_open = Option.get (Extraction.find (List.nth (Lazy.force extractions) 3) "uses_open") in
        equal_strings "what a declaration uses, in its statement or its proof"
          (List.sort_uniq compare (uses_open.Extraction.type_deps @ uses_open.Extraction.value_deps))
          (names (Intel_graph.dependencies g "uses_open"));
        check "the theorem its proof applies is among them" (List.mem "open_one" (names (Intel_graph.dependencies g "uses_open")));
        equal_strings "what uses a theorem" [ "uses_open" ] (names (Intel_graph.dependents g "open_one"));
        equal_strings "what uses a definition"
          [ "Maithic.Algo.insertionSort_idem"; "Maithic.Algo.insertionSort_of_sorted"; "Maithic.Algo.insertionSort_perm"; "Maithic.Algo.length_insertionSort";
            "Maithic.Algo.mem_insertionSort"; "Maithic.Algo.sorted_insertionSort" ]
          (names (Intel_graph.dependents g "Maithic.Algo.insertionSort"));
        check "nothing uses a result that stands alone" (Intel_graph.dependents g "by_shortcut" = []);
        equal_strings "what a module contains begins with its first declaration" [ "double" ]
          (List.filteri (fun i _ -> i < 1) (List.sort compare (List.filter (fun n -> n = "double") (names (Intel_graph.contents g "m:fixtures.intel.lean.Demo"))))));
    case "P07-02-05 counts by kind and degrees" (fun () ->
        let s = Intel_graph.statistics (Lazy.force graph) in
        equal_int "files" 4 (List.assoc "file" s.Intel_graph.nodes_by_kind);
        equal_int "theorems" 28 (List.assoc "theorem" s.Intel_graph.declarations_by_kind);
        equal_int "structures" 2 (List.assoc "structure" s.Intel_graph.declarations_by_kind);
        equal_int "instances are counted as edges too" 3 (List.assoc "instance-of" s.Intel_graph.edges_by_kind);
        equal_int "declarations in all" (List.assoc "declaration" s.Intel_graph.nodes_by_kind)
          (s.Intel_graph.external_declarations + List.fold_left (fun acc (_, n) -> acc + n) 0 s.Intel_graph.declarations_by_kind);
        equal_string "the most used of the files" "Maithic.Algo.Sorted" (fst (List.hd s.Intel_graph.most_used_internal));
        equal_int "used by twelve" 12 (snd (List.hd s.Intel_graph.most_used_internal));
        check "degrees" (s.Intel_graph.used_by.Intel_graph.least = 0 && s.Intel_graph.used_by.Intel_graph.greatest = 12 && s.Intel_graph.uses.Intel_graph.mean > 1.0);
        check "at most ten are named" (List.length s.Intel_graph.most_used <= 10);
        let j = Intel_graph.statistics_to_json s in
        equal_int "as a document" 4 (Json.int_or 0 "file" (Option.get (Json.member "nodes" j)))) ]

let query_tests =
  [ case "P07-03-01 everything a declaration depends on, as far as asked" (fun () ->
        let g = Lazy.force graph in
        let found ?depth name = List.map (fun ((n : Intel_graph.node), d) -> (n.Intel_graph.name, d)) (Intel_query.dependencies ?depth ~internal_only:true g name) in
        let direct = found ~depth:1 "Maithic.Algo.sorted_insertionSort" in
        check "directly: the definition, the predicate and the lemma its proof applies"
          (List.for_all (fun name -> List.mem (name, 1) direct) [ "Maithic.Algo.Sorted"; "Maithic.Algo.insertionSort"; "Maithic.Algo.sorted_insertSorted" ]);
        check "all of them at distance one, and all from the scanned files"
          (List.for_all (fun (name, d) -> d = 1 && (node ("d:" ^ name)).Intel_graph.internal) direct);
        let all = found "Maithic.Algo.sorted_insertionSort" in
        check "and through others, with the distance" (List.mem ("Maithic.Algo.Sorted", 1) all && List.exists (fun (_, d) -> d = 2) all);
        check "each once, at its least distance" (List.length all = List.length (List.sort_uniq compare (List.map fst all)));
        check "nearest first" (List.map snd all = List.sort compare (List.map snd all));
        let outside = Intel_query.dependencies ~depth:1 g "Bad_Name" in
        equal_strings "with what comes from outside" [ "Eq"; "HAdd.hAdd"; "Nat"; "Nat.add_comm"; "instAddNat"; "instHAdd" ] (names (List.map fst outside));
        check "or without" (Intel_query.dependencies ~depth:1 ~internal_only:true g "Bad_Name" = []));
    case "P07-03-02 everything that depends on a declaration" (fun () ->
        let g = Lazy.force graph in
        let found ?depth name = List.map (fun ((n : Intel_graph.node), d) -> (n.Intel_graph.name, d)) (Intel_query.dependents ?depth g name) in
        check "directly" (found "open_one" = [ ("uses_open", 1) ]);
        let of_sorted = found "Maithic.Algo.Sorted" in
        equal_int "twelve use the predicate itself" 12 (List.length (List.filter (fun (_, d) -> d = 1) of_sorted));
        check "more reach it through those" (List.exists (fun (_, d) -> d >= 2) of_sorted);
        check "a depth limit cuts the rest off" (List.length (found ~depth:1 "Maithic.Algo.Sorted") = 12);
        check "nothing depends on a result that stands alone" (found "term_gap" = []));
    case "P07-03-03 the declarations with a sorry of their own, and those that rest on one" (fun () ->
        let g = Lazy.force graph in
        equal_strings "unfinished" [ "open_one"; "term_gap" ] (names (Intel_query.unfinished g));
        equal_strings "resting on one of those" [ "uses_open" ] (names (Intel_query.affected g))) ]

let health_tests =
  [ case "P07-05-01 every unfinished proof, with its place and its goals" (fun () ->
        let gaps = Intel_health.sorry_inventory (Lazy.force extractions) in
        equal_strings "in the order of the file" [ "open_one"; "uses_open"; "term_gap" ] (List.map (fun (g : Intel_health.gap) -> g.Intel_health.name) gaps);
        let first = List.hd gaps in
        equal_string "the place of the sorry" "fixtures/intel/lean/Demo.lean:18:2"
          (Printf.sprintf "%s:%d:%d" first.Intel_health.file first.Intel_health.line first.Intel_health.column);
        equal_strings "what is to be shown there" [ "n : Nat\n\xE2\x8A\xA2 n + 0 = n" ] first.Intel_health.goals;
        let second = List.nth gaps 1 in
        check "a theorem that rests on another's sorry names it" (second.Intel_health.through = Some "open_one" && second.Intel_health.goals = []);
        let third = List.nth gaps 2 in
        check "a sorry that is a term has a place and no goals" (third.Intel_health.line = 36 && third.Intel_health.goals = [] && third.Intel_health.through = None);
        let snapshot = Intel_health.snapshot (Lazy.force extractions) in
        check "the report conforms to its schema" (Json_schema.conforms (schema "health") snapshot = Ok ());
        equal_int "three declarations rest on sorry" 3 (Json.int_or 0 "restingOnSorry" snapshot);
        check "the report in the repository is this one" (Json.equal snapshot (Support.json "fixtures/intel/health.json")));
    case "P07-05-02 the axioms in use, and what rests on each" (fun () ->
        let census = Intel_health.axiom_census (Lazy.force extractions) in
        let first = List.hd census in
        equal_string "an axiom beyond Lean's logic comes first" "shortcut" first.Intel_health.axiom;
        check "it is not one of the three" (not first.Intel_health.standard);
        check "with what rests on it, and where" (first.Intel_health.users = [ ("by_shortcut", demo_file, 30) ]);
        equal_strings "then the axioms of the logic that are used" [ "Quot.sound"; "propext" ]
          (List.map (fun (a : Intel_health.axiom_use) -> a.Intel_health.axiom) (List.tl census));
        check "they are marked" (List.for_all (fun (a : Intel_health.axiom_use) -> a.Intel_health.standard) (List.tl census));
        check "sorry is not counted as an axiom" (not (List.exists (fun (a : Intel_health.axiom_use) -> a.Intel_health.axiom = "sorryAx") census));
        check "an axiom does not rest on itself" (not (List.exists (fun (name, _, _) -> name = "shortcut") first.Intel_health.users)));
    case "P07-02-02 the graph in the repository is the one these extractions give" (fun () ->
        check "equal" (Json.equal (Intel_graph.to_json (Lazy.force graph)) (Support.json "fixtures/intel/graph.json"))) ]

let files_tests =
  [ case "P07-02-03 a file stands for a module, below the project or the current directory" (fun () ->
        equal_string "below the project" "Maithic.Algo.Sort.Insertion" (Lean_files.module_of ~project:"lean" "lean/Maithic/Algo/Sort/Insertion.lean");
        equal_string "a project given with a slash" "Maithic.Version" (Lean_files.module_of ~project:"lean/" "./lean/Maithic/Version.lean");
        equal_string "outside the project" "examples.double" (Lean_files.module_of ~project:"lean" "examples/double.lean");
        equal_string "with no project" "examples.double" (Lean_files.module_of "examples/double.lean");
        let found = is_ok "expand" (Lean_files.expand [ Support.path "fixtures/intel/lean"; Support.path "examples/double.lean" ]) in
        equal_strings "directories are searched for Lean files, and files are taken as they are" [ "Demo.lean"; "double.lean" ]
          (List.sort compare (List.map Fs.basename found));
        contains "a path that is not there" (is_error "missing" (Lean_files.expand [ "no/such/place" ])) "does not exist") ]

let () = Testkit.run "intel" [ ("graph", graph_tests); ("queries", query_tests); ("health", health_tests); ("files", files_tests) ]
