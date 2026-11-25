(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the search over declarations: the documents, the words, the index, the scores and the
   entry point that merges retrievers. They run on the declarations the repository keeps for its
   demonstrations, so they need neither Lean nor a network. *)

open Maithic_base
open Maithic_schema
open Maithic_lookup
open Testkit

let documents = lazy (is_ok "the declarations" (Decl_document.of_jsonl (Support.read "fixtures/intel/extractions/declarations.jsonl")))
let index = lazy (Inverted_index.build (Lazy.force documents))
let names hits = List.map (fun (h : Decl_search.hit) -> h.Decl_search.document.Decl_document.name) hits
let search ?options ?lean query = fst (Decl_search.search ?options ?lean (Lazy.force index) query)
let document name = List.find (fun (d : Decl_document.t) -> d.Decl_document.name = name) (Lazy.force documents)

let plain name statement doc =
  { Decl_document.name; kind = "theorem"; statement; doc; module_ = "M"; attributes = []; file = "M.lean"; line = 1 }

let document_tests =
  [ case "P04-01-01 a declaration becomes a document with its name, kind, statement, documentation, module and attributes" (fun () ->
        let docs = Lazy.force documents in
        equal_int "the demonstration has 41 declarations" 41 (List.length docs);
        let d = document "Maithic.Algo.sorted_nil" in
        equal_string "kind" "theorem" d.Decl_document.kind;
        contains "the statement as Lean prints it" d.Decl_document.statement "Maithic.Algo.Sorted []";
        equal_string "module" "Maithic.Algo.Spec.Sorted" d.Decl_document.module_;
        equal_strings "the attribute it was declared with" [ "simp" ] d.Decl_document.attributes;
        equal_string "where it stands" "lean/Maithic/Algo/Spec/Sorted.lean" d.Decl_document.file;
        check "at the line of its name" (d.Decl_document.line > 0);
        equal_string "its documentation" "Insertion sort." (document "Maithic.Algo.insertionSort").Decl_document.doc;
        equal_string "what a reader calls it" "insertionSort" (Decl_document.short_name (document "Maithic.Algo.insertionSort"));
        check "a declaration that rests on sorry says so" (List.mem "sorry" (document "open_one").Decl_document.attributes);
        check "the same document back from its own JSON" (Decl_document.of_json (Decl_document.to_json d) = Ok d);
        equal_strings "attributes are read before the keyword only" [ "simp"; "grind" ]
          (Decl_document.attributes_of_source "@[simp, grind =]\ntheorem t : xs = @[a] := rfl");
        equal_strings "none" [] (Decl_document.attributes_of_source "theorem t : True := trivial");
        let listed =
          is_ok "two lines"
            (Decl_document.of_jsonl
               "{\"name\":\"A.b\",\"kind\":\"theorem\",\"type\":\"True\",\"module\":\"A\",\"isPrivate\":false}\n\n{\"name\":\"A.c\",\"kind\":\"definition\",\"type\":\"Nat\",\"isPrivate\":true}\n")
        in
        equal_strings "a private declaration is not for the search" [ "A.b" ] (List.map (fun (d : Decl_document.t) -> d.Decl_document.name) listed);
        contains "a line that is not a declaration" (is_error "refused" (Decl_document.of_jsonl "{\"kind\":\"theorem\"}")) "line 1";
        (* straight from what the Bridge extracts *)
        let extraction = is_ok "an extraction" (Maithic_bridge.Extraction.of_json (Support.json "fixtures/intel/extractions/Maithic.Algo.Spec.Sorted.json")) in
        let extracted = Decl_document.of_extractions [ extraction ] in
        check "the same documents as from the export" (List.for_all (fun (d : Decl_document.t) -> document d.Decl_document.name = d) extracted && extracted <> [])) ]

let index_tests =
  [ case "P04-04-01 names, statements and prose are taken apart into one vocabulary" (fun () ->
        equal_strings "a name, whole and in parts" [ "nat.add_comm"; "nat"; "add"; "comm" ] (Tokens.of_name "Nat.add_comm");
        equal_strings "capitals open a part" [ "insertion"; "sort"; "perm" ] (Tokens.of_text "insertionSort_perm");
        equal_strings "notation is called what its lemmas call it, and variables are dropped" [ "add"; "eq"; "add" ] (Tokens.of_text "a + b = b + a");
        equal_strings "longer notation before shorter" [ "append"; "le"; "iff"; "mem" ] (Tokens.of_text "l₁ ++ l₂ ≤ m ↔ x ∈ l");
        equal_strings "prose loses its small words and names operations as names do" [ "add"; "comm" ] (Tokens.of_text ~prose:true "the addition is commutative");
        equal_strings "numbers stay" [ "add"; "0"; "eq" ] (Tokens.of_text "n + 0 = n"));
    case "P04-04-01 the index lists for every word where it stands, in the order of the documents" (fun () ->
        let small = Inverted_index.build [ plain "A.add_zero" "n + 0 = n" "Adding zero."; plain "A.mul_one" "n * 1 = n" ""; plain "B.add_comm" "a + b = b + a" "Addition is commutative." ] in
        equal_int "three documents" 3 (Inverted_index.size small);
        let where word = List.map (fun (p : Inverted_index.posting) -> (p.Inverted_index.document, Inverted_index.field_name p.Inverted_index.field, p.Inverted_index.count)) (Inverted_index.postings small word) in
        check "a word of names, statements and prose" (where "add" = [ (0, "name", 1); (0, "statement", 1); (0, "doc", 1); (2, "name", 1); (2, "statement", 2); (2, "doc", 1) ]);
        check "a word of one document" (where "mul" = [ (1, "name", 1); (1, "statement", 1) ]);
        check "a word nobody has" (where "sub" = []);
        equal_int "in how many documents a word stands" 2 (Inverted_index.document_frequency small "add");
        equal_int "the statement of the third has three words" 3 small.Inverted_index.lengths.(Inverted_index.field_index Inverted_index.Statement).(2);
        check "the mean length of a statement" (abs_float (small.Inverted_index.averages.(Inverted_index.field_index Inverted_index.Statement) -. 3.0) < 1e-9);
        let real = Lazy.force index in
        equal_int "the demonstration" 41 (Inverted_index.size real);
        check "has some hundred words" (Inverted_index.vocabulary real > 100);
        check "an empty index is an index" (Inverted_index.size (Inverted_index.build []) = 0)) ]

let scoring_tests =
  [ case "P04-04-02 BM25 ranks by the words a declaration shares with the query, a rare word above a common one" (fun () ->
        let docs = [ plain "A.add_zero" "n + 0 = n" "Adding zero."; plain "A.add_succ" "n + succ m = succ (n + m)" ""; plain "A.mul_one" "n * 1 = n" "Multiplying by one." ] in
        let index = Inverted_index.build docs in
        check "a word of fewer documents tells more" (Bm25.idf index "mul" > Bm25.idf index "add");
        check "and no word tells less than nothing" (Bm25.idf index "eq" >= 0.0 && Bm25.idf index "nowhere" > 0.0);
        let ranked words = List.map (fun (s : Bm25.scored) -> s.Bm25.document) (Bm25.score index words) in
        equal_ints "the one document with both words comes first" [ 0; 1 ] (ranked [ "add"; "zero" ]);
        equal_ints "a document without a word of the query is not listed" [ 2 ] (ranked [ "mul" ]);
        check "nothing for words nobody has" (Bm25.score index [ "group" ] = []);
        let scores words = List.map (fun (s : Bm25.scored) -> s.Bm25.score) (Bm25.score index words) in
        check "a word the query repeats counts once" (scores [ "add"; "add" ] = scores [ "add" ]);
        check "scores fall along the ranking" (let s = scores [ "add"; "succ"; "eq" ] in s = List.sort (fun a b -> compare b a) s);
        check "the words a document has are named" ((List.hd (Bm25.score index [ "zero"; "mul"; "add" ])).Bm25.matched = [ "zero"; "add" ]));
    case "P04-04-02 a word in a name weighs more than the same word in the documentation" (fun () ->
        let index = Inverted_index.build [ plain "A.unrelated" "True" "About the tail of things."; plain "A.tail" "True" "" ] in
        equal_int "by default the name wins" 1 (List.hd (Bm25.score index [ "tail" ])).Bm25.document;
        let prose_first = { Bm25.default_weights with Bm25.name = 0.1; doc = 5.0 } in
        equal_int "with other weights the documentation does" 0 (List.hd (Bm25.score ~weights:prose_first index [ "tail" ])).Bm25.document;
        (* a long field dilutes a word; with b = 0 it does not *)
        let long = Inverted_index.build [ plain "A.x" "sorted and and and and and and and and and and and and" ""; plain "A.y" "sorted" "" ] in
        equal_int "the short statement first" 1 (List.hd (Bm25.score long [ "sorted" ])).Bm25.document;
        check "without length normalisation they are equal" (let s = Bm25.score ~b:0.0 long [ "sorted" ] in (List.nth s 0).Bm25.score = (List.nth s 1).Bm25.score));
    (let open Gen in
     let word = one_of [ "add"; "mul"; "zero"; "succ"; "comm"; "sorted"; "perm"; "list" ] in
     property ~count:200 "P04-04-02 scores are positive, fall along the ranking, and are the same on every run" (list ~max:5 word) (fun words ->
         let a = Bm25.score (Lazy.force index) words and b = Bm25.score (Lazy.force index) words in
         a = b && List.for_all (fun (s : Bm25.scored) -> s.Bm25.score > 0.0) a
         && List.map (fun (s : Bm25.scored) -> s.Bm25.score) a = List.sort (fun x y -> compare y x) (List.map (fun (s : Bm25.scored) -> s.Bm25.score) a))) ]

let without_times = function
  | Json.Obj members ->
    Json.Obj
      (List.map
         (function
           | "results", Json.List results ->
             ("results", Json.List (List.map (function Json.Obj r -> Json.Obj (List.filter (fun (k, _) -> k <> "elapsed_ms") r) | other -> other) results))
           | member -> member)
         members)
  | other -> other

let entry_tests =
  [ case "P04-05-01 one function asks the retrievers and merges their answers by rank" (fun () ->
        equal_string "words of a statement in prose" "Maithic.Algo.Sorted.tail" (List.hd (names (search "the tail of a sorted list")));
        equal_string "notation finds the statement" "Bad_Name" (List.hd (names (search "a + b = b + a")));
        let by_name = List.hd (search "insertionSort_perm") in
        equal_string "a name finds its declaration" "Maithic.Algo.insertionSort_perm" by_name.Decl_search.document.Decl_document.name;
        check "and both retrievers say so" (List.map fst by_name.Decl_search.found_by = [ Decl_search.Lexical; Decl_search.Name ]);
        let hits = search "sorted" in
        equal_int "ten hits unless asked otherwise" 10 (List.length hits);
        equal_int "each declaration once" 10 (List.length (List.sort_uniq compare (names hits)));
        check "scores fall" (let s = List.map (fun (h : Decl_search.hit) -> h.Decl_search.score) hits in s = List.sort (fun a b -> compare b a) s);
        check "nothing for a query nobody answers" (search "cohomology" = []);
        contains "which is said in words" (Decl_search.render []) "nothing was found";
        let shown = Decl_search.render (search ~options:{ Decl_search.default_options with Decl_search.limit = 1 } "tail of a sorted list") in
        contains "a hit names the declaration, its kind and its place" shown " 1. Maithic.Algo.Sorted.tail  theorem  lean/Maithic/Algo/Spec/Sorted.lean:";
        contains "and who found it" shown "found by lexical (rank 1); words: tail sorted list");
    case "P04-05-01 options choose the retrievers, the kinds, the modules and the number of hits" (fun () ->
        let only r = { Decl_search.default_options with Decl_search.retrievers = [ r ] } in
        equal_strings "the name retriever takes a query for a part of a name" [ "Maithic.Algo.linearSearch"; "Maithic.Algo.linearSearch_none"; "Maithic.Algo.linearSearch_some" ]
          (names (search ~options:{ (only Decl_search.Name) with Decl_search.limit = 3 } "linearSearch"));
        check "and has nothing to say about a sentence" (search ~options:(only Decl_search.Name) "the tail of a sorted list" = []);
        let definitions = search ~options:{ Decl_search.default_options with Decl_search.kinds = [ "definition" ] } "sorted" in
        check "only definitions" (definitions <> [] && List.for_all (fun (h : Decl_search.hit) -> h.Decl_search.document.Decl_document.kind = "definition") definitions);
        let of_module = search ~options:{ Decl_search.default_options with Decl_search.module_prefix = Some "Maithic.Algo.Sort.Linear" } "sorted list element" in
        check "only one module" (of_module <> [] && List.for_all (fun (h : Decl_search.hit) -> h.Decl_search.document.Decl_document.module_ = "Maithic.Algo.Sort.Linear") of_module);
        equal_int "at most two" 2 (List.length (search ~options:{ Decl_search.default_options with Decl_search.limit = 2 } "sorted"));
        check "the names of the retrievers" (List.map Decl_search.retriever_of_name [ "lexical"; "name"; "lean"; "semantic" ] = [ Some Decl_search.Lexical; Some Decl_search.Name; Some Decl_search.Lean; None ]));
    case "P04-05-01 Lean is one more retriever, and the others answer when it cannot" (fun () ->
        let with_lean = { Decl_search.default_options with Decl_search.retrievers = [ Decl_search.Lexical; Decl_search.Name; Decl_search.Lean ] } in
        let core = { (plain "Nat.succ_mul" "∀ (n m : Nat), n.succ * m = n * m + m" "") with Decl_document.module_ = ""; file = "" } in
        let asked = ref [] in
        let lean query ~limit =
          asked := (query, limit) :: !asked;
          Ok [ core; document "double_eq" ]
        in
        let hits, note = Decl_search.search ~options:with_lean ~lean (Lazy.force index) "succ mul" in
        check "Lean was asked the query" (!asked = [ ("succ mul", 10) ]);
        check "no trouble" (note = None);
        check "what only Lean knows is among the hits" (List.mem "Nat.succ_mul" (names hits));
        let both = List.find (fun (h : Decl_search.hit) -> h.Decl_search.document.Decl_document.name = "double_eq") hits in
        check "what two retrievers found is listed once, with both" (List.map fst both.Decl_search.found_by = [ Decl_search.Lexical; Decl_search.Lean ]);
        equal_string "and it comes first" "double_eq" (List.hd (names hits));
        contains "a declaration of the libraries has no file" (Decl_search.render hits) "Nat.succ_mul  theorem  (in Lean's scope)";
        let hits, note = Decl_search.search ~options:with_lean ~lean:(fun _ ~limit:_ -> Error "the Bridge is gone") (Lazy.force index) "double" in
        check "the others still answer" (List.hd (names hits) = "double");
        contains "and the reason is given" (Option.get note) "Lean could not be asked: the Bridge is gone";
        let _, note = Decl_search.search ~options:with_lean (Lazy.force index) "double" in
        contains "without a Bridge" (Option.get note) "no Bridge is at hand";
        let json = Decl_search.to_json ~query:"double" ~options:with_lean ~declarations:41 ?note hits in
        equal_strings "a document of the schema" [] (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/search/results.schema.json")) json)));
    case "P04-05-01 on the query set of the repository the declaration that was meant is among the first five" (fun () ->
        let queryset = Support.json "fixtures/search/queries.json" in
        let rankings = is_ok "rankings" (Decl_search.rankings (Lazy.force index) queryset) in
        equal_strings "a document of the schema" []
          (List.map Json_schema.error_to_string (Json_schema.validate (Json_schema.of_json (Support.json "schemas/search/rankings.schema.json")) rankings));
        check "the rankings the repository keeps" (Json.equal (without_times rankings) (without_times (Support.json "fixtures/search/rankings-merged.json")));
        let relevant q = List.map (function Json.String id -> id | other -> Json.string_or "" "id" other) (Json.list_or_empty "relevant" q) in
        let wanted = List.map (fun q -> (Json.string_or "" "id" q, relevant q)) (Json.list_or_empty "queries" queryset) in
        let best result =
          let ranked = Json.strings (Json.List (Json.list_or_empty "ranked" result)) in
          let positions = List.filter_map (fun id -> Option.map (fun i -> i + 1) (List.find_index (fun r -> r = id) ranked)) (List.assoc (Json.string_or "" "query" result) wanted) in
          List.fold_left min max_int positions
        in
        let ranks = List.map best (Json.list_or_empty "results" rankings) in
        equal_int "sixteen queries" 16 (List.length ranks);
        check "every one is answered within the first five" (List.for_all (fun r -> r <= 5) ranks);
        check "and at least fourteen at the first rank" (List.length (List.filter (fun r -> r = 1) ranks) >= 14);
        equal_string "the engine names its retrievers" "maithic-lexical+name" (Json.string_or "" "engine" rankings);
        contains "an empty query set" (is_error "refused" (Decl_search.rankings (Lazy.force index) (Json.Obj []))) "no query") ]

let () = run "lookup" [ ("documents", document_tests); ("index", index_tests); ("scoring", scoring_tests); ("entry point", entry_tests) ]
