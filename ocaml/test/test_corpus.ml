(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Tests of the corpus: records, the store and the harvest. The tracer is stood in for by the
   extractions kept under fixtures/intel, so no Lean is involved. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_corpus
open Testkit

let extraction name = is_ok name (Extraction.of_json (Support.json ("fixtures/intel/extractions/" ^ name ^ ".json")))
let demo = lazy (extraction "fixtures.intel.lean.Demo")
let library = lazy (List.map extraction [ "Maithic.Algo.Sort.Insertion"; "Maithic.Algo.Sort.Linear"; "Maithic.Algo.Spec.Sorted" ])
let records_of e = Corpus_record.of_extraction e
let of_kind kind records = List.filter_map (fun (k, r) -> if k = kind then Some r else None) records
let field name record = Json.string_or "" name record
let named name records = List.find (fun r -> field "name" r = name || field "theorem" r = name) records
let temp () = Fs.temp_dir "maithic-corpus"

let write_demo ?shard_size dir =
  is_ok "write" (Corpus_store.write ?shard_size ~dir ~name:"demo" ~version:"1" ~sources:[] (records_of (Lazy.force demo)))

let record_tests =
  [ case "P11-04-01 theorems, steps, premises and pairs are records with schemas of their own" (fun () ->
        let records = records_of (Lazy.force demo) in
        List.iter
          (fun (kind, record) -> ignore (is_ok (Corpus_record.kind_to_string kind) (Corpus_store.validate kind record)))
          records;
        equal_strings "the theorems of the file" [ "double_eq"; "Bad_Name"; "open_one"; "uses_open"; "by_shortcut"; "term_gap" ]
          (List.map (field "name") (of_kind Corpus_record.Theorem records));
        let restated = named "Bad_Name" (of_kind Corpus_record.Theorem records) in
        equal_string "statement" "\xE2\x88\x80 (a b : Nat), a + b = b + a" (field "statement" restated);
        equal_string "proof" "Nat.add_comm a b" (field "proof" restated);
        equal_int "a term proof has no steps" 0 (Json.int_or (-1) "steps" restated);
        equal_int "and two premises" 2 (Json.int_or (-1) "premises" restated);
        check "complete" (Json.bool_or false "complete" restated);
        check "a theorem that rests on sorry is not complete"
          (not (Json.bool_or true "complete" (named "open_one" (of_kind Corpus_record.Theorem records)))
           && not (Json.bool_or true "complete" (named "uses_open" (of_kind Corpus_record.Theorem records))));
        let steps = List.filter (fun r -> field "theorem" r = "double_eq") (of_kind Corpus_record.Step records) in
        equal_strings "the steps of a proof, numbered within it" [ "0"; "1"; "2"; "3" ] (List.map (fun r -> string_of_int (Json.int_or (-1) "index" r)) steps);
        equal_strings "the smallest ones become pairs" [ "rfl"; "simp [double]"; "omega" ]
          (List.map (field "tactic") (List.filter (fun r -> field "theorem" r = "double_eq") (of_kind Corpus_record.Pair records)));
        let pair = named "double_eq" (of_kind Corpus_record.Pair records) in
        equal_string "a pair holds the state as one text" "case zero\n\xE2\x8A\xA2 double 0 = 2 * 0" (field "state" pair);
        equal_int "and how many goals the tactic left" 0 (Json.int_or (-1) "goals_after" pair);
        equal_strings "a gap is a step of its proof" [ "sorry" ]
          (List.map (field "tactic") (List.filter (fun r -> field "theorem" r = "open_one") (of_kind Corpus_record.Step records)));
        check "and never a pair" (not (List.exists (fun r -> field "theorem" r = "open_one") (of_kind Corpus_record.Pair records)));
        (* Pairs come only from proofs the kernel accepted whole. Were double_eq to rest on an unfinished lemma, its steps
           would still be recorded, and none of them offered as an example of a proof step. *)
        let demo = Lazy.force demo in
        let resting =
          { demo with
            Extraction.declarations =
              List.map
                (fun (d : Extraction.declaration) -> if d.Extraction.name = "double_eq" then { d with Extraction.uses_sorry = true } else d)
                demo.Extraction.declarations }
        in
        let unfinished = Corpus_record.of_extraction resting in
        check "the theorem is marked" (not (Json.bool_or true "complete" (named "double_eq" (of_kind Corpus_record.Theorem unfinished))));
        equal_int "its steps are kept" 4 (List.length (List.filter (fun r -> field "theorem" r = "double_eq") (of_kind Corpus_record.Step unfinished)));
        check "and it gives no pairs" (not (List.exists (fun r -> field "theorem" r = "double_eq") (of_kind Corpus_record.Pair unfinished)));
        equal_strings "premises of a proof"
          [ "Nat"; "Nat.add_comm" ]
          (List.map (field "premise") (List.filter (fun r -> field "theorem" r = "Bad_Name") (of_kind Corpus_record.Premise records)));
        check "definitions are not theorems" (not (List.exists (fun r -> field "name" r = "double") (of_kind Corpus_record.Theorem records)));
        (* The schema files are the schemas the program checks with. *)
        List.iter
          (fun (file, built_in) -> check file (Json.equal built_in (Support.json ("schemas/" ^ file))))
          Corpus_schema.files);
    case "P11-04-03 a record is named by the hash of its content" (fun () ->
        let records = records_of (Lazy.force demo) in
        List.iter
          (fun (_, record) ->
            let without = match record with Json.Obj fields -> Json.Obj (List.remove_assoc "id" fields) | j -> j in
            equal_string "sixteen digits of the SHA-256 of the canonical text"
              (String.sub (Sha256.hex (Json.to_string ~canonical:true without)) 0 16)
              (field "id" record))
          records;
        check "made again, the records are the same" (records = records_of (Lazy.force demo));
        let ids = List.map (fun (_, r) -> field "id" r) records in
        check "different records have different names" (List.length (List.sort_uniq compare ids) = List.length ids);
        let changed = match snd (List.hd records) with Json.Obj fields -> Json.Obj (List.map (fun (k, v) -> if k = "line" then (k, Json.Int 999) else (k, v)) fields) | j -> j in
        check "a record that was changed no longer carries its name" (Result.is_error (Corpus_store.validate Corpus_record.Theorem changed))) ]

let store_tests =
  [ case "P11-04-02 a dataset has a manifest: name, version, sources, counts and hashes" (fun () ->
        let dir = temp () in
        let source = Json.Obj [ ("repository", Json.String "demo"); ("revision", Json.String "abc"); ("toolchain", Json.String "4.36.0"); ("files", Json.Int 1) ] in
        let manifest = is_ok "write" (Corpus_store.write ~dir ~name:"demo" ~version:"2" ~sources:[ source ] (records_of (Lazy.force demo))) in
        let json = Support.json_file Fs.(dir / "manifest.json") in
        let schema = is_ok "schema" (Json_schema.load (Support.path "schemas/corpus/manifest.schema.json")) in
        check "the manifest follows its schema" (Json_schema.conforms schema json = Ok ());
        equal_string "name and version" "demo 2" (field "name" json ^ " " ^ field "version" json);
        check "the source" (Json.list_or_empty "sources" json = [ source ]);
        equal_int "theorems" 6 (List.assoc Corpus_record.Theorem manifest.Corpus_store.counts);
        equal_int "steps" 5 (List.assoc Corpus_record.Step manifest.Corpus_store.counts);
        equal_int "pairs" 3 (List.assoc Corpus_record.Pair manifest.Corpus_store.counts);
        equal_int "a shard for each kind" 4 (List.length manifest.Corpus_store.shards);
        List.iter
          (fun (s : Corpus_store.shard) ->
            let text = is_ok "shard" (Fs.read Fs.(dir / s.Corpus_store.file)) in
            equal_string "the hash of a shard" (Sha256.hex text) s.Corpus_store.sha256;
            equal_int "one record on each line" s.Corpus_store.records (List.length (List.filter (fun l -> l <> "") (String.split_on_char '\n' text))))
          manifest.Corpus_store.shards;
        check "read back, the manifest is the same" (Corpus_store.read_manifest dir = Ok manifest);
        equal_strings "its members, none of which says when it was made" [ "schema"; "name"; "version"; "sources"; "records"; "shards" ]
          (match json with Json.Obj fields -> List.map fst fields | _ -> []);
        Fs.remove_tree dir);
    case "P11-04-03 shards are named by the hash of their content, and equal sources give equal bytes" (fun () ->
        let a = temp () and b = temp () in
        let first = write_demo a and second = write_demo b in
        check "the same manifest" (first = second);
        List.iter
          (fun (s : Corpus_store.shard) ->
            equal_string "the name carries the first digits of the hash"
              (Printf.sprintf "shards/%s-%s.jsonl" (Corpus_record.kind_to_string s.Corpus_store.kind) (String.sub s.Corpus_store.sha256 0 16))
              s.Corpus_store.file;
            check "the same bytes" (Fs.read Fs.(a / s.Corpus_store.file) = Fs.read Fs.(b / s.Corpus_store.file)))
          first.Corpus_store.shards;
        (* A record that is given twice is stored once. *)
        let records = records_of (Lazy.force demo) in
        let twice = is_ok "twice" (Corpus_store.write ~dir:b ~name:"demo" ~version:"1" ~sources:[] (records @ records)) in
        check "stored once" (twice.Corpus_store.counts = first.Corpus_store.counts);
        (* Small shards: every kind is split, and loading puts the records together again. *)
        let small = write_demo ~shard_size:2 a in
        equal_int "six theorems in shards of two" 3
          (List.length (List.filter (fun (s : Corpus_store.shard) -> s.Corpus_store.kind = Corpus_record.Theorem) small.Corpus_store.shards));
        check "loaded in order" (is_ok "load" (Corpus_store.load a Corpus_record.Theorem) = of_kind Corpus_record.Theorem records);
        (* Written into a directory that holds a dataset, the new one replaces it: no shard of the old one stays. *)
        let listed (m : Corpus_store.manifest) = List.sort compare (List.map (fun (s : Corpus_store.shard) -> Fs.basename s.Corpus_store.file) m.Corpus_store.shards) in
        equal_strings "the directory holds the shards of the manifest" (listed small) (List.sort compare (Fs.list_dir Fs.(a / "shards")));
        check "and the dataset is sound" (Corpus_store.verify a = []);
        ignore (is_ok "notes" (Fs.write Fs.(a / "shards" / "NOTES.txt") "kept"));
        let again = write_demo a in
        equal_strings "back to one shard for each kind, and a file that is not a shard is left alone"
          (List.sort compare ("NOTES.txt" :: listed again)) (List.sort compare (Fs.list_dir Fs.(a / "shards")));
        equal_strings "which the check of the dataset points out" [ "shards/NOTES.txt is not in the manifest" ] (Corpus_store.verify a);
        check "a shard of nothing is refused" (Result.is_error (Corpus_store.write ~shard_size:0 ~dir:a ~name:"demo" ~version:"1" ~sources:[] records));
        Fs.remove_tree a;
        Fs.remove_tree b);
    case "P11-04-04 a record that does not follow its schema is refused, and nothing is written" (fun () ->
        let dir = temp () in
        let records = records_of (Lazy.force demo) in
        let broken =
          List.map
            (fun (kind, record) ->
              match kind, record with
              | Corpus_record.Pair, Json.Obj fields -> (kind, Json.Obj (List.remove_assoc "tactic" fields))
              | other -> other)
            records
        in
        contains "why" (is_error "refused" (Corpus_store.write ~dir ~name:"demo" ~version:"1" ~sources:[] broken)) "a pair record does not follow its schema";
        check "no manifest" (not (Fs.exists Fs.(dir / "manifest.json")));
        let odd = List.map (fun (kind, record) -> match record with Json.Obj fields -> (kind, Json.Obj (fields @ [ ("extra", Json.Int 1) ])) | _ -> (kind, record)) records in
        check "a member the schema does not know is refused too" (Result.is_error (Corpus_store.write ~dir ~name:"demo" ~version:"1" ~sources:[] odd));
        (* A dataset that was changed after it was written does not pass. *)
        let manifest = write_demo dir in
        check "sound as written" (Corpus_store.verify dir = []);
        let shard = List.hd manifest.Corpus_store.shards in
        let text = is_ok "read" (Fs.read Fs.(dir / shard.Corpus_store.file)) in
        ignore (is_ok "tamper" (Fs.write Fs.(dir / shard.Corpus_store.file) (Text.replace_all ~sub:"double_eq" ~by:"double_xx" text)));
        let problems = String.concat "\n" (Corpus_store.verify dir) in
        contains "the hash" problems "does not have the hash the manifest states";
        contains "the record" problems "carries the identifier";
        check "loading stops at the changed shard" (Result.is_error (Corpus_store.load dir shard.Corpus_store.kind));
        Fs.remove (Fs.(dir / shard.Corpus_store.file));
        contains "a missing shard" (String.concat "\n" (Corpus_store.verify dir)) "is missing";
        Fs.remove_tree dir) ]

let harvest_tests =
  let known = lazy (List.map (fun (e : Extraction.t) -> (e.Extraction.file, e)) (Lazy.force demo :: Lazy.force library)) in
  let tracer file =
    match List.assoc_opt file (Lazy.force known) with
    | Some e -> Ok e
    | None -> Error "the Bridge did not answer"
  in
  [ case "P11-03-01 the tracer is run over every file, and a file it fails on is noted and left out" (fun () ->
        let files = [ "lean/Maithic/Algo/Spec/Sorted.lean"; "missing/Nowhere.lean"; "fixtures/intel/lean/Demo.lean" ] in
        let extractions, outcomes = Corpus_harvest.trace ~tracer files in
        equal_strings "what was traced, in order" [ "lean/Maithic/Algo/Spec/Sorted.lean"; "fixtures/intel/lean/Demo.lean" ]
          (List.map (fun (e : Extraction.t) -> e.Extraction.file) extractions);
        equal_strings "an outcome for every file" [ "ok"; "failed"; "ok" ] (List.map (fun (o : Corpus_harvest.outcome) -> o.Corpus_harvest.result) outcomes);
        let failed = List.nth outcomes 1 in
        equal_string "with the reason" "the Bridge did not answer" failed.Corpus_harvest.reason;
        let last = List.nth outcomes 2 in
        equal_int "theorems of the file" 6 last.Corpus_harvest.theorems;
        equal_int "steps of the file" 5 last.Corpus_harvest.steps;
        (* A file that elaborated with errors is traced as far as it went, and marked. *)
        let faulty file = Result.map (fun (e : Extraction.t) -> { e with Extraction.ok = false; errors = 2 }) (tracer file) in
        let _, marked = Corpus_harvest.trace ~tracer:faulty [ "fixtures/intel/lean/Demo.lean" ] in
        check "errors are counted" ((List.hd marked).Corpus_harvest.result = "errors" && (List.hd marked).Corpus_harvest.errors = 2));
    case "P11-03-02 a harvest leaves a dataset and a record of where it came from" (fun () ->
        let dir = temp () in
        let files = [ "fixtures/intel/lean/Demo.lean"; "lean/Maithic/Algo/Sort/Insertion.lean"; "lean/Maithic/Algo/Sort/Linear.lean"; "lean/Maithic/Algo/Spec/Sorted.lean"; "gone.lean" ] in
        let record = is_ok "harvest" (Corpus_harvest.harvest ~tracer ~repository:dir ~toolchain:"4.36.0" ~name:"demo" ~version:"1" ~dir files) in
        let json = Support.json_file Fs.(dir / "harvest.json") in
        let schema = is_ok "schema" (Json_schema.load (Support.path "schemas/corpus/harvest.schema.json")) in
        check "the record follows its schema" (Json_schema.conforms schema json = Ok ());
        check "it is the record that was returned" (Json.equal json (Corpus_harvest.record_to_json record));
        equal_string "toolchain" "4.36.0" (field "toolchain" json);
        equal_string "a directory that is under no version control has no revision" "unknown" (field "revision" json);
        let stated = is_ok "stated" (Corpus_harvest.harvest ~revision:"v1.2.0" ~tracer ~repository:dir ~toolchain:"4.36.0" ~name:"demo" ~version:"1" ~dir files) in
        equal_string "unless one is stated" "v1.2.0" stated.Corpus_harvest.revision;
        check "which the manifest records as well"
          (List.map (Json.string_or "" "revision") stated.Corpus_harvest.manifest.Corpus_store.sources = [ "v1.2.0" ]);
        equal_string "where the dataset is" Fs.(dir / "manifest.json") (field "manifest" json);
        equal_int "every file has an outcome" 5 (List.length (Json.list_or_empty "files" json));
        equal_int "theorems of the four files that were traced" 28 (Json.int_or 0 "theorem" (Option.get (Json.member "records" json)));
        check "the dataset is sound" (Corpus_store.verify dir = []);
        (* The shards are the ones kept in the repository: same sources, same bytes. *)
        let kept = is_ok "kept" (Corpus_store.read_manifest (Support.path "fixtures/corpus/demo")) in
        check "the same shards as the dataset in the repository" (record.Corpus_harvest.manifest.Corpus_store.shards = kept.Corpus_store.shards);
        Fs.remove_tree dir);
    case "P11-03-02 the revision is read from the version control directory" (fun () ->
        let dir = temp () in
        ignore (is_ok "mkdir" (Fs.mkdir_p Fs.(dir / ".git" / "refs" / "heads")));
        ignore (is_ok "mkdir" (Fs.mkdir_p Fs.(dir / "lean" / "Deep")));
        ignore (is_ok "head" (Fs.write Fs.(dir / ".git" / "HEAD") "ref: refs/heads/main\n"));
        ignore (is_ok "ref" (Fs.write Fs.(dir / ".git" / "refs" / "heads" / "main") "0123456789abcdef0123456789abcdef01234567\n"));
        equal_string "through the branch" "0123456789abcdef0123456789abcdef01234567" (Corpus_harvest.revision_of dir);
        equal_string "from a directory below" "0123456789abcdef0123456789abcdef01234567" (Corpus_harvest.revision_of Fs.(dir / "lean" / "Deep"));
        ignore (is_ok "head" (Fs.write Fs.(dir / ".git" / "HEAD") "fedcba9876543210fedcba9876543210fedcba98\n"));
        equal_string "or a commit that is checked out directly" "fedcba9876543210fedcba9876543210fedcba98" (Corpus_harvest.revision_of dir);
        ignore (is_ok "head" (Fs.write Fs.(dir / ".git" / "HEAD") "ref: refs/heads/absent\n"));
        equal_string "a branch with no commit" "unknown" (Corpus_harvest.revision_of dir);
        Fs.remove_tree dir) ]

let () = Testkit.run "corpus" [ ("records", record_tests); ("store", store_tests); ("harvest", harvest_tests) ]
