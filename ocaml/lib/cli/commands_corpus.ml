(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [corpus] commands: datasets of theorems, tactic steps and premises. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_corpus
open Cli_args
open Commands

let corpus_fault number ~exit message = Fault.make ~area:"CORPUS" ~number ~exit message

let counts_line counts =
  String.concat ", "
    (List.map
       (fun kind -> Printf.sprintf "%d %s record(s)" (Option.value ~default:0 (List.assoc_opt kind counts)) (Corpus_record.kind_to_string kind))
       Corpus_record.kinds)

let harvest_cmd =
  { path = [ "corpus"; "harvest" ]; summary = "Trace the proofs of Lean files and write them as a dataset."; synopsis = "PATH...";
    details =
      "Every Lean file among the paths is elaborated with tracing: each theorem with its proof, each tactic with the goals \
       before and after it, the constants each proof term refers to, and a state-and-tactic pair for every smallest step. The \
       records are checked against their schemas and written as shards of JSON lines with a manifest of hashes. A file that \
       cannot be traced is noted in the record of the harvest and left out.";
    options =
      [ value "project" "DIR" "A Lake project whose modules the files may import."; value "out" "DIR" "The dataset directory (dataset).";
        value "name" "NAME" "The name of the dataset (the name of the output directory).";
        value "dataset-version" "V" "The version of the dataset (1)."; value "shard-size" "N" "Records in a shard at most (5000).";
        value "revision" "TEXT" "The revision of the sources to record (the commit read from version control)." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* files = Result.map_error usage_fault (Lean_files.expand parsed.positional) in
            let* () = if files = [] then Error (usage_fault "no Lean file among the paths") else Ok () in
            let dir = Option.value ~default:"dataset" (get parsed "out") in
            let* shard_size =
              match get parsed "shard-size" with
              | None -> Ok 5000
              | Some text -> (match int_of_string_opt text with Some n when n >= 1 -> Ok n | _ -> Error (usage_fault "--shard-size needs a number, one or more"))
            in
            let project = match Config.string ctx.config "bridge.project" with "" -> None | p -> Some p in
            let toolchain = ref "" in
            let tracer file =
              let ( let* ) = Result.bind in
              let* text = Fs.read file in
              let* bridge, _ = Result.map_error (fun (f : Fault.t) -> f.Fault.message) (start_bridge ctx) in
              toolchain := (Bridge_client.handshake bridge).Bridge_types.lean;
              let answer =
                Result.map_error Bridge_error.to_string
                  (Bridge_client.extract ~file ~module_name:(Lean_files.module_of ?project file) ~trace:true ~limits:(limits ctx) bridge text)
              in
              Bridge_client.shutdown bridge;
              if ctx.verbose then Printf.eprintf "%s: %s\n%!" file (match answer with Ok _ -> "traced" | Error e -> e);
              answer
            in
            let repository = match project with Some p -> p | None -> "." in
            (* The toolchain is known once the first file has been traced, so the harvest is run
               and its record completed afterwards. *)
            let extractions, outcomes = Corpus_harvest.trace ~tracer files in
            let replay = Hashtbl.create 64 in
            List.iter (fun (e : Extraction.t) -> Hashtbl.replace replay e.Extraction.file e) extractions;
            let reasons = List.map (fun (o : Corpus_harvest.outcome) -> (o.Corpus_harvest.file, o.Corpus_harvest.reason)) outcomes in
            let again file = match Hashtbl.find_opt replay file with Some e -> Ok e | None -> Error (Option.value ~default:"" (List.assoc_opt file reasons)) in
            let* record =
              Result.map_error (corpus_fault 1 ~exit:Exit_code.Failure)
                (Corpus_harvest.harvest ~shard_size ?revision:(get parsed "revision") ~tracer:again ~repository ~toolchain:!toolchain
                   ~name:(Option.value ~default:(Fs.basename dir) (get parsed "name"))
                   ~version:(Option.value ~default:"1" (get parsed "dataset-version"))
                   ~dir files)
            in
            let failed = List.filter (fun (o : Corpus_harvest.outcome) -> o.Corpus_harvest.result = "failed") record.Corpus_harvest.files in
            if ctx.json then Term.print_json (Corpus_harvest.record_to_json record)
            else begin
              Printf.printf "harvested %d file(s) at revision %s:\n  %s\n" (List.length files) record.Corpus_harvest.revision
                (counts_line record.Corpus_harvest.manifest.Corpus_store.counts);
              List.iter (fun (o : Corpus_harvest.outcome) -> Printf.printf "  not traced: %s (%s)\n" o.Corpus_harvest.file o.Corpus_harvest.reason) failed;
              Printf.printf "wrote %s with %d shard(s)\n" Fs.(dir / "manifest.json") (List.length record.Corpus_harvest.manifest.Corpus_store.shards)
            end;
            Ok 0)) }

let verify_cmd =
  { path = [ "corpus"; "verify" ]; summary = "Check a dataset against its manifest and its schemas."; synopsis = "DIR";
    details = "Every shard must be there with the hash and the number of records the manifest states, and every record must follow the schema of its kind.";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* dir = match parsed.positional with [ dir ] -> Ok dir | _ -> Error (usage_fault "corpus verify needs exactly one DIR") in
            let problems = Corpus_store.verify dir in
            if ctx.json then
              Term.print_json (Json.Obj [ ("dataset", Json.String dir); ("sound", Json.Bool (problems = [])); ("problems", Json.List (List.map (fun p -> Json.String p) problems)) ])
            else if problems = [] then begin
              match Corpus_store.read_manifest dir with
              | Ok m -> Printf.printf "%s %s is sound: %s in %d shard(s)\n" m.Corpus_store.name m.Corpus_store.version (counts_line m.Corpus_store.counts) (List.length m.Corpus_store.shards)
              | Error e -> print_endline e
            end
            else List.iter (fun p -> Printf.printf "%s\n" p) problems;
            Ok (if problems = [] then 0 else Exit_code.to_int Exit_code.Not_verified))) }

let all = [ harvest_cmd; verify_cmd ]
