(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

type outcome = { file : string; result : string; errors : int; theorems : int; steps : int; reason : string }

type record = {
  repository : string;
  revision : string;
  toolchain : string;
  files : outcome list;
  dataset : string;
  manifest : Corpus_store.manifest;
}

let revision_of dir =
  let rec up dir depth =
    let git = Fs.(dir / ".git") in
    if Fs.is_dir git then Some git
    else
      let parent = Fs.dirname dir in
      if parent = dir || depth > 32 then None else up parent (depth + 1)
  in
  let start = if Filename.is_relative dir then Fs.(Sys.getcwd () / dir) else dir in
  match up start 0 with
  | None -> "unknown"
  | Some git ->
    (match Fs.read Fs.(git / "HEAD") with
     | Error _ -> "unknown"
     | Ok head ->
       let head = String.trim head in
       if Text.starts_with ~prefix:"ref: " head then
         match Fs.read Fs.(git / String.sub head 5 (String.length head - 5)) with
         | Ok commit -> String.trim commit
         | Error _ -> "unknown"
       else head)

let trace ~tracer files =
  let extractions, outcomes =
    List.fold_left
      (fun (extractions, outcomes) file ->
        match tracer file with
        | Error reason -> (extractions, { file; result = "failed"; errors = 0; theorems = 0; steps = 0; reason } :: outcomes)
        | Ok (e : Extraction.t) ->
          let theorems = List.length (List.filter (fun (d : Extraction.declaration) -> d.Extraction.kind = "theorem") e.Extraction.declarations) in
          ( e :: extractions,
            { file; result = (if e.Extraction.ok then "ok" else "errors"); errors = e.Extraction.errors; theorems;
              steps = List.length e.Extraction.steps; reason = "" }
            :: outcomes ))
      ([], []) files
  in
  (List.rev extractions, List.rev outcomes)

let record_to_json r =
  Json.Obj
    [ ("schema", Json.String Corpus_schema.harvest_name); ("repository", Json.String r.repository); ("revision", Json.String r.revision);
      ("toolchain", Json.String r.toolchain);
      ( "files",
        Json.List
          (List.map
             (fun o ->
               Json.Obj
                 ([ ("file", Json.String o.file); ("outcome", Json.String o.result); ("errors", Json.Int o.errors);
                    ("theorems", Json.Int o.theorems); ("steps", Json.Int o.steps) ]
                 @ if o.reason = "" then [] else [ ("reason", Json.String o.reason) ]))
             r.files) );
      ("dataset", Json.String r.dataset); ("manifest", Json.String Fs.(r.dataset / "manifest.json"));
      ( "records",
        Json.Obj
          (List.map
             (fun kind -> (Corpus_record.kind_to_string kind, Json.Int (Option.value ~default:0 (List.assoc_opt kind r.manifest.Corpus_store.counts))))
             Corpus_record.kinds) ) ]

let harvest ?shard_size ?revision ~tracer ~repository ~toolchain ~name ~version ~dir files =
  let extractions, outcomes = trace ~tracer files in
  let revision = match revision with Some stated -> stated | None -> revision_of repository in
  let source =
    Json.Obj
      [ ("repository", Json.String repository); ("revision", Json.String revision); ("toolchain", Json.String toolchain);
        ("files", Json.Int (List.length files)) ]
  in
  let records = List.concat_map Corpus_record.of_extraction extractions in
  Result.bind (Corpus_store.write ?shard_size ~dir ~name ~version ~sources:[ source ] records) (fun manifest ->
      let record = { repository; revision; toolchain; files = outcomes; dataset = dir; manifest } in
      Result.map (fun () -> record) (Fs.write Fs.(dir / "harvest.json") (Json.pretty (record_to_json record) ^ "\n")))

