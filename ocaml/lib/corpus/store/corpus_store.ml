(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema

type shard = { kind : Corpus_record.kind; file : string; records : int; sha256 : string }

type manifest = {
  name : string;
  version : string;
  sources : Json.t list;
  counts : (Corpus_record.kind * int) list;
  shards : shard list;
}

let ( let* ) = Result.bind
let count_of counts kind = Option.value ~default:0 (List.assoc_opt kind counts)

let manifest_to_json m =
  Json.Obj
    [ ("schema", Json.String Corpus_schema.manifest_name); ("name", Json.String m.name); ("version", Json.String m.version);
      ("sources", Json.List m.sources);
      ("records", Json.Obj (List.map (fun kind -> (Corpus_record.kind_to_string kind, Json.Int (count_of m.counts kind))) Corpus_record.kinds));
      ( "shards",
        Json.List
          (List.map
             (fun s ->
               Json.Obj
                 [ ("kind", Json.String (Corpus_record.kind_to_string s.kind)); ("file", Json.String s.file); ("records", Json.Int s.records);
                   ("sha256", Json.String s.sha256) ])
             m.shards) ) ]

let manifest_schema = lazy (Json_schema.of_json Corpus_schema.manifest)
let record_schemas = lazy (List.map (fun kind -> (kind, Json_schema.of_json (Corpus_record.schema_of kind))) Corpus_record.kinds)

let manifest_of_json j =
  let* () = Result.map_error (fun e -> "the manifest does not follow its schema:\n" ^ e) (Json_schema.conforms (Lazy.force manifest_schema) j) in
  let records = Option.value ~default:(Json.Obj []) (Json.member "records" j) in
  let shards =
    List.filter_map
      (fun s ->
        Option.map
          (fun kind -> { kind; file = Json.string_or "" "file" s; records = Json.int_or 0 "records" s; sha256 = Json.string_or "" "sha256" s })
          (Corpus_record.kind_of_string (Json.string_or "" "kind" s)))
      (Json.list_or_empty "shards" j)
  in
  Ok
    { name = Json.string_or "" "name" j; version = Json.string_or "" "version" j; sources = Json.list_or_empty "sources" j;
      counts = List.map (fun kind -> (kind, Json.int_or 0 (Corpus_record.kind_to_string kind) records)) Corpus_record.kinds; shards }

let validate kind record =
  let* () =
    Result.map_error
      (fun e -> Printf.sprintf "a %s record does not follow its schema:\n%s" (Corpus_record.kind_to_string kind) e)
      (Json_schema.conforms (List.assoc kind (Lazy.force record_schemas)) record)
  in
  let stated = Json.string_or "" "id" record in
  if stated = Corpus_record.id_of record then Ok ()
  else Error (Printf.sprintf "a %s record carries the identifier %s, and its content gives %s" (Corpus_record.kind_to_string kind) stated (Corpus_record.id_of record))

(* Splits a list into pieces of at most [n]. *)
let rec pieces n = function
  | [] -> []
  | items ->
    let rec take k acc rest = match rest with x :: tail when k > 0 -> take (k - 1) (x :: acc) tail | _ -> (List.rev acc, rest) in
    let first, rest = take n [] items in
    first :: pieces n rest

let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

(* Whether a file name is one this module gives a shard: a kind, a dash,
   sixteen hexadecimal digits and the extension. *)
let shard_named file =
  List.exists
    (fun kind ->
      let prefix = Corpus_record.kind_to_string kind ^ "-" in
      let n = String.length prefix in
      String.length file = n + 16 + 6
      && String.sub file 0 n = prefix
      && String.sub file (n + 16) 6 = ".jsonl"
      && String.for_all is_hex (String.sub file n 16))
    Corpus_record.kinds

(* The files under [shards/] that a manifest does not list. *)
let unlisted dir shards =
  List.filter
    (fun file -> not (List.exists (fun s -> s.file = "shards/" ^ file) shards))
    (List.sort compare (Fs.list_dir Fs.(dir / "shards")))

let write ?(shard_size = 5000) ~dir ~name ~version ~sources records =
  let* () = if shard_size >= 1 then Ok () else Error "a shard holds one record or more" in
  let* () = List.fold_left (fun acc (kind, record) -> Result.bind acc (fun () -> validate kind record)) (Ok ()) records in
  let seen = Hashtbl.create 1024 in
  let unique =
    List.filter
      (fun (_, record) ->
        let id = Json.string_or "" "id" record in
        if Hashtbl.mem seen id then false
        else begin
          Hashtbl.replace seen id ();
          true
        end)
      records
  in
  let* () = Fs.mkdir_p Fs.(dir / "shards") in
  let* shards =
    List.fold_left
      (fun acc kind ->
        let* done_ = acc in
        let mine = List.filter_map (fun (k, record) -> if k = kind then Some record else None) unique in
        List.fold_left
          (fun acc piece ->
            let* done_ = acc in
            let text = String.concat "" (List.map (fun record -> Json.to_string record ^ "\n") piece) in
            let sha256 = Sha256.hex text in
            let file = Printf.sprintf "shards/%s-%s.jsonl" (Corpus_record.kind_to_string kind) (String.sub sha256 0 16) in
            let* () = Fs.write Fs.(dir / file) text in
            Ok (done_ @ [ { kind; file; records = List.length piece; sha256 } ]))
          (Ok done_) (pieces shard_size mine))
      (Ok []) Corpus_record.kinds
  in
  let counts = List.map (fun kind -> (kind, List.length (List.filter (fun (k, _) -> k = kind) unique))) Corpus_record.kinds in
  let manifest = { name; version; sources; counts; shards } in
  let* () = Fs.write Fs.(dir / "manifest.json") (Json.pretty (manifest_to_json manifest) ^ "\n") in
  List.iter (fun file -> if shard_named file then Fs.remove Fs.(dir / "shards" / file)) (unlisted dir shards);
  Ok manifest

let read_manifest dir =
  let* text = Fs.read Fs.(dir / "manifest.json") in
  let* j = Json.parse text in
  manifest_of_json j

let records_of_text text = List.filter (fun line -> String.trim line <> "") (String.split_on_char '\n' text)

let load dir kind =
  let* manifest = read_manifest dir in
  List.fold_left
    (fun acc shard ->
      let* done_ = acc in
      if shard.kind <> kind then Ok done_
      else
        let* text = Fs.read Fs.(dir / shard.file) in
        if Sha256.hex text <> shard.sha256 then Error (Printf.sprintf "%s does not have the hash the manifest states" shard.file)
        else
          let* records =
            List.fold_left
              (fun acc line ->
                let* done_ = acc in
                let* record = Json.parse line in
                Ok (record :: done_))
              (Ok []) (records_of_text text)
          in
          Ok (done_ @ List.rev records))
    (Ok []) manifest.shards

let verify dir =
  match read_manifest dir with
  | Error e -> [ e ]
  | Ok manifest ->
    let per_shard =
      List.concat_map
        (fun shard ->
          match Fs.read Fs.(dir / shard.file) with
          | Error _ -> [ Printf.sprintf "%s is missing" shard.file ]
          | Ok text ->
            let lines = records_of_text text in
            (if Sha256.hex text <> shard.sha256 then [ Printf.sprintf "%s does not have the hash the manifest states" shard.file ] else [])
            @ (if List.length lines <> shard.records then
                 [ Printf.sprintf "%s holds %d record(s), and the manifest states %d" shard.file (List.length lines) shard.records ]
               else [])
            @ List.filter_map
                (fun line ->
                  match Result.bind (Json.parse line) (validate shard.kind) with
                  | Ok () -> None
                  | Error e -> Some (Printf.sprintf "%s: %s" shard.file (List.hd (String.split_on_char '\n' e))))
                lines)
        manifest.shards
    in
    let totals =
      List.filter_map
        (fun kind ->
          let in_shards = List.fold_left (fun acc s -> if s.kind = kind then acc + s.records else acc) 0 manifest.shards in
          if in_shards <> count_of manifest.counts kind then
            Some (Printf.sprintf "the manifest counts %d %s record(s), and its shards hold %d" (count_of manifest.counts kind) (Corpus_record.kind_to_string kind) in_shards)
          else None)
        Corpus_record.kinds
    in
    per_shard @ totals @ List.map (fun file -> Printf.sprintf "shards/%s is not in the manifest" file) (unlisted dir manifest.shards)
