(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The contracts between the languages, checked against the files that
    cross them.

    Every document that one part of Maithic writes for another names its
    schema, such as [maithic.prove.run/1], and the schema is a file under
    [schemas/]. This check reads every JSON document of the repository that
    names a schema and validates it, so that a fixture written by Julia, a
    result written by the OCaml command and a session read by the Lean tests
    are all held to the published shape. It also lints the schemas
    themselves and validates every recorded Bridge response. *)

open Maithic_base
open Maithic_schema

(** The file of a schema name: [maithic.<area>.<kind>/<major>] is
    [schemas/<area>/<kind>.schema.json], with [.v<major>] before
    [.schema.json] from the second major version on. Dots inside the kind
    become dashes. [None] when the name does not have that form. *)
let schema_file name =
  match String.split_on_char '/' name with
  | [ dotted; major ] when int_of_string_opt major <> None && int_of_string major >= 1 ->
    (match String.split_on_char '.' dotted with
     | "maithic" :: area :: (_ :: _ as kind) when List.for_all (fun part -> part <> "") (area :: kind) ->
       let suffix = if major = "1" then "" else ".v" ^ major in
       Some (Printf.sprintf "schemas/%s/%s%s.schema.json" area (String.concat "-" kind) suffix)
     | _ -> None)
  | _ -> None

type schemas = { root : string; loaded : (string, (Json_schema.t, string) result) Hashtbl.t }

let schemas root = { root; loaded = Hashtbl.create 16 }

let load (s : schemas) rel =
  match Hashtbl.find_opt s.loaded rel with
  | Some result -> result
  | None ->
    let result = Json_schema.load Fs.(s.root / rel) in
    Hashtbl.replace s.loaded rel result;
    result

(** The problems of one instance against the schema file [rel], each
    prefixed with [what]. *)
let against (s : schemas) ~what rel instance =
  match load s rel with
  | Error e -> [ Printf.sprintf "%s: the schema %s cannot be read: %s" what rel e ]
  | Ok schema -> List.map (fun e -> Printf.sprintf "%s: %s" what (Json_schema.error_to_string e)) (Json_schema.validate schema instance)

(** Validates a document against the schema it names. A document that names
    no Maithic schema has no contract to keep and gives no problem. *)
let document (s : schemas) ~what instance =
  match Option.bind (Json.member "schema" instance) Json.string_opt with
  | Some name when Text.starts_with ~prefix:"maithic." name ->
    (match schema_file name with
     | None -> [ Printf.sprintf "%s: the schema name %S is not of the form maithic.<area>.<kind>/<major>" what name ]
     | Some rel ->
       if Fs.exists Fs.(s.root / rel) then against s ~what rel instance
       else [ Printf.sprintf "%s: names the schema %s, which is not published (%s is missing)" what name rel ])
  | _ -> []

(** Problems in the schema files themselves. *)
let lint_schemas (s : schemas) files =
  List.concat_map
    (fun rel ->
      match load s rel with
      | Error e -> [ Printf.sprintf "%s: %s" rel e ]
      | Ok schema ->
        let raw = match Fs.read Fs.(s.root / rel) with Ok text -> Result.value (Json.parse text) ~default:Json.Null | Error _ -> Json.Null in
        let described key = match Option.bind (Json.member key raw) Json.string_opt with Some text -> String.trim text <> "" | None -> false in
        (if described "title" && described "description" then [] else [ rel ^ ": a schema needs a title and a description" ])
        @ List.map (fun e -> Printf.sprintf "%s: %s" rel (Json_schema.error_to_string e)) (Json_schema.lint schema))
    files

(** Problems in a transcript of the Bridge: every recorded response is a
    JSON-RPC response, every report in it a check report, every tactic
    outcome and goal of the published shape. *)
let transcript (s : schemas) rel text =
  List.concat
    (List.mapi
       (fun i line ->
         if Text.is_blank line then []
         else
           let what = Printf.sprintf "%s:%d" rel (i + 1) in
           match Json.parse line with
           | Error e -> [ Printf.sprintf "%s: %s" what e ]
           | Ok entry ->
             (match Json.member "response" entry with
              | None -> [ what ^ ": the entry has no response" ]
              | Some response ->
                let result = Option.value (Json.member "result" response) ~default:(Json.Obj []) in
                let meth = match Json.member "request" entry with Some r -> Json.string_or "" "method" r | None -> "" in
                let reports =
                  (match Json.member "report" result with Some r -> [ r ] | None -> [])
                  @ List.filter_map (fun item -> Json.member "report" item) (Json.list_or_empty "results" result)
                in
                let outcomes =
                  match meth with
                  | "runTactic" when Json.member "ok" result <> None -> [ result ]
                  | "tryTactics" -> Json.list_or_empty "results" result
                  | _ -> []
                in
                let goals = Json.list_or_empty "goals" result @ List.concat_map (Json.list_or_empty "goals") (Json.list_or_empty "states" result) in
                against s ~what "schemas/bridge/response.schema.json" response
                @ List.concat_map (against s ~what:(what ^ " report") "schemas/bridge/report.schema.json") reports
                @ List.concat_map (against s ~what:(what ^ " outcome") "schemas/bridge/tactic-outcome.schema.json") outcomes
                @ List.concat_map (against s ~what:(what ^ " goal") "schemas/bridge/goal.schema.json") goals))
       (Text.lines text))

type summary = { schema_files : int; documents : int; transcripts : int; problems : string list }

(** Runs the whole check on a repository. *)
let run root =
  let s = schemas root in
  let files = Repo.files root in
  let schema_files = List.filter (fun rel -> Text.starts_with ~prefix:"schemas/" rel && Text.ends_with ~suffix:".schema.json" rel) files in
  let stray = List.filter (fun rel -> Text.starts_with ~prefix:"schemas/" rel && not (Text.ends_with ~suffix:".schema.json" rel)) files in
  let documents = List.filter (fun rel -> Fs.extension rel = ".json" && not (Text.starts_with ~prefix:"schemas/" rel)) files in
  let transcripts = List.filter (fun rel -> Text.starts_with ~prefix:"fixtures/bridge/transcripts/" rel && Fs.extension rel = ".jsonl") files in
  let checked = ref 0 in
  let document_problems =
    List.concat_map
      (fun rel ->
        match Result.bind (Fs.read Fs.(root / rel)) Json.parse with
        | Error e -> [ Printf.sprintf "%s: %s" rel e ]
        | Ok instance ->
          if Option.bind (Json.member "schema" instance) Json.string_opt <> None then incr checked;
          document s ~what:rel instance)
      documents
  in
  let transcript_problems =
    List.concat_map (fun rel -> match Fs.read Fs.(root / rel) with Ok text -> transcript s rel text | Error e -> [ e ]) transcripts
  in
  { schema_files = List.length schema_files; documents = !checked; transcripts = List.length transcripts;
    problems =
      List.map (fun rel -> rel ^ ": only schema files (*.schema.json) belong under schemas/") stray
      @ lint_schemas s schema_files @ document_problems @ transcript_problems }
