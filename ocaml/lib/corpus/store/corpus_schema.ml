(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The schemas of the records of a dataset.

    They are values of the program, so that a record can be checked where it
    is written, wherever the program runs. The files under [schemas/corpus/]
    are written from these values and compared with them by the repository
    checks. *)

open Maithic_base

let str = Json.Obj [ ("type", Json.String "string") ]
let text = Json.Obj [ ("type", Json.String "string"); ("minLength", Json.Int 1) ]
let count = Json.Obj [ ("type", Json.String "integer"); ("minimum", Json.Int 0) ]
let flag = Json.Obj [ ("type", Json.String "boolean") ]
let texts = Json.Obj [ ("type", Json.String "array"); ("items", str) ]
let id = Json.Obj [ ("type", Json.String "string"); ("pattern", Json.String "^[0-9a-f]{16}$") ]
let digest = Json.Obj [ ("type", Json.String "string"); ("pattern", Json.String "^[0-9a-f]{64}$") ]

let document ~title ~description ~name properties =
  Json.Obj
    [ ("$schema", Json.String "https://json-schema.org/draft/2020-12/schema"); ("title", Json.String title);
      ("description", Json.String description); ("type", Json.String "object");
      ("properties", Json.Obj (("schema", Json.Obj [ ("const", Json.String name) ]) :: properties));
      ("required", Json.List (List.map (fun (key, _) -> Json.String key) (("schema", Json.Null) :: properties)));
      ("additionalProperties", Json.Bool false) ]

let theorem_name = "maithic.corpus.theorem/1"
let step_name = "maithic.corpus.step/1"
let premise_name = "maithic.corpus.premise/1"
let pair_name = "maithic.corpus.pair/1"
let manifest_name = "maithic.corpus.manifest/1"
let harvest_name = "maithic.corpus.harvest/1"

let theorem =
  document ~title:"Theorem record" ~name:theorem_name
    ~description:
      "A theorem of a Lean file: its name, where it stands, its statement as Lean prints it, its proof as it is written, how many \
       smallest tactic steps the proof has and how many constants its proof term refers to, and whether it is complete, which \
       means that it does not rest on sorry. The identifier is the first sixteen digits of the SHA-256 of the record without it."
    [ ("id", id); ("name", text); ("module", str); ("file", str); ("line", count); ("statement", text); ("proof", str);
      ("steps", count); ("premises", count); ("complete", flag) ]

let step =
  document ~title:"Tactic step record" ~name:step_name
    ~description:
      "One tactic of a proof as it ran: the theorem it belongs to, its position among the steps of that theorem, its text, where \
       it stands, every goal before and after it as Lean prints them, how many steps it stands inside (depth, zero for the outline \
       of the proof) and how many stand inside it (inner, zero for a smallest move)."
    [ ("id", id); ("theorem", text); ("module", str); ("index", count); ("tactic", text); ("goals_before", texts);
      ("goals_after", texts); ("depth", count); ("inner", count); ("line", count); ("column", count) ]

let premise =
  document ~title:"Premise record" ~name:premise_name
    ~description:"One constant that the proof term of a theorem refers to: a lemma it applies, a definition it unfolds, an instance it uses."
    [ ("id", id); ("theorem", text); ("module", str); ("premise", text) ]

let pair =
  document ~title:"State and tactic pair" ~name:pair_name
    ~description:
      "A proof state and the tactic that was run on it, taken from a smallest step of a proof: the goals before the tactic as one \
       text, the tactic, and how many goals there were before and after. A tactic that leaves no goal closes the state. \
       Pairs are taken only from proofs that are complete: a proof that rests on sorry, its own or that of a lemma it uses, gives none."
    [ ("id", id); ("theorem", text); ("module", str); ("state", text); ("tactic", text); ("goals_before", count); ("goals_after", count) ]

let manifest =
  document ~title:"Dataset manifest" ~name:manifest_name
    ~description:
      "What a dataset holds: its name and version, where its records come from, how many records of each kind there are, and the \
       shard files with the number of records and the SHA-256 of each. A shard is a file of JSON lines, one record on each line, \
       named by its kind and the first sixteen digits of its hash."
    [ ("name", text); ("version", text);
      ( "sources",
        Json.Obj
          [ ("type", Json.String "array");
            ( "items",
              Json.Obj
                [ ("type", Json.String "object");
                  ("properties", Json.Obj [ ("repository", str); ("revision", str); ("toolchain", str); ("files", count) ]);
                  ("required", Json.List (List.map (fun k -> Json.String k) [ "repository"; "revision"; "toolchain"; "files" ]));
                  ("additionalProperties", Json.Bool false) ] ) ] );
      ( "records",
        Json.Obj
          [ ("type", Json.String "object"); ("properties", Json.Obj [ ("theorem", count); ("step", count); ("premise", count); ("pair", count) ]);
            ("required", Json.List (List.map (fun k -> Json.String k) [ "theorem"; "step"; "premise"; "pair" ]));
            ("additionalProperties", Json.Bool false) ] );
      ( "shards",
        Json.Obj
          [ ("type", Json.String "array");
            ( "items",
              Json.Obj
                [ ("type", Json.String "object");
                  ( "properties",
                    Json.Obj [ ("kind", Json.Obj [ ("enum", Json.List (List.map (fun k -> Json.String k) [ "theorem"; "step"; "premise"; "pair" ])) ]);
                               ("file", text); ("records", count); ("sha256", digest) ] );
                  ("required", Json.List (List.map (fun k -> Json.String k) [ "kind"; "file"; "records"; "sha256" ]));
                  ("additionalProperties", Json.Bool false) ] ) ] ) ]

let harvest =
  document ~title:"Harvest record" ~name:harvest_name
    ~description:
      "The record of one harvest: the repository that was read and its revision, the Lean toolchain, for every file whether it \
       elaborated and what it gave, and where the dataset was written."
    [ ("repository", str); ("revision", str); ("toolchain", str);
      ( "files",
        Json.Obj
          [ ("type", Json.String "array");
            ( "items",
              Json.Obj
                [ ("type", Json.String "object");
                  ( "properties",
                    Json.Obj
                      [ ("file", text); ("outcome", Json.Obj [ ("enum", Json.List (List.map (fun k -> Json.String k) [ "ok"; "errors"; "failed" ])) ]);
                        ("errors", count); ("theorems", count); ("steps", count); ("reason", str) ] );
                  ("required", Json.List (List.map (fun k -> Json.String k) [ "file"; "outcome"; "errors"; "theorems"; "steps" ]));
                  ("additionalProperties", Json.Bool false) ] ) ] );
      ("dataset", text); ("manifest", text);
      ( "records",
        Json.Obj
          [ ("type", Json.String "object"); ("properties", Json.Obj [ ("theorem", count); ("step", count); ("premise", count); ("pair", count) ]);
            ("additionalProperties", Json.Bool false) ] ) ]

(** Every schema with the file it is kept in, below [schemas/]. *)
let files =
  [ ("corpus/theorem.schema.json", theorem); ("corpus/step.schema.json", step); ("corpus/premise.schema.json", premise);
    ("corpus/pair.schema.json", pair); ("corpus/manifest.schema.json", manifest); ("corpus/harvest.schema.json", harvest) ]
