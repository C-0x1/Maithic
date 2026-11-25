(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The records of a dataset, made from what the Bridge extracted. *)

open Maithic_base
open Maithic_bridge

type kind = Theorem | Step | Premise | Pair

let kinds = [ Theorem; Step; Premise; Pair ]
let kind_to_string = function Theorem -> "theorem" | Step -> "step" | Premise -> "premise" | Pair -> "pair"

let kind_of_string = function
  | "theorem" -> Some Theorem
  | "step" -> Some Step
  | "premise" -> Some Premise
  | "pair" -> Some Pair
  | _ -> None

let schema_of = function
  | Theorem -> Corpus_schema.theorem
  | Step -> Corpus_schema.step
  | Premise -> Corpus_schema.premise
  | Pair -> Corpus_schema.pair

let schema_name = function
  | Theorem -> Corpus_schema.theorem_name
  | Step -> Corpus_schema.step_name
  | Premise -> Corpus_schema.premise_name
  | Pair -> Corpus_schema.pair_name

(** The identifier of a record: the first sixteen digits of the SHA-256 of
    its canonical JSON, without the member [id]. Equal records have equal
    identifiers, wherever and whenever they are made. *)
let id_of = function
  | Json.Obj fields -> String.sub (Sha256.hex (Json.to_string ~canonical:true (Json.Obj (List.remove_assoc "id" fields)))) 0 16
  | other -> String.sub (Sha256.hex (Json.to_string ~canonical:true other)) 0 16

(** A record with its schema and its identifier in front. *)
let make kind fields =
  let body = Json.Obj (("schema", Json.String (schema_name kind)) :: fields) in
  Json.Obj (("schema", Json.String (schema_name kind)) :: ("id", Json.String (id_of body)) :: fields)

let strings items = Json.List (List.map (fun s -> Json.String s) items)

(** The records of one extracted file: a theorem record for every theorem,
    a step record for every tactic of its proof, a premise record for every
    constant its proof term refers to, and a pair record for every smallest
    step of a proof that is complete. Steps are numbered within their
    theorem.

    A pair says "on this state, this tactic is a step of a proof", and that
    is only known of a proof the kernel accepted whole: a proof that rests
    on [sorry], its own or that of a lemma it uses, gives no pairs. Its
    theorem and step records are kept, the theorem marked as not complete. *)
let of_extraction (e : Extraction.t) =
  List.concat_map
    (fun (d : Extraction.declaration) ->
      if d.Extraction.kind <> "theorem" then []
      else begin
        let name = d.Extraction.name and module_ = e.Extraction.module_name in
        let steps = List.filter (fun (s : Extraction.step) -> s.Extraction.decl = name) e.Extraction.steps in
        let smallest = List.filter (fun (s : Extraction.step) -> s.Extraction.inner = 0) steps in
        let theorem =
          make Theorem
            [ ("name", Json.String name); ("module", Json.String module_); ("file", Json.String e.Extraction.file);
              ("line", Json.Int d.Extraction.name_line); ("statement", Json.String d.Extraction.type_);
              ("proof", Json.String (Option.value ~default:"" d.Extraction.proof)); ("steps", Json.Int (List.length smallest));
              ("premises", Json.Int (List.length d.Extraction.value_deps)); ("complete", Json.Bool (not d.Extraction.uses_sorry)) ]
        in
        let step_records =
          List.mapi
            (fun index (s : Extraction.step) ->
              ( Step,
                make Step
                  [ ("theorem", Json.String name); ("module", Json.String module_); ("index", Json.Int index);
                    ("tactic", Json.String s.Extraction.tactic); ("goals_before", strings s.Extraction.goals_before);
                    ("goals_after", strings s.Extraction.goals_after); ("depth", Json.Int s.Extraction.depth);
                    ("inner", Json.Int s.Extraction.inner); ("line", Json.Int s.Extraction.at_line); ("column", Json.Int s.Extraction.at_column) ] ))
            steps
        in
        let premise_records =
          List.map
            (fun premise -> (Premise, make Premise [ ("theorem", Json.String name); ("module", Json.String module_); ("premise", Json.String premise) ]))
            d.Extraction.value_deps
        in
        let pair_records =
          List.map
            (fun (s : Extraction.step) ->
              ( Pair,
                make Pair
                  [ ("theorem", Json.String name); ("module", Json.String module_);
                    ("state", Json.String (String.concat "\n\n" s.Extraction.goals_before)); ("tactic", Json.String s.Extraction.tactic);
                    ("goals_before", Json.Int (List.length s.Extraction.goals_before));
                    ("goals_after", Json.Int (List.length s.Extraction.goals_after)) ] ))
            (if d.Extraction.uses_sorry then [] else smallest)
        in
        ((Theorem, theorem) :: step_records) @ premise_records @ pair_records
      end)
    e.Extraction.declarations
