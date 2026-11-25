(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The entry point of the search: several ways of finding declarations,
    asked together and merged into one ranked answer. *)

open Maithic_base

type retriever =
  | Lexical  (** Words of the query in names, statements and documentation, ranked by BM25. *)
  | Name  (** The query as a name or a part of one. *)
  | Lean  (** Lean itself, asked about everything in scope, the libraries included. *)

let retriever_name = function Lexical -> "lexical" | Name -> "name" | Lean -> "lean"
let retriever_of_name = function "lexical" -> Some Lexical | "name" -> Some Name | "lean" -> Some Lean | _ -> None

type options = {
  limit : int;
  kinds : string list;  (** Only declarations of these kinds; empty for all. *)
  module_prefix : string option;  (** Only declarations of modules with this prefix. *)
  retrievers : retriever list;
  weights : Bm25.weights;
  k1 : float;
  b : float;
}

let default_options = { limit = 10; kinds = []; module_prefix = None; retrievers = [ Lexical; Name ]; weights = Bm25.default_weights; k1 = Bm25.default_k1; b = Bm25.default_b }

type hit = {
  document : Decl_document.t;
  score : float;  (** The merged score: higher is better; comparable only within one answer. *)
  found_by : (retriever * int) list;  (** Each retriever that found it, with the rank it gave. *)
  matched : string list;  (** The words of the query it has. *)
}

(** What Lean is asked: a query and how many answers at most. *)
type lean = string -> limit:int -> (Decl_document.t list, string) result

let admits options (d : Decl_document.t) =
  (options.kinds = [] || List.mem d.Decl_document.kind options.kinds)
  && match options.module_prefix with None -> true | Some prefix -> Text.starts_with ~prefix d.Decl_document.module_

(* The words of a query: its identifiers taken apart and its notation named, as in the index. *)
let query_words query = Tokens.of_text ~prose:true query

let lexical options index query =
  List.filter_map
    (fun (s : Bm25.scored) ->
      let d = index.Inverted_index.documents.(s.Bm25.document) in
      if admits options d then Some (d, s.Bm25.matched, 1.0) else None)
    (Bm25.score ~weights:options.weights ~k1:options.k1 ~b:options.b index (query_words query))

(* Names that are the query, end with it, or contain it; the closest first, and the shorter name before the longer.
   A name that is the query counts double when the answers are merged: whoever types a name wants that declaration. *)
let by_name options index query =
  let wanted = String.lowercase_ascii (Text.trim query) in
  if wanted = "" || String.contains wanted ' ' then []
  else
    let ranked =
      List.filter_map
        (fun (d : Decl_document.t) ->
          if not (admits options d) then None
          else
            let name = String.lowercase_ascii d.Decl_document.name and short = String.lowercase_ascii (Decl_document.short_name d) in
            let closeness =
              if name = wanted then Some 0
              else if short = wanted then Some 1
              else if Text.ends_with ~suffix:("." ^ wanted) name then Some 2
              else if Text.starts_with ~prefix:wanted short then Some 3
              else if Text.contains name wanted then Some 4
              else None
            in
            Option.map (fun c -> ((c, String.length d.Decl_document.name, d.Decl_document.name), d)) closeness)
        (Array.to_list index.Inverted_index.documents)
    in
    List.map (fun ((closeness, _, _), d) -> (d, [], if closeness <= 1 then 2.0 else 1.0)) (List.sort (fun (a, _) (b, _) -> compare a b) ranked)

(* Reciprocal rank fusion: a declaration scores weight / (60 + rank) with each retriever that found it. *)
let rrf_constant = 60.0

(** Searches. Each retriever ranks on its own; the answers are merged by
    their ranks, a declaration that several found is listed once, and the
    filters of the options hold for every retriever. When Lean is among the
    retrievers and cannot be asked, the others still answer, and the reason
    comes back with the hits. *)
let search ?(options = default_options) ?lean index query =
  let trouble = ref None in
  let lists =
    List.filter_map
      (fun retriever ->
        match retriever with
        | Lexical -> Some (Lexical, lexical options index query)
        | Name -> Some (Name, by_name options index query)
        | Lean ->
          (match lean with
           | None ->
             trouble := Some "Lean was not asked: no Bridge is at hand";
             None
           | Some ask ->
             (match ask query ~limit:(max options.limit 10) with
              | Ok documents -> Some (Lean, List.filter_map (fun d -> if admits options d then Some (d, [], 1.0) else None) documents)
              | Error why ->
                trouble := Some ("Lean could not be asked: " ^ why);
                None)))
      options.retrievers
  in
  let merged = Hashtbl.create 64 and order = ref [] in
  List.iter
    (fun (retriever, ranked) ->
      List.iteri
        (fun i ((d : Decl_document.t), matched, weight) ->
          let rank = i + 1 in
          let part = weight /. (rrf_constant +. float_of_int rank) in
          match Hashtbl.find_opt merged d.Decl_document.name with
          | Some hit -> Hashtbl.replace merged d.Decl_document.name { hit with score = hit.score +. part; found_by = hit.found_by @ [ (retriever, rank) ]; matched = (if hit.matched = [] then matched else hit.matched) }
          | None ->
            order := d.Decl_document.name :: !order;
            Hashtbl.replace merged d.Decl_document.name { document = d; score = part; found_by = [ (retriever, rank) ]; matched })
        ranked)
    lists;
  let hits = List.mapi (fun i name -> (i, Hashtbl.find merged name)) (List.rev !order) in
  let sorted = List.sort (fun (i, a) (j, b) -> match compare b.score a.score with 0 -> compare i j | c -> c) hits in
  (List.filteri (fun i _ -> i < options.limit) (List.map snd sorted), !trouble)

(* ---- output ---- *)

let results_schema = "maithic.search.results/1"

let hit_to_json rank hit =
  Json.Obj
    [ ("rank", Json.Int rank); ("score", Json.Float hit.score); ("declaration", Decl_document.to_json hit.document);
      ("found_by", Json.List (List.map (fun (r, rank) -> Json.Obj [ ("retriever", Json.String (retriever_name r)); ("rank", Json.Int rank) ]) hit.found_by));
      ("matched", Json.List (List.map (fun w -> Json.String w) hit.matched)) ]

let to_json ~query ~options ~declarations ?note hits =
  Json.Obj
    ([ ("schema", Json.String results_schema); ("query", Json.String query); ("declarations", Json.Int declarations);
       ("retrievers", Json.List (List.map (fun r -> Json.String (retriever_name r)) options.retrievers)); ("limit", Json.Int options.limit) ]
    @ (match note with Some note -> [ ("note", Json.String note) ] | None -> [])
    @ [ ("hits", Json.List (List.mapi (fun i hit -> hit_to_json (i + 1) hit) hits)) ])

(** The hits as lines for a terminal: rank, name, kind, where it stands, and below it the statement and who found it. *)
let render ?(statement_width = 96) hits =
  if hits = [] then "nothing was found"
  else
    String.concat "\n"
      (List.concat
         (List.mapi
            (fun i hit ->
              let d = hit.document in
              let where =
                if d.Decl_document.file <> "" then Printf.sprintf "%s:%d" d.Decl_document.file d.Decl_document.line
                else if d.Decl_document.module_ <> "" then d.Decl_document.module_
                else "(in Lean's scope)"
              in
              let one_line = String.concat " " (List.filter (fun w -> w <> "") (String.split_on_char ' ' (Text.replace_all ~sub:"\n" ~by:" " d.Decl_document.statement))) in
              [ Printf.sprintf "%2d. %s  %s  %s" (i + 1) d.Decl_document.name d.Decl_document.kind where;
                "    " ^ Text.truncate_words statement_width one_line ]
              @ (if d.Decl_document.doc = "" then [] else [ "    " ^ Text.truncate_words statement_width (List.hd (String.split_on_char '\n' d.Decl_document.doc)) ])
              @ [ "    found by "
                  ^ String.concat ", " (List.map (fun (r, rank) -> Printf.sprintf "%s (rank %d)" (retriever_name r) rank) hit.found_by)
                  ^ if hit.matched = [] then "" else "; words: " ^ String.concat " " hit.matched ])
            hits))

(* ---- answering a set of queries, for evaluation ---- *)

let rankings_schema = "maithic.search.rankings/1"

(** The answers to the queries of a query set, in the form the evaluation reads. *)
let rankings ?(options = default_options) ?engine index queryset =
  let engine = match engine with Some name -> name | None -> "maithic-" ^ String.concat "+" (List.map retriever_name options.retrievers) in
  let queries = Json.list_or_empty "queries" queryset in
  if queries = [] then Error "the query set has no query"
  else
    Ok
      (Json.Obj
         [ ("schema", Json.String rankings_schema); ("engine", Json.String engine); ("queryset", Json.String (Json.string_or "" "name" queryset));
           ( "config",
             Json.Obj
               [ ("retrievers", Json.List (List.map (fun r -> Json.String (retriever_name r)) options.retrievers)); ("limit", Json.Int options.limit);
                 ("k1", Json.Float options.k1); ("b", Json.Float options.b) ] );
           ( "results",
             Json.List
               (List.map
                  (fun q ->
                    let started = Unix.gettimeofday () in
                    let hits, _ = search ~options index (Json.string_or "" "text" q) in
                    let elapsed = (Unix.gettimeofday () -. started) *. 1000.0 in
                    Json.Obj
                      [ ("query", Json.String (Json.string_or "" "id" q)); ("ranked", Json.List (List.map (fun hit -> Json.String hit.document.Decl_document.name) hits));
                        ("elapsed_ms", Json.Float elapsed) ])
                  queries) ) ])
