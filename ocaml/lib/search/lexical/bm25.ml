(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** BM25 over the fields of a declaration, each with a weight. *)

type weights = { name : float; statement : float; doc : float; module_ : float }

let default_weights = { name = 3.0; statement = 1.5; doc = 1.0; module_ = 0.5 }
let default_k1 = 1.2
let default_b = 0.75

let weight weights = function
  | Inverted_index.Name -> weights.name
  | Inverted_index.Statement -> weights.statement
  | Inverted_index.Doc -> weights.doc
  | Inverted_index.Module -> weights.module_

(** How much a word tells: a word of few documents tells more than a word of many. Never negative. *)
let idf index word =
  let n = float_of_int (Inverted_index.size index) and df = float_of_int (Inverted_index.document_frequency index word) in
  log (1.0 +. ((n -. df +. 0.5) /. (df +. 0.5)))

type scored = { document : int; score : float; matched : string list  (** The words of the query the document has. *) }

(** Scores every document that has a word of the query. A word that the
    query repeats counts once. The best comes first; among equals, the
    earlier document. *)
let score ?(weights = default_weights) ?(k1 = default_k1) ?(b = default_b) index words =
  let words = List.fold_left (fun seen w -> if List.mem w seen then seen else seen @ [ w ]) [] words in
  let totals = Hashtbl.create 64 and matched = Hashtbl.create 64 in
  List.iter
    (fun word ->
      let idf = idf index word in
      List.iter
        (fun (p : Inverted_index.posting) ->
          let fi = Inverted_index.field_index p.Inverted_index.field in
          let length = float_of_int index.Inverted_index.lengths.(fi).(p.Inverted_index.document) and average = index.Inverted_index.averages.(fi) in
          let tf = float_of_int p.Inverted_index.count in
          let norm = if average = 0.0 then 1.0 else 1.0 -. b +. (b *. length /. average) in
          let part = weight weights p.Inverted_index.field *. idf *. (tf *. (k1 +. 1.0) /. (tf +. (k1 *. norm))) in
          let d = p.Inverted_index.document in
          Hashtbl.replace totals d (part +. Option.value ~default:0.0 (Hashtbl.find_opt totals d));
          let had = Option.value ~default:[] (Hashtbl.find_opt matched d) in
          if not (List.mem word had) then Hashtbl.replace matched d (had @ [ word ]))
        (Inverted_index.postings index word))
    words;
  let scored = Hashtbl.fold (fun document score acc -> { document; score; matched = Hashtbl.find matched document } :: acc) totals [] in
  List.sort (fun a b -> match compare b.score a.score with 0 -> compare a.document b.document | c -> c) scored
