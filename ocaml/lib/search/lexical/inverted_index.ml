(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** An inverted index over declarations: for every word, where it occurs. *)

type field = Name | Statement | Doc | Module

let fields = [ Name; Statement; Doc; Module ]
let field_name = function Name -> "name" | Statement -> "statement" | Doc -> "doc" | Module -> "module"
let field_index = function Name -> 0 | Statement -> 1 | Doc -> 2 | Module -> 3

(** One word in one field of one document, and how often it stands there. *)
type posting = { document : int; field : field; count : int }

type t = {
  documents : Decl_document.t array;
  postings : (string, posting list) Hashtbl.t;  (** For each word, its postings in the order of the documents. *)
  lengths : int array array;  (** For each field, the number of words in each document. *)
  averages : float array;  (** For each field, the mean number of words. *)
}

let words_of field (d : Decl_document.t) =
  match field with
  | Name -> Tokens.of_name d.Decl_document.name
  | Statement -> Tokens.of_text d.Decl_document.statement
  | Doc -> Tokens.of_text ~prose:true d.Decl_document.doc
  | Module -> Tokens.of_text d.Decl_document.module_

let build documents =
  let documents = Array.of_list documents in
  let n = Array.length documents in
  let postings = Hashtbl.create 1024 in
  let lengths = Array.init (List.length fields) (fun _ -> Array.make n 0) in
  Array.iteri
    (fun i d ->
      List.iter
        (fun field ->
          let words = words_of field d in
          lengths.(field_index field).(i) <- List.length words;
          let counts = Hashtbl.create 16 in
          List.iter (fun w -> Hashtbl.replace counts w (1 + Option.value ~default:0 (Hashtbl.find_opt counts w))) words;
          (* in the order the words first occur, so that the index does not depend on the hash function *)
          let seen = Hashtbl.create 16 in
          List.iter
            (fun w ->
              if not (Hashtbl.mem seen w) then begin
                Hashtbl.replace seen w ();
                Hashtbl.replace postings w ({ document = i; field; count = Hashtbl.find counts w } :: Option.value ~default:[] (Hashtbl.find_opt postings w))
              end)
            words)
        fields)
    documents;
  Hashtbl.filter_map_inplace (fun _ list -> Some (List.rev list)) postings;
  let averages =
    Array.map (fun per_doc -> if n = 0 then 0.0 else float_of_int (Array.fold_left ( + ) 0 per_doc) /. float_of_int n) lengths
  in
  { documents; postings; lengths; averages }

let size index = Array.length index.documents
let vocabulary index = Hashtbl.length index.postings
let postings index word = Option.value ~default:[] (Hashtbl.find_opt index.postings word)

(** In how many documents a word occurs, in any field. *)
let document_frequency index word = List.length (List.sort_uniq compare (List.map (fun p -> p.document) (postings index word)))
