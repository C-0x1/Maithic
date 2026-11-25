(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What the search knows about a declaration: one document for each. *)

open Maithic_base
open Maithic_bridge

type t = {
  name : string;
  kind : string;  (** [theorem], [definition], [structure] and so on, as Lean reports it. *)
  statement : string;  (** The type as Lean prints it. *)
  doc : string;  (** The documentation comment; empty when there is none. *)
  module_ : string;
  attributes : string list;  (** The attributes it was declared with, and [private] or [sorry] where they apply. *)
  file : string;
  line : int;
}

(** The attributes written in front of a declaration: [@[simp, grind =]] gives [simp] and [grind]. *)
let attributes_of_source source =
  (* only what stands before the declaration keyword counts *)
  let keywords = [ "theorem "; "def "; "lemma "; "structure "; "inductive "; "class "; "instance "; "abbrev "; "opaque "; "axiom " ] in
  let head = match List.sort compare (List.filter_map (fun keyword -> Text.find_sub source keyword) keywords) with first :: _ -> String.sub source 0 first | [] -> "" in
  let rec scan from acc =
    match Text.find_sub ~from head "@[" with
    | None -> List.rev acc
    | Some at ->
      (match String.index_from_opt head (at + 2) ']' with
       | None -> List.rev acc
       | Some close ->
         let names =
           List.filter_map
             (fun part -> match String.split_on_char ' ' (Text.trim part) with name :: _ when name <> "" -> Some name | _ -> None)
             (String.split_on_char ',' (String.sub head (at + 2) (close - at - 2)))
         in
         scan (close + 1) (List.rev_append names acc))
  in
  scan 0 []

let of_declaration ?(file = "") (d : Extraction.declaration) =
  { name = d.Extraction.name; kind = d.Extraction.kind; statement = d.Extraction.type_; doc = Option.value ~default:"" d.Extraction.doc; module_ = d.Extraction.module_;
    attributes =
      (match d.Extraction.source with Some source -> attributes_of_source source | None -> [])
      @ (if d.Extraction.is_private then [ "private" ] else [])
      @ if d.Extraction.uses_sorry then [ "sorry" ] else [];
    file; line = d.Extraction.name_line }

(** The documents of extracted files, in their order, private declarations left out. *)
let of_extractions (extractions : Extraction.t list) =
  List.concat_map
    (fun (e : Extraction.t) ->
      List.filter_map (fun (d : Extraction.declaration) -> if d.Extraction.is_private then None else Some (of_declaration ~file:e.Extraction.file d)) e.Extraction.declarations)
    extractions

let to_json d =
  Json.Obj
    [ ("name", Json.String d.name); ("kind", Json.String d.kind); ("statement", Json.String d.statement); ("doc", Json.String d.doc);
      ("module", Json.String d.module_); ("attributes", Json.List (List.map (fun a -> Json.String a) d.attributes)); ("file", Json.String d.file);
      ("line", Json.Int d.line) ]

(** Reads a document from its own JSON, or from a line of the declarations
    that [maithic intel extract] writes. *)
let of_json json =
  match Json.string_or "" "name" json with
  | "" -> Error "a declaration without a name"
  | name ->
    let statement = match Json.string_or "" "statement" json with "" -> Json.string_or "" "type" json | s -> s in
    let attributes =
      match Json.member "attributes" json with
      | Some (Json.List items) -> Json.strings (Json.List items)
      | _ ->
        attributes_of_source (Json.string_or "" "source" json)
        @ (if Json.bool_or false "isPrivate" json then [ "private" ] else [])
        @ if Json.bool_or false "usesSorry" json then [ "sorry" ] else []
    in
    Ok
      { name; kind = Json.string_or "" "kind" json; statement; doc = Json.string_or "" "doc" json; module_ = Json.string_or "" "module" json; attributes;
        file = Json.string_or "" "file" json; line = (match Json.int_or 0 "nameLine" json with 0 -> Json.int_or 0 "line" json | n -> n) }

(** Reads declarations written one to a line. *)
let of_jsonl text =
  List.fold_left
    (fun acc (n, line) ->
      match acc with
      | Error _ -> acc
      | Ok documents ->
        if Text.is_blank line then acc
        else (
          match Result.bind (Json.parse line) of_json with
          | Ok d -> Ok (if List.mem "private" d.attributes then documents else d :: documents)
          | Error e -> Error (Printf.sprintf "line %d: %s" n e)))
    (Ok [])
    (List.mapi (fun i line -> (i + 1, line)) (String.split_on_char '\n' text))
  |> Result.map List.rev

(** The last part of a name: what a reader calls the declaration. *)
let short_name d = match List.rev (String.split_on_char '.' d.name) with last :: _ -> last | [] -> d.name
