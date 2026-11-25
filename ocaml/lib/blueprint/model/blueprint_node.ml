(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A node of a blueprint: one definition or one claim, said informally and,
    when it has been stated, formally. *)

open Maithic_base

type kind = Definition | Lemma | Proposition | Theorem | Corollary

let kinds = [ Definition; Lemma; Proposition; Theorem; Corollary ]

let kind_name = function
  | Definition -> "definition"
  | Lemma -> "lemma"
  | Proposition -> "proposition"
  | Theorem -> "theorem"
  | Corollary -> "corollary"

let kind_of_name name = List.find_opt (fun kind -> kind_name kind = Text.lowercase (Text.trim name)) kinds

(** The kind as a heading shows it. *)
let kind_title kind = String.capitalize_ascii (kind_name kind)

type t = {
  id : string;  (** Unique in the blueprint; what other nodes refer to. *)
  kind : kind;
  title : string;
  statement : string;  (** The informal statement. *)
  proof : string;  (** The informal proof; empty for a definition and for a claim nobody argued yet. *)
  lean : string option;  (** The name of the Lean declaration, relative to the namespace of the blueprint. *)
  formal : string option;  (** The Lean text: a whole definition, or what stands after the name of a theorem. *)
  uses : string list;  (** Nodes the statement refers to. *)
  proof_uses : string list;  (** Nodes only the proof refers to. *)
}

let make ?(proof = "") ?lean ?formal ?(uses = []) ?(proof_uses = []) ~id ~kind ~title statement =
  { id; kind; title; statement; proof; lean; formal; uses; proof_uses }

(** A claim is anything that needs a proof. *)
let is_claim node = node.kind <> Definition

(** Identifiers are short and plain, so that they can stand in headings, in
    comments of Lean files and in file names. *)
let valid_id id =
  let plain = function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | ':' -> true | _ -> false in
  id <> "" && String.length id <= 64 && String.for_all plain id && (match id.[0] with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false)

(** Makes an identifier of any text: lower case, with one dash for each run of other characters. *)
let slug text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | '0' .. '9' -> Buffer.add_char buffer c
      | 'A' .. 'Z' -> Buffer.add_char buffer (Char.lowercase_ascii c)
      | _ -> if Buffer.length buffer > 0 && Buffer.nth buffer (Buffer.length buffer - 1) <> '-' then Buffer.add_char buffer '-')
    text;
  let s = Buffer.contents buffer in
  let s = if s <> "" && s.[String.length s - 1] = '-' then String.sub s 0 (String.length s - 1) else s in
  if s = "" then "node" else if String.length s > 64 then String.sub s 0 64 else s

(** Every node this one refers to, the ones of the statement first, each once. *)
let dependencies node =
  List.fold_left (fun seen id -> if List.mem id seen then seen else seen @ [ id ]) [] (node.uses @ node.proof_uses)

let optional name = function Some text -> [ (name, Json.String text) ] | None -> []

let to_json node =
  Json.Obj
    ([ ("id", Json.String node.id); ("kind", Json.String (kind_name node.kind)); ("title", Json.String node.title);
       ("statement", Json.String node.statement); ("proof", Json.String node.proof) ]
    @ optional "lean" node.lean @ optional "formal" node.formal
    @ [ ("uses", Json.List (List.map (fun id -> Json.String id) node.uses));
        ("proof_uses", Json.List (List.map (fun id -> Json.String id) node.proof_uses)) ])

let of_json json =
  let text name = Json.string_or "" name json in
  let names name = Json.strings (Json.List (Json.list_or_empty name json)) in
  let present name = match Text.trim (text name) with "" -> None | value -> Some value in
  match kind_of_name (text "kind") with
  | None -> Error (Printf.sprintf "'%s' is not a kind of node; the kinds are %s" (text "kind") (String.concat ", " (List.map kind_name kinds)))
  | Some kind ->
    Ok
      { id = Text.trim (text "id"); kind; title = Text.trim (text "title"); statement = Text.trim (text "statement"); proof = Text.trim (text "proof");
        lean = present "lean"; formal = present "formal"; uses = names "uses"; proof_uses = names "proof_uses" }
