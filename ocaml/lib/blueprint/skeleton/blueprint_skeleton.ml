(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The Lean skeleton of a blueprint: every node that has a formal statement,
    stated, with [sorry] where its proof will stand. *)

open Maithic_base

(** Where a node stands in the skeleton. Lines are one-based and inclusive. *)
type entry = { id : string; first : int; last : int }

type t = {
  text : string;
  entries : entry list;  (** In the order of the text. *)
  unstated : string list;  (** Nodes with no formal statement yet: they are named in a comment and nothing more. *)
}

let tag = "-- blueprint: "

(* Words a whole declaration begins with. A formal text that begins with one
   is written out as it stands; any other is what follows the name. *)
let opens_declaration formal =
  let first = match String.split_on_char ' ' (Text.trim (List.hd (String.split_on_char '\n' (Text.trim formal)))) with word :: _ -> word | [] -> "" in
  List.mem first
    [ "def"; "abbrev"; "structure"; "inductive"; "class"; "instance"; "theorem"; "lemma"; "opaque"; "noncomputable"; "private"; "protected"; "partial"; "unsafe" ]
  || Text.starts_with ~prefix:"@[" first

let declaration (node : Blueprint_node.t) ~name ~formal =
  if opens_declaration formal then formal
  else if node.Blueprint_node.kind = Blueprint_node.Definition then Printf.sprintf "def %s %s := sorry" name formal
  else Printf.sprintf "theorem %s %s := by\n  sorry" name formal

(* A documentation comment must not end early. *)
let in_comment text = Text.replace_all ~sub:"-/" ~by:"- /" text

let doc_comment (node : Blueprint_node.t) =
  let title = if node.Blueprint_node.title = "" then Blueprint_node.kind_title node.Blueprint_node.kind else node.Blueprint_node.title in
  let title = if Text.ends_with ~suffix:"." title then title else title ^ "." in
  match node.Blueprint_node.statement with
  | "" -> Printf.sprintf "/-- %s -/" (in_comment title)
  | statement -> Printf.sprintf "/-- %s\n\n%s -/" (in_comment title) (in_comment statement)

(** Writes the skeleton. Declarations come in an order in which everything
    a node uses stands before it; a blueprint with a circle has no skeleton. *)
let emit (doc : Blueprint_doc.t) =
  match Blueprint_check.order doc with
  | Error cycle -> Error (Blueprint_check.describe (Blueprint_check.Cycle cycle))
  | Ok ordered ->
    let buffer = Buffer.create 2048 and line = ref 1 in
    let add text =
      Buffer.add_string buffer text;
      Buffer.add_char buffer '\n';
      line := !line + List.length (String.split_on_char '\n' text)
    in
    add (Printf.sprintf "-- The skeleton of the blueprint \"%s\"." doc.Blueprint_doc.title);
    add "-- Every declaration carries the identifier of its node; `maithic blueprint status` reads what Lean says about each.";
    add "";
    if doc.Blueprint_doc.imports <> [] then begin
      List.iter (fun m -> add ("import " ^ m)) doc.Blueprint_doc.imports;
      add ""
    end;
    (match doc.Blueprint_doc.namespace with
     | Some ns ->
       add ("namespace " ^ ns);
       add ""
     | None -> ());
    let entries, unstated =
      List.fold_left
        (fun (entries, unstated) (node : Blueprint_node.t) ->
          match (node.Blueprint_node.lean, node.Blueprint_node.formal) with
          | Some name, Some formal ->
            let first = !line in
            add (tag ^ node.Blueprint_node.id);
            add (doc_comment node);
            add (declaration node ~name ~formal);
            let last = !line - 1 in
            add "";
            ({ id = node.Blueprint_node.id; first; last } :: entries, unstated)
          | _ ->
            add (Printf.sprintf "%s%s (not stated yet)" tag node.Blueprint_node.id);
            add "";
            (entries, node.Blueprint_node.id :: unstated))
        ([], []) ordered
    in
    (match doc.Blueprint_doc.namespace with Some ns -> add ("end " ^ ns) | None -> ());
    Ok { text = Buffer.contents buffer; entries = List.rev entries; unstated = List.rev unstated }

(** The node a line of the skeleton belongs to. *)
let node_at skeleton line = List.find_map (fun e -> if e.first <= line && line <= e.last then Some e.id else None) skeleton.entries

(** The identifiers tagged in any Lean text, with the line of each tag. *)
let tags text =
  List.filter_map Fun.id
    (List.mapi
       (fun i line ->
         let line = Text.trim line in
         if Text.starts_with ~prefix:tag line then
           let rest = Text.trim (String.sub line (String.length tag) (String.length line - String.length tag)) in
           match String.split_on_char ' ' rest with id :: _ when id <> "" -> Some (id, i + 1) | _ -> None
         else None)
       (String.split_on_char '\n' text))

(** What Lean said about a text, reduced to what the skeleton needs. *)
type diagnostic = { severity : string; line : int; message : string }

type outcome = {
  compiles : bool;
  problems : (string option * int * string) list;  (** The node, the line and Lean's message, for every error. *)
}

(** Gives the skeleton to Lean. A skeleton compiles when Lean reports no
    error; the warnings about [sorry] are what a skeleton is made of. *)
let compile ~elaborate skeleton =
  match elaborate skeleton.text with
  | Error why -> Error why
  | Ok diagnostics ->
    let problems =
      List.filter_map (fun d -> if d.severity = "error" then Some (node_at skeleton d.line, d.line, d.message) else None) diagnostics
    in
    Ok { compiles = problems = []; problems }
