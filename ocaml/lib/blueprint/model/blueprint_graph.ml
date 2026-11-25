(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The graph of a blueprint for other programs: JSON, and DOT for Graphviz. *)

open Maithic_base

let schema = "maithic.blueprint.graph/1"

let to_json (doc : Blueprint_doc.t) =
  let node (n : Blueprint_node.t) =
    Json.Obj
      ([ ("id", Json.String n.Blueprint_node.id); ("kind", Json.String (Blueprint_node.kind_name n.Blueprint_node.kind));
         ("title", Json.String n.Blueprint_node.title) ]
      @ (match Blueprint_doc.lean_name doc n with Some name -> [ ("lean", Json.String name) ] | None -> [])
      @ [ ("stated", Json.Bool (n.Blueprint_node.formal <> None)); ("argued", Json.Bool (n.Blueprint_node.proof <> "")) ])
  in
  let edge (e : Blueprint_doc.edge) =
    Json.Obj
      [ ("from", Json.String e.Blueprint_doc.source); ("to", Json.String e.Blueprint_doc.target);
        ("role", Json.String (Blueprint_doc.role_name e.Blueprint_doc.role)) ]
  in
  Json.Obj
    ([ ("schema", Json.String schema); ("title", Json.String doc.Blueprint_doc.title) ]
    @ (match doc.Blueprint_doc.namespace with Some ns -> [ ("namespace", Json.String ns) ] | None -> [])
    @ [ ("nodes", Json.List (List.map node doc.Blueprint_doc.nodes)); ("edges", Json.List (List.map edge (Blueprint_doc.edges doc))) ])

let quoted text = "\"" ^ Text.replace_all ~sub:"\"" ~by:"\\\"" text ^ "\""

(** The graph in the DOT language: definitions as boxes, claims as ellipses,
    and edges of proofs dashed. An edge points from a node to what it uses. *)
let to_dot (doc : Blueprint_doc.t) =
  let node (n : Blueprint_node.t) =
    Printf.sprintf "  %s [label=%s, shape=%s];" (quoted n.Blueprint_node.id)
      (quoted (n.Blueprint_node.id ^ "\\n" ^ n.Blueprint_node.title))
      (if n.Blueprint_node.kind = Blueprint_node.Definition then "box" else "ellipse")
  in
  let edge (e : Blueprint_doc.edge) =
    Printf.sprintf "  %s -> %s%s;" (quoted e.Blueprint_doc.source) (quoted e.Blueprint_doc.target)
      (if e.Blueprint_doc.role = Blueprint_doc.In_proof then " [style=dashed]" else "")
  in
  String.concat "\n" ([ "digraph blueprint {"; "  rankdir=BT;" ] @ List.map node doc.Blueprint_doc.nodes @ List.map edge (Blueprint_doc.edges doc) @ [ "}" ]) ^ "\n"
