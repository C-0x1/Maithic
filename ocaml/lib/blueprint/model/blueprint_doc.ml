(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A blueprint: the plan of a formalization as nodes that refer to each other. *)

type edge_role = In_statement | In_proof

let role_name = function In_statement -> "statement" | In_proof -> "proof"

(** [source] uses [target]. *)
type edge = { source : string; target : string; role : edge_role }

type t = {
  title : string;
  namespace : string option;  (** The Lean namespace the declarations live in. *)
  imports : string list;  (** Modules the Lean files import. *)
  meta : (string * string) list;  (** Front matter this version does not know, kept as it stood. *)
  intro : string;  (** Text before the first node. *)
  nodes : Blueprint_node.t list;
}

let make ?namespace ?(imports = []) ?(meta = []) ?(intro = "") ~title nodes = { title; namespace; imports; meta; intro; nodes }
let find doc id = List.find_opt (fun (node : Blueprint_node.t) -> node.Blueprint_node.id = id) doc.nodes
let ids doc = List.map (fun (node : Blueprint_node.t) -> node.Blueprint_node.id) doc.nodes

(** The edges of the graph. A node that the statement uses is not listed a
    second time for the proof: what a statement needs, its proof needs too. *)
let edges doc =
  List.concat_map
    (fun (node : Blueprint_node.t) ->
      let in_statement = List.sort_uniq compare node.Blueprint_node.uses in
      List.map (fun target -> { source = node.Blueprint_node.id; target; role = In_statement }) in_statement
      @ List.filter_map
          (fun target -> if List.mem target in_statement then None else Some { source = node.Blueprint_node.id; target; role = In_proof })
          (List.sort_uniq compare node.Blueprint_node.proof_uses))
    doc.nodes

(** The nodes that refer to [id], in the order of the file. *)
let dependents doc id =
  List.filter (fun (node : Blueprint_node.t) -> List.mem id (Blueprint_node.dependencies node)) doc.nodes

(** The full name of a node's declaration in Lean. *)
let lean_name doc (node : Blueprint_node.t) =
  Option.map (fun name -> match doc.namespace with Some ns when ns <> "" -> ns ^ "." ^ name | _ -> name) node.Blueprint_node.lean
