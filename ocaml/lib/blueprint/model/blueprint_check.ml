(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What a blueprint has to satisfy before anything is built on it. *)

type problem =
  | Duplicate_id of string
  | Unknown_reference of { node : string; target : string }
  | Self_reference of string
  | Cycle of string list  (** Each node uses the next, and the last uses the first. *)
  | Duplicate_lean of { name : string; nodes : string list }
  | Proof_of_definition of string
  | Empty_statement of string
  | Formal_without_name of string

let describe = function
  | Duplicate_id id -> Printf.sprintf "the identifier '%s' is used by more than one node" id
  | Unknown_reference { node; target } -> Printf.sprintf "'%s' refers to '%s', and no node has that identifier" node target
  | Self_reference id -> Printf.sprintf "'%s' refers to itself" id
  | Cycle ids -> Printf.sprintf "the nodes %s depend on each other in a circle" (String.concat " -> " (ids @ [ List.hd ids ]))
  | Duplicate_lean { name; nodes } -> Printf.sprintf "the Lean name '%s' is given to more than one node: %s" name (String.concat ", " nodes)
  | Proof_of_definition id -> Printf.sprintf "'%s' is a definition and has a proof; a definition is not proved" id
  | Empty_statement id -> Printf.sprintf "'%s' has no statement, neither in words nor in Lean" id
  | Formal_without_name id -> Printf.sprintf "'%s' has a statement in Lean and no Lean name to state it under" id

let rec duplicates = function
  | [] -> []
  | x :: rest -> if List.mem x rest then x :: duplicates (List.filter (fun y -> y <> x) rest) else duplicates rest

(* A cycle among the nodes, if there is one: depth-first, with the path so far. *)
let find_cycle (doc : Blueprint_doc.t) =
  let known = Blueprint_doc.ids doc in
  let finished = Hashtbl.create 16 in
  let rec visit path id =
    if Hashtbl.mem finished id then None
    else if List.mem id path then
      (* the part of the path from the first visit of [id] *)
      let rec from = function [] -> [] | x :: rest -> if x = id then x :: rest else from rest in
      Some (from (List.rev path))
    else (
      match Blueprint_doc.find doc id with
      | None -> None
      | Some node ->
        let next = List.filter (fun target -> List.mem target known && target <> id) (Blueprint_node.dependencies node) in
        let found = List.find_map (visit (id :: path)) next in
        if found = None then Hashtbl.replace finished id ();
        found)
  in
  List.find_map (visit []) known

let validate (doc : Blueprint_doc.t) =
  let ids = Blueprint_doc.ids doc in
  let unique = List.sort_uniq compare ids in
  let per_node =
    List.concat_map
      (fun (node : Blueprint_node.t) ->
        let id = node.Blueprint_node.id in
        let targets = Blueprint_node.dependencies node in
        (if List.mem id targets then [ Self_reference id ] else [])
        @ List.filter_map (fun target -> if List.mem target unique || target = id then None else Some (Unknown_reference { node = id; target })) targets
        @ (if node.Blueprint_node.kind = Blueprint_node.Definition && (node.Blueprint_node.proof <> "" || node.Blueprint_node.proof_uses <> []) then
             [ Proof_of_definition id ]
           else [])
        @ (if node.Blueprint_node.statement = "" && node.Blueprint_node.formal = None then [ Empty_statement id ] else [])
        @ if node.Blueprint_node.formal <> None && node.Blueprint_node.lean = None then [ Formal_without_name id ] else [])
      doc.Blueprint_doc.nodes
  in
  let lean_names = List.filter_map (fun (node : Blueprint_node.t) -> node.Blueprint_node.lean) doc.Blueprint_doc.nodes in
  let shared =
    List.map
      (fun name ->
        Duplicate_lean
          { name; nodes = List.filter_map (fun (node : Blueprint_node.t) -> if node.Blueprint_node.lean = Some name then Some node.Blueprint_node.id else None) doc.Blueprint_doc.nodes })
      (duplicates lean_names)
  in
  List.map (fun id -> Duplicate_id id) (duplicates ids) @ per_node @ shared @ match find_cycle doc with Some cycle -> [ Cycle cycle ] | None -> []

(** The nodes with everything a node uses before it. Among nodes that could
    come next, the order of the file decides, so the result does not depend
    on anything but the blueprint. Fails with a cycle. *)
let order (doc : Blueprint_doc.t) =
  match find_cycle doc with
  | Some cycle -> Error cycle
  | None ->
    let known = Blueprint_doc.ids doc in
    let rec place placed remaining =
      if remaining = [] then List.rev placed
      else
        let ready (node : Blueprint_node.t) =
          List.for_all
            (fun target -> (not (List.mem target known)) || target = node.Blueprint_node.id || List.exists (fun (p : Blueprint_node.t) -> p.Blueprint_node.id = target) placed)
            (Blueprint_node.dependencies node)
        in
        match List.find_opt ready remaining with
        | Some node -> place (node :: placed) (List.filter (fun n -> n != node) remaining)
        | None -> List.rev_append placed remaining
    in
    Ok (place [] doc.Blueprint_doc.nodes)

(** Everything [id] rests on, directly or through other nodes. *)
let ancestors (doc : Blueprint_doc.t) id =
  let rec grow seen = function
    | [] -> seen
    | next :: rest ->
      if List.mem next seen then grow seen rest
      else (
        match Blueprint_doc.find doc next with
        | Some node -> grow (next :: seen) (Blueprint_node.dependencies node @ rest)
        | None -> grow seen rest)
  in
  match Blueprint_doc.find doc id with
  | Some node -> List.rev (grow [] (List.filter (fun target -> target <> id) (Blueprint_node.dependencies node)))
  | None -> []

(** Everything that rests on [id], directly or through other nodes. *)
let descendants (doc : Blueprint_doc.t) id =
  let rec grow seen = function
    | [] -> seen
    | next :: rest ->
      if List.mem next seen || next = id then grow seen rest
      else grow (next :: seen) (List.map (fun (n : Blueprint_node.t) -> n.Blueprint_node.id) (Blueprint_doc.dependents doc next) @ rest)
  in
  List.rev (grow [] (List.map (fun (n : Blueprint_node.t) -> n.Blueprint_node.id) (Blueprint_doc.dependents doc id)))
