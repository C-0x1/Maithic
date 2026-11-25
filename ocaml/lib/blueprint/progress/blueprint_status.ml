(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Where a formalization stands: the blueprint, read together with what Lean
    reports about the declarations it names. *)

open Maithic_base

type state =
  | Planned  (** Not in Lean yet. *)
  | Stated  (** In Lean, with [sorry] in its own text. *)
  | Proved  (** Its own text is finished; something it rests on is not. *)
  | Done  (** Finished, with everything it rests on. *)

let states = [ Planned; Stated; Proved; Done ]
let state_name = function Planned -> "planned" | Stated -> "stated" | Proved -> "proved" | Done -> "done"

(** A node is settled when nobody has to work on its own text any more. *)
let settled = function Proved | Done -> true | Planned | Stated -> false

(** What Lean reports about one declaration. *)
type lean_decl = {
  name : string;
  own_sorry : bool;  (** Its own text contains [sorry]. *)
  uses_sorry : bool;  (** It rests on [sorry], its own or that of something it uses. *)
  deps : string list;  (** The constants its statement and its body refer to. *)
}

type entry = {
  node : Blueprint_node.t;
  lean_name : string option;
  state : state;
  blockers : string list;  (** What it uses that is not settled; empty for a settled node. *)
  ready : bool;  (** Not settled, and nothing it uses stands in the way. *)
  unblocks : int;  (** Nodes resting on it, directly or not, that are not settled. *)
  notes : string list;  (** Where the blueprint and the Lean text disagree about what it uses. *)
}

type t = { doc : Blueprint_doc.t; entries : entry list; lean_file : string option; lean_errors : int; label : string option }

let find_entry status id = List.find_opt (fun e -> e.node.Blueprint_node.id = id) status.entries

let compute ?lean_file ?(lean_errors = 0) ?label (doc : Blueprint_doc.t) (decls : lean_decl list) =
  let decl_of (node : Blueprint_node.t) =
    match Blueprint_doc.lean_name doc node with
    | None -> None
    | Some full ->
      (match List.find_opt (fun d -> d.name = full) decls with
       | Some d -> Some d
       | None -> Option.bind node.Blueprint_node.lean (fun short -> List.find_opt (fun d -> d.name = short) decls))
  in
  let state_of node =
    match decl_of node with None -> Planned | Some d -> if d.own_sorry then Stated else if d.uses_sorry then Proved else Done
  in
  let states = List.map (fun (node : Blueprint_node.t) -> (node.Blueprint_node.id, state_of node)) doc.Blueprint_doc.nodes in
  let state id = Option.value ~default:Planned (List.assoc_opt id states) in
  let known id = List.mem_assoc id states in
  let entries =
    List.map
      (fun (node : Blueprint_node.t) ->
        let id = node.Blueprint_node.id in
        let own = state id in
        let uses = List.filter (fun target -> known target && target <> id) (Blueprint_node.dependencies node) in
        let blockers = if settled own then [] else List.filter (fun target -> not (settled (state target))) uses in
        let unblocks = List.length (List.filter (fun d -> not (settled (state d))) (Blueprint_check.descendants doc id)) in
        (* where the blueprint and Lean disagree, once the node's own text is finished *)
        let notes =
          match decl_of node with
          | Some d when not d.own_sorry ->
            let lean_of target = Option.bind (Blueprint_doc.find doc target) (fun n -> Option.map (fun (x : lean_decl) -> x.name) (decl_of n)) in
            let unused =
              List.filter_map
                (fun target -> match lean_of target with Some name when not (List.mem name d.deps) -> Some target | _ -> None)
                uses
            in
            let unlisted =
              List.filter_map
                (fun (other : Blueprint_node.t) ->
                  let oid = other.Blueprint_node.id in
                  if oid = id || List.mem oid uses then None
                  else match decl_of other with Some o when List.mem o.name d.deps -> Some oid | _ -> None)
                doc.Blueprint_doc.nodes
            in
            (if unused = [] then [] else [ Printf.sprintf "the blueprint lists %s, and the Lean text does not refer to %s" (String.concat ", " unused) (if List.length unused = 1 then "it" else "them") ])
            @ if unlisted = [] then [] else [ Printf.sprintf "the Lean text refers to %s, which the blueprint does not list" (String.concat ", " unlisted) ]
          | _ -> []
        in
        { node; lean_name = Blueprint_doc.lean_name doc node; state = own; blockers; ready = (not (settled own)) && blockers = []; unblocks; notes })
      doc.Blueprint_doc.nodes
  in
  { doc; entries; lean_file; lean_errors; label }

(* ---- questions ---- *)

let count status state = List.length (List.filter (fun e -> e.state = state) status.entries)

(** How many nodes of each kind are in each state; kinds without a node are left out. *)
let by_kind status =
  List.filter_map
    (fun kind ->
      let of_kind = List.filter (fun e -> e.node.Blueprint_node.kind = kind) status.entries in
      if of_kind = [] then None else Some (kind, List.map (fun state -> (state, List.length (List.filter (fun e -> e.state = state) of_kind))) states))
    Blueprint_node.kinds

(** The nodes one can work on now, the most useful first: the one that the
    most unsettled nodes rest on, then one that is already stated before one
    that is not, then the order of the file. *)
let frontier status =
  let indexed = List.mapi (fun i e -> (i, e)) (List.filter (fun e -> e.ready) status.entries) in
  let rank e = if e.state = Stated then 0 else 1 in
  List.map snd (List.sort (fun (i, a) (j, b) -> compare (-a.unblocks, rank a, i) (-b.unblocks, rank b, j)) indexed)

(** The nodes that wait for others, each with what it waits for. *)
let blocked status = List.filter (fun e -> e.blockers <> []) status.entries

let complete status = List.for_all (fun e -> e.state = Done) status.entries

(* ---- output ---- *)

let schema = "maithic.blueprint.status/1"
let strings items = Json.List (List.map (fun s -> Json.String s) items)

let to_json status =
  let entry e =
    Json.Obj
      ([ ("id", Json.String e.node.Blueprint_node.id); ("kind", Json.String (Blueprint_node.kind_name e.node.Blueprint_node.kind));
         ("title", Json.String e.node.Blueprint_node.title) ]
      @ (match e.lean_name with Some name -> [ ("lean", Json.String name) ] | None -> [])
      @ [ ("status", Json.String (state_name e.state)); ("uses", strings e.node.Blueprint_node.uses);
          ("proof_uses", strings e.node.Blueprint_node.proof_uses); ("blockers", strings e.blockers); ("ready", Json.Bool e.ready);
          ("unblocks", Json.Int e.unblocks); ("notes", strings e.notes) ])
  in
  let counts pairs = Json.Obj (List.map (fun (state, n) -> (state_name state, Json.Int n)) pairs) in
  Json.Obj
    ([ ("schema", Json.String schema); ("title", Json.String status.doc.Blueprint_doc.title) ]
    @ (match status.label with Some label -> [ ("label", Json.String label) ] | None -> [])
    @ (match status.lean_file with Some file -> [ ("lean", Json.Obj [ ("file", Json.String file); ("errors", Json.Int status.lean_errors) ]) ] | None -> [])
    @ [ ( "summary",
          Json.Obj
            [ ("nodes", Json.Int (List.length status.entries)); ("by_status", counts (List.map (fun state -> (state, count status state)) states));
              ("by_kind", Json.Obj (List.map (fun (kind, pairs) -> (Blueprint_node.kind_name kind, counts pairs)) (by_kind status)));
              ("complete", Json.Bool (complete status)) ] );
        ("nodes", Json.List (List.map entry status.entries));
        ("frontier", strings (List.map (fun e -> e.node.Blueprint_node.id) (frontier status)));
        ( "blocked",
          Json.List
            (List.map (fun e -> Json.Obj [ ("id", Json.String e.node.Blueprint_node.id); ("blockers", strings e.blockers) ]) (blocked status)) ) ])

let paint ~color state text =
  if not color then text
  else
    let code = match state with Done -> "32" | Proved -> "36" | Stated -> "33" | Planned -> "2" in
    Printf.sprintf "\027[%sm%s\027[0m" code text

let summary status =
  let total = List.length status.entries in
  Printf.sprintf "%d node(s): %s" total (String.concat ", " (List.map (fun state -> Printf.sprintf "%d %s" (count status state) (state_name state)) (List.rev states)))

(** The status as a table for a terminal, with the summary below it. *)
let render ?(color = false) status =
  let width f = List.fold_left (fun w e -> max w (Text.utf8_length (f e))) 0 status.entries in
  let id e = e.node.Blueprint_node.id and kind e = Blueprint_node.kind_name e.node.Blueprint_node.kind in
  let wid = max 4 (width id) and wkind = max 4 (width kind) in
  let waits e =
    if e.blockers <> [] then "waits for " ^ String.concat ", " e.blockers
    else if e.ready then (if e.state = Stated then "ready to prove" else "ready to state") ^ if e.unblocks > 0 then Printf.sprintf "; %d node(s) rest on it" e.unblocks else ""
    else ""
  in
  let line e =
    Printf.sprintf "  %s  %s  %s  %s" (Text.pad_right wid (id e)) (Text.pad_right wkind (kind e))
      (paint ~color e.state (Text.pad_right 7 (state_name e.state)))
      (waits e)
    :: List.map (fun note -> Printf.sprintf "  %s  note: %s" (String.make wid ' ') note) e.notes
  in
  let head = Printf.sprintf "  %s  %s  %s  %s" (Text.pad_right wid "node") (Text.pad_right wkind "kind") (Text.pad_right 7 "status") "" in
  let kinds =
    List.map
      (fun (k, pairs) ->
        Printf.sprintf "  %s: %s" (Text.pad_right 11 (Blueprint_node.kind_name k))
          (String.concat ", " (List.filter_map (fun (state, n) -> if n = 0 then None else Some (Printf.sprintf "%d %s" n (state_name state))) (List.rev pairs))))
      (by_kind status)
  in
  String.concat "\n"
    ([ status.doc.Blueprint_doc.title; "  " ^ Text.trim head ]
    @ List.concat_map line status.entries
    @ (if status.lean_errors > 0 then [ Printf.sprintf "Lean reports %d error(s) in the file; declarations after an error may be missing" status.lean_errors ] else [])
    @ [ summary status ] @ kinds)

(** The frontier as a numbered list. *)
let render_next ?(color = false) status =
  match frontier status with
  | [] -> if complete status then "nothing is left: every node is done" else "nothing is ready: every unsettled node waits for another"
  | ready ->
    String.concat "\n"
      (List.mapi
         (fun i e ->
           Printf.sprintf "%d. %s  %s  %s  %s%s" (i + 1) e.node.Blueprint_node.id
             (Blueprint_node.kind_name e.node.Blueprint_node.kind)
             (paint ~color e.state (if e.state = Stated then "to prove" else "to state"))
             e.node.Blueprint_node.title
             (if e.unblocks > 0 then Printf.sprintf "  (%d node(s) rest on it)" e.unblocks else ""))
         ready)
