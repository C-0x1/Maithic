(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type status = Open | Proved | Failed

type outcome =
  | Subgoals of int list
  | Rejected of { error_class : string; message : string }
  | Blocked of { node : int }

type edge = { tactic : string; prior : float; outcome : outcome }

type 'a node = {
  id : int;
  parent : int option;
  depth : int;
  cost : float;
  key : string;
  payload : 'a;
  mutable alias_of : int option;
  mutable aliases : int list;
  mutable status : status;
  mutable expanded : bool;
  mutable edges : edge list;
  mutable proved_by : int option;
  mutable abandoned : string option;
  mutable tainted : bool;
}

type 'a t = { nodes : 'a node Dynarray.t; keys : (string, int) Hashtbl.t; mutable detached : int list }
type 'a step = Leaves of (string * 'a) list | Fails of { error_class : string; message : string }

let create () = { nodes = Dynarray.create (); keys = Hashtbl.create 64; detached = [] }
let size t = Dynarray.length t.nodes
let node t id = Dynarray.get t.nodes id
let status_to_string = function Open -> "open" | Proved -> "proved" | Failed -> "failed"

let new_node t ~parent ~depth ~cost ~key ~alias_of payload =
  let id = size t in
  let n =
    { id; parent; depth; cost; key; payload; alias_of; aliases = []; status = Open; expanded = false; edges = [];
      proved_by = None; abandoned = None; tainted = false }
  in
  Dynarray.add_last t.nodes n;
  (match alias_of with
   | Some canonical ->
     let c = node t canonical in
     c.aliases <- id :: c.aliases;
     n.status <- c.status
   | None -> Hashtbl.replace t.keys key id);
  id

let add_root t ~key payload = new_node t ~parent:None ~depth:0 ~cost:0.0 ~key ~alias_of:None payload

let effective t id =
  let n = node t id in
  match n.alias_of with Some canonical -> (node t canonical).status | None -> n.status

(* The node with this key among a node and those above it. *)
let rec on_the_way t id key =
  let n = node t id in
  if n.key = key then Some id else match n.parent with Some p -> on_the_way t p key | None -> None

let add_edge t id ~tactic ~prior step =
  let n = node t id in
  let prior = if Float.is_nan prior || prior <= 0.0 then Float.min_float else Float.min prior 1.0 in
  let outcome =
    match step with
    | Fails { error_class; message } -> Rejected { error_class; message }
    | Leaves leaves ->
      (match List.find_map (fun (key, _) -> on_the_way t id key) leaves with
       | Some node -> Blocked { node }
       | None ->
         let cost = n.cost -. Float.log prior in
         Subgoals
           (List.map
              (fun (key, payload) ->
                (* A node that failed where it stands says nothing about the goal elsewhere. *)
                let alias_of =
                  match Hashtbl.find_opt t.keys key with
                  | Some canonical when not ((node t canonical).status = Failed && (node t canonical).tainted) -> Some canonical
                  | _ -> None
                in
                new_node t ~parent:(Some id) ~depth:(n.depth + 1) ~cost ~key ~alias_of payload)
              leaves))
  in
  let edge = { tactic; prior; outcome } in
  n.edges <- n.edges @ [ edge ];
  edge

let edge_proved t edge = match edge.outcome with Subgoals cs -> List.for_all (fun c -> effective t c = Proved) cs | _ -> false
let edge_dead t edge = match edge.outcome with Subgoals cs -> List.exists (fun c -> effective t c = Failed) cs | _ -> true

let position p xs =
  let rec go i = function [] -> None | x :: rest -> if p x then Some i else go (i + 1) rest in
  go 0 xs

(* Whether an edge that cannot succeed fails for a reason that holds wherever the node stands:
   the tactic was rejected, or a subgoal failed with no taint. *)
let edge_clean t edge =
  match edge.outcome with
  | Rejected _ -> true
  | Blocked _ -> false
  | Subgoals cs ->
    List.exists
      (fun c ->
        let c = node t c in
        let c = match c.alias_of with Some canonical -> node t canonical | None -> c in
        c.status = Failed && not c.tainted)
      cs

(* Recomputes the status of a node and, when it changes, of what depends on
   it: its parent, its aliases and their parents. A status only ever moves
   from open to proved or failed, so this ends. *)
let rec refresh t id =
  let n = node t id in
  let now =
    match n.alias_of with
    | Some canonical -> (node t canonical).status
    | None ->
      if n.status <> Open then n.status
      else if n.abandoned <> None then begin
        n.tainted <- true;
        Failed
      end
      else
        (match position (edge_proved t) n.edges with
         | Some i ->
           n.proved_by <- Some i;
           Proved
         | None ->
           if n.expanded && List.for_all (edge_dead t) n.edges then begin
             n.tainted <- not (List.for_all (edge_clean t) n.edges);
             Failed
           end
           else Open)
  in
  if now <> n.status then begin
    n.status <- now;
    if now = Failed && n.tainted then begin
      (* The aliases are searched where they stand. *)
      let loose = List.rev n.aliases in
      n.aliases <- [];
      List.iter (fun alias -> (node t alias).alias_of <- None) loose;
      t.detached <- t.detached @ loose
    end
    else List.iter (refresh t) (List.rev n.aliases);
    Option.iter (refresh t) n.parent
  end

let take_detached t =
  let loose = t.detached in
  t.detached <- [];
  loose

let finish t id =
  (node t id).expanded <- true;
  refresh t id

let abandon t id reason =
  let n = node t id in
  if n.status = Open && n.alias_of = None then begin
    n.abandoned <- Some reason;
    refresh t id
  end

let relevant t id =
  let visited = Hashtbl.create 16 in
  (* Whether some way up from the node stays among open nodes and live edges. *)
  let rec wanted id =
    if Hashtbl.mem visited id then false
    else begin
      Hashtbl.replace visited id ();
      let n = node t id in
      effective t id = Open
      && (via_parent n || List.exists (fun alias -> via_parent (node t alias)) n.aliases)
    end
  and via_parent n =
    match n.parent with
    | None -> n.alias_of = None
    | Some p ->
      let parent = node t p in
      List.exists
        (fun edge -> match edge.outcome with Subgoals cs -> List.mem n.id cs && not (edge_dead t edge) | _ -> false)
        parent.edges
      && wanted p
  in
  let n = node t id in
  n.alias_of = None && (not n.expanded) && n.status = Open && wanted id

let rec script t id =
  let n = node t id in
  match n.alias_of with
  | Some canonical -> script t canonical
  | None ->
    (match n.status, n.proved_by with
     | Proved, Some i ->
       let edge = List.nth n.edges i in
       let children = match edge.outcome with Subgoals cs -> cs | _ -> [] in
       let parts = List.map (script t) children in
       if List.exists Option.is_none parts then None
       else begin
         let parts = List.map Option.get parts in
         let below =
           match parts with
           | [ only ] -> only
           | several ->
             List.concat_map
               (function [] -> [] | first :: rest -> ("\xC2\xB7 " ^ first) :: List.map (fun line -> "  " ^ line) rest)
               several
         in
         Some (if edge.tactic = "" then below else edge.tactic :: below)
       end
     | _ -> None)

let check_invariants t =
  let problem = ref None in
  let fail id message = if !problem = None then problem := Some (Printf.sprintf "node %d: %s" id message) in
  Dynarray.iteri
    (fun id n ->
      if n.id <> id then fail id "the node does not know its own index";
      (match n.parent with
       | Some p ->
         if p >= id then fail id "the parent is not older than the node";
         let parent = node t p in
         if n.depth <> parent.depth + 1 then fail id "the depth is not one more than the parent's";
         if not (List.exists (fun e -> match e.outcome with Subgoals cs -> List.mem id cs | _ -> false) parent.edges) then
           fail id "no edge of the parent leads to the node"
       | None -> if n.depth <> 0 then fail id "a root has depth zero");
      (match n.alias_of with
       | Some canonical ->
         let c = node t canonical in
         if c.alias_of <> None then fail id "an alias points to an alias";
         if c.key <> n.key then fail id "an alias has another key than its node";
         if not (List.mem id c.aliases) then fail id "the node does not list its alias";
         if n.edges <> [] || n.expanded then fail id "an alias was expanded";
         if n.status <> c.status then fail id "an alias has another status than its node";
         if c.status = Failed && c.tainted then fail id "an alias shares a failure that holds only where its node stands"
       | None ->
         let proved = List.exists (edge_proved t) n.edges in
         let dead = n.abandoned <> None || (n.expanded && List.for_all (edge_dead t) n.edges) in
         (match n.status with
          | Proved ->
            (match n.proved_by with
             | Some i when i < List.length n.edges && edge_proved t (List.nth n.edges i) -> ()
             | _ -> fail id "a proved node names no edge that proves it")
          | Failed -> if not dead then fail id "a failed node could still succeed"
          | Open -> if proved then fail id "an open node has a proved edge" else if dead then fail id "an open node has no way left"));
      List.iter
        (fun e ->
          match e.outcome with
          | Subgoals cs -> List.iter (fun c -> if (node t c).parent <> Some id then fail id "a subgoal has another parent") cs
          | Blocked { node = b } -> if b > id then fail id "a block points below the node"
          | Rejected _ -> ())
        n.edges)
    t.nodes;
  match !problem with Some message -> Error message | None -> Ok ()
