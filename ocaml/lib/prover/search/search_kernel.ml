(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type strategy = Best_first | Breadth_first of { width : int } | Depth_first of { depth : int; branching : int }
type budget = { expansions : int; seconds : float; lean_calls : int; tokens : int }

let unlimited = { expansions = max_int; seconds = Float.infinity; lean_calls = max_int; tokens = max_int }

type 'a tried = { tactic : string; prior : float; step : 'a Search_tree.step }
type 'a expansion = { tried : 'a tried list; lean_calls : int; tokens : int }
type stop = Found | Exhausted | Out_of of string

type step = { node : int; started : float; seconds : float }

type 'a result = {
  stop : stop;
  tree : 'a Search_tree.t;
  root : int;
  script : string list option;
  steps : step list;
  expansions : int;
  lean_calls : int;
  tokens : int;
  seconds : float;
}

let stop_to_string = function Found -> "found" | Exhausted -> "exhausted" | Out_of what -> "out of " ^ what

(* A binary heap of nodes by cost, then by age. *)
module Heap = struct
  type t = { mutable items : (float * int) array; mutable size : int }

  let create () = { items = Array.make 16 (0.0, 0); size = 0 }
  let before (c1, i1) (c2, i2) = c1 < c2 || (c1 = c2 && i1 < i2)

  let push h item =
    if h.size = Array.length h.items then begin
      let bigger = Array.make (2 * h.size) (0.0, 0) in
      Array.blit h.items 0 bigger 0 h.size;
      h.items <- bigger
    end;
    let i = ref h.size in
    h.items.(!i) <- item;
    h.size <- h.size + 1;
    while !i > 0 && before h.items.(!i) h.items.((!i - 1) / 2) do
      let parent = (!i - 1) / 2 in
      let moved = h.items.(parent) in
      h.items.(parent) <- h.items.(!i);
      h.items.(!i) <- moved;
      i := parent
    done

  let pop h =
    if h.size = 0 then None
    else begin
      let top = h.items.(0) in
      h.size <- h.size - 1;
      h.items.(0) <- h.items.(h.size);
      let i = ref 0 and settled = ref false in
      while not !settled do
        let left = (2 * !i) + 1 and right = (2 * !i) + 2 in
        let least = ref !i in
        if left < h.size && before h.items.(left) h.items.(!least) then least := left;
        if right < h.size && before h.items.(right) h.items.(!least) then least := right;
        if !least = !i then settled := true
        else begin
          let moved = h.items.(!least) in
          h.items.(!least) <- h.items.(!i);
          h.items.(!i) <- moved;
          i := !least
        end
      done;
      Some (snd top)
    end
end

(* What is waiting to be expanded, in the order of the strategy. *)
type frontier =
  | By_cost of Heap.t
  | By_level of { queue : int Queue.t; width : int; mutable level : int; mutable taken : int }
  | By_depth of { mutable stack : int list; depth : int; branching : int }

let search ?(now = Unix.gettimeofday) ?(strategy = Best_first) ?(budget = unlimited) ~expand ~root goals =
  let tree = Search_tree.create () in
  let started = now () in
  let root_id = Search_tree.add_root tree ~key:"" root in
  let frontier =
    match strategy with
    | Best_first -> By_cost (Heap.create ())
    | Breadth_first { width } -> By_level { queue = Queue.create (); width; level = 0; taken = 0 }
    | Depth_first { depth; branching } -> By_depth { stack = []; depth; branching }
  in
  (* Nodes that join the frontier on their own: aliases that were cut loose. *)
  let enqueue_nodes ids =
    match frontier with
    | By_cost heap -> List.iter (fun id -> Heap.push heap ((Search_tree.node tree id).Search_tree.cost, id)) ids
    | By_level f -> List.iter (fun id -> Queue.add id f.queue) ids
    | By_depth f -> f.stack <- ids @ f.stack
  in
  (* The subgoals of the edges just added to a node join the frontier. *)
  let enqueue edges =
    let nodes_of (edge : Search_tree.edge) = match edge.Search_tree.outcome with Search_tree.Subgoals cs -> cs | _ -> [] in
    let canonical id = (Search_tree.node tree id).Search_tree.alias_of = None in
    match frontier with
    | By_cost heap ->
      List.iter
        (fun edge ->
          List.iter (fun id -> if canonical id then Heap.push heap ((Search_tree.node tree id).Search_tree.cost, id)) (nodes_of edge))
        edges
    | By_level f -> List.iter (fun edge -> List.iter (fun id -> if canonical id then Queue.add id f.queue) (nodes_of edge)) edges
    | By_depth f ->
      (* Only the first tactics that worked are followed; the subgoals of the others are given up. *)
      let working = List.filter (fun edge -> match edge.Search_tree.outcome with Search_tree.Subgoals _ -> true | _ -> false) edges in
      let followed = List.filteri (fun i _ -> i < f.branching) working in
      List.iteri
        (fun i edge -> if i >= f.branching then List.iter (fun id -> Search_tree.abandon tree id "branching limit") (nodes_of edge))
        working;
      let fresh = List.concat_map (fun edge -> List.filter canonical (nodes_of edge)) followed in
      f.stack <- fresh @ f.stack
  in
  let rec next () =
    match frontier with
    | By_cost heap -> (match Heap.pop heap with None -> None | Some id -> usable id)
    | By_level f ->
      (match Queue.take_opt f.queue with
       | None -> None
       | Some id ->
         let depth = (Search_tree.node tree id).Search_tree.depth in
         if depth > f.level then begin
           f.level <- depth;
           f.taken <- 0
         end;
         if not (Search_tree.relevant tree id) then next ()
         else if f.taken >= f.width then begin
           Search_tree.abandon tree id "width cap";
           next ()
         end
         else begin
           f.taken <- f.taken + 1;
           Some id
         end)
    | By_depth f ->
      (match f.stack with
       | [] -> None
       | id :: rest ->
         f.stack <- rest;
         if not (Search_tree.relevant tree id) then next ()
         else if (Search_tree.node tree id).Search_tree.depth > f.depth then begin
           Search_tree.abandon tree id "depth limit";
           next ()
         end
         else Some id)
  and usable id = if Search_tree.relevant tree id then Some id else next () in
  let expansions = ref 0 and lean_calls = ref 0 and tokens = ref 0 and steps = ref [] in
  let finish stop =
    Ok
      { stop; tree; root = root_id; script = (if stop = Found then Search_tree.script tree root_id else None);
        steps = List.rev !steps; expansions = !expansions; lean_calls = !lean_calls; tokens = !tokens; seconds = now () -. started }
  in
  let spent () =
    if !expansions >= budget.expansions then Some "expansions"
    else if now () -. started >= budget.seconds then Some "time"
    else if !lean_calls >= budget.lean_calls then Some "lean calls"
    else if !tokens >= budget.tokens then Some "tokens"
    else None
  in
  (* The goals are the subgoals of one edge of the root, so the root is proved when all of them are. *)
  let first = Search_tree.add_edge tree root_id ~tactic:"" ~prior:1.0 (Search_tree.Leaves goals) in
  Search_tree.finish tree root_id;
  enqueue [ first ];
  let rec loop () =
    enqueue_nodes (Search_tree.take_detached tree);
    match Search_tree.effective tree root_id with
    | Search_tree.Proved -> finish Found
    | Search_tree.Failed -> finish Exhausted
    | Search_tree.Open ->
      (match next () with
       | None -> finish Exhausted
       | Some id ->
         (match spent () with
          | Some what -> finish (Out_of what)
          | None ->
            let began = now () in
            (match expand (Search_tree.node tree id) with
             | Error e -> Error e
             | Ok (expansion : _ expansion) ->
               incr expansions;
               lean_calls := !lean_calls + expansion.lean_calls;
               tokens := !tokens + expansion.tokens;
               let edges =
                 List.map
                   (fun (tried : _ tried) -> Search_tree.add_edge tree id ~tactic:tried.tactic ~prior:tried.prior tried.step)
                   expansion.tried
               in
               Search_tree.finish tree id;
               enqueue edges;
               steps := { node = id; started = began -. started; seconds = now () -. began } :: !steps;
               loop ())))
  in
  loop ()
