(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(* Breadth-first over one direction of the uses edges. *)
let reach ?depth ?(keep = fun (_ : Intel_graph.node) -> true) step t name =
  let seen = Hashtbl.create 256 in
  Hashtbl.replace seen name 0;
  let rec go frontier distance acc =
    if frontier = [] || (match depth with Some limit -> distance > limit | None -> false) then acc
    else begin
      let next =
        List.concat_map
          (fun current ->
            List.filter
              (fun (n : Intel_graph.node) ->
                if Hashtbl.mem seen n.Intel_graph.name || not (keep n) then false
                else begin
                  Hashtbl.replace seen n.Intel_graph.name distance;
                  true
                end)
              (step t current))
          frontier
      in
      go (List.map (fun (n : Intel_graph.node) -> n.Intel_graph.name) next) (distance + 1) (acc @ List.map (fun n -> (n, distance)) next)
    end
  in
  List.sort
    (fun ((a : Intel_graph.node), x) ((b : Intel_graph.node), y) -> compare (x, a.Intel_graph.name) (y, b.Intel_graph.name))
    (go [ name ] 1 [])

let dependencies ?depth ?(internal_only = false) t name =
  reach ?depth ~keep:(fun n -> (not internal_only) || n.Intel_graph.internal) Intel_graph.dependencies t name

let dependents ?depth t name = reach ?depth Intel_graph.dependents t name

let by_place nodes =
  List.sort
    (fun (a : Intel_graph.node) (b : Intel_graph.node) ->
      compare (a.Intel_graph.file, a.Intel_graph.line, a.Intel_graph.name) (b.Intel_graph.file, b.Intel_graph.line, b.Intel_graph.name))
    nodes

let unfinished t = by_place (List.filter (fun (n : Intel_graph.node) -> n.Intel_graph.contains_sorry) (Intel_graph.nodes t))

let affected t =
  by_place (List.filter (fun (n : Intel_graph.node) -> n.Intel_graph.uses_sorry && not n.Intel_graph.contains_sorry) (Intel_graph.nodes t))
