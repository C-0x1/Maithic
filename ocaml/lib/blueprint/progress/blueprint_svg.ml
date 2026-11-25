(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A picture of a blueprint and its status: the nodes in layers, everything
    a node uses above it, coloured by how far each one is. *)

open Maithic_base

let box_w = 208
let gap_x = 22
let gap_y = 54
let margin = 40

let escape text =
  let buffer = Buffer.create (String.length text) in
  String.iter
    (function '&' -> Buffer.add_string buffer "&amp;" | '<' -> Buffer.add_string buffer "&lt;" | '>' -> Buffer.add_string buffer "&gt;" | '"' -> Buffer.add_string buffer "&quot;" | c -> Buffer.add_char buffer c)
    text;
  Buffer.contents buffer

(* The characters of a title that fit in one line of a box. *)
let title_chars = 29

(** The words of a title in at most two lines of a box. What does not fit in
    the second line is left out, with a mark. *)
let title_lines title =
  let words = List.filter (fun w -> w <> "") (String.split_on_char ' ' title) in
  let rec fill line = function
    | [] -> (line, [])
    | word :: rest as all ->
      if line = "" then fill word rest
      else if Text.utf8_length line + 1 + Text.utf8_length word <= title_chars then fill (line ^ " " ^ word) rest
      else (line, all)
  in
  let first, rest = fill "" words in
  if rest = [] then [ Text.truncate title_chars first ] else [ Text.truncate title_chars first; Text.truncate_words title_chars (String.concat " " rest) ]

(* fill, stroke, colour of the text, dashed border *)
let style = function
  | Blueprint_status.Done -> ("#0FA89B", "#0B7F75", "#FFFFFF", false)
  | Blueprint_status.Proved -> ("#2B2F77", "#14162B", "#FFFFFF", false)
  | Blueprint_status.Stated -> ("#FFF4DB", "#F0A92E", "#14162B", false)
  | Blueprint_status.Planned -> ("#FFFFFF", "#8A90A6", "#5B6477", true)

(* The layer of every node: one below the lowest thing it uses. *)
let layers (status : Blueprint_status.t) =
  let doc = status.Blueprint_status.doc in
  let known = Blueprint_doc.ids doc in
  let memo = Hashtbl.create 16 in
  let rec layer path id =
    match Hashtbl.find_opt memo id with
    | Some l -> l
    | None ->
      let uses =
        match Blueprint_doc.find doc id with
        | Some node -> List.filter (fun t -> List.mem t known && t <> id && not (List.mem t path)) (Blueprint_node.dependencies node)
        | None -> []
      in
      let l = List.fold_left (fun acc t -> max acc (1 + layer (id :: path) t)) 0 uses in
      Hashtbl.replace memo id l;
      l
  in
  List.map (fun id -> (id, layer [] id)) known

(* The edges worth drawing: one that follows from others is left out. An edge
   from a node to something it also reaches through another node it uses says
   nothing the picture does not show already. *)
let drawn_edges (doc : Blueprint_doc.t) =
  List.filter
    (fun (edge : Blueprint_doc.edge) ->
      match Blueprint_doc.find doc edge.Blueprint_doc.source with
      | None -> false
      | Some node ->
        not
          (List.exists
             (fun other -> other <> edge.Blueprint_doc.target && List.mem edge.Blueprint_doc.target (Blueprint_check.ancestors doc other))
             (Blueprint_node.dependencies node)))
    (Blueprint_doc.edges doc)

let render (status : Blueprint_status.t) =
  let doc = status.Blueprint_status.doc in
  let layer_of = layers status in
  let depth = List.fold_left (fun acc (_, l) -> max acc l) 0 layer_of in
  (* the nodes of each layer, ordered by the mean position of what they use in the layers above *)
  let position = Hashtbl.create 16 in
  let rows =
    List.init (depth + 1) (fun l ->
        let ids = List.filter_map (fun (id, layer) -> if layer = l then Some id else None) layer_of in
        let weight id =
          match Blueprint_doc.find doc id with
          | None -> 0.0
          | Some node ->
            let xs = List.filter_map (fun t -> Hashtbl.find_opt position t) (Blueprint_node.dependencies node) in
            if xs = [] then 0.0 else List.fold_left ( +. ) 0.0 xs /. float_of_int (List.length xs)
        in
        let ordered = List.stable_sort (fun a b -> compare (weight a) (weight b)) ids in
        List.iteri (fun i id -> Hashtbl.replace position id (float_of_int i)) ordered;
        ordered)
  in
  let widest = List.fold_left (fun acc row -> max acc (List.length row)) 1 rows in
  let inner = (widest * box_w) + ((widest - 1) * gap_x) in
  let width = max 720 (inner + (2 * margin) + 60) in
  let top = 96 in
  (* every box is as high as the longest title needs: one line of it, or two *)
  let two_lines = List.exists (fun (entry : Blueprint_status.entry) -> List.length (title_lines entry.Blueprint_status.node.Blueprint_node.title) > 1) status.Blueprint_status.entries in
  let box_h = if two_lines then 78 else 58 in
  let height = top + ((depth + 1) * box_h) + (depth * gap_y) + 112 in
  let place = Hashtbl.create 16 in
  List.iteri
    (fun l row ->
      let n = List.length row in
      let row_w = (n * box_w) + ((n - 1) * gap_x) in
      let x0 = (width - row_w) / 2 in
      List.iteri (fun i id -> Hashtbl.replace place id (x0 + (i * (box_w + gap_x)), top + (l * (box_h + gap_y)))) row)
    rows;
  let buffer = Buffer.create 4096 in
  let add fmt = Printf.ksprintf (fun s -> Buffer.add_string buffer s; Buffer.add_char buffer '\n') fmt in
  let summary = Blueprint_status.summary status in
  add "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" width=\"%d\" height=\"%d\" role=\"img\" aria-labelledby=\"t d\" font-family=\"'Helvetica Neue', Arial, sans-serif\">" width height width height;
  add "  <title id=\"t\">%s</title>" (escape doc.Blueprint_doc.title);
  add "  <desc id=\"d\">The nodes of the blueprint in layers, each below what it uses, coloured by status. %s.</desc>" (escape summary);
  add "  <rect width=\"%d\" height=\"%d\" fill=\"#FAF8F4\"/>" width height;
  add "  <text x=\"%d\" y=\"40\" font-size=\"20\" font-weight=\"700\" fill=\"#14162B\">%s</text>" margin (escape doc.Blueprint_doc.title);
  add "  <text x=\"%d\" y=\"62\" font-size=\"13\" fill=\"#5B6477\">%s</text>" margin (escape summary);
  (* edges first, so that the boxes cover their ends; an edge that passes a
     layer goes round its boxes *)
  let layer id = Option.value ~default:0 (List.assoc_opt id layer_of) in
  let extent l =
    List.fold_left
      (fun (lo, hi) id -> match Hashtbl.find_opt place id with Some (x, _) -> (min lo x, max hi (x + box_w)) | None -> (lo, hi))
      (max_int, min_int)
      (List.nth rows l)
  in
  (* Where an edge leaves the bottom of a node and where it arrives at the top of another: the
     edges at one side of a node are spread along it in the order of their other ends, so that
     no two of them share a point. *)
  let edges = drawn_edges doc in
  let centre id = match Hashtbl.find_opt place id with Some (x, _) -> x + (box_w / 2) | None -> 0 in
  let port left others other_end edge =
    let sorted = List.stable_sort (fun a b -> compare (centre (other_end a)) (centre (other_end b))) others in
    let rec index i = function [] -> 0 | e :: rest -> if e == edge then i else index (i + 1) rest in
    left + (box_w * (index 0 sorted + 1) / (List.length sorted + 1))
  in
  List.iter
    (fun (edge : Blueprint_doc.edge) ->
      match (Hashtbl.find_opt place edge.Blueprint_doc.target, Hashtbl.find_opt place edge.Blueprint_doc.source) with
      | Some (x1, y1), Some (x2, y2) ->
        let leaving = List.filter (fun (e : Blueprint_doc.edge) -> e.Blueprint_doc.target = edge.Blueprint_doc.target) edges in
        let arriving = List.filter (fun (e : Blueprint_doc.edge) -> e.Blueprint_doc.source = edge.Blueprint_doc.source) edges in
        let ax = port x1 leaving (fun (e : Blueprint_doc.edge) -> e.Blueprint_doc.source) edge
        and bx = port x2 arriving (fun (e : Blueprint_doc.edge) -> e.Blueprint_doc.target) edge in
        let ay = y1 + box_h and by = y2 in
        let dash = if edge.Blueprint_doc.role = Blueprint_doc.In_proof then " stroke-dasharray=\"5 4\"" else "" in
        let from_layer = layer edge.Blueprint_doc.target and to_layer = layer edge.Blueprint_doc.source in
        if to_layer - from_layer <= 1 then begin
          let mid = (ay + by) / 2 in
          add "  <path d=\"M%d %d C%d %d %d %d %d %d\" fill=\"none\" stroke=\"#8A90A6\" stroke-width=\"1.4\"%s/>" ax ay ax mid bx mid bx (by - 6) dash
        end
        else begin
          let lo, hi =
            List.fold_left (fun (lo, hi) l -> let a, b = extent l in (min lo a, max hi b)) (max_int, min_int)
              (List.init (to_layer - from_layer - 1) (fun i -> from_layer + 1 + i))
          in
          let lane = if ax + bx >= lo + hi then hi + 24 else lo - 24 in
          let turn = gap_y / 2 in
          add "  <path d=\"M%d %d C%d %d %d %d %d %d L%d %d C%d %d %d %d %d %d\" fill=\"none\" stroke=\"#8A90A6\" stroke-width=\"1.4\"%s/>" ax ay ax (ay + turn) lane
            (ay + turn) lane (ay + gap_y) lane (by - gap_y) lane (by - turn) bx (by - turn) bx (by - 6) dash
        end;
        add "  <path d=\"M%d %d l5 -8 h-10 z\" fill=\"#8A90A6\"/>" bx by
      | _ -> ())
    edges;
  List.iter
    (fun (entry : Blueprint_status.entry) ->
      let node = entry.Blueprint_status.node in
      match Hashtbl.find_opt place node.Blueprint_node.id with
      | None -> ()
      | Some (x, y) ->
        let fill, stroke, ink, dashed = style entry.Blueprint_status.state in
        add "  <rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"9\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1.6\"%s/>" x y box_w box_h fill stroke
          (if dashed then " stroke-dasharray=\"5 4\"" else "");
        add "  <text x=\"%d\" y=\"%d\" font-size=\"12.5\" font-weight=\"700\" fill=\"%s\" font-family=\"Menlo, Consolas, monospace\">%s</text>" (x + 12) (y + 21) ink
          (escape (Text.truncate 20 node.Blueprint_node.id));
        List.iteri
          (fun i line -> add "  <text x=\"%d\" y=\"%d\" font-size=\"11\" fill=\"%s\">%s</text>" (x + 12) (y + 38 + (i * 14)) ink (escape line))
          (title_lines node.Blueprint_node.title);
        add "  <text x=\"%d\" y=\"%d\" font-size=\"10.5\" fill=\"%s\" opacity=\"0.85\">%s · %s%s</text>" (x + 12) (y + box_h - (if two_lines then 10 else 7)) ink
          (Blueprint_node.kind_name node.Blueprint_node.kind)
          (Blueprint_status.state_name entry.Blueprint_status.state)
          (if entry.Blueprint_status.ready then " · ready" else ""))
    status.Blueprint_status.entries;
  (* the legend *)
  let ly = height - 78 in
  List.iteri
    (fun i state ->
      let fill, stroke, _, dashed = style state in
      let x = margin + (i * 112) in
      add "  <rect x=\"%d\" y=\"%d\" width=\"18\" height=\"18\" rx=\"4\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1.4\"%s/>" x ly fill stroke
        (if dashed then " stroke-dasharray=\"4 3\"" else "");
      add "  <text x=\"%d\" y=\"%d\" font-size=\"12\" fill=\"#14162B\">%s</text>" (x + 26) (ly + 14) (Blueprint_status.state_name state))
    Blueprint_status.states;
  add "  <text x=\"%d\" y=\"%d\" font-size=\"11.5\" fill=\"#5B6477\">An arrow points from what is used to what uses it; it is dashed when only the proof uses it.</text>" margin (ly + 42);
  add "  <text x=\"%d\" y=\"%d\" font-size=\"11.5\" fill=\"#5B6477\">An arrow that follows from others is left out.</text>" margin (ly + 58);
  Buffer.add_string buffer "</svg>\n";
  Buffer.contents buffer
