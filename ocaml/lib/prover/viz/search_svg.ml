(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Search_record

let ink = "#14162B"
let indigo = "#2B2F77"
let teal = "#0FA89B"
let coral = "#E5484D"
let slate = "#5B6477"
let mist = "#E8E9F3"
let paper = "#FAF8F4"
let rule = "#C5C8DC"
let mono = "ui-monospace, SFMono-Regular, Menlo, Consolas, 'DejaVu Sans Mono', monospace"
let sans = "'Helvetica Neue', Arial, sans-serif"

(* Sizes, in the units of the picture. *)
let card_w = 300.0
let card_h = 74.0
let chip_w = 176.0
let chip_h = 40.0
let pill_h = 24.0
let gap = 24.0
let drop = 22.0
let margin = 40.0
let header = 118.0
let row = card_h +. drop +. pill_h +. drop

let escape text =
  let buf = Buffer.create (String.length text + 16) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&#39;"
      | '\n' | '\r' | '\t' -> Buffer.add_char buf ' '
      | c when Char.code c < 0x20 -> ()
      | c -> Buffer.add_char buf c)
    text;
  Buffer.contents buf

(* The width of a line of text set in the fixed-width face. *)
let text_width size text = float_of_int (Text.utf8_length text) *. size *. 0.61
(* What of a text fits in a width: its first words, with a mark when the others are left out. *)
let fit size width text = Text.truncate_words (max 4 (int_of_float (width /. (size *. 0.61)))) text
let in_order f xs = List.rev (List.fold_left (fun acc x -> f x :: acc) [] xs)
let sum f xs = List.fold_left (fun acc x -> acc +. f x) 0.0 xs
let spread f xs = match xs with [] -> 0.0 | _ -> sum f xs +. (gap *. float_of_int (List.length xs - 1))
let px = Printf.sprintf "%.1f"

(* A goal the search worked on is drawn as a card. A goal it never expanded, or one that stands
   for a goal drawn elsewhere, is drawn small. *)
type shown = { node : Search_record.node; on_proof : bool; small : bool; pills : pill list; mutable width : float }

and pill = {
  label : string;
  winning : bool;
  children : shown list;
  mutable span : float;
}

let status_color = function Search_tree.Proved -> teal | Search_tree.Failed -> coral | Search_tree.Open -> slate

(* What happened on a goal apart from the tactics that are drawn under it. *)
let note (n : Search_record.node) =
  match n.alias_of, n.abandoned with
  | Some other, _ -> Printf.sprintf "the same goal as #%d" other
  | None, Some why -> "given up: " ^ why
  | None, None when not n.expanded -> "not expanded"
  | None, None ->
    let failed = List.filter_map (fun e -> match e.outcome with Rejected _ -> Some e.tactic | _ -> None) n.edges in
    let back = List.filter_map (fun e -> match e.outcome with Blocked _ -> Some e.tactic | _ -> None) n.edges in
    let also =
      List.filteri (fun position e -> e.outcome = Subgoals [] && n.proved_by <> Some position) n.edges |> List.map (fun e -> e.tactic)
    in
    let part mark = function [] -> [] | tactics -> [ mark ^ " " ^ String.concat " \xC2\xB7 " tactics ] in
    String.concat "   " (part "\xE2\x9C\x97" failed @ part "\xE2\x86\xBA" back @ part "\xE2\x9C\x93" also)

let layout ~max_nodes (t : Search_record.t) =
  let proof = Hashtbl.create 32 in
  let rec mark id =
    if not (Hashtbl.mem proof id) then
      match Search_record.node t id with
      | Some n when n.status = Search_tree.Proved ->
        Hashtbl.replace proof id ();
        (match n.alias_of, n.proved_by with
         | Some other, _ -> mark other
         | None, Some position ->
           (match List.nth_opt n.edges position with Some { outcome = Subgoals ids; _ } -> List.iter mark ids | _ -> ())
         | None, None -> ())
      | _ -> ()
  in
  if t.stop = "found" then mark t.root;
  let budget = ref max_nodes in
  let rec show ?(top = false) (n : Search_record.node) =
    decr budget;
    let on_proof = Hashtbl.mem proof n.id in
    let pills =
      if n.alias_of <> None then []
      else
        List.concat
          (in_order
             (fun (position, e) ->
               match e.outcome with
               | Subgoals ids ->
                 let proving = n.proved_by = Some position in
                 if ids = [] && not proving then []
                 else begin
                   let nodes = List.filter_map (Search_record.node t) ids in
                   let fits = List.length nodes <= !budget in
                   let children = if fits then in_order (fun child -> show child) nodes else [] in
                   let name = if e.tactic = "" then "goals" else e.tactic in
                   let label =
                     if ids = [] then name ^ " \xE2\x9C\x93"
                     else if fits then name
                     else Printf.sprintf "%s (%d not drawn)" name (List.length ids)
                   in
                   [ { label; winning = on_proof && proving; children; span = 0.0 } ]
                 end
               | Rejected _ | Blocked _ -> [])
             (List.mapi (fun position e -> (position, e)) n.edges))
    in
    { node = n; on_proof; small = (not top) && pills = [] && (n.alias_of <> None || not n.expanded); pills; width = 0.0 }
  in
  let top =
    match Search_record.first_goals t with
    | [ only ] -> Search_record.node t only
    | _ -> Search_record.node t t.root
  in
  let rec measure s =
    List.iter
      (fun p ->
        List.iter measure p.children;
        p.span <- Float.max (Float.max 44.0 (text_width 11.5 p.label +. 24.0)) (spread (fun c -> c.width) p.children))
      s.pills;
    s.width <- (if s.small then chip_w else Float.max card_w (spread (fun p -> p.span) s.pills))
  in
  Option.map
    (fun n ->
      let s = show ~top:true n in
      measure s;
      s)
    top

let render ?(max_nodes = 60) (t : Search_record.t) =
  let tree = layout ~max_nodes t in
  let lines = Buffer.create 2048 and proof_lines = Buffer.create 1024 and boxes = Buffer.create 8192 in
  let bottom = ref header in
  let connector (x1, y1) (x2, y2) winning =
    let middle = (y1 +. y2) /. 2.0 in
    Buffer.add_string
      (if winning then proof_lines else lines)
      (Printf.sprintf "  <path d=\"M%s %sV%sH%sV%s\" fill=\"none\" stroke=\"%s\" stroke-width=\"%s\"/>\n" (px x1) (px y1) (px middle) (px x2)
         (px y2)
         (if winning then teal else rule)
         (if winning then "2.4" else "1.4"))
  in
  let card (s : shown) center y =
    let n = s.node in
    let x = center -. (card_w /. 2.0) in
    let color = status_color n.status in
    let inner = card_w -. 30.0 in
    let goal = if n.goal = "" then "all goals" else "\xE2\x8A\xA2 " ^ n.goal in
    Buffer.add_string boxes
      (Printf.sprintf "  <g class=\"goal %s\">\n" (Search_tree.status_to_string n.status));
    Buffer.add_string boxes
      (Printf.sprintf "    <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"9\" fill=\"#FFFFFF\" stroke=\"%s\" stroke-width=\"%s\"%s/>\n" (px x)
         (px y) (px card_w) (px card_h) color
         (if s.on_proof then "2.2" else "1.4")
         (if n.alias_of <> None then " stroke-dasharray=\"5 4\"" else ""));
    Buffer.add_string boxes
      (Printf.sprintf "    <rect x=\"%s\" y=\"%s\" width=\"5\" height=\"%s\" rx=\"2.5\" fill=\"%s\"/>\n" (px (x +. 7.0)) (px (y +. 9.0))
         (px (card_h -. 18.0)) color);
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"10.5\" fill=\"%s\">%s</text>\n" (px (x +. 22.0)) (px (y +. 20.0)) slate
         (escape (fit 10.5 (inner -. 34.0) (String.concat ", " n.hypotheses))));
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"10\" fill=\"%s\" text-anchor=\"end\">#%d</text>\n" (px (x +. card_w -. 10.0))
         (px (y +. 20.0)) slate n.id);
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"12.5\" font-weight=\"700\" fill=\"%s\">%s</text>\n" (px (x +. 22.0)) (px (y +. 42.0)) ink
         (escape (fit 12.5 inner goal)));
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"10.5\" fill=\"%s\" xml:space=\"preserve\">%s</text>\n" (px (x +. 22.0))
         (px (y +. 62.0)) slate
         (escape (fit 10.5 inner (note n))));
    Buffer.add_string boxes "  </g>\n"
  in
  let chip (s : shown) center y =
    let n = s.node in
    let x = center -. (chip_w /. 2.0) in
    let color = status_color n.status in
    Buffer.add_string boxes (Printf.sprintf "  <g class=\"goal %s small\">\n" (Search_tree.status_to_string n.status));
    Buffer.add_string boxes
      (Printf.sprintf "    <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"9\" fill=\"#FFFFFF\" stroke=\"%s\" stroke-width=\"%s\"%s/>\n" (px x)
         (px y) (px chip_w) (px chip_h) color
         (if s.on_proof then "2.2" else "1.2")
         (if n.alias_of <> None then " stroke-dasharray=\"5 4\"" else ""));
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"10.5\" font-weight=\"700\" fill=\"%s\">%s</text>\n" (px (x +. 12.0)) (px (y +. 17.0))
         ink
         (escape (fit 10.5 (chip_w -. 50.0) ("\xE2\x8A\xA2 " ^ n.goal))));
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"9.5\" fill=\"%s\" text-anchor=\"end\">#%d</text>\n" (px (x +. chip_w -. 9.0))
         (px (y +. 17.0)) slate n.id);
    Buffer.add_string boxes
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"10\" fill=\"%s\">%s</text>\n" (px (x +. 12.0)) (px (y +. 32.0)) slate
         (escape (fit 10.0 (chip_w -. 24.0) (note n))));
    Buffer.add_string boxes "  </g>\n"
  in
  let pill p center y =
    let width = Float.max 44.0 (text_width 11.5 p.label +. 24.0) in
    Buffer.add_string boxes
      (Printf.sprintf
         "  <g class=\"tactic%s\">\n    <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"12\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1.2\"/>\n    <text x=\"%s\" y=\"%s\" font-size=\"11.5\" font-weight=\"600\" fill=\"%s\" text-anchor=\"middle\">%s</text>\n  </g>\n"
         (if p.winning then " proof" else "")
         (px (center -. (width /. 2.0)))
         (px y) (px width) (px pill_h)
         (if p.winning then teal else mist)
         (if p.winning then teal else rule)
         (px center)
         (px (y +. 16.0))
         (if p.winning then "#FFFFFF" else indigo)
         (escape p.label))
  in
  let rec place s left depth =
    let center = left +. (s.width /. 2.0) in
    let y = header +. (float_of_int depth *. row) in
    if s.small then chip s center y else card s center y;
    bottom := Float.max !bottom (y +. if s.small then chip_h else card_h);
    let x = ref (left +. ((s.width -. spread (fun p -> p.span) s.pills) /. 2.0)) in
    List.iter
      (fun p ->
        let pill_center = !x +. (p.span /. 2.0) in
        let pill_y = y +. card_h +. drop in
        connector (center, y +. card_h) (pill_center, pill_y) p.winning;
        pill p pill_center pill_y;
        bottom := Float.max !bottom (pill_y +. pill_h);
        let child_left = ref (!x +. ((p.span -. spread (fun c -> c.width) p.children) /. 2.0)) in
        List.iter
          (fun c ->
            connector (pill_center, pill_y +. pill_h) (!child_left +. (c.width /. 2.0), y +. row) p.winning;
            place c !child_left (depth + 1);
            child_left := !child_left +. c.width +. gap)
          p.children;
        x := !x +. p.span +. gap)
      s.pills
  in
  let script = Option.value ~default:[] t.script in
  let script_lines = if script = [] then [] else "by" :: List.map (fun line -> "  " ^ line) script in
  let script_w = List.fold_left (fun acc line -> Float.max acc (text_width 12.0 line)) 200.0 script_lines +. 36.0 in
  let tree_w = match tree with Some s -> s.width | None -> 0.0 in
  let legend_w = 640.0 in
  let width = Float.max (Float.max tree_w legend_w) script_w +. (2.0 *. margin) in
  (match tree with Some s -> place s ((width -. s.width) /. 2.0) 0 | None -> ());
  let legend_y = !bottom +. 44.0 in
  let script_y = legend_y +. 30.0 in
  let script_h = if script_lines = [] then 0.0 else 46.0 +. (17.0 *. float_of_int (List.length script_lines)) in
  let height = (if script_lines = [] then legend_y +. 16.0 else script_y +. script_h) +. margin -. 10.0 in
  let out = Buffer.create 16384 in
  let title = if t.statement = "" then "A proof search" else "The search for a proof of " ^ t.statement in
  let drawn = match tree with None -> 0 | Some s -> let rec count s = 1 + List.fold_left (fun acc p -> List.fold_left (fun acc c -> acc + count c) acc p.children) 0 s.pills in count s in
  let cost =
    Printf.sprintf "%s \xC2\xB7 %s \xC2\xB7 %s \xC2\xB7 %s" t.stop
      (if t.expansions = 1 then "1 goal expanded" else Printf.sprintf "%d goals expanded" t.expansions)
      (if t.lean_calls = 1 then "1 call to Lean" else Printf.sprintf "%d calls to Lean" t.lean_calls)
      (if drawn = 1 then "1 goal drawn" else Printf.sprintf "%d goals drawn" drawn)
  in
  Buffer.add_string out
    (Printf.sprintf
       "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %s %s\" role=\"img\" aria-labelledby=\"t d\" font-family=\"%s\">\n" (px width)
       (px height) mono);
  Buffer.add_string out (Printf.sprintf "  <title id=\"t\">%s</title>\n" (escape title));
  Buffer.add_string out
    (Printf.sprintf "  <desc id=\"d\">%s. %s</desc>\n" (escape cost)
       (escape
          (if script = [] then "No proof was found."
           else "The proof that was found: " ^ String.concat "; " (List.map String.trim script) ^ ".")));
  Buffer.add_string out (Printf.sprintf "  <rect width=\"%s\" height=\"%s\" fill=\"%s\"/>\n" (px width) (px height) paper);
  Buffer.add_string out
    (Printf.sprintf "  <text x=\"%s\" y=\"44\" font-family=\"%s\" font-size=\"22\" font-weight=\"700\" fill=\"%s\">Proof search</text>\n" (px margin) sans ink);
  let across = width -. (2.0 *. margin) in
  if t.statement <> "" then
    Buffer.add_string out
      (Printf.sprintf "  <text x=\"%s\" y=\"70\" font-size=\"13\" fill=\"%s\">%s</text>\n" (px margin) indigo (escape (fit 13.0 across t.statement)));
  Buffer.add_string out
    (Printf.sprintf "  <text x=\"%s\" y=\"%s\" font-family=\"%s\" font-size=\"13\" fill=\"%s\">%s</text>\n" (px margin)
       (if t.statement = "" then "70" else "92")
       sans slate (escape cost));
  Buffer.add_buffer out lines;
  Buffer.add_buffer out proof_lines;
  Buffer.add_buffer out boxes;
  (* The legend: one line of text, so that the spacing is the font's own. *)
  let key first color mark text =
    Printf.sprintf "<tspan%s fill=\"%s\">%s</tspan> %s" (if first then "" else " dx=\"18\"") color mark (escape text)
  in
  Buffer.add_string out
    (Printf.sprintf "  <text x=\"%s\" y=\"%s\" font-family=\"%s\" font-size=\"12.5\" fill=\"%s\">%s</text>\n" (px margin) (px legend_y) sans slate
       (String.concat ""
          [ key true teal "\xE2\x96\xA0" "proved"; key false coral "\xE2\x96\xA0" "failed"; key false slate "\xE2\x96\xA0" "open";
            key false slate "\xE2\x9C\x97" "tactics that failed"; key false slate "\xE2\x86\xBA" "tactics that lead in a circle";
            key false slate "\xE2\x9C\x93" "closes the goal" ]));
  if script_lines <> [] then begin
    Buffer.add_string out
      (Printf.sprintf "  <g class=\"script\">\n    <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"9\" fill=\"%s\"/>\n" (px margin) (px script_y)
         (px script_w) (px script_h) ink);
    Buffer.add_string out
      (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-family=\"%s\" font-size=\"11\" font-weight=\"700\" fill=\"%s\" letter-spacing=\"1\">THE PROOF</text>\n"
         (px (margin +. 18.0))
         (px (script_y +. 24.0))
         sans teal);
    List.iteri
      (fun i line ->
        Buffer.add_string out
          (Printf.sprintf "    <text x=\"%s\" y=\"%s\" font-size=\"12\" fill=\"%s\" xml:space=\"preserve\">%s</text>\n" (px (margin +. 18.0))
             (px (script_y +. 46.0 +. (17.0 *. float_of_int i)))
             paper (escape line)))
      script_lines;
    Buffer.add_string out "  </g>\n"
  end;
  Buffer.add_string out "</svg>\n";
  Buffer.contents out
