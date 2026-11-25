(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Pictures of a terminal session.

    The documentation shows what the commands print. So that a picture can
    never drift from the program, it is drawn from the program: a command is
    run, what it wrote is read with its colours, and the text is laid out as
    an SVG in the colours of the project. *)

open Maithic_base

type span = { text : string; fill : string; bold : bool }

let ink = "#14162B"
let bar = "#1E2142"
let mist = "#E8E9F3"
let faint = "#8A90A6"
let teal = "#2CC4B6"
let gold = "#F0A92E"
let coral = "#F26D72"
let green = "#57C98F"

type style = { color : string option; strong : bool; dim : bool }

let plain = { color = None; strong = false; dim = false }

let fill_of style = match style.color with Some c -> c | None -> if style.dim then faint else mist

(* One code of a "select graphic rendition" sequence, applied to a style. *)
let apply style = function
  | 0 -> plain
  | 1 -> { style with strong = true }
  | 2 -> { style with dim = true }
  | 22 -> { style with strong = false; dim = false }
  | 31 -> { style with color = Some coral }
  | 32 -> { style with color = Some green }
  | 33 -> { style with color = Some gold }
  | 36 -> { style with color = Some teal }
  | 39 -> { style with color = None }
  | _ -> style

(** The lines of a text that carries ANSI colours, as runs of one style.
    Sequences other than colours are dropped. *)
let lines_of_ansi text =
  let n = String.length text in
  let lines = ref [] and spans = ref [] and current = Buffer.create 80 and style = ref plain in
  let flush () =
    if Buffer.length current > 0 then begin
      spans := { text = Buffer.contents current; fill = fill_of !style; bold = !style.strong } :: !spans;
      Buffer.clear current
    end
  in
  let newline () =
    flush ();
    lines := List.rev !spans :: !lines;
    spans := []
  in
  let rec go i =
    if i >= n then ()
    else if text.[i] = '\027' && i + 1 < n && text.[i + 1] = '[' then begin
      (* Up to the letter that ends the sequence. *)
      let rec stop j = if j < n && not ((text.[j] >= 'a' && text.[j] <= 'z') || (text.[j] >= 'A' && text.[j] <= 'Z')) then stop (j + 1) else j in
      let j = stop (i + 2) in
      if j < n && text.[j] = 'm' then begin
        flush ();
        let codes = String.split_on_char ';' (String.sub text (i + 2) (j - i - 2)) in
        style := List.fold_left (fun s code -> apply s (Option.value ~default:0 (int_of_string_opt code))) !style codes
      end;
      go (j + 1)
    end
    else if text.[i] = '\n' then (newline (); go (i + 1))
    else if text.[i] = '\r' then go (i + 1)
    else if text.[i] = '\t' then (Buffer.add_string current "    "; go (i + 1))
    else (Buffer.add_char current text.[i]; go (i + 1))
  in
  go 0;
  if Buffer.length current > 0 || !spans <> [] then newline ();
  List.rev !lines

let escape text =
  let b = Buffer.create (String.length text + 16) in
  String.iter (function '&' -> Buffer.add_string b "&amp;" | '<' -> Buffer.add_string b "&lt;" | '>' -> Buffer.add_string b "&gt;" | c -> Buffer.add_char b c) text;
  Buffer.contents b

let width_of line = List.fold_left (fun w s -> w + Text.utf8_length s.text) 0 line

(* The characters of a UTF-8 text, one string for each. *)
let characters text =
  let n = String.length text in
  let rec go i found =
    if i >= n then List.rev found
    else begin
      let c = Char.code text.[i] in
      let length = min (n - i) (if c < 0x80 then 1 else if c < 0xE0 then 2 else if c < 0xF0 then 3 else 4) in
      go (i + length) (String.sub text i length :: found)
    end
  in
  go 0 []

(* Spans from characters that each carry their span: neighbours of one style are joined. *)
let spans_of styled =
  List.fold_right
    (fun (c, s) spans ->
      match spans with
      | head :: rest when head.fill = s.fill && head.bold = s.bold -> { head with text = c ^ head.text } :: rest
      | _ -> { s with text = c } :: spans)
    styled []

(* Where the lines that continue a long line begin: under its last column, when the line has
   columns set apart by two spaces or more in its first half, and four characters in when it has
   none. *)
let hanging columns (texts : string array) =
  let n = Array.length texts in
  let leading =
    let rec go i = if i < n && texts.(i) = " " then go (i + 1) else i in
    go 0
  in
  let column = ref None in
  let i = ref leading in
  while !i < n - 1 && !i < columns / 2 do
    if texts.(!i) = " " && texts.(!i + 1) = " " then begin
      let j = ref !i in
      while !j < n && texts.(!j) = " " do incr j done;
      if !j <= columns / 2 then column := Some !j;
      i := !j
    end
    else incr i
  done;
  min (columns - 40) (match !column with Some c -> c | None -> leading + 4)

(* A line in pieces of at most [columns] characters, as a terminal of that width shows it, but
   broken between words and with the later pieces set in, so that the picture stays readable. *)
let wrap columns line =
  if width_of line <= columns then [ line ]
  else begin
    let styled = Array.of_list (List.concat_map (fun s -> List.map (fun c -> (c, s)) (characters s.text)) line) in
    let texts = Array.map fst styled in
    let n = Array.length styled in
    let indent = hanging columns texts in
    let margin = { text = String.make indent ' '; fill = mist; bold = false } in
    let piece first start stop =
      (* the characters from [start] up to [stop], without spaces at the end *)
      let stop = ref stop in
      while !stop > start && texts.(!stop - 1) = " " do decr stop done;
      let spans = spans_of (Array.to_list (Array.sub styled start (!stop - start))) in
      if first then spans else margin :: spans
    in
    let rec go first start pieces =
      let room = if first then columns else columns - indent in
      if n - start <= room then List.rev (piece first start n :: pieces)
      else begin
        (* the last space that leaves a piece of at most [room] characters, if there is one past the start *)
        let floor = if first then indent else start in
        let rec back i = if i <= floor then None else if texts.(i) = " " then Some i else back (i - 1) in
        match back (start + room) with
        | Some space ->
          let next = ref space in
          while !next < n && texts.(!next) = " " do incr next done;
          go false !next (piece first start space :: pieces)
        | None -> go false (start + room) (piece first start (start + room) :: pieces)
      end
    in
    go true 0 []
  end

let char_width = 7.83
let line_height = 19.0
let pad = 22.0
let bar_height = 36.0

(** The runs of a line: stretches of one style that hold no two spaces in a
    row, each with the column at which it begins. A picture places every run
    by its column and states its length, so that the layout depends neither
    on the width of the reader's font nor on what a browser does with
    spaces: columns stay columns everywhere. *)
let runs line =
  let found = ref [] and column = ref 0 in
  List.iter
    (fun s ->
      let chars = Array.of_list (characters s.text) in
      let n = Array.length chars in
      let space i = i < n && chars.(i) = " " in
      let i = ref 0 in
      while !i < n do
        if space !i then incr i
        else begin
          (* up to the end of the span, or to the first of two spaces in a row *)
          let j = ref !i in
          while !j < n && not (space !j && (space (!j + 1) || !j + 1 = n)) do incr j done;
          found := (!column + !i, { s with text = String.concat "" (Array.to_list (Array.sub chars !i (!j - !i))) }) :: !found;
          i := !j
        end
      done;
      column := !column + n)
    line;
  List.rev !found

(** One command and what it printed. *)
type session = { command : string; output : string }

(** The picture: a window with a title bar, and for each session the command
    after a prompt and its output below. A line of more than [columns]
    characters goes on in the next line, as it does in a terminal of that
    width; output beyond [max_lines] lines is left out, with a note of how
    much. [label] is read out by screen readers. *)
let render ?(columns = 120) ?(max_lines = 60) ~title ~label sessions =
  let prompt command = [ { text = "$ "; fill = teal; bold = true }; { text = command; fill = mist; bold = true } ] in
  let body =
    List.concat
      (List.mapi
         (fun i s ->
           let output = lines_of_ansi s.output in
           let shown, left = if List.length output > max_lines then (List.filteri (fun k _ -> k < max_lines) output, List.length output - max_lines) else (output, 0) in
           (if i = 0 then [] else [ [] ])
           @ [ prompt s.command ]
           @ shown
           @ if left = 0 then [] else [ [ { text = Printf.sprintf "\xE2\x80\xA6 %d more line%s" left (if left = 1 then "" else "s"); fill = faint; bold = false } ] ])
         sessions)
  in
  let body = List.concat_map (wrap columns) body in
  let widest = List.fold_left (fun w line -> max w (width_of line)) 40 body in
  let width = (2.0 *. pad) +. (float_of_int widest *. char_width) in
  let height = bar_height +. pad +. (float_of_int (List.length body) *. line_height) +. (pad *. 0.6) in
  let b = Buffer.create 4096 in
  let add fmt = Printf.ksprintf (Buffer.add_string b) fmt in
  add "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %.0f %.0f\" width=\"%.0f\" height=\"%.0f\" role=\"img\" aria-label=\"%s\">\n" width height width height
    (escape label);
  add "  <title>%s</title>\n" (escape label);
  add "  <rect width=\"%.0f\" height=\"%.0f\" rx=\"10\" fill=\"%s\"/>\n" width height ink;
  add "  <path d=\"M0 10a10 10 0 0 1 10-10h%.0fa10 10 0 0 1 10 10v%.0fh-%.0fz\" fill=\"%s\"/>\n" (width -. 20.0) (bar_height -. 10.0) width bar;
  List.iteri (fun i colour -> add "  <circle cx=\"%.0f\" cy=\"18\" r=\"5.5\" fill=\"%s\"/>\n" (22.0 +. (float_of_int i *. 18.0)) colour) [ "#F26D72"; "#F0A92E"; "#57C98F" ];
  add "  <text x=\"%.0f\" y=\"23\" text-anchor=\"middle\" font-family=\"'Helvetica Neue', Arial, sans-serif\" font-size=\"13\" fill=\"%s\">%s</text>\n" (width /. 2.0) faint
    (escape title);
  add "  <g font-family=\"Menlo, Consolas, 'DejaVu Sans Mono', monospace\" font-size=\"13\">\n";
  List.iteri
    (fun i line ->
      let y = bar_height +. pad +. (float_of_int i *. line_height) in
      List.iter
        (fun (column, s) ->
          add "    <text x=\"%.2f\" y=\"%.0f\" textLength=\"%.2f\" lengthAdjust=\"spacingAndGlyphs\" fill=\"%s\"%s>%s</text>\n"
            (pad +. (float_of_int column *. char_width))
            y
            (float_of_int (Text.utf8_length s.text) *. char_width)
            s.fill
            (if s.bold then " font-weight=\"bold\"" else "")
            (escape s.text))
        (runs line))
    body;
  add "  </g>\n</svg>\n";
  Buffer.contents b
