(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Terminal output: colour that respects the environment, status labels
    and aligned tables. *)

open Maithic_base
open Maithic_schema

let colour_enabled = ref false

(** Colour is used only on a terminal, and never when [NO_COLOR] is set, the
    terminal is dumb or [--no-color] was given. [CLICOLOR_FORCE], set to
    anything but [0], asks for colour where the output is not a terminal: for
    a pager that understands it, or a program that records the output. *)
let init ~no_color =
  let forced = match Sys.getenv_opt "CLICOLOR_FORCE" with None | Some "" | Some "0" -> false | Some _ -> true in
  colour_enabled :=
    (not no_color)
    && Sys.getenv_opt "NO_COLOR" = None
    && (forced || (Sys.getenv_opt "TERM" <> Some "dumb" && (try Unix.isatty Unix.stdout with Unix.Unix_error _ -> false)))

let paint code text = if !colour_enabled then Printf.sprintf "\027[%sm%s\027[0m" code text else text
let bold = paint "1"
let dim = paint "2"
let teal = paint "36"
let gold = paint "33"
let coral = paint "31"
let green = paint "32"

(** A status word in the colour that matches its meaning. *)
let status st =
  let text = Status.to_string st in
  match st with
  | Status.Verified -> green text
  | Status.Checked -> teal text
  | Status.Refuted | Status.Failed -> coral text
  | Status.Conjectured | Status.Unverified | Status.Unknown | Status.Abstained -> gold text

let ok_mark ok = if ok then green "ok" else coral "missing"

(** The number of characters a text takes on the screen: a colour code takes none. *)
let visible_length text =
  let n = String.length text and shown = Buffer.create (String.length text) in
  let rec go i =
    if i < n then
      if text.[i] = '\027' then begin
        (* a colour code runs from the escape character to the letter m *)
        let j = ref (i + 1) in
        while !j < n && text.[!j] <> 'm' do incr j done;
        go (!j + 1)
      end
      else begin
        Buffer.add_char shown text.[i];
        go (i + 1)
      end
  in
  go 0;
  Text.utf8_length (Buffer.contents shown)

(** The width that tables are fitted to: the width of the terminal when the environment variable
    COLUMNS gives it, and 118 characters when it does not. *)
let width () =
  match Option.bind (Sys.getenv_opt "COLUMNS") (fun v -> int_of_string_opt (String.trim v)) with Some n when n >= 40 -> n | _ -> 118

(* The words of a cell in lines of at most [room] characters. A word longer than the room stands
   on a line of its own, whole. *)
let wrap_cell room cell =
  let words = List.filter (fun w -> w <> "") (String.split_on_char ' ' cell) in
  let lines, current =
    List.fold_left
      (fun (lines, current) word ->
        if current = "" then (lines, word)
        else if visible_length current + 1 + visible_length word <= room then (lines, current ^ " " ^ word)
        else (current :: lines, word))
      ([], "") words
  in
  List.rev (if current = "" then lines else current :: lines)

(** Rows of cells as an aligned table; the first row is the header. A cell is as wide as what it
    shows, so a coloured cell lines up with a plain one. A table that is wider than [width] is
    fitted to it where it can be: the text of its last column goes on in further lines, under the
    start of that column. *)
let table ?width:(limit = width ()) rows =
  match rows with
  | [] -> ""
  | header :: _ ->
    let columns = List.length header in
    let natural i = List.fold_left (fun w row -> max w (visible_length (try List.nth row i with _ -> ""))) 0 rows in
    let lead = List.init (columns - 1) natural in
    let before = List.fold_left ( + ) 0 lead + (2 * (columns - 1)) in
    let room = max 24 (limit - before) in
    (* The last cell of a full row, in the lines it is printed in. A coloured cell is left whole. *)
    let pieces row =
      match List.nth_opt row (columns - 1) with
      | Some cell when visible_length cell > room && not (String.contains cell '\027') -> (
        match wrap_cell room cell with [] -> [ cell ] | lines -> lines)
      | Some cell -> [ cell ]
      | None -> []
    in
    let widths = lead @ [ List.fold_left (fun w row -> List.fold_left (fun w piece -> max w (visible_length piece)) w (pieces row)) 0 rows ] in
    let padded width cell = cell ^ String.make (max 0 (width - visible_length cell)) ' ' in
    let lines row =
      let first, rest = match pieces row with [] -> (None, []) | first :: rest -> (Some first, rest) in
      let cells = List.filteri (fun i _ -> i < columns - 1) row in
      String.concat "  " (List.mapi (fun i cell -> padded (List.nth widths i) cell) cells @ Option.to_list first)
      :: List.map (fun piece -> String.make before ' ' ^ piece) rest
    in
    let rule = String.concat "  " (List.map (fun w -> String.make w '-') widths) in
    String.concat "\n" (lines header @ (rule :: List.concat_map lines (List.tl rows))) ^ "\n"

let print_json j = print_endline (Json.pretty j)

(** Prints an error with its code, cause and remedy, and returns its exit code. *)
let fault ~json (f : Fault.t) =
  if json then print_json (Json.Obj [ ("error", Fault.to_json f) ])
  else begin
    prerr_endline (coral "error" ^ ": " ^ f.Fault.code ^ ": " ^ f.Fault.message);
    Option.iter (fun hint -> prerr_endline ("  hint: " ^ hint)) f.Fault.hint
  end;
  Exit_code.to_int f.Fault.exit
