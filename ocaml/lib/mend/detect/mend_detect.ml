(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge
open Maithic_prover

type check = string -> (Bridge_types.report, string) result

let first_line text = match List.filter (fun l -> String.trim l <> "") (Text.lines text) with line :: _ -> String.trim line | [] -> ""

let count_lines text =
  let n = ref 0 in
  String.iter (fun c -> if c = '\n' then incr n) text;
  !n

(* The lines a theorem covers, with its documentation: from the line after its prelude to the
   line its text ends on. *)
let span source (d : Lean_source.decl) =
  let start = String.length d.Lean_source.prelude and stop = String.length source - String.length d.Lean_source.rest in
  let text = String.trim (String.sub source start (max 0 (stop - start))) in
  let leading =
    (* Blank lines between the prelude and the declaration are not part of it. *)
    let rec go i = if start + i < stop && (source.[start + i] = '\n' || source.[start + i] = ' ') then go (i + 1) else i in
    go 0
  in
  let first = count_lines (String.sub source 0 (start + leading)) + 1 in
  (first, first + count_lines text, text)

let rank (d : Bridge_types.diagnostic) = match d.Bridge_types.severity with "error" -> 0 | "warning" -> 1 | _ -> 2

let of_report ~file ~source (report : Bridge_types.report) =
  let theorems = List.map (fun d -> (d, span source d)) (Lean_source.declarations source) in
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let relevant = List.filter (fun d -> Mend_class.of_diagnostic d <> None) report.Bridge_types.diagnostics in
  let home (d : Bridge_types.diagnostic) = List.find_opt (fun (_, (first, last, _)) -> d.Bridge_types.line >= first && d.Bridge_types.line <= last) theorems in
  let make ~declaration ~kind ~context ~starts diagnostics =
    let ordered = List.stable_sort (fun a b -> compare (rank a) (rank b)) diagnostics in
    let lead = List.hd ordered in
    let class_ = Option.value ~default:Mend_class.Other (Mend_class.of_diagnostic lead) in
    let said = first_line lead.Bridge_types.text in
    (* The line that says most: the suggestion itself, and the name when Lean's first line does not hold it. *)
    let error =
      match class_ with
      | Mend_class.Suggestion (suggested :: _) -> "Try this: " ^ first_line suggested
      | Mend_class.Unknown_identifier name when not (Text.contains said name) -> Printf.sprintf "Unknown identifier `%s`: %s" name said
      | _ -> said
    in
    { Mend_breakage.file; declaration; kind; line = lead.Bridge_types.line; column = lead.Bridge_types.column;
      severity = lead.Bridge_types.severity; class_; error; diagnostics = ordered; context; first_line = starts }
  in
  let inside =
    List.filter_map
      (fun ((decl : Lean_source.decl), (first, _, text)) ->
        match List.filter (fun d -> match home d with Some (h, _) -> h == decl | None -> false) relevant with
        | [] -> None
        | diagnostics -> Some (make ~declaration:decl.Lean_source.name ~kind:decl.Lean_source.keyword ~context:text ~starts:first diagnostics))
      theorems
  in
  let outside =
    List.filter_map
      (fun (d : Bridge_types.diagnostic) ->
        if home d <> None then None
        else
          let line = d.Bridge_types.line in
          let context = if line >= 1 && line <= Array.length lines then lines.(line - 1) else "" in
          Some (make ~declaration:"" ~kind:"" ~context ~starts:line [ d ]))
      relevant
  in
  List.stable_sort (fun (a : Mend_breakage.t) b -> compare (a.Mend_breakage.first_line, a.Mend_breakage.line) (b.Mend_breakage.first_line, b.Mend_breakage.line)) (inside @ outside)

let collect ~check ~file source = Result.map (fun report -> (of_report ~file ~source report, report)) (check source)

let count_by_class breakages =
  List.map (fun name -> (name, List.length (List.filter (fun (b : Mend_breakage.t) -> Mend_class.name b.Mend_breakage.class_ = name) breakages))) Mend_class.names
