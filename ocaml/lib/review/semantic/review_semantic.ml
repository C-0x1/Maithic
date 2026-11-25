(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Checks on what was extracted from a file. *)

open Maithic_bridge

(** The duplicate-lemma check: a theorem whose statement is the statement
    of another theorem, up to the names of bound variables. The comparison
    was made by Lean, on the types themselves and against everything the
    file imports. Two theorems of one file with one statement are reported
    once, at the later one; a theorem that restates a lemma from elsewhere
    names that lemma. *)
let duplicates (e : Extraction.t) =
  let position name = Option.map (fun (d : Extraction.declaration) -> d.Extraction.line) (Extraction.find e name) in
  List.filter_map
    (fun (d : Extraction.declaration) ->
      if d.Extraction.kind <> "theorem" then None
      else begin
        let elsewhere = List.filter (fun twin -> position twin = None) d.Extraction.same_statement in
        let earlier =
          List.filter (fun twin -> match position twin with Some line -> line < d.Extraction.line | None -> false) d.Extraction.same_statement
        in
        match (if elsewhere <> [] then elsewhere else earlier) with
        | [] -> None
        | twins ->
          let named = String.concat " and " twins in
          Some
            { Review_finding.rule = "duplicate"; origin = Review_finding.Semantic; severity = Review_finding.Warning; file = e.Extraction.file;
              line = d.Extraction.name_line; declaration = d.Extraction.name;
              message =
                (if elsewhere <> [] then Printf.sprintf "states the same as %s, which already exists" named
                 else Printf.sprintf "states the same as %s, earlier in this file" named);
              edit = None }
      end)
    e.Extraction.declarations
