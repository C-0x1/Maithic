(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

let schema = "maithic.intel.health/1"

type gap = { name : string; file : string; line : int; column : int; goals : string list; through : string option }

let sorry_inventory (extractions : Extraction.t list) =
  let gaps =
    List.concat_map
      (fun (e : Extraction.t) ->
        List.concat_map
          (fun (d : Extraction.declaration) ->
            let name = d.Extraction.name in
            if not d.Extraction.uses_sorry then []
            else if Extraction.contains_sorry d then
              match List.filter (fun (h : Extraction.hole) -> h.Extraction.hole_decl = name) e.Extraction.holes with
              | [] -> [ { name; file = e.Extraction.file; line = d.Extraction.name_line; column = d.Extraction.name_column; goals = []; through = None } ]
              | holes ->
                List.map
                  (fun (h : Extraction.hole) ->
                    { name; file = e.Extraction.file; line = h.Extraction.hole_line; column = h.Extraction.hole_column;
                      goals = h.Extraction.goals; through = None })
                  holes
            else begin
              (* Complete in itself: name the unfinished declaration it uses, if it is one of ours. *)
              let unfinished dep =
                List.exists
                  (fun (other : Extraction.t) ->
                    List.exists (fun (o : Extraction.declaration) -> o.Extraction.name = dep && o.Extraction.uses_sorry) other.Extraction.declarations)
                  extractions
              in
              let through = List.find_opt unfinished (d.Extraction.value_deps @ d.Extraction.type_deps) in
              [ { name; file = e.Extraction.file; line = d.Extraction.name_line; column = d.Extraction.name_column; goals = [];
                  through = Some (Option.value ~default:"a declaration outside the scanned files" through) } ]
            end)
          e.Extraction.declarations)
      extractions
  in
  List.sort (fun a b -> compare (a.file, a.line, a.column, a.name) (b.file, b.line, b.column, b.name)) gaps

type axiom_use = { axiom : string; standard : bool; users : (string * string * int) list }

let standard_axioms = [ "propext"; "Classical.choice"; "Quot.sound" ]

let axiom_census (extractions : Extraction.t list) =
  let users = Hashtbl.create 16 in
  List.iter
    (fun (e : Extraction.t) ->
      List.iter
        (fun (d : Extraction.declaration) ->
          List.iter
            (fun axiom ->
              if axiom <> "sorryAx" && axiom <> d.Extraction.name then
                Hashtbl.replace users axiom ((d.Extraction.name, e.Extraction.file, d.Extraction.name_line) :: Option.value ~default:[] (Hashtbl.find_opt users axiom)))
            d.Extraction.axioms)
        e.Extraction.declarations)
    extractions;
  let all = Hashtbl.fold (fun axiom who acc -> { axiom; standard = List.mem axiom standard_axioms; users = List.sort compare who } :: acc) users [] in
  List.sort (fun a b -> compare (a.standard, a.axiom) (b.standard, b.axiom)) all

let snapshot (extractions : Extraction.t list) =
  let declarations = List.concat_map (fun (e : Extraction.t) -> e.Extraction.declarations) extractions in
  let count p = List.length (List.filter p declarations) in
  let kinds = List.sort_uniq compare (List.map (fun (d : Extraction.declaration) -> d.Extraction.kind) declarations) in
  Json.Obj
    [ ("schema", Json.String schema); ("files", Json.Int (List.length extractions));
      ("filesWithErrors", Json.Int (List.length (List.filter (fun (e : Extraction.t) -> not e.Extraction.ok) extractions)));
      ("declarations", Json.Int (List.length declarations));
      ("byKind", Json.Obj (List.map (fun kind -> (kind, Json.Int (count (fun d -> d.Extraction.kind = kind)))) kinds));
      ("documented", Json.Int (count (fun d -> d.Extraction.doc <> None)));
      ("restingOnSorry", Json.Int (count (fun d -> d.Extraction.uses_sorry)));
      ( "unfinished",
        Json.List
          (List.map
             (fun g ->
               Json.Obj
                 ([ ("name", Json.String g.name); ("file", Json.String g.file); ("line", Json.Int g.line); ("column", Json.Int g.column);
                    ("goals", Json.List (List.map (fun s -> Json.String s) g.goals)) ]
                 @ match g.through with Some t -> [ ("through", Json.String t) ] | None -> []))
             (sorry_inventory extractions)) );
      ( "axioms",
        Json.List
          (List.map
             (fun a ->
               Json.Obj
                 [ ("axiom", Json.String a.axiom); ("standard", Json.Bool a.standard);
                   ( "usedBy",
                     Json.List
                       (List.map
                          (fun (name, file, line) -> Json.Obj [ ("name", Json.String name); ("file", Json.String file); ("line", Json.Int line) ])
                          a.users) ) ])
             (axiom_census extractions)) ) ]
