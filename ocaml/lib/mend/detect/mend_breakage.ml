(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

type t = {
  file : string;
  declaration : string;
  kind : string;
  line : int;
  column : int;
  severity : string;
  class_ : Mend_class.t;
  error : string;
  diagnostics : Bridge_types.diagnostic list;
  context : string;
  first_line : int;
}

let key b =
  Printf.sprintf "%s|%s|%s"
    (if b.declaration = "" then Printf.sprintf "line %d" b.line else b.declaration)
    (Mend_class.name b.class_) (Mend_class.detail b.class_)

let classes b = List.filter_map Mend_class.of_diagnostic b.diagnostics
let has_error b = List.exists (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error") b.diagnostics

let to_json b =
  Json.Obj
    ([ ("file", Json.String b.file); ("declaration", Json.String b.declaration) ]
     @ (if b.kind = "" then [] else [ ("kind", Json.String b.kind) ])
     @ [ ("line", Json.Int b.line); ("column", Json.Int b.column); ("severity", Json.String b.severity);
         ("class", Json.String (Mend_class.name b.class_)) ]
     @ (match Mend_class.detail b.class_ with "" -> [] | detail -> [ ("detail", Json.String detail) ])
     @ [ ("error", Json.String b.error);
         ( "diagnostics",
           Json.List
             (List.map
                (fun (d : Bridge_types.diagnostic) ->
                  Json.Obj
                    [ ("severity", Json.String d.Bridge_types.severity); ("line", Json.Int d.Bridge_types.line);
                      ("column", Json.Int d.Bridge_types.column); ("text", Json.String d.Bridge_types.text) ])
                b.diagnostics) );
         ("context", Json.String b.context); ("first_line", Json.Int b.first_line) ])

let describe b =
  Printf.sprintf "%s:%d  %s  %s: %s" b.file b.line
    (if b.declaration = "" then "(outside a theorem)" else b.declaration)
    (Mend_class.name b.class_) b.error
