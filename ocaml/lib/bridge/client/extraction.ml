(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

let schema = "maithic.bridge.extraction/1"

type declaration = {
  name : string;
  kind : string;
  type_ : string;
  module_ : string;
  line : int;
  column : int;
  end_line : int;
  end_column : int;
  name_line : int;
  name_column : int;
  doc : string option;
  is_private : bool;
  uses_sorry : bool;
  axioms : string list;
  type_deps : string list;
  value_deps : string list;
  instance_of : string option;
  parents : string list;
  statement_hash : string;
  same_statement : string list;
  source : string option;
  proof : string option;
}

type step = {
  decl : string;
  index : int;
  tactic : string;
  at_line : int;
  at_column : int;
  to_line : int;
  to_column : int;
  goals_before : string list;
  goals_after : string list;
  depth : int;
  inner : int;
}

type hole = { hole_decl : string; hole_line : int; hole_column : int; goals : string list }
type finding = { rule : string; severity : string; declaration : string; finding_line : int; message : string }

type t = {
  file : string;
  module_name : string;
  ok : bool;
  errors : int;
  warnings : int;
  imports : string list;
  module_docs : string list;
  declarations : declaration list;
  steps : step list;
  holes : hole list;
  findings : finding list;
}

let ( let* ) = Result.bind

let rec collect f = function
  | [] -> Ok []
  | x :: rest ->
    let* y = f x in
    let* ys = collect f rest in
    Ok (y :: ys)

let strings name j = Json.strings (Option.value ~default:(Json.List []) (Json.member name j))
let text_opt name j = Option.bind (Json.member name j) Json.string_opt

let declaration_of_json j =
  let* name = Json.get_string "name" j in
  let* kind = Json.get_string "kind" j in
  let line = Json.int_or 0 "line" j and column = Json.int_or 0 "column" j in
  (* A record without the place of the name is read as if the name stood where the declaration begins. *)
  let name_line, name_column = match Json.int_or 0 "nameLine" j with 0 -> (line, column) | n -> (n, Json.int_or 0 "nameColumn" j) in
  Ok
    { name; kind; type_ = Json.string_or "" "type" j; module_ = Json.string_or "" "module" j; line; column;
      end_line = Json.int_or 0 "endLine" j; end_column = Json.int_or 0 "endColumn" j; name_line; name_column;
      doc = text_opt "doc" j; is_private = Json.bool_or false "isPrivate" j; uses_sorry = Json.bool_or false "usesSorry" j;
      axioms = strings "axioms" j; type_deps = strings "typeDeps" j; value_deps = strings "valueDeps" j;
      instance_of = text_opt "instanceOf" j; parents = strings "parents" j; statement_hash = Json.string_or "" "statementHash" j;
      same_statement = strings "sameStatement" j; source = text_opt "source" j; proof = text_opt "proof" j }

let step_of_json j =
  let* tactic = Json.get_string "tactic" j in
  Ok
    { decl = Json.string_or "" "decl" j; index = Json.int_or 0 "index" j; tactic; at_line = Json.int_or 0 "line" j;
      at_column = Json.int_or 0 "column" j; to_line = Json.int_or 0 "endLine" j; to_column = Json.int_or 0 "endColumn" j;
      goals_before = strings "goalsBefore" j; goals_after = strings "goalsAfter" j; depth = Json.int_or 0 "depth" j;
      inner = Json.int_or 0 "inner" j }

let hole_of_json j =
  Ok
    { hole_decl = Json.string_or "" "decl" j; hole_line = Json.int_or 0 "line" j; hole_column = Json.int_or 0 "column" j;
      goals = strings "goals" j }

let finding_of_json j =
  let* rule = Json.get_string "rule" j in
  let* message = Json.get_string "message" j in
  Ok
    { rule; message; severity = Json.string_or "info" "severity" j; declaration = Json.string_or "" "declaration" j;
      finding_line = Json.int_or 0 "line" j }

let of_json ?file j =
  let report = Option.value ~default:(Json.Obj []) (Json.member "report" j) in
  let* declarations = collect declaration_of_json (Json.list_or_empty "declarations" j) in
  let* steps = collect step_of_json (Json.list_or_empty "steps" j) in
  let* holes = collect hole_of_json (Json.list_or_empty "holes" j) in
  let* findings = collect finding_of_json (Json.list_or_empty "findings" j) in
  if Json.member "declarations" j = None then Error "the document lists no declarations"
  else
    Ok
      { file = (match file with Some f -> f | None -> Json.string_or "" "file" j);
        module_name =
          (match Json.string_or "" "module" j, declarations with "", d :: _ -> d.module_ | name, _ -> name);
        ok = Json.bool_or true "ok" report; errors = Json.int_or 0 "errors" report; warnings = Json.int_or 0 "warnings" report;
        imports = strings "imports" j; module_docs = strings "moduleDocs" j; declarations; steps; holes; findings }

let optional name = function None -> [] | Some text -> [ (name, Json.String text) ]
let list_of name items = (name, Json.List (List.map (fun s -> Json.String s) items))

let declaration_to_json (d : declaration) =
  Json.Obj
    ([ ("name", Json.String d.name); ("kind", Json.String d.kind); ("type", Json.String d.type_); ("module", Json.String d.module_);
       ("line", Json.Int d.line); ("column", Json.Int d.column); ("endLine", Json.Int d.end_line); ("endColumn", Json.Int d.end_column);
       ("nameLine", Json.Int d.name_line); ("nameColumn", Json.Int d.name_column) ]
    @ optional "doc" d.doc
    @ [ ("isPrivate", Json.Bool d.is_private); ("usesSorry", Json.Bool d.uses_sorry); list_of "axioms" d.axioms;
        list_of "typeDeps" d.type_deps; list_of "valueDeps" d.value_deps ]
    @ optional "instanceOf" d.instance_of
    @ [ list_of "parents" d.parents; ("statementHash", Json.String d.statement_hash); list_of "sameStatement" d.same_statement ]
    @ optional "source" d.source @ optional "proof" d.proof)

let step_to_json (s : step) =
  Json.Obj
    [ ("decl", Json.String s.decl); ("index", Json.Int s.index); ("tactic", Json.String s.tactic); ("line", Json.Int s.at_line);
      ("column", Json.Int s.at_column); ("endLine", Json.Int s.to_line); ("endColumn", Json.Int s.to_column);
      list_of "goalsBefore" s.goals_before; list_of "goalsAfter" s.goals_after; ("depth", Json.Int s.depth); ("inner", Json.Int s.inner) ]

let to_json t =
  Json.Obj
    [ ("schema", Json.String schema); ("file", Json.String t.file); ("module", Json.String t.module_name);
      ("report", Json.Obj [ ("ok", Json.Bool t.ok); ("errors", Json.Int t.errors); ("warnings", Json.Int t.warnings) ]);
      list_of "imports" t.imports; list_of "moduleDocs" t.module_docs;
      ("declarations", Json.List (List.map declaration_to_json t.declarations));
      ("steps", Json.List (List.map step_to_json t.steps));
      ( "holes",
        Json.List
          (List.map
             (fun h ->
               Json.Obj
                 [ ("decl", Json.String h.hole_decl); ("line", Json.Int h.hole_line); ("column", Json.Int h.hole_column);
                   list_of "goals" h.goals ])
             t.holes) );
      ( "findings",
        Json.List
          (List.map
             (fun f ->
               Json.Obj
                 [ ("rule", Json.String f.rule); ("severity", Json.String f.severity); ("declaration", Json.String f.declaration);
                   ("line", Json.Int f.finding_line); ("message", Json.String f.message) ])
             t.findings) ) ]

let contains_sorry (d : declaration) = List.mem "sorryAx" d.value_deps || List.mem "sorryAx" d.type_deps
let find t name = List.find_opt (fun (d : declaration) -> d.name = name) t.declarations
