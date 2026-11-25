(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** One finding of a review, whichever check made it.

    The lint rules of the Lean library, the checks on statements, the
    verified suggestions and the model all report in this one form, so that
    policy, suppression and the reports treat them alike. *)

open Maithic_base

type severity = Info | Warning | Error

(** Where a finding comes from: a lint rule run in Lean, a check on what was
    extracted, a simplification that Lean verified, or a model. *)
type origin = Lint | Semantic | Suggestion | Model

(** A change to the text of a proof that Lean has checked. *)
type edit = { before : string; after : string }

type t = {
  rule : string;
  origin : origin;
  severity : severity;
  file : string;
  line : int;
  declaration : string;
  message : string;
  edit : edit option;
}

let severity_to_string = function Info -> "info" | Warning -> "warning" | Error -> "error"

let severity_of_string = function
  | "info" -> Some Info
  | "warning" -> Some Warning
  | "error" -> Some Error
  | _ -> None

let severity_rank = function Info -> 0 | Warning -> 1 | Error -> 2
let origin_to_string = function Lint -> "lint" | Semantic -> "semantic" | Suggestion -> "suggestion" | Model -> "model"

let origin_of_string = function
  | "lint" -> Some Lint
  | "semantic" -> Some Semantic
  | "suggestion" -> Some Suggestion
  | "model" -> Some Model
  | _ -> None

(** Findings in the order of a report: by file, by line, the most severe
    first, then by rule. *)
let compare a b =
  compare (a.file, a.line, - severity_rank a.severity, a.rule, a.message) (b.file, b.line, - severity_rank b.severity, b.rule, b.message)

let to_json f =
  Json.Obj
    ([ ("rule", Json.String f.rule); ("origin", Json.String (origin_to_string f.origin)); ("severity", Json.String (severity_to_string f.severity));
       ("file", Json.String f.file); ("line", Json.Int f.line); ("declaration", Json.String f.declaration); ("message", Json.String f.message) ]
    @ match f.edit with
      | Some e -> [ ("edit", Json.Obj [ ("before", Json.String e.before); ("after", Json.String e.after) ]) ]
      | None -> [])

let of_json j =
  let ( let* ) = Result.bind in
  let* rule = Json.get_string "rule" j in
  let* message = Json.get_string "message" j in
  let* severity = Option.to_result ~none:"a finding has no severity" (severity_of_string (Json.string_or "" "severity" j)) in
  let* origin = Option.to_result ~none:"a finding does not say where it comes from" (origin_of_string (Json.string_or "" "origin" j)) in
  let edit =
    Option.bind (Json.member "edit" j) (fun e ->
        match Json.member "before" e, Json.member "after" e with
        | Some (Json.String before), Some (Json.String after) -> Some { before; after }
        | _ -> None)
  in
  Ok { rule; origin; severity; file = Json.string_or "" "file" j; line = Json.int_or 0 "line" j; declaration = Json.string_or "" "declaration" j; message; edit }

(** A finding of a lint rule, as the Bridge reports it. *)
let of_lint ~file (f : Maithic_bridge.Extraction.finding) =
  { rule = f.Maithic_bridge.Extraction.rule; origin = Lint;
    severity = Option.value ~default:Info (severity_of_string f.Maithic_bridge.Extraction.severity); file;
    line = f.Maithic_bridge.Extraction.finding_line; declaration = f.Maithic_bridge.Extraction.declaration;
    message = f.Maithic_bridge.Extraction.message; edit = None }
