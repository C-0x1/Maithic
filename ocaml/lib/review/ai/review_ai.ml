(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The part of a review that a model writes.

    The model is shown the file and asked for findings in a fixed form. Two
    things keep this part from doing harm.

    The text of the file is data. It is put between the data markers of the
    prompt with its line numbers, and the markers are defused inside it, so
    nothing in a file can end the fence it stands in or pose as an
    instruction.

    The answer is data too. It is read against a schema, and only findings
    survive: a rule from a short list, a severity no higher than a warning,
    a line that exists in the file. Whatever else the answer says is dropped.
    The model can add findings to a review; it cannot take one away, lower
    one, or approve anything. *)

open Maithic_base
open Maithic_gateway

(** The rules a model may report under. *)
let rules = [ "statement"; "proof"; "definition" ]

(** The schema of the answer that is asked for. *)
let schema =
  Json.Obj
    [ ("type", Json.String "object");
      ( "properties",
        Json.Obj
          [ ( "findings",
              Json.Obj
                [ ("type", Json.String "array");
                  ( "items",
                    Json.Obj
                      [ ("type", Json.String "object");
                        ( "properties",
                          Json.Obj
                            [ ("rule", Json.Obj [ ("enum", Json.List (List.map (fun r -> Json.String r) rules)) ]);
                              ("severity", Json.Obj [ ("enum", Json.List [ Json.String "info"; Json.String "warning" ]) ]);
                              ("line", Json.Obj [ ("type", Json.String "integer"); ("minimum", Json.Int 1) ]);
                              ("declaration", Json.Obj [ ("type", Json.String "string") ]);
                              ("message", Json.Obj [ ("type", Json.String "string"); ("minLength", Json.Int 1) ]) ] );
                        ("required", Json.List (List.map (fun k -> Json.String k) [ "rule"; "severity"; "line"; "declaration"; "message" ]));
                        ("additionalProperties", Json.Bool false) ] ) ] ) ] );
      ("required", Json.List [ Json.String "findings" ]); ("additionalProperties", Json.Bool false) ]

(** The file with a number in front of every line, as the model is shown it. *)
let numbered source =
  String.concat "\n" (List.mapi (fun i line -> Printf.sprintf "%4d | %s" (i + 1) line) (String.split_on_char '\n' source))

(** The system and user text of the request. *)
let render ?(count = 5) prompt ~file ~source =
  Prompt.render prompt [ ("count", string_of_int count); ("file", file); ("source", numbered source) ]

(** Reads an answer. Findings that do not have the asked-for form, name a
    rule outside the list, claim a severity above a warning or point at a
    line the file does not have are dropped, and so is everything in the
    answer that is not a finding. An answer that is not JSON gives none. *)
let read ?(count = 5) ~file ~source answer =
  let lines = List.length (String.split_on_char '\n' source) in
  let text = String.trim answer in
  (* A model may wrap its JSON in a code fence. *)
  let text =
    if Text.starts_with ~prefix:"```" text then
      match String.index_opt text '\n', Text.find_sub text "\n```" with
      | Some first, Some last when last > first -> String.sub text (first + 1) (last - first - 1)
      | _ -> text
    else text
  in
  match Json.parse text with
  | Error _ -> []
  | Ok j ->
    List.filteri
      (fun i _ -> i < count)
      (List.filter_map
         (fun item ->
           match Json.member "rule" item, Json.member "severity" item, Json.member "line" item, Json.member "message" item with
           | Some (Json.String rule), Some (Json.String severity), Some (Json.Int line), Some (Json.String message)
             when List.mem rule rules && (severity = "info" || severity = "warning") && line >= 1 && line <= lines && String.trim message <> "" ->
             Some
               { Review_finding.rule; origin = Review_finding.Model;
                 severity = (if severity = "warning" then Review_finding.Warning else Review_finding.Info); file; line;
                 declaration = Json.string_or "" "declaration" item; message = String.trim message; edit = None }
           | _ -> None)
         (Json.list_or_empty "findings" j))

(** Asks the model of a gateway to review a file. A gateway that cannot
    answer is an error; an answer with nothing usable in it is no finding. *)
let review ?(count = 5) ?(max_tokens = 1024) gateway ~file ~source =
  match Gateway.prompt gateway "review.findings" with
  | Error e -> Error e
  | Ok prompt ->
    (match render ~count prompt ~file ~source with
     | Error e -> Error e
     | Ok (system, user) ->
       let request = Gateway_types.request ~purpose:"review.findings" ~system ~max_tokens ~schema:("review_findings", schema) [ Gateway_types.user user ] in
       (match Gateway.complete gateway request with
        | Ok response -> Ok (read ~count ~file ~source response.Gateway_types.text)
        | Error e -> Error (Gateway_types.error_to_string e)))
