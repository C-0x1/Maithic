(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What is asked of the laboratory: an algorithm in words, with the
    signature and the constraints when the asker has them, and examples the
    result has to get right. *)

open Maithic_base
open Maithic_bridge

type example = {
  call : string;  (** A Lean expression that calls the function. *)
  expect : string;  (** A Lean expression for what it should return. *)
}

type t = {
  name : string;  (** The name the function is to have. *)
  description : string;
  signature : string option;  (** What stands between the name and [:=], as in [(l : List Nat) : Nat]. *)
  constraints : string list;
  examples : example list;
}

let is_identifier name =
  name <> ""
  && (match name.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false)
  && String.for_all (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true | _ -> false) name

(** A task that can be worked on: a name that Lean can take, something that
    is asked, and examples that are whole. *)
let make ?signature ?(constraints = []) ?(examples = []) ~name description =
  let name = Text.trim name and description = Text.trim description in
  if not (is_identifier name) then Error (Printf.sprintf "'%s' cannot be the name of a function: letters, digits and _ are allowed, and no dot" name)
  else if description = "" then Error "the task says nothing about what the function should do"
  else
    let whole e = not (Text.is_blank e.call || Text.is_blank e.expect) in
    (* An example is an expression that Lean evaluates: one line, and nothing a proof may not contain either. *)
    let plain text = (not (String.contains text '\n')) && Screen.violations text = [] in
    match (List.find_opt (fun e -> not (whole e)) examples, List.find_opt (fun e -> not (plain e.call && plain e.expect)) examples) with
    | Some _, _ -> Error "an example needs a call and what the call should give"
    | None, Some e -> Error (Printf.sprintf "the example '%s' is not a plain expression on one line" (Text.trim e.call))
    | None, None ->
      Ok
        { name; description; signature = (match Option.map Text.trim signature with Some "" | None -> None | some -> some);
          constraints = List.filter (fun c -> c <> "") (List.map Text.trim constraints);
          examples = List.map (fun e -> { call = Text.trim e.call; expect = Text.trim e.expect }) examples }

(** Reads a task from a parsed TOML file: [name], [description], and when
    wanted [signature], [constraints] and a table [[example]] for each
    example, with [call] and [expect]. *)
let of_document doc =
  let examples = List.map (fun e -> { call = Json.string_or "" "call" e; expect = Json.string_or "" "expect" e }) (Json.list_or_empty "example" doc) in
  make ?signature:(Option.bind (Json.member "signature" doc) Json.string_opt)
    ~constraints:(Json.strings (Json.List (Json.list_or_empty "constraints" doc)))
    ~examples ~name:(Json.string_or "" "name" doc) (Json.string_or "" "description" doc)

let read path = Result.bind (Toml.parse_file path) (fun doc -> Result.map_error (fun e -> path ^ ": " ^ e) (of_document doc))

(** The first line of the definition that is asked for. *)
let header task = match task.signature with Some s -> Printf.sprintf "def %s %s" task.name s | None -> Printf.sprintf "def %s (no signature is given: choose one)" task.name

let to_json task =
  Json.Obj
    ([ ("name", Json.String task.name); ("description", Json.String task.description) ]
    @ (match task.signature with Some s -> [ ("signature", Json.String s) ] | None -> [])
    @ [ ("constraints", Json.List (List.map (fun c -> Json.String c) task.constraints));
        ("examples", Json.List (List.map (fun e -> Json.Obj [ ("call", Json.String e.call); ("expect", Json.String e.expect) ]) task.examples)) ])
