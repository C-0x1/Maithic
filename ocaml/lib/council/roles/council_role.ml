(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type ask = purpose:string -> schema:string * Json.t -> (string * string) list -> (string, string) result

type context = {
  problem : Council_problem.t;
  board : Council_board.t;
  transcript : Council_transcript.t;
  statement : int;
  claim : int;
  round : int;
  task : Council_plan.task;
}

type status = Done | Idle | Failed of string

let status_name = function Done -> "done" | Idle -> "idle" | Failed _ -> "failed"

type work = { status : status; summary : string; posted : int list }
type t = { name : string; purpose : string; tasks : Council_plan.task list; uses_model : bool; tools : string list; prompt : string }

let formal_statement c =
  match List.find_opt (fun e -> Council_evidence.kind e = Council_evidence.Elaborated) (Council_board.evidence c.board c.claim) with
  | Some e -> Council_evidence.statement e
  | None -> ""

let say c role recipients performative ~subject ?refers content =
  ignore (Council_transcript.say c.transcript ~sender:role.name ~recipients performative ~subject ?refers content)

let answer text =
  match (String.index_opt text '{', String.rindex_opt text '}') with
  | Some first, Some last when last > first -> Result.to_option (Json.parse (String.sub text first (last - first + 1)))
  | _ -> None

let strings name j =
  List.filter (fun s -> s <> "") (List.filter_map (function Json.String s -> Some (String.trim s) | _ -> None) (Json.list_or_empty name j))

let text_list = Json.Obj [ ("type", Json.String "array"); ("items", Json.Obj [ ("type", Json.String "string") ]) ]

let object_schema members =
  Json.Obj
    [ ("type", Json.String "object"); ("properties", Json.Obj members); ("required", Json.List (List.map (fun (name, _) -> Json.String name) members));
      ("additionalProperties", Json.Bool false) ]

let first_line text = match String.split_on_char '\n' (String.trim text) with line :: _ -> line | [] -> ""
