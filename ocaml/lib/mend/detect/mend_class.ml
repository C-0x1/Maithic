(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

type t =
  | Unknown_identifier of string
  | Deprecated of { old_name : string; new_name : string }
  | Unsolved_goals
  | Type_mismatch
  | Suggestion of string list
  | Other

let name = function
  | Unknown_identifier _ -> "unknown-identifier"
  | Deprecated _ -> "deprecated"
  | Unsolved_goals -> "unsolved-goals"
  | Type_mismatch -> "type-mismatch"
  | Suggestion _ -> "suggestion"
  | Other -> "other"

let names = [ "unknown-identifier"; "deprecated"; "unsolved-goals"; "type-mismatch"; "suggestion"; "other" ]

let detail = function
  | Unknown_identifier n -> n
  | Deprecated { old_name; _ } -> old_name
  | Suggestion (first :: _) -> first
  | Suggestion [] | Unsolved_goals | Type_mismatch | Other -> ""

let treatment = function
  | Unknown_identifier _ -> "look the name up in the table of renames"
  | Deprecated { new_name = ""; _ } -> "look the name up in the table of renames: Lean names no replacement"
  | Deprecated _ -> "replace the name by the one Lean names"
  | Suggestion _ -> "write what Lean suggests"
  | Unsolved_goals -> "ask a model for the smallest change to the proof, then prove the statement again"
  | Type_mismatch -> "ask a model for the smallest change to the proof, then prove the statement again"
  | Other -> "ask a model for the smallest change to the proof, then prove the statement again"

(* Lean writes names between backticks; older versions wrote them between single quotes. *)
let quoted text =
  let n = String.length text in
  let rec go i acc =
    if i >= n then List.rev acc
    else if text.[i] = '`' || text.[i] = '\'' then
      match String.index_from_opt text (i + 1) text.[i] with
      | Some j when j > i + 1 && not (String.contains (String.sub text (i + 1) (j - i - 1)) '\n') -> go (j + 1) (String.sub text (i + 1) (j - i - 1) :: acc)
      | _ -> go (i + 1) acc
    else go (i + 1) acc
  in
  go 0 []

let starts prefix text = Text.starts_with ~prefix (Text.lowercase (String.trim text))

(* The texts Lean suggests: the lines after "Try this:", each begun by "[apply]" in newer
   versions, with the deeper lines that continue a suggestion. *)
let suggestions text =
  match Text.lines text with
  | [] -> []
  | first :: rest ->
    let first = String.trim first in
    let after_colon = match String.index_opt first ':' with Some i -> String.trim (String.sub first (i + 1) (String.length first - i - 1)) | None -> "" in
    let strip line =
      let line = String.trim line in
      if Text.starts_with ~prefix:"[apply]" line then (true, String.trim (String.sub line 7 (String.length line - 7))) else (false, line)
    in
    let indent line =
      let rec go i = if i < String.length line && line.[i] = ' ' then go (i + 1) else i in
      go 0
    in
    let rest = List.filter (fun l -> String.trim l <> "") rest in
    let marked = List.exists (fun l -> fst (strip l)) rest in
    let base = List.fold_left (fun acc l -> min acc (indent l)) max_int rest in
    let grouped =
      List.fold_left
        (fun acc line ->
          let begins, body = strip line in
          match acc with
          | current :: earlier when (marked && not begins) || ((not marked) && indent line > base) ->
            (* A line that continues the suggestion before it keeps its depth below the first line. *)
            let depth = max 0 (indent line - base) in
            (current ^ "\n" ^ String.make depth ' ' ^ body) :: earlier
          | _ -> body :: acc)
        [] rest
    in
    (if after_colon = "" then [] else [ after_colon ]) @ List.rev grouped

let of_diagnostic (d : Bridge_types.diagnostic) =
  let text = d.Bridge_types.text in
  let lower = Text.lowercase text in
  let has needle = Text.contains lower needle in
  match d.Bridge_types.severity with
  | "error" ->
    if starts "unknown identifier" text || starts "unknown constant" text then
      Some (match quoted text with n :: _ -> Unknown_identifier n | [] -> Other)
    else if has "the identifier `" && has "is unknown" then
      (* A name that is not there, in a statement where Lean took it for a variable. *)
      let from = Option.value ~default:0 (Text.find_sub lower "the identifier `") in
      Some (match quoted (String.sub text from (String.length text - from)) with n :: _ -> Unknown_identifier n | [] -> Other)
    else if starts "unsolved goals" text || has "made no progress" || has "could not prove the goal" || has "could not close the goal"
            || has "`grind` failed" || has "the rfl tactic failed" || (starts "tactic `" text && has "failed") || has "linarith failed" then
      Some Unsolved_goals
    else if starts "type mismatch" text || has "has type" && has "but is expected to have type" then Some Type_mismatch
    else Some Other
  | "warning" ->
    if has "has been deprecated" then
      let before, after =
        match Text.find_sub lower "has been deprecated" with
        | Some i -> (String.sub text 0 i, String.sub text i (String.length text - i))
        | None -> (text, "")
      in
      (* The replacement is named on the line of the message, before any hint that follows it. *)
      let after = match Text.lines after with line :: _ -> line | [] -> "" in
      (match quoted before with
       | old_name :: _ -> Some (Deprecated { old_name; new_name = (match quoted after with n :: _ -> n | [] -> "") })
       | [] -> None)
    else None
  | "info" -> if starts "try this" text || starts "try these" text then (match suggestions text with [] -> None | found -> Some (Suggestion found)) else None
  | _ -> None
