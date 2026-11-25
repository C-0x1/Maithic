(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_gateway

let prompt = "formalize.repair"

type lookup = string list -> (string * string) list

let keywords =
  [ "fun"; "let"; "in"; "if"; "then"; "else"; "match"; "with"; "do"; "have"; "show"; "from"; "at"; "by"; "Type"; "Prop"; "Sort"; "forall"; "exists"; "sorry" ]

type token = Name of string | Symbol of string

(* The text as names and symbols. A name is ASCII: letters, digits, the underscore, the prime and the dots between its parts. *)
let tokens text =
  let n = String.length text in
  let letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let inside c = letter c || (c >= '0' && c <= '9') || c = '\'' || c = '!' || c = '?' in
  let rec name_end i = if i < n && (inside text.[i] || (text.[i] = '.' && i + 1 < n && letter text.[i + 1])) then name_end (i + 1) else i in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c = text.[i] in
      if c = ' ' || c = '\n' || c = '\t' || c = '\r' then go (i + 1) acc
      else if letter c then
        let stop = name_end i in
        go stop (Name (String.sub text i (stop - i)) :: acc)
      else if c >= '0' && c <= '9' then
        (* A numeral, with what clings to it. *)
        let rec stop j = if j < n && (inside text.[j] || text.[j] = '.') then stop (j + 1) else j in
        go (stop i) acc
      else
        (* One symbol: a character of one or several bytes; `=>` and `:=` are kept whole. *)
        let width =
          if c = '=' && i + 1 < n && text.[i + 1] = '>' then 2
          else if c = ':' && i + 1 < n && text.[i + 1] = '=' then 2
          else if Char.code c < 0x80 then 1
          else if Char.code c < 0xE0 then 2
          else if Char.code c < 0xF0 then 3
          else 4
        in
        let width = min width (n - i) in
        go (i + width) (Symbol (String.sub text i width) :: acc)
  in
  go 0 []

let bound text =
  let opens = [ "("; "{"; "["; "⦃"; "∀"; "∃"; "λ"; "∃!"; "Σ"; "∑"; "∏" ] and closes = [ ":"; ","; "=>"; "↦" ] in
  let rec names acc = function Name name :: rest when not (List.mem name [ "fun" ]) -> names (name :: acc) rest | rest -> (List.rev acc, rest) in
  let rec go acc = function
    | [] -> List.rev acc
    | (Symbol s | Name s) :: rest when List.mem s opens || s = "fun" ->
      (match names [] rest with
       | (_ :: _ as found), Symbol stop :: _ when List.mem stop closes -> go (List.rev_append found acc) rest
       | _ -> go acc rest)
    | _ :: rest -> go acc rest
  in
  List.sort_uniq compare (go [] (tokens text))

let identifiers text =
  let own = bound text in
  let wanted name = String.length name >= 2 && (not (List.mem name keywords)) && not (List.mem name own) in
  let found =
    List.fold_left (fun acc -> function Name name when wanted name && not (List.mem name acc) -> name :: acc | _ -> acc) [] (tokens text)
  in
  List.filteri (fun i _ -> i < 16) (List.rev found)

let signatures_text = function [] -> "(none)" | found -> String.concat "\n" (List.map (fun (name, signature) -> if Text.starts_with ~prefix:name signature then signature else name ^ " : " ^ signature) found)

let attempts_text = function
  | [] -> "(none)"
  | failures -> String.concat "\n\n" (List.map (fun (statement, said) -> Printf.sprintf "%s\n-- Lean: %s" statement (Text.truncate 600 (String.trim said))) failures)

let bindings ~imports ~(intake : Formalize_intake.t) ~(candidate : Formalize_candidate.t) ~error ~said ~signatures ~failures =
  [ ("libraries", Formalize_draft.libraries imports); ("problem", intake.Formalize_intake.text); ("statement", candidate.Formalize_candidate.signature);
    ("class", Printf.sprintf "%s. To repair it: %s." (Formalize_error.name error) (Formalize_error.hint error));
    ("errors", (match String.trim said with "" -> "(none)" | text -> Text.truncate 1500 text)); ("signatures", signatures_text signatures);
    ("attempts", attempts_text failures) ]

let schema =
  Json.Obj
    [ ("type", Json.String "object"); ("properties", Json.Obj [ ("statement", Json.Obj [ ("type", Json.String "string") ]) ]);
      ("required", Json.List [ Json.String "statement" ]); ("additionalProperties", Json.Bool false) ]

let statement_of_answer text =
  match Option.bind (Formalize_candidate.answer text) (Json.member "statement") with
  | Some (Json.String statement) -> Formalize_candidate.signature_of statement
  | _ -> ""

let ask gateway ~max_tokens ~(lookup : lookup) ~imports ~intake : Formalize_elab.repair =
 fun ~candidate ~error ~said ~failures ->
  let signatures = lookup (identifiers candidate.Formalize_candidate.signature) in
  match Gateway.prompt gateway prompt with
  | Error e -> Error e
  | Ok loaded ->
    (match Prompt.render loaded (bindings ~imports ~intake ~candidate ~error ~said ~signatures ~failures) with
     | Error e -> Error e
     | Ok (system, user) ->
       let request = Gateway_types.request ~purpose:prompt ~system ~max_tokens ~schema:("statement_repair", schema) [ Gateway_types.user user ] in
       (match Gateway.complete gateway request with
        | Error e -> Error (Gateway_types.error_to_string e)
        | Ok response ->
          Ok
            ( statement_of_answer response.Gateway_types.text,
              { Formalize_candidate.origin = "repair"; provider = response.Gateway_types.provider; model = response.Gateway_types.model; prompt; round = 0 } )))
