(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_prover

type provenance = { origin : string; provider : string; model : string; prompt : string; round : int }
type t = { id : int; name : string; signature : string; imports : string list; reading : string; provenance : provenance }

let given = { origin = "given"; provider = ""; model = ""; prompt = ""; round = 0 }

let strip_prefix prefix text = if Text.starts_with ~prefix text then Some (String.trim (String.sub text (String.length prefix) (String.length text - String.length prefix))) else None

let signature_of raw =
  let text = String.trim raw in
  (* A code fence around the statement. *)
  let text =
    if Text.starts_with ~prefix:"```" text then
      let body = match String.index_opt text '\n' with Some i -> String.sub text (i + 1) (String.length text - i - 1) | None -> "" in
      String.trim (match Text.find_sub body "```" with Some stop -> String.sub body 0 stop | None -> body)
    else text
  in
  (* The keyword and the name, when the model wrote a whole theorem. *)
  let text =
    match List.find_map (fun keyword -> strip_prefix keyword text) [ "theorem "; "lemma "; "example " ] with
    | Some rest when not (Text.starts_with ~prefix:"example " text) ->
      (* The name ends at the first blank, bracket or colon. *)
      let n = String.length rest in
      let rec stop i = if i < n && not (List.mem rest.[i] [ ' '; '\n'; '('; '{'; '['; ':' ]) then stop (i + 1) else i in
      String.trim (String.sub rest (stop 0) (n - stop 0))
    | Some rest -> rest
    | None -> text
  in
  (* The proof, when the model wrote one: everything from the last `:=` that is followed by sorry. *)
  let masked = Lean_source.mask text in
  let rec last_assign from found = match Text.find_sub ~from masked ":=" with Some i -> last_assign (i + 2) (Some i) | None -> found in
  match last_assign 0 None with
  | Some i ->
    let after = String.trim (String.sub text (i + 2) (String.length text - i - 2)) in
    let after = match strip_prefix "by" after with Some rest -> rest | None -> after in
    if after = "sorry" || after = "" then String.trim (String.sub text 0 i) else text
  | None -> text

let key c = String.concat " " (List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.concat " " (String.split_on_char '\n' c.signature))))
let header c = String.concat "" (List.map (fun m -> "import " ^ m ^ "\n") c.imports)

let theorem ?(proof = "sorry") c =
  let proof = String.trim proof in
  let body = if String.contains proof '\n' then " :=\n" ^ Text.indent 2 proof else " := " ^ proof in
  Printf.sprintf "set_option autoImplicit false in\ntheorem %s %s%s\n" c.name c.signature body

let answer text =
  match (String.index_opt text '{', String.rindex_opt text '}') with
  | Some first, Some last when last > first -> Result.to_option (Json.parse (String.sub text first (last - first + 1)))
  | _ -> None

let of_answer ~name ~imports ~provenance ~count text =
  let items = match Option.bind (answer text) (Json.member "candidates") with Some (Json.List items) -> items | _ -> [] in
  let read = function
    | Json.String statement -> Some (statement, "")
    | Json.Obj _ as item -> Option.map (fun statement -> (statement, Json.string_or "" "reading" item)) (Option.bind (Json.member "statement" item) Json.string_opt)
    | _ -> None
  in
  let kept, _ =
    List.fold_left
      (fun (kept, seen) (statement, reading) ->
        let signature = signature_of statement in
        let candidate = { id = List.length kept + 1; name = Printf.sprintf "%s_%d" name (List.length kept + 1); signature; imports; reading = String.trim reading; provenance } in
        if signature = "" || List.mem (key candidate) seen || List.length kept >= count then (kept, seen) else (kept @ [ candidate ], key candidate :: seen))
      ([], []) (List.filter_map read items)
  in
  kept

let to_json c =
  Json.Obj
    [ ("id", Json.Int c.id); ("name", Json.String c.name); ("statement", Json.String c.signature);
      ("imports", Json.List (List.map (fun m -> Json.String m) c.imports)); ("reading", Json.String c.reading);
      ( "provenance",
        Json.Obj
          ([ ("origin", Json.String c.provenance.origin) ]
           @ (if c.provenance.provider = "" then [] else [ ("provider", Json.String c.provenance.provider); ("model", Json.String c.provenance.model) ])
           @ (if c.provenance.prompt = "" then [] else [ ("prompt", Json.String c.provenance.prompt) ])
           @ [ ("round", Json.Int c.provenance.round) ]) ) ]
