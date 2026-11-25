(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

let version = 1
let proof_marker = "**Proof.**"

(* ---- printing ---- *)

let names ids = String.concat ", " (List.map (fun id -> "`" ^ id ^ "`") ids)

let print_node (node : Blueprint_node.t) =
  let attributes =
    (match node.Blueprint_node.lean with Some name -> [ "- lean: `" ^ name ^ "`" ] | None -> [])
    @ (if node.Blueprint_node.uses = [] then [] else [ "- uses: " ^ names node.Blueprint_node.uses ])
    @ if node.Blueprint_node.proof_uses = [] then [] else [ "- proof uses: " ^ names node.Blueprint_node.proof_uses ]
  in
  let parts =
    [ Printf.sprintf "## %s `%s`: %s" (Blueprint_node.kind_title node.Blueprint_node.kind) node.Blueprint_node.id node.Blueprint_node.title ]
    @ (if attributes = [] then [] else [ String.concat "\n" attributes ])
    @ (if node.Blueprint_node.statement = "" then [] else [ node.Blueprint_node.statement ])
    @ (match node.Blueprint_node.formal with Some formal -> [ "```lean\n" ^ formal ^ "\n```" ] | None -> [])
    @ if node.Blueprint_node.proof = "" then [] else [ proof_marker ^ " " ^ node.Blueprint_node.proof ]
  in
  String.concat "\n\n" parts

let print (doc : Blueprint_doc.t) =
  let front =
    [ ("blueprint", string_of_int version); ("title", doc.Blueprint_doc.title) ]
    @ (match doc.Blueprint_doc.namespace with Some ns -> [ ("namespace", ns) ] | None -> [])
    @ (if doc.Blueprint_doc.imports = [] then [] else [ ("imports", String.concat ", " doc.Blueprint_doc.imports) ])
    @ doc.Blueprint_doc.meta
  in
  let head = "---\n" ^ Text.concat_map "\n" (fun (key, value) -> key ^ ": " ^ value) front ^ "\n---" in
  String.concat "\n\n" ([ head ] @ (if doc.Blueprint_doc.intro = "" then [] else [ doc.Blueprint_doc.intro ]) @ List.map print_node doc.Blueprint_doc.nodes) ^ "\n"

(* ---- parsing ---- *)

let fail line message = Error (Printf.sprintf "line %d: %s" line message)
let ( let* ) = Result.bind

(* Trims the blank lines at both ends of a block of lines and joins it. *)
let block lines =
  let rec drop = function line :: rest when Text.is_blank line -> drop rest | lines -> lines in
  String.concat "\n" (List.rev (drop (List.rev (drop lines))))

(* The names of an attribute: separated by commas, each in backticks or bare. *)
let names_of value =
  List.filter_map
    (fun part ->
      let part = Text.trim part in
      let part = if String.length part >= 2 && part.[0] = '`' && part.[String.length part - 1] = '`' then String.sub part 1 (String.length part - 2) else part in
      if part = "" then None else Some part)
    (String.split_on_char ',' value)

let front_matter lines =
  match lines with
  | (_, first) :: rest when Text.trim first = "---" ->
    let rec take acc = function
      | [] -> fail 1 "the front matter is not closed: a line with --- is missing"
      | (_, line) :: rest when Text.trim line = "---" -> Ok (List.rev acc, rest)
      | (n, line) :: rest ->
        if Text.is_blank line then take acc rest
        else (
          match String.index_opt line ':' with
          | None -> fail n "a line of the front matter reads 'key: value'"
          | Some i -> take ((n, Text.trim (String.sub line 0 i), Text.trim (String.sub line (i + 1) (String.length line - i - 1))) :: acc) rest)
    in
    take [] rest
  | _ -> fail 1 "a blueprint begins with front matter between two lines of ---"

(* The heading of a node: ## Kind `id`: title *)
let heading n line =
  let rest = Text.trim (String.sub line 2 (String.length line - 2)) in
  match String.index_opt rest '`' with
  | None -> fail n "the heading of a node reads: ## Kind `identifier`: title"
  | Some open_ ->
    (match String.index_from_opt rest (open_ + 1) '`' with
     | None -> fail n "the identifier of the node is not closed by a backtick"
     | Some close ->
       let kind_text = Text.trim (String.sub rest 0 open_) and id = String.sub rest (open_ + 1) (close - open_ - 1) in
       let after = Text.trim (String.sub rest (close + 1) (String.length rest - close - 1)) in
       let title = if Text.starts_with ~prefix:":" after then Text.trim (String.sub after 1 (String.length after - 1)) else after in
       (match Blueprint_node.kind_of_name kind_text with
        | None ->
          fail n
            (Printf.sprintf "'%s' is not a kind of node; the kinds are %s" kind_text
               (String.concat ", " (List.map Blueprint_node.kind_title Blueprint_node.kinds)))
        | Some kind ->
          if not (Blueprint_node.valid_id id) then
            fail n (Printf.sprintf "'%s' cannot be an identifier: letters, digits, and - _ . : after the first character" id)
          else Ok (kind, id, title)))

let is_fence line = Text.starts_with ~prefix:"```" (Text.trim line)

(* One node from the lines below its heading. *)
let node_of n (kind, id, title) lines =
  (* attributes: list items at the head of the section *)
  let rec attributes acc = function
    | (_, line) :: rest when Text.is_blank line && acc = [] -> attributes acc rest
    | (m, line) :: rest when Text.starts_with ~prefix:"- " (Text.trim line) ->
      let item = Text.trim line in
      let item = String.sub item 2 (String.length item - 2) in
      (match String.index_opt item ':' with
       | None -> fail m "an attribute of a node reads '- key: value'"
       | Some i ->
         let key = Text.lowercase (Text.trim (String.sub item 0 i)) and value = Text.trim (String.sub item (i + 1) (String.length item - i - 1)) in
         if List.mem key [ "lean"; "uses"; "proof uses" ] then attributes ((key, value) :: acc) rest
         else fail m (Printf.sprintf "'%s' is not an attribute of a node; the attributes are lean, uses and proof uses" key))
    | rest -> Ok (List.rev acc, rest)
  in
  let* attrs, body = attributes [] lines in
  (* the body: statement text, at most one Lean block, and the proof after its marker *)
  let rec walk ~in_proof statement formal proof = function
    | [] -> Ok (List.rev statement, formal, List.rev proof)
    | (m, line) :: rest when is_fence line ->
      let language = Text.trim (String.sub (Text.trim line) 3 (String.length (Text.trim line) - 3)) in
      let rec fenced acc = function
        | [] -> fail m "a code block is not closed"
        | (_, line) :: rest when is_fence line -> Ok (List.rev acc, rest)
        | (_, line) :: rest -> fenced (line :: acc) rest
      in
      let* code, rest = fenced [] rest in
      if language = "lean" && (not in_proof) && formal = None then walk ~in_proof statement (Some (String.concat "\n" code)) proof rest
      else
        let kept = (line :: code) @ [ "```" ] in
        if in_proof then walk ~in_proof statement formal (List.rev_append kept proof) rest
        else walk ~in_proof (List.rev_append kept statement) formal proof rest
    | (_, line) :: rest when (not in_proof) && Text.starts_with ~prefix:proof_marker (Text.trim line) ->
      let trimmed = Text.trim line in
      let first = Text.trim (String.sub trimmed (String.length proof_marker) (String.length trimmed - String.length proof_marker)) in
      walk ~in_proof:true statement formal (if first = "" then proof else first :: proof) rest
    | (_, line) :: rest -> if in_proof then walk ~in_proof statement formal (line :: proof) rest else walk ~in_proof (line :: statement) formal proof rest
  in
  let* statement, formal, proof = walk ~in_proof:false [] None [] body in
  let attribute key = List.assoc_opt key attrs in
  let lean = match Option.map names_of (attribute "lean") with Some (name :: _) -> Some name | _ -> None in
  let formal = match Option.map Text.trim formal with Some "" | None -> None | Some _ -> Option.map (fun text -> block (String.split_on_char '\n' text)) formal in
  ignore n;
  Ok
    { Blueprint_node.id; kind; title; statement = block statement; proof = block proof; lean; formal;
      uses = (match attribute "uses" with Some value -> names_of value | None -> []);
      proof_uses = (match attribute "proof uses" with Some value -> names_of value | None -> []) }

let parse text =
  let lines = List.mapi (fun i line -> (i + 1, line)) (String.split_on_char '\n' text) in
  let* front, body = front_matter lines in
  let value key = List.find_map (fun (_, k, v) -> if k = key then Some v else None) front in
  let* () =
    match value "blueprint" with
    | None -> fail 1 "the front matter does not say 'blueprint: 1'"
    | Some v when int_of_string_opt v = Some version -> Ok ()
    | Some v -> fail 1 (Printf.sprintf "this is version %s of the format, and version %d is the one that is read" v version)
  in
  let* title = match value "title" with Some t when t <> "" -> Ok t | _ -> fail 1 "the front matter has no title" in
  (* sections: a line that begins with "## " outside a code block opens a node *)
  let rec split ~fenced intro sections current = function
    | [] -> (List.rev intro, List.rev (match current with Some (n, h, ls) -> (n, h, List.rev ls) :: sections | None -> sections))
    | ((n, line) as numbered) :: rest ->
      let fenced' = if is_fence line then not fenced else fenced in
      if (not fenced) && Text.starts_with ~prefix:"## " line then
        let sections = match current with Some (m, h, ls) -> (m, h, List.rev ls) :: sections | None -> sections in
        split ~fenced:fenced' intro sections (Some (n, line, [])) rest
      else (
        match current with
        | None -> split ~fenced:fenced' (line :: intro) sections None rest
        | Some (m, h, ls) -> split ~fenced:fenced' intro sections (Some (m, h, numbered :: ls)) rest)
  in
  let intro, sections = split ~fenced:false [] [] None body in
  let* nodes =
    List.fold_left
      (fun acc (n, line, lines) ->
        let* acc = acc in
        let* head = heading n line in
        let* node = node_of n head lines in
        Ok (node :: acc))
      (Ok []) sections
  in
  let known = [ "blueprint"; "title"; "namespace"; "imports" ] in
  Ok
    { Blueprint_doc.title; namespace = (match value "namespace" with Some "" | None -> None | some -> some);
      imports = (match value "imports" with Some v -> names_of v | None -> []);
      meta = List.filter_map (fun (_, k, v) -> if List.mem k known then None else Some (k, v)) front; intro = block intro; nodes = List.rev nodes }

let read path = Result.bind (Fs.read path) (fun text -> Result.map_error (fun e -> path ^ ": " ^ e) (parse text))
