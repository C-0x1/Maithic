(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** From an argument in words to a blueprint: a model names the definitions
    and the lemmas and says what rests on what; the answer is put in order
    and checked before it becomes a file. *)

open Maithic_base
open Maithic_gateway

let prompt = "blueprint.decompose"
let default_libraries = "the Lean core library, without Mathlib"

let bindings ~imports ~namespace text =
  [ ("argument", text);
    ("libraries", if imports = [] then default_libraries else "the Lean core library and the modules " ^ String.concat ", " imports);
    ("namespace", match namespace with Some ns when ns <> "" -> ns | _ -> "(none)") ]

let text_schema = Json.Obj [ ("type", Json.String "string") ]
let list_schema = Json.Obj [ ("type", Json.String "array"); ("items", text_schema) ]

(* The schema of the answer: a title and the nodes. *)
let schema =
  let node =
    Json.Obj
      [ ("type", Json.String "object");
        ( "properties",
          Json.Obj
            [ ("id", text_schema); ("kind", text_schema); ("title", text_schema); ("statement", text_schema); ("proof", text_schema); ("lean", text_schema);
              ("formal", text_schema); ("uses", list_schema); ("proof_uses", list_schema) ] );
        ("required", Json.List (List.map (fun name -> Json.String name) [ "id"; "kind"; "title"; "statement" ])) ]
  in
  Json.Obj
    [ ("type", Json.String "object");
      ("properties", Json.Obj [ ("title", text_schema); ("nodes", Json.Obj [ ("type", Json.String "array"); ("items", node) ]) ]);
      ("required", Json.List [ Json.String "nodes" ]) ]

type outcome = {
  doc : Blueprint_doc.t;
  notes : string list;  (** What was changed to make the answer a blueprint. *)
}

(* The JSON object of an answer, which may stand in a code fence. *)
let answer_json text =
  let text = Text.trim text in
  let text =
    if Text.starts_with ~prefix:"```" text then
      match (String.index_opt text '\n', Text.find_sub (Text.trim text) "\n```") with
      | Some first, _ ->
        let body = String.sub text (first + 1) (String.length text - first - 1) in
        (match Text.find_sub body "```" with Some close -> String.sub body 0 close | None -> body)
      | None, _ -> text
    else text
  in
  Json.parse text

(** Makes a blueprint of a model's answer. Identifiers are made plain and
    unique, references to nothing and to the node itself are dropped, a
    definition keeps neither a proof nor what the proof referred to, and each
    of these is noted. An answer whose
    nodes depend on each other in a circle is not a plan, and is refused. *)
let of_answer ?namespace ?(imports = []) ?title text =
  match answer_json text with
  | Error e -> Error ("the answer is not JSON: " ^ e)
  | Ok json ->
    let raw = Json.list_or_empty "nodes" json in
    if raw = [] then Error "the answer names no node"
    else begin
      let notes = ref [] in
      let note text = notes := text :: !notes in
      let parsed = List.filter_map (fun item -> match Blueprint_node.of_json item with Ok node -> Some node | Error e -> note ("a node was left out: " ^ e); None) raw in
      (* plain and unique identifiers, with the old names remembered for the references *)
      let renamed = Hashtbl.create 16 and taken = Hashtbl.create 16 in
      let fresh wanted =
        let base = if Blueprint_node.valid_id wanted then wanted else Blueprint_node.slug wanted in
        let rec pick n =
          let candidate = if n = 1 then base else Printf.sprintf "%s-%d" base n in
          if Hashtbl.mem taken candidate then pick (n + 1) else candidate
        in
        let id = pick 1 in
        Hashtbl.replace taken id ();
        id
      in
      let nodes =
        List.map
          (fun (node : Blueprint_node.t) ->
            let id = fresh (if node.Blueprint_node.id = "" then node.Blueprint_node.title else node.Blueprint_node.id) in
            if id <> node.Blueprint_node.id then note (Printf.sprintf "the identifier '%s' became '%s'" node.Blueprint_node.id id);
            if not (Hashtbl.mem renamed node.Blueprint_node.id) then Hashtbl.replace renamed node.Blueprint_node.id id;
            { node with Blueprint_node.id })
          parsed
      in
      let known = List.map (fun (node : Blueprint_node.t) -> node.Blueprint_node.id) nodes in
      let nodes =
        List.map
          (fun (node : Blueprint_node.t) ->
            let resolve targets =
              List.filter_map
                (fun target ->
                  let target = Option.value ~default:target (Hashtbl.find_opt renamed target) in
                  if target = node.Blueprint_node.id then (note (Printf.sprintf "'%s' referred to itself" target); None)
                  else if List.mem target known then Some target
                  else (note (Printf.sprintf "'%s' referred to '%s', which is not a node" node.Blueprint_node.id target); None))
                targets
            in
            let uses = List.sort_uniq compare (resolve node.Blueprint_node.uses) in
            let proof_uses = List.filter (fun target -> not (List.mem target uses)) (List.sort_uniq compare (resolve node.Blueprint_node.proof_uses)) in
            if node.Blueprint_node.kind = Blueprint_node.Definition && (node.Blueprint_node.proof <> "" || proof_uses <> []) then begin
              note (Printf.sprintf "'%s' is a definition, and its proof was dropped" node.Blueprint_node.id);
              { node with Blueprint_node.uses; proof_uses = []; proof = "" }
            end
            else { node with Blueprint_node.uses; proof_uses })
          nodes
      in
      let title = match title with Some t when t <> "" -> t | _ -> (match Text.trim (Json.string_or "" "title" json) with "" -> "Blueprint" | t -> t) in
      let doc = Blueprint_doc.make ?namespace ~imports ~title nodes in
      match Blueprint_check.validate doc with
      | [] -> Ok { doc; notes = List.rev !notes }
      | problems -> Error (String.concat "; " (List.map Blueprint_check.describe problems))
    end

(** Asks a model to take an argument apart. *)
let ask gateway ~max_tokens ?namespace ?(imports = []) ?title text =
  match Gateway.prompt gateway prompt with
  | Error e -> Error e
  | Ok loaded ->
    (match Prompt.render loaded (bindings ~imports ~namespace text) with
     | Error e -> Error e
     | Ok (system, user) ->
       let request = Gateway_types.request ~purpose:prompt ~system ~max_tokens ~schema:("blueprint_plan", schema) [ Gateway_types.user user ] in
       (match Gateway.complete gateway request with
        | Error e -> Error (Gateway_types.error_to_string e)
        | Ok response -> of_answer ?namespace ~imports ?title response.Gateway_types.text))

(** Writes a plan as a blueprint file. *)
let write ~path outcome = Fs.write path (Blueprint_file.print outcome.doc)
