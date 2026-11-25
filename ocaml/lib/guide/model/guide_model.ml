(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type template = { name : string; count : int; closes : float }
type t = { dim : int; max_length : int; pooling : string; mixer : string; vocabulary : int; templates : template list; parameters : int }

let schema = "maithic.guide.model/1"
let ( let* ) = Result.bind
let member key j = match Json.member key j with Some v -> Ok v | None -> Error (Printf.sprintf "the model has no member \"%s\"" key)

(* The number of numbers in a matrix written as a list of vectors, or in a vector. *)
let size = function
  | Json.List rows -> List.fold_left (fun n row -> n + match row with Json.List cells -> List.length cells | _ -> 1) 0 rows
  | _ -> 0

let of_json j =
  match Json.member "schema" j with
  | Some (Json.String s) when s = schema ->
    let* config = member "config" j in
    let* dim = Json.get_int "dim" config in
    let* max_length = Json.get_int "max_length" config in
    let* pooling = Json.get_string "pooling" config in
    let* mixer = Json.get_string "mixer" config in
    let* vocabulary = member "vocabulary" j in
    let* tokens = Json.get_list "tokens" vocabulary in
    let* classes = member "templates" j in
    let* names = Json.get_list "templates" classes in
    let* counts = Json.get_list "counts" classes in
    let* closes = Json.get_list "closes" classes in
    let* weights = member "weights" j in
    if List.length names <> List.length counts || List.length names <> List.length closes then
      Error "the model does not have a count and a closing rate for each of its templates"
    else
      let templates =
        List.map2
          (fun name (count, closes) ->
            { name = Option.value ~default:"" (Json.string_opt name); count = Option.value ~default:0 (Json.int_opt count);
              closes = Option.value ~default:0.0 (Json.float_opt closes) })
          names (List.combine counts closes)
      in
      let parameters = match weights with Json.Obj members -> List.fold_left (fun n (_, m) -> n + size m) 0 members | _ -> 0 in
      Ok { dim; max_length; pooling; mixer; vocabulary = List.length tokens; templates; parameters }
  | Some (Json.String s) -> Error (Printf.sprintf "the document has schema \"%s\"; a model has \"%s\"" s schema)
  | _ -> Error (Printf.sprintf "the document names no schema; a model has \"%s\"" schema)

let load path =
  if not (Sys.file_exists path) then Error (Printf.sprintf "there is no model at %s" path)
  else
    let* text = Fs.read path in
    let* j = Result.map_error (fun e -> Printf.sprintf "%s is not JSON: %s" path e) (Json.parse text) in
    Result.map_error (fun e -> Printf.sprintf "%s is not a model: %s" path e) (of_json j)

let to_json t =
  Json.Obj
    [ ("dim", Json.Int t.dim); ("max_length", Json.Int t.max_length); ("pooling", Json.String t.pooling); ("mixer", Json.String t.mixer);
      ("vocabulary", Json.Int t.vocabulary); ("parameters", Json.Int t.parameters);
      ( "templates",
        Json.List
          (List.map (fun c -> Json.Obj [ ("template", Json.String c.name); ("count", Json.Int c.count); ("closes", Json.Float c.closes) ]) t.templates) ) ]

let render ?(color = false) ?(top = 15) t =
  let paint code s = if color then Printf.sprintf "\027[%sm%s\027[0m" code s else s in
  let total = List.fold_left (fun n c -> n + c.count) 0 t.templates in
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer
    (Printf.sprintf "%s  %s pooling%s, vectors of %d, reads up to %d tokens\n" (paint "1" "tactic model")
       t.pooling (if t.mixer = "none" then "" else " after " ^ t.mixer) t.dim t.max_length);
  Buffer.add_string buffer
    (Printf.sprintf "  %d weights, %d tokens, %d tactic templates from %d recorded steps\n\n" t.parameters t.vocabulary (List.length t.templates) total);
  let width = List.fold_left (fun w c -> Int.max w (Text.utf8_length c.name)) 8 t.templates in
  Buffer.add_string buffer (paint "2" (Printf.sprintf "  %s  %5s  %6s  %s\n" (Text.pad_right width "template") "steps" "share" "closes a goal"));
  let shown = List.filteri (fun i _ -> i < top) (List.stable_sort (fun a b -> compare b.count a.count) t.templates) in
  List.iter
    (fun c ->
      Buffer.add_string buffer
        (Printf.sprintf "  %s  %5d  %5.1f%%  %5.0f%%\n" (paint "36" (Text.pad_right width c.name)) c.count
           (if total = 0 then 0.0 else 100.0 *. float_of_int c.count /. float_of_int total)
           (100.0 *. c.closes)))
    shown;
  if List.length t.templates > top then
    Buffer.add_string buffer (paint "2" (Printf.sprintf "  and %d more\n" (List.length t.templates - top)));
  Buffer.contents buffer
