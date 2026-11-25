(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

let replacement renames = function
  | Mend_class.Deprecated { new_name; _ } when new_name <> "" -> Some new_name
  | Mend_class.Deprecated { old_name; _ } | Mend_class.Unknown_identifier old_name -> List.assoc_opt old_name renames
  | _ -> None

let old_name = function Mend_class.Deprecated { old_name; _ } | Mend_class.Unknown_identifier old_name -> old_name | _ -> ""

(* Where the old name stands in the stretch of a diagnostic. Lean prints a name in full, and the
   file may write it short inside a namespace, so the last parts of the name are looked for too. *)
let locate source (start, stop) name =
  let rec suffixes name = name :: (match String.index_opt name '.' with Some i -> suffixes (String.sub name (i + 1) (String.length name - i - 1)) | None -> []) in
  List.find_map
    (fun written -> Option.map (fun at -> (at, at + String.length written, written)) (Mend_edit.find_word source written ~from:start ~upto:stop))
    (suffixes name)

let propose (context : Mend_rule.context) (b : Mend_breakage.t) =
  let source = context.Mend_rule.source in
  let found =
    List.filter_map
      (fun (d : Bridge_types.diagnostic) ->
        match Mend_class.of_diagnostic d with
        | None -> None
        | Some class_ ->
          (match replacement context.Mend_rule.renames class_, Mend_edit.span source d with
           | Some new_name, Some stretch ->
             Option.map (fun (start, stop, written) -> ({ Mend_edit.start; stop; text = new_name }, written, new_name)) (locate source stretch (old_name class_))
           | _ -> None))
      b.Mend_breakage.diagnostics
  in
  (* Two diagnostics may point at the same use of a name. *)
  let distinct = List.fold_left (fun kept ((r : Mend_edit.replacement), w, n) -> if List.exists (fun ((k : Mend_edit.replacement), _, _) -> k.Mend_edit.start = r.Mend_edit.start) kept then kept else kept @ [ (r, w, n) ]) [] found in
  match distinct with
  | [] -> []
  | _ ->
    let pairs = List.sort_uniq compare (List.map (fun (_, written, new_name) -> (written, new_name)) distinct) in
    [ { Mend_edit.rule = "rename";
        description = "rename " ^ String.concat ", " (List.map (fun (written, new_name) -> Printf.sprintf "%s to %s" written new_name) pairs);
        replacements = List.map (fun (r, _, _) -> r) distinct;
        untrusted = String.concat "\n" (List.sort_uniq compare (List.map (fun (_, _, new_name) -> new_name) distinct)) } ]

let rule =
  { Mend_rule.name = "rename"; priority = 10;
    summary = "Replaces a name that is gone or deprecated by its successor, from Lean's deprecation message or the table of renames.";
    propose }

let parse_table text =
  match Toml.parse text with
  | Error e -> Error ("the table of renames is not TOML: " ^ e)
  | Ok doc ->
    (match Json.member "renames" doc with
     | Some (Json.Obj members) ->
       let rec go seen = function
         | [] -> Ok (List.rev seen)
         | (old_name, Json.String new_name) :: rest ->
           if old_name = new_name then Error (Printf.sprintf "the table renames %s to itself" old_name)
           else if List.mem_assoc old_name seen then Error (Printf.sprintf "the table renames %s twice" old_name)
           else if String.trim new_name = "" then Error (Printf.sprintf "the table renames %s to nothing" old_name)
           else go ((old_name, new_name) :: seen) rest
         | (old_name, _) :: _ -> Error (Printf.sprintf "the new name of %s must be a text" old_name)
       in
       go [] members
     | Some _ -> Error "`renames` must be a table of old names and new names"
     | None -> Error "the file has no table [renames]")

let load_table path = Result.bind (Fs.read path) (fun text -> Result.map_error (fun e -> path ^ ": " ^ e) (parse_table text))
