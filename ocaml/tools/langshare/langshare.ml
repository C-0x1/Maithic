(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The language-share gate.

    At least 85 percent of the source code, measured in bytes, must be OCaml,
    Lean, Julia and R together, and each of the four must meet its own floor.
    Files are classified the way a code-hosting language bar classifies
    them, so the bar a visitor sees agrees with the gate.

    Counted as code: the four project languages and the other programming
    languages. Not counted: prose, data and configuration (Markdown, TOML,
    YAML, JSON), images, and anything under [fixtures/] or [assets/] or
    marked generated, vendored or documentation in [.gitattributes]. *)

open Maithic_base

type language = OCaml | Lean | Julia | R | Other of string

let project_languages = [ OCaml; Lean; Julia; R ]

let name = function OCaml -> "OCaml" | Lean -> "Lean" | Julia -> "Julia" | R -> "R" | Other n -> n

(** The language of a file, or [None] when the file is not counted as code. *)
let classify rel =
  match Filename.basename rel, Fs.extension rel with
  | _, (".ml" | ".mli" | ".mll" | ".mly") -> Some OCaml
  | _, ".lean" -> Some Lean
  | _, ".jl" -> Some Julia
  | _, ".r" -> Some R
  | _, ".py" -> Some (Other "Python")
  | _, (".ts" | ".tsx") -> Some (Other "TypeScript")
  | _, (".js" | ".mjs") -> Some (Other "JavaScript")
  | _, (".sh" | ".bash") -> Some (Other "Shell")
  | _, ".css" -> Some (Other "CSS")
  | _, ".html" -> Some (Other "HTML")
  | _, ".sql" -> Some (Other "SQL")
  | ("Makefile" | "GNUmakefile"), _ -> Some (Other "Makefile")
  | "Dockerfile", _ -> Some (Other "Dockerfile")
  | _ -> None

let excluded_roots = [ "fixtures/"; "assets/" ]

(* Path patterns that .gitattributes marks as generated, vendored or
   documentation. A pattern is a directory prefix ("docs/"), a suffix glob
   ("*.min.js") or an exact path. *)
let attribute_exclusions text =
  Text.lines text
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if line = "" || line.[0] = '#' then None
         else
           match String.split_on_char ' ' line |> List.filter (fun w -> w <> "") with
           | pattern :: attributes
             when List.exists
                    (fun a -> List.mem a [ "linguist-generated"; "linguist-generated=true"; "linguist-vendored"; "linguist-vendored=true"; "linguist-documentation"; "linguist-documentation=true" ])
                    attributes -> Some pattern
           | _ -> None)

let matches_pattern pattern rel =
  let n = String.length pattern in
  if n = 0 then false
  else if Text.ends_with ~suffix:"/**" pattern then Text.starts_with ~prefix:(String.sub pattern 0 (n - 2)) rel
  else if pattern.[n - 1] = '/' then Text.starts_with ~prefix:pattern rel
  else if pattern.[0] = '*' then Text.ends_with ~suffix:(String.sub pattern 1 (n - 1)) rel
  else pattern = rel || Text.starts_with ~prefix:(pattern ^ "/") rel

type share = { language : language; bytes : int; files : int; percent : float }

type report = { shares : share list; total_bytes : int; project_percent : float }

(** Measures the share of each language over a list of files with sizes. *)
let measure ?(exclusions = []) entries =
  let counted =
    List.filter_map
      (fun (rel, size) ->
        if List.exists (fun prefix -> Text.starts_with ~prefix rel) excluded_roots then None
        else if List.exists (fun pattern -> matches_pattern pattern rel) exclusions then None
        else Option.map (fun language -> (language, size)) (classify rel))
      entries
  in
  let total_bytes = List.fold_left (fun acc (_, size) -> acc + size) 0 counted in
  let languages = List.sort_uniq compare (List.map fst counted) in
  let percent bytes = if total_bytes = 0 then 0.0 else 100.0 *. float_of_int bytes /. float_of_int total_bytes in
  let shares =
    List.map
      (fun language ->
        let mine = List.filter (fun (l, _) -> l = language) counted in
        let bytes = List.fold_left (fun acc (_, size) -> acc + size) 0 mine in
        { language; bytes; files = List.length mine; percent = percent bytes })
      languages
    |> List.sort (fun a b -> compare b.bytes a.bytes)
  in
  let project_bytes =
    List.fold_left (fun acc s -> if List.mem s.language project_languages then acc + s.bytes else acc) 0 shares
  in
  { shares; total_bytes; project_percent = percent project_bytes }

type thresholds = { combined : float; ocaml : float; lean : float; julia : float; r : float }

let default_thresholds = { combined = 85.0; ocaml = 30.0; lean = 15.0; julia = 12.0; r = 8.0 }

let thresholds_of_project doc =
  let table = Option.value (Json.member "language_share" doc) ~default:(Json.Obj []) in
  let number key default = Option.value (Option.bind (Json.member key table) Json.float_opt) ~default in
  { combined = number "combined_min" default_thresholds.combined;
    ocaml = number "ocaml_min" default_thresholds.ocaml;
    lean = number "lean_min" default_thresholds.lean;
    julia = number "julia_min" default_thresholds.julia;
    r = number "r_min" default_thresholds.r }

(** The reasons the gate fails; empty when it passes. *)
let violations thresholds report =
  let percent_of language =
    match List.find_opt (fun s -> s.language = language) report.shares with Some s -> s.percent | None -> 0.0
  in
  let floor language minimum =
    let actual = percent_of language in
    if actual +. 1e-9 < minimum then
      [ Printf.sprintf "%s is %.1f%% of the code, below its floor of %.1f%%" (name language) actual minimum ]
    else []
  in
  (if report.project_percent +. 1e-9 < thresholds.combined then
     [ Printf.sprintf "OCaml, Lean, Julia and R together are %.1f%% of the code, below %.1f%%" report.project_percent thresholds.combined ]
   else [])
  @ floor OCaml thresholds.ocaml @ floor Lean thresholds.lean @ floor Julia thresholds.julia @ floor R thresholds.r

let to_json report =
  Json.Obj
    [ ("total_bytes", Json.Int report.total_bytes); ("project_percent", Json.Float report.project_percent);
      ( "languages",
        Json.List
          (List.map
             (fun s ->
               Json.Obj
                 [ ("language", Json.String (name s.language)); ("bytes", Json.Int s.bytes); ("files", Json.Int s.files);
                   ("percent", Json.Float s.percent) ])
             report.shares) ) ]

let render report =
  let rows =
    List.map
      (fun s -> Printf.sprintf "%-12s %9d bytes %5d files %6.1f%%" (name s.language) s.bytes s.files s.percent)
      report.shares
  in
  String.concat "\n"
    (rows
     @ [ Printf.sprintf "%-12s %9d bytes" "total" report.total_bytes;
         Printf.sprintf "OCaml + Lean + Julia + R: %.1f%%" report.project_percent ])
  ^ "\n"

(** Measures a repository on disk. *)
let of_repository root =
  let exclusions = attribute_exclusions (Repo.read root ".gitattributes") in
  let entries =
    List.filter_map
      (fun rel -> match (Unix.stat Fs.(root / rel)).Unix.st_size with size -> Some (rel, size) | exception Unix.Unix_error _ -> None)
      (Repo.files root)
  in
  measure ~exclusions entries
