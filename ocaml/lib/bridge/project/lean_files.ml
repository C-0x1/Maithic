(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The Lean files of a project, and the module a file stands for. *)

open Maithic_base

(* Directories that hold build products or other people's code. *)
let skipped rel = List.exists (fun part -> List.mem part [ ".lake"; "_build"; ".git"; "lake-packages"; "node_modules" ]) (String.split_on_char '/' rel)

(** The Lean files among [paths]: a file is taken as it is, a directory is
    searched for [.lean] files, without build directories. Sorted, each
    once. A path that does not exist is an error. *)
let expand paths =
  let rec go acc = function
    | [] -> Ok (List.sort_uniq compare acc)
    | path :: rest ->
      if Fs.is_dir path then
        go (List.map (fun rel -> Fs.(path / rel)) (List.filter (fun rel -> Fs.extension rel = ".lean") (Fs.walk ~skip:skipped path)) @ acc) rest
      else if Fs.exists path then go (path :: acc) rest
      else Error (Printf.sprintf "%s does not exist" path)
  in
  go [] paths

(** The module name of a file: its path below the project directory when it
    lies there, below the current directory otherwise, with dots for
    slashes and without the extension. *)
let module_of ?project path =
  let strip prefix s =
    let prefix = if Text.ends_with ~suffix:"/" prefix then prefix else prefix ^ "/" in
    if Text.starts_with ~prefix s then Some (String.sub s (String.length prefix) (String.length s - String.length prefix)) else None
  in
  let path = if Text.starts_with ~prefix:"./" path then String.sub path 2 (String.length path - 2) else path in
  let relative = match project with Some dir when dir <> "" -> Option.value ~default:path (strip dir path) | _ -> path in
  let without = if Fs.extension relative = ".lean" then String.sub relative 0 (String.length relative - 5) else relative in
  String.concat "." (List.filter (fun part -> part <> "" && part <> ".") (String.split_on_char '/' without))
