(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Finding the Bridge executable and the project it should run in.

    Order of the search for the executable: the configured command, then a
    build inside the Maithic source tree at or above the current directory,
    then [maithic-bridge] on [PATH]. When a Lake project is configured, the
    Bridge is started through [lake env] in that project, so that the
    project's own modules can be imported. *)

open Maithic_base

let executable = "maithic-bridge"

(* A build of the Bridge inside a source tree at or above [dir]. *)
let in_source_tree dir =
  let rec up dir depth =
    let candidate = Fs.(dir / "lean" / ".lake" / "build" / "bin" / executable) in
    if Fs.exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir || depth > 32 then None else up parent (depth + 1)
  in
  up dir 0

(** The Lake project root at or above a directory: the nearest directory
    holding a [lakefile.toml] or a [lakefile.lean]. *)
let project_root dir =
  let rec up dir depth =
    if Fs.exists Fs.(dir / "lakefile.toml") || Fs.exists Fs.(dir / "lakefile.lean") then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir || depth > 32 then None else up parent (depth + 1)
  in
  up dir 0

(** The Lean version a project pins in its [lean-toolchain] file. *)
let toolchain_of_project dir =
  match Fs.read Fs.(dir / "lean-toolchain") with
  | Error _ -> None
  | Ok text ->
    let text = String.trim text in
    (match String.rindex_opt text ':' with
     | Some i -> Some (String.sub text (i + 1) (String.length text - i - 1))
     | None -> Some text)
    |> Option.map (fun v -> if Text.starts_with ~prefix:"v" v then String.sub v 1 (String.length v - 1) else v)

type located = { argv : string list; cwd : string option; path : string }

(** Where the Bridge is and how to start it. *)
let locate ?(command = "") ?(project = "") ?(lake = "lake") ~cwd () =
  let path =
    if command <> "" then (if Proc.find_in_path command <> None then Ok command else Error (Bridge_error.Not_found command))
    else
      match in_source_tree cwd with
      | Some path -> Ok path
      | None ->
        (match Proc.find_in_path executable with
         | Some path -> Ok path
         | None -> Error (Bridge_error.Not_found (executable ^ " is neither built in this source tree nor on PATH")))
  in
  match path with
  | Error _ as e -> e
  | Ok path ->
    let absolute = if Filename.is_relative path && String.contains path '/' then Filename.concat cwd path else path in
    if project = "" then Ok { argv = [ absolute ]; cwd = None; path = absolute }
    else if not (Fs.is_dir project) then Error (Bridge_error.Process ("the Lean project directory does not exist: " ^ project))
    else if Proc.find_in_path lake = None then Error (Bridge_error.Not_found lake)
    else Ok { argv = [ lake; "env"; absolute ]; cwd = Some project; path = absolute }
