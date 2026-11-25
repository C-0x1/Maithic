(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type t = { julia : string; project : string }

let rec find_up dir relative =
  let candidate = Filename.concat dir relative in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_up parent relative

let locate ~package ?(hint = "") ?(julia = "julia") ?(project = "") ~cwd () =
  match Proc.find_in_path julia with
  | None -> Error (Printf.sprintf "the Julia executable \"%s\" is not on the path; Julia 1.11 or later is needed" julia)
  | Some _ ->
    if project <> "" then
      if Sys.file_exists (Filename.concat project "Project.toml") then Ok { julia; project }
      else Error (Printf.sprintf "%s is not a Julia package directory" project)
    else
      (match find_up cwd (Filename.concat "julia" (Filename.concat package "Project.toml")) with
       | Some file -> Ok { julia; project = Filename.dirname file }
       | None ->
         Error
           (Printf.sprintf "the package %s was not found in the source tree around the current directory%s" package
              (if hint = "" then "" else "; " ^ hint)))

let script t name = Filename.concat (Filename.concat t.project "bin") name
let arguments t = [ "--startup-file=no"; "--project=" ^ t.project ]
let offline = [ ("JULIA_PKG_OFFLINE", "true") ]
let setup_command t = (t.julia, arguments t @ [ "-e"; "using Pkg; Pkg.resolve(io = devnull); Pkg.instantiate(io = devnull)" ])

let prepare ?(timeout = 600.0) t =
  if Sys.file_exists (Filename.concat t.project "Manifest.toml") then Ok ()
  else begin
    let program, args = setup_command t in
    let failed why =
      Error
        (Printf.sprintf "the Julia package in %s could not be set up: %s; `make build-julia` in the source tree does the same by hand" t.project
           why)
    in
    match Proc.run ~env:offline ~timeout program args with
    | Ok out when out.Proc.status = 0 -> Ok ()
    | Ok out -> failed (Printf.sprintf "Julia stopped with status %d: %s" out.Proc.status (Text.truncate 400 (String.trim out.Proc.stderr)))
    | Error e -> failed e
  end
