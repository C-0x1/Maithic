(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The verifier: the Julia package MaithicNNV, run as a process. *)

open Maithic_base

let locate ?julia ?project ~cwd () = Julia_package.locate ~package:"MaithicNNV" ~hint:"set nnv.project" ?julia ?project ~cwd ()
let prepare = Julia_package.prepare

(** Runs the verifier on a network and a property, and reads its verdict. *)
let verify ?(timeout = 120.0) ?(max_corners = 1024) engine ~model ~property () =
  let args = Julia_package.arguments engine @ [ Julia_package.script engine "verify.jl"; model; property; string_of_int max_corners ] in
  match Proc.run ~env:Julia_package.offline ~timeout engine.Julia_package.julia args with
  | Error e -> Error e
  | Ok out ->
    if out.Proc.status <> 0 then
      Error (match Text.trim out.Proc.stderr with "" -> Printf.sprintf "the verifier ended with status %d" out.Proc.status | said -> List.hd (List.rev (String.split_on_char '\n' said)))
    else Result.map_error (fun e -> "the verifier printed something that is not JSON: " ^ e) (Json.parse out.Proc.stdout)
