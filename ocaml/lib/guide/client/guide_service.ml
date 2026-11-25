(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

let locate ?julia ?project ~cwd () = Julia_package.locate ~package:"MaithicGuide" ~hint:"set guide.project" ?julia ?project ~cwd ()
let command engine ~model = (engine.Julia_package.julia, Julia_package.arguments engine @ [ Julia_package.script engine "serve.jl"; model ])

let train_command engine ~dataset ~out options =
  (engine.Julia_package.julia, Julia_package.arguments engine @ [ Julia_package.script engine "train.jl"; dataset; out ] @ options)

let default_startup = 120.0
let default_timeout = 30.0

let start ?(startup = default_startup) ?(timeout = default_timeout) engine ~model =
  if not (Sys.file_exists model) then Error (Printf.sprintf "there is no model at %s" model)
  else
    let program, args = command engine ~model in
    match Proc.spawn ~env:Julia_package.offline program args with
    | Error e -> Error ("the guidance service could not be started: " ^ e)
    | Ok worker ->
      let first = ref true and stopped = ref false in
      let stop () =
        if not !stopped then begin
          stopped := true;
          ignore (Proc.stop worker)
        end
      in
      let exchange request =
        if !stopped then Error "the guidance service was stopped"
        else
          let wait = if !first then startup else timeout in
          first := false;
          let answer =
            match Proc.send_line worker (Json.to_string request) with
            | Error e -> Error ("the guidance service did not take the request: " ^ e)
            | Ok () ->
              (match Proc.read_line ~timeout:wait worker with
               | Error e -> Error ("the guidance service did not answer: " ^ e)
               | Ok line -> Result.map_error (fun e -> "the guidance service wrote something that is not JSON: " ^ e) (Json.parse line))
          in
          (* A service that has failed once is not asked again: its answers would be out of step. *)
          if Result.is_error answer then stop ();
          answer
      in
      Ok { Guide_client.exchange; close = stop }

let connect ?startup ?timeout engine ~model =
  let ( let* ) = Result.bind in
  let* () = if Sys.file_exists model then Ok () else Error (Printf.sprintf "there is no model at %s" model) in
  let* () = Julia_package.prepare engine in
  let* transport = start ?startup ?timeout engine ~model in
  Guide_client.connect transport
