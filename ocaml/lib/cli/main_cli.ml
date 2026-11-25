(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The entry point of the command: selects a command, parses its options
    and runs it. *)

open Maithic_schema
open Cli_args

(* A mistake in the arguments is an error like any other: a line on standard
   error, or a document on standard output when the arguments ask for JSON. *)
let usage argv ?hint ~text message =
  if List.mem "--json" argv then Term.fault ~json:true (Fault.make ?hint ~area:"CLI" ~number:1 ~exit:Exit_code.Usage message)
  else begin
    prerr_endline text;
    Option.iter prerr_endline hint;
    Exit_code.to_int Exit_code.Usage
  end

let run argv =
  let commands = Commands_admin.all in
  match argv with
  | [] | [ "help" ] | [ "--help" ] | [ "-h" ] ->
    print_string (overview Commands.program ~tagline:Commands.tagline commands);
    0
  | [ "--version" ] -> Commands.version_cmd.run { positional = []; values = []; switches = [] }
  | _ ->
    (match select commands argv with
     | None ->
       let word = List.hd argv in
       let known = List.sort_uniq compare (List.map (fun c -> List.hd c.path) commands) in
       let message =
         Printf.sprintf "unknown command '%s'%s" word
           (match Maithic_config.Config.nearest word known with Some near -> Printf.sprintf " (did you mean '%s'?)" near | None -> "")
       in
       usage argv ~hint:(Printf.sprintf "Run `%s --help` for the list of commands." Commands.program)
         ~text:(Printf.sprintf "%s: %s" Commands.program message) message
     | Some (cmd, rest) ->
       (match parse (cmd.options @ global_options) rest with
        | Error message ->
          let name = String.concat " " cmd.path in
          usage argv ~text:(Printf.sprintf "%s %s: %s" Commands.program name message) (Printf.sprintf "%s: %s" name message)
        | Ok parsed ->
          if has parsed "help" then begin
            print_string (command_help Commands.program cmd);
            0
          end
          else cmd.run parsed))
