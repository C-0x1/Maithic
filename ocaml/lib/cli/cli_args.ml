(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** A small declarative command-line library.

    A command is a value: its path, a summary, its options and a function.
    Parsing, help text and error messages are derived from the declarations,
    so every command behaves the same way. *)

open Maithic_base

type option_spec = {
  long : string;  (** without the leading dashes *)
  metavar : string;  (** the name of the value; empty for a switch *)
  doc : string;
}

let switch long doc = { long; metavar = ""; doc }
let value long metavar doc = { long; metavar; doc }

type parsed = { positional : string list; values : (string * string) list; switches : string list }

let has parsed name = List.mem name parsed.switches
let get parsed name = List.assoc_opt name parsed.values
let get_all parsed name = List.filter_map (fun (k, v) -> if k = name then Some v else None) parsed.values

(** Options accepted by every command. *)
let global_options =
  [ value "config" "FILE" "Read this configuration file instead of the discovered ones.";
    value "provider" "NAME" "Model provider: anthropic, openai, xai, local or mock.";
    value "model" "ID" "Model identifier; overrides the tier.";
    value "tier" "TIER" "Quality tier: fast, balanced or best.";
    value "mode" "MODE" "Gateway mode: live, record, replay, mock or off.";
    switch "json" "Print machine-readable JSON instead of text.";
    switch "quiet" "Print only results and errors.";
    switch "verbose" "Print progress details.";
    switch "no-color" "Do not colour the output.";
    switch "help" "Show this help." ]

(** Parses arguments against option declarations. [--name=value] and
    [--name value] are both accepted; [--] ends the options. *)
let parse specs argv =
  let find name = List.find_opt (fun spec -> spec.long = name) specs in
  let rec go args acc =
    match args with
    | [] -> Ok { acc with positional = List.rev acc.positional; values = List.rev acc.values }
    | "--" :: rest -> Ok { acc with positional = List.rev_append acc.positional rest; values = List.rev acc.values }
    | arg :: rest when Text.starts_with ~prefix:"--" arg ->
      let body = String.sub arg 2 (String.length arg - 2) in
      let name, inline =
        match String.index_opt body '=' with
        | Some i -> (String.sub body 0 i, Some (String.sub body (i + 1) (String.length body - i - 1)))
        | None -> (body, None)
      in
      (match find name, inline, rest with
       | None, _, _ ->
         let hint =
           match Maithic_config.Config.nearest name (List.map (fun s -> s.long) specs) with
           | Some near -> Printf.sprintf " (did you mean --%s?)" near
           | None -> ""
         in
         Error (Printf.sprintf "unknown option --%s%s" name hint)
       | Some spec, None, _ when spec.metavar = "" -> go rest { acc with switches = name :: acc.switches }
       | Some spec, Some _, _ when spec.metavar = "" -> Error (Printf.sprintf "option --%s takes no value" name)
       | Some _, Some v, _ -> go rest { acc with values = (name, v) :: acc.values }
       | Some _, None, v :: rest' -> go rest' { acc with values = (name, v) :: acc.values }
       | Some spec, None, [] -> Error (Printf.sprintf "option --%s needs a value (%s)" name spec.metavar))
    | arg :: rest -> go rest { acc with positional = arg :: acc.positional }
  in
  go argv { positional = []; values = []; switches = [] }

type command = {
  path : string list;  (** for example [["config"; "show"]] *)
  summary : string;
  synopsis : string;  (** the positional arguments, for the usage line *)
  details : string;  (** a paragraph for the help page; may be empty *)
  options : option_spec list;  (** besides the global ones *)
  run : parsed -> int;  (** returns the exit code *)
}

let option_line spec =
  let left = if spec.metavar = "" then "--" ^ spec.long else Printf.sprintf "--%s %s" spec.long spec.metavar in
  Printf.sprintf "  %s %s" (Text.pad_right 22 left) spec.doc

(** The help page of one command. *)
let command_help program cmd =
  String.concat "\n"
    ([ Printf.sprintf "%s %s - %s" program (String.concat " " cmd.path) cmd.summary; "";
       Printf.sprintf "Usage: %s %s%s [options]" program (String.concat " " cmd.path)
         (if cmd.synopsis = "" then "" else " " ^ cmd.synopsis) ]
     @ (if cmd.details = "" then [] else [ ""; cmd.details ])
     @ (if cmd.options = [] then [] else ("" :: "Options:" :: List.map option_line cmd.options))
     @ ("" :: "Global options:" :: List.map option_line global_options))
  ^ "\n"

(** The overview of every command. *)
let overview program ~tagline commands =
  String.concat "\n"
    ([ Printf.sprintf "%s - %s" program tagline; ""; Printf.sprintf "Usage: %s COMMAND [arguments] [options]" program; ""; "Commands:" ]
     @ List.map (fun cmd -> Printf.sprintf "  %s %s" (Text.pad_right 18 (String.concat " " cmd.path)) cmd.summary) commands
     @ [ ""; Printf.sprintf "Run `%s COMMAND --help` for the options of a command." program ])
  ^ "\n"

(** The command whose path is the longest prefix of the arguments, and the
    arguments that remain. *)
let select commands argv =
  let matches cmd =
    let n = List.length cmd.path in
    List.length argv >= n && List.filteri (fun i _ -> i < n) argv = cmd.path
  in
  let candidates = List.filter matches commands in
  match List.sort (fun a b -> compare (List.length b.path) (List.length a.path)) candidates with
  | cmd :: _ -> Some (cmd, List.filteri (fun i _ -> i >= List.length cmd.path) argv)
  | [] -> None
