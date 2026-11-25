(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Lean's side of the synthesis loop: compiling a drafted definition and
    running the examples on it.

    A draft reaches Lean only after {!Algolab_screen} let it pass. It is
    elaborated on a Bridge, and each example is then evaluated twice on the
    environment that holds it: once for the value, to show it, and once for
    the comparison with what was expected. A draft that makes Lean stop, by
    running too long or too deep, takes the Bridge with it; the next draft
    gets a new one. *)

open Maithic_base
open Maithic_bridge

type session = {
  start : unit -> (Bridge_client.t, string) result;  (** Starts a Bridge. *)
  imports : string list;
  limits : Bridge_types.limits option;
  mutable live : (Bridge_client.t * int) option;  (** The Bridge in use, and the environment drafts are compiled on. *)
}

let session ?limits ?(imports = []) start = { start; imports; limits; live = None }
let said (e : Bridge_error.t) = Bridge_error.to_string e
let first_line text = match String.split_on_char '\n' (Text.trim text) with line :: _ -> line | [] -> ""

let connect s =
  match s.live with
  | Some (bridge, env) when Bridge_client.is_open bridge -> Ok (bridge, env)
  | _ ->
    Result.bind (s.start ()) (fun bridge ->
        match Bridge_client.load_header bridge s.imports with
        | Error e ->
          Bridge_client.shutdown bridge;
          Error (said e)
        | Ok (env, _) ->
          s.live <- Some (bridge, env);
          Ok (bridge, env))

let close s =
  (match s.live with Some (bridge, _) -> Bridge_client.shutdown bridge | None -> ());
  s.live <- None

(** The two commands that run one example: the value, then the comparison. *)
let example_commands (e : Algolab_task.example) =
  Printf.sprintf "#eval (%s)\n#eval ((%s) == (%s))" e.Algolab_task.call e.Algolab_task.call e.Algolab_task.expect

(** Reads what Lean said to {!example_commands}. *)
let example_result (e : Algolab_task.example) (report : Bridge_types.report) =
  let at line severity =
    List.filter_map
      (fun (d : Bridge_types.diagnostic) -> if d.Bridge_types.line = line && d.Bridge_types.severity = severity then Some (Text.trim d.Bridge_types.text) else None)
      report.Bridge_types.diagnostics
  in
  let got = match at 1 "info" with text :: _ -> text | [] -> "" in
  match (at 2 "info", at 2 "error") with
  | answer :: _, [] -> { Algolab_synth.example = e; got; passed = answer = "true"; error = "" }
  | _, why :: _ -> { Algolab_synth.example = e; got; passed = false; error = first_line why }
  | [], [] -> { Algolab_synth.example = e; got; passed = false; error = (match at 1 "error" with why :: _ -> first_line why | [] -> "Lean gave no answer") }

let errors_of (report : Bridge_types.report) =
  List.filter_map
    (fun (d : Bridge_types.diagnostic) -> if d.Bridge_types.severity = "error" then Some (Printf.sprintf "line %d: %s" d.Bridge_types.line (Text.trim d.Bridge_types.text)) else None)
    report.Bridge_types.diagnostics

(** Compiles an implementation and runs the task's examples on it. *)
let verdict s (task : Algolab_task.t) implementation =
  Result.bind (connect s) (fun (bridge, env) ->
      match Bridge_client.run_command ~keep:true ?limits:s.limits bridge ~env implementation with
      | Error e -> if Bridge_client.is_open bridge then Error (said e) else Ok { Algolab_synth.errors = [ "Lean stopped while it compiled the definition: " ^ said e ]; examples = [] }
      | Ok (report, None) | Ok (({ Bridge_types.ok = false; _ } as report), _) ->
        Ok { Algolab_synth.errors = (match errors_of report with [] -> [ "Lean did not accept the definition" ] | errors -> errors); examples = [] }
      | Ok (_, Some held) ->
        let examples =
          List.map
            (fun (e : Algolab_task.example) ->
              if not (Bridge_client.is_open bridge) then { Algolab_synth.example = e; got = ""; passed = false; error = "not run: Lean had stopped" }
              else
                match Bridge_client.run_command ?limits:s.limits bridge ~env:held (example_commands e) with
                | Error err -> { Algolab_synth.example = e; got = ""; passed = false; error = first_line (said err) }
                | Ok (report, _) -> example_result e report)
            task.Algolab_task.examples
        in
        if Bridge_client.is_open bridge then ignore (Bridge_client.release_env bridge held);
        Ok { Algolab_synth.errors = []; examples })

(** The tools of the loop: Lean on a session, and a model when there is one. *)
let tools ?gateway ?(max_tokens = 2048) s (task : Algolab_task.t) =
  let no_model = "no model may be asked" in
  { Algolab_synth.draft = (fun () -> match gateway with Some g -> Algolab_draft.draft g ~max_tokens task | None -> Error no_model);
    repair =
      (fun ~implementation ~report ~earlier ->
        match gateway with Some g -> Algolab_draft.repair g ~max_tokens task ~implementation ~report ~earlier | None -> Error no_model);
    lean = verdict s task }
