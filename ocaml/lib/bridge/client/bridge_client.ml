(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Bridge_types

type t = {
  worker : Proc.worker;
  mutable next_id : int;
  mutable live : bool;
  default_timeout : float;
  mutable greeting : handshake;
}

let ( let* ) = Result.bind
let is_open t = t.live
let handshake t = t.greeting

let close t =
  if t.live then begin
    t.live <- false;
    ignore (Proc.stop t.worker)
  end

let decode what f j = match f j with Ok v -> Ok v | Error e -> Error (Bridge_error.Protocol (what ^ ": " ^ e))

let call ?timeout t meth params =
  if not t.live then Error (Bridge_error.Process "the connection is closed")
  else begin
    let id = t.next_id in
    t.next_id <- id + 1;
    let timeout = Option.value timeout ~default:t.default_timeout in
    let request =
      Json.Obj [ ("jsonrpc", Json.String "2.0"); ("id", Json.Int id); ("method", Json.String meth); ("params", params) ]
    in
    match Proc.send_line t.worker (Json.to_string request) with
    | Error e -> close t; Error (Bridge_error.Process e)
    | Ok () ->
      (match Proc.read_line ~timeout t.worker with
       | Error e ->
         close t;
         if Text.contains e "timed out" then Error (Bridge_error.Timeout timeout) else Error (Bridge_error.Process e)
       | Ok line ->
         (match Json.parse line with
          | Error e -> close t; Error (Bridge_error.Protocol e)
          | Ok response ->
            if Json.member "id" response <> Some (Json.Int id) then begin
              close t;
              Error (Bridge_error.Protocol (Printf.sprintf "response does not carry request identifier %d" id))
            end
            else
              match Json.member "error" response, Json.member "result" response with
              | Some err, _ ->
                Error
                  (Bridge_error.Remote
                     { code = Json.int_or 0 "code" err;
                       tag = Option.value (Option.bind (Json.path [ "data"; "code" ] err) Json.string_opt) ~default:"MTH-BRG-0000";
                       message = Json.string_or "" "message" err })
              | None, Some result -> Ok result
              | None, None -> close t; Error (Bridge_error.Protocol "response has neither result nor error")))
  end

let start ?(env = []) ?cwd ?(timeout = 120.0) argv =
  match argv with
  | [] -> Error (Bridge_error.Not_found "empty command")
  | prog :: args ->
    if Proc.find_in_path prog = None then Error (Bridge_error.Not_found prog)
    else
      match Proc.spawn ~env:(Screen.scrub_env (Unix.environment ()) @ env) ~inherit_env:false ?cwd prog args with
      | Error e -> Error (Bridge_error.Process e)
      | Ok worker ->
        let t =
          { worker; next_id = 1; live = true; default_timeout = timeout;
            greeting = { bridge = ""; protocol = 0; lean = ""; methods = [] } }
        in
        (match Result.bind (call t "handshake" (Json.Obj [])) (decode "handshake" handshake_of_json) with
         | Error e -> close t; Error e
         | Ok greeting ->
           if greeting.protocol <> Maithic_schema.Version.bridge_protocol then begin
             close t;
             Error
               (Bridge_error.Incompatible
                  (Printf.sprintf "it speaks protocol %d, this build speaks protocol %d" greeting.protocol
                     Maithic_schema.Version.bridge_protocol))
           end
           else begin
             t.greeting <- greeting;
             Ok t
           end)

let names_member = function
  | None | Some [] -> []
  | Some names -> [ ("names", Json.List (List.map (fun n -> Json.String n) names)) ]

let run_result j =
  let* report =
    match Json.member "report" j with
    | Some r -> decode "report" report_of_json r
    | None -> Error (Bridge_error.Protocol "result has no report")
  in
  Ok (report, Option.bind (Json.member "env" j) Json.int_opt)

let load_header ?timeout t imports =
  let* j = call ?timeout t "loadHeader" (Json.Obj [ ("imports", Json.List (List.map (fun m -> Json.String m) imports)) ]) in
  match Json.get_int "env" j with
  | Ok env -> Ok (env, Json.bool_or false "cached" j)
  | Error e -> Error (Bridge_error.Protocol e)

let run_command ?timeout ?names ?(keep = false) ?(limits = no_limits) t ~env source =
  let params =
    Json.Obj
      ([ ("env", Json.Int env); ("source", Json.String source); ("keep", Json.Bool keep) ]
       @ names_member names @ limits_members limits)
  in
  Result.bind (call ?timeout t "runCommand" params) run_result

let check_file ?timeout ?names ?(keep = false) ?(limits = no_limits) t source =
  let params =
    Json.Obj ([ ("source", Json.String source); ("keep", Json.Bool keep) ] @ names_member names @ limits_members limits)
  in
  Result.bind (call ?timeout t "checkFile" params) run_result

let extract ?timeout ?file ?(module_name = "") ?(trace = false) ?(duplicates = false) ?(lint = false) ?(rules = [])
    ?(convention = []) ?(limits = no_limits) t source =
  let params =
    Json.Obj
      ([ ("source", Json.String source); ("module", Json.String module_name); ("trace", Json.Bool trace);
         ("duplicates", Json.Bool duplicates); ("lint", Json.Bool lint);
         ("rules", Json.List (List.map (fun r -> Json.String r) rules)) ]
       @ (if convention = [] then [] else [ ("convention", Json.Obj (List.map (fun (k, v) -> (k, Json.String v)) convention)) ])
       @ limits_members limits)
  in
  let* j = call ?timeout t "extract" params in
  let* extraction = decode "extract" (Extraction.of_json ?file) j in
  Ok { extraction with Extraction.module_name }

let try_candidates ?timeout ?names ?(stop_on_first = false) ?(limits = no_limits) t ~env candidates =
  let params =
    Json.Obj
      ([ ("env", Json.Int env);
         ("candidates", Json.List (List.map candidate_to_json candidates));
         ("stopOnFirst", Json.Bool stop_on_first) ]
       @ names_member names @ limits_members limits)
  in
  let* j = call ?timeout t "tryCandidates" params in
  match Json.get_list "results" j with
  | Error e -> Error (Bridge_error.Protocol e)
  | Ok items -> decode "results" (map_result candidate_report_of_json) items

let release_env ?timeout t env =
  let* j = call ?timeout t "releaseEnv" (Json.Obj [ ("env", Json.Int env) ]) in
  Ok (Json.bool_or false "released" j)

let start_proof ?timeout ?(limits = no_limits) t ~env text =
  let member = match text with `Statement s -> ("statement", Json.String s) | `Source s -> ("source", Json.String s) in
  let* j = call ?timeout t "startProof" (Json.Obj ([ ("env", Json.Int env); member ] @ limits_members limits)) in
  let* report =
    match Json.member "report" j with
    | Some r -> decode "report" report_of_json r
    | None -> Error (Bridge_error.Protocol "startProof: the result has no report")
  in
  let* states = decode "states" (map_result opened_proof_of_json) (Json.list_or_empty "states" j) in
  Ok (report, states)

let tactic_params ?goal ~limits state =
  [ ("state", Json.Int state) ] @ (match goal with Some g -> [ ("goal", Json.Int g) ] | None -> []) @ limits_members limits

let run_tactic ?timeout ?goal ?(limits = no_limits) t ~state tactic =
  let* j = call ?timeout t "runTactic" (Json.Obj (tactic_params ?goal ~limits state @ [ ("tactic", Json.String tactic) ])) in
  decode "outcome" tactic_outcome_of_json j

let try_tactics ?timeout ?goal ?(stop_on_solved = false) ?(limits = no_limits) t ~state tactics =
  let params =
    tactic_params ?goal ~limits state
    @ [ ("tactics", Json.List (List.map (fun tactic -> Json.String tactic) tactics)) ]
    @ if stop_on_solved then [ ("stopOnSolved", Json.Bool true) ] else []
  in
  let* j = call ?timeout t "tryTactics" (Json.Obj params) in
  decode "results" (map_result tactic_outcome_of_json) (Json.list_or_empty "results" j)

let get_state ?timeout t state =
  let* j = call ?timeout t "getState" (Json.Obj [ ("state", Json.Int state) ]) in
  decode "state" proof_state_of_json j

let release_state ?timeout t state =
  let* j = call ?timeout t "releaseState" (Json.Obj [ ("state", Json.Int state) ]) in
  Ok (Json.bool_or false "released" j)

let guard ?timeout t candidate =
  let* j = call ?timeout t "guard" (Json.Obj [ ("candidate", Json.String candidate) ]) in
  Ok (Json.bool_or false "admitted" j, Json.string_or "" "rejection" j)

let stats ?timeout t = call ?timeout t "stats" (Json.Obj [])

let self_test ?timeout t =
  let* j = call ?timeout t "selfTest" (Json.Obj []) in
  let checks = List.map (fun c -> (Json.string_or "" "name" c, Json.bool_or false "passed" c)) (Json.list_or_empty "checks" j) in
  Ok (Json.bool_or false "healthy" j, checks)

let shutdown t =
  if t.live then begin
    ignore (call ~timeout:5.0 t "shutdown" (Json.Obj []));
    close t
  end

let with_bridge ?env ?cwd ?timeout argv f =
  let* t = start ?env ?cwd ?timeout argv in
  match f t with
  | result -> shutdown t; result
  | exception e -> shutdown t; raise e
