(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The wire types of the Bridge protocol, mirroring
    [lean/MaithicBridge/Serialize/Types.lean], with their JSON codecs. *)

open Maithic_base

type diagnostic = {
  severity : string;  (** ["error"], ["warning"] or ["info"] *)
  line : int;  (** one-based *)
  column : int;  (** zero-based, in code points *)
  end_line : int option;
  end_column : int option;
  text : string;
}

type decl = {
  name : string;
  found : bool;
  kind : string;
  type_ : string;
  axioms : string list;
  uses_sorry : bool;
}

type report = {
  ok : bool;
  errors : int;
  warnings : int;
  diagnostics : diagnostic list;
  decls : decl list;
  elapsed_ms : int;
}

type candidate = { source : string; untrusted : string }

type candidate_report = { index : int; admitted : bool; rejection : string; report : report option }

type hypothesis = { hyp_name : string; hyp_type : string; hyp_value : string option  (** present for a [let] *) }

type goal = {
  goal_id : string;  (** the same for as long as the goal stays open *)
  tag : string;  (** the case name, empty when the goal has none *)
  hypotheses : hypothesis list;
  target : string;
  pretty : string;  (** the whole goal as Lean prints it *)
}

(** A proof state opened at a [sorry]: its handle, where the [sorry] stands, and its goals. *)
type opened_proof = { state : int; at_line : int; at_column : int; goals : goal list }

(** Where a tactic was stopped. *)
type tactic_failure_kind =
  | Refused_by_guard  (** never parsed *)
  | Parse_failure
  | Tactic_failure  (** Lean raised an error or ran out of steps *)

type tactic_outcome =
  | Stepped of { state : int; goals : goal list; solved : bool; diagnostics : diagnostic list }
  | Stopped of { kind : tactic_failure_kind; error : string; diagnostics : diagnostic list }

type proof_state = { state : int; env : int; parent : int option; depth : int; goals : goal list; solved : bool }

type limits = { max_heartbeats : int; max_rec_depth : int }

let no_limits = { max_heartbeats = 0; max_rec_depth = 0 }

type handshake = { bridge : string; protocol : int; lean : string; methods : string list }

let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x :: rest ->
    let* y = f x in
    let* ys = map_result f rest in
    Ok (y :: ys)

let diagnostic_of_json j =
  let* severity = Json.get_string "severity" j in
  let* line = Json.get_int "line" j in
  let* column = Json.get_int "column" j in
  let* text = Json.get_string "text" j in
  Ok
    { severity; line; column; text;
      end_line = Option.bind (Json.member "endLine" j) Json.int_opt;
      end_column = Option.bind (Json.member "endColumn" j) Json.int_opt }

let diagnostic_to_json d =
  Json.Obj
    ([ ("severity", Json.String d.severity); ("line", Json.Int d.line); ("column", Json.Int d.column) ]
     @ (match d.end_line with Some l -> [ ("endLine", Json.Int l) ] | None -> [])
     @ (match d.end_column with Some c -> [ ("endColumn", Json.Int c) ] | None -> [])
     @ [ ("text", Json.String d.text) ])

let decl_of_json j =
  let* name = Json.get_string "name" j in
  let* found = Json.get_bool "found" j in
  Ok
    { name; found;
      kind = Json.string_or "" "kind" j;
      type_ = Json.string_or "" "type" j;
      axioms = Json.strings (Json.List (Json.list_or_empty "axioms" j));
      uses_sorry = Json.bool_or false "usesSorry" j }

let decl_to_json d =
  Json.Obj
    [ ("name", Json.String d.name); ("found", Json.Bool d.found); ("kind", Json.String d.kind);
      ("type", Json.String d.type_);
      ("axioms", Json.List (List.map (fun a -> Json.String a) d.axioms));
      ("usesSorry", Json.Bool d.uses_sorry) ]

let report_of_json j =
  let* ok = Json.get_bool "ok" j in
  let* errors = Json.get_int "errors" j in
  let* warnings = Json.get_int "warnings" j in
  let* diagnostics = Result.bind (Json.get_list "diagnostics" j) (map_result diagnostic_of_json) in
  let* decls = Result.bind (Json.get_list "decls" j) (map_result decl_of_json) in
  Ok { ok; errors; warnings; diagnostics; decls; elapsed_ms = Json.int_or 0 "elapsedMs" j }

let report_to_json r =
  Json.Obj
    [ ("ok", Json.Bool r.ok); ("errors", Json.Int r.errors); ("warnings", Json.Int r.warnings);
      ("diagnostics", Json.List (List.map diagnostic_to_json r.diagnostics));
      ("decls", Json.List (List.map decl_to_json r.decls));
      ("elapsedMs", Json.Int r.elapsed_ms) ]

let candidate_to_json c = Json.Obj [ ("source", Json.String c.source); ("untrusted", Json.String c.untrusted) ]

let candidate_report_of_json j =
  let* index = Json.get_int "index" j in
  let* admitted = Json.get_bool "admitted" j in
  let* report =
    match Json.member "report" j with
    | None | Some Json.Null -> Ok None
    | Some r -> Result.map Option.some (report_of_json r)
  in
  Ok { index; admitted; rejection = Json.string_or "" "rejection" j; report }

let candidate_report_to_json c =
  Json.Obj
    ([ ("index", Json.Int c.index); ("admitted", Json.Bool c.admitted); ("rejection", Json.String c.rejection) ]
     @ match c.report with Some r -> [ ("report", report_to_json r) ] | None -> [])

let hypothesis_of_json j =
  let* hyp_name = Json.get_string "name" j in
  let* hyp_type = Json.get_string "type" j in
  Ok { hyp_name; hyp_type; hyp_value = Option.bind (Json.member "value" j) Json.string_opt }

let hypothesis_to_json h =
  Json.Obj
    ([ ("name", Json.String h.hyp_name); ("type", Json.String h.hyp_type) ]
     @ match h.hyp_value with Some v -> [ ("value", Json.String v) ] | None -> [])

let goal_of_json j =
  let* goal_id = Json.get_string "id" j in
  let* target = Json.get_string "target" j in
  let* hypotheses = map_result hypothesis_of_json (Json.list_or_empty "hypotheses" j) in
  Ok { goal_id; tag = Json.string_or "" "tag" j; hypotheses; target; pretty = Json.string_or "" "pretty" j }

let goal_to_json g =
  Json.Obj
    [ ("id", Json.String g.goal_id); ("tag", Json.String g.tag);
      ("hypotheses", Json.List (List.map hypothesis_to_json g.hypotheses));
      ("target", Json.String g.target); ("pretty", Json.String g.pretty) ]

let goals_of_json j = map_result goal_of_json (Json.list_or_empty "goals" j)

let diagnostics_of_json j = map_result diagnostic_of_json (Json.list_or_empty "diagnostics" j)

let opened_proof_of_json j : (opened_proof, string) result =
  let* state = Json.get_int "state" j in
  let* goals = goals_of_json j in
  Ok { state; at_line = Json.int_or 0 "line" j; at_column = Json.int_or 0 "column" j; goals }

let tactic_failure_kind_of_string = function
  | "guard" -> Ok Refused_by_guard
  | "parse" -> Ok Parse_failure
  | "tactic" -> Ok Tactic_failure
  | other -> Error (Printf.sprintf "unknown kind of tactic failure '%s'" other)

let tactic_failure_kind_to_string = function
  | Refused_by_guard -> "guard"
  | Parse_failure -> "parse"
  | Tactic_failure -> "tactic"

let tactic_outcome_of_json j =
  let* ok = Json.get_bool "ok" j in
  let* diagnostics = diagnostics_of_json j in
  if ok then
    let* state = Json.get_int "state" j in
    let* goals = goals_of_json j in
    Ok (Stepped { state; goals; solved = Json.bool_or (goals = []) "solved" j; diagnostics })
  else
    let* kind = Result.bind (Json.get_string "kind" j) tactic_failure_kind_of_string in
    Ok (Stopped { kind; error = Json.string_or "" "error" j; diagnostics })

let proof_state_of_json j : (proof_state, string) result =
  let* state = Json.get_int "state" j in
  let* env = Json.get_int "env" j in
  let* goals = goals_of_json j in
  Ok
    { state; env; goals;
      parent = Option.bind (Json.member "parent" j) Json.int_opt;
      depth = Json.int_or 0 "depth" j;
      solved = Json.bool_or (goals = []) "solved" j }

(** A goal the way a person reads it: the tag, the hypotheses and the target. *)
let goal_text g = if g.pretty <> "" then g.pretty else Printf.sprintf "⊢ %s" g.target

let limits_members l =
  (if l.max_heartbeats > 0 then [ ("maxHeartbeats", Json.Int l.max_heartbeats) ] else [])
  @ if l.max_rec_depth > 0 then [ ("maxRecDepth", Json.Int l.max_rec_depth) ] else []

let handshake_of_json j =
  let* bridge = Json.get_string "bridge" j in
  let* protocol = Json.get_int "protocol" j in
  let* lean = Json.get_string "lean" j in
  Ok { bridge; protocol; lean; methods = Json.strings (Json.List (Json.list_or_empty "methods" j)) }

(** The error diagnostics of a report, one per line, for people and prompts. *)
let error_text report =
  report.diagnostics
  |> List.filter (fun d -> d.severity = "error")
  |> List.map (fun d -> Printf.sprintf "%d:%d: %s" d.line d.column d.text)
  |> String.concat "\n"
