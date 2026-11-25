(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_ledger

type settings = { rounds : int; candidates : int }

let default_settings = { rounds = 3; candidates = 3 }

type model = purpose:string -> schema:string * Json.t -> (string * string) list -> (string * int * int, string) result

type turn_result = { turn : Council_schedule.turn; status : string; summary : string; posted : int list; model_calls : int; lean_calls : int; seconds : float }
type ending = Settled | Rounds_over | Out_of_budget of string | Stopped of string

type t = {
  problem : Council_problem.t;
  settings : settings;
  policy : Trust_policy.t;
  board : Council_board.t;
  transcript : Council_transcript.t;
  statement : int;
  claim : int;
  turns : turn_result list;
  ending : ending;
  answer : Council_synthesis.t;
  tools : Council_tools.t;
  with_model : bool;
}

let schema = "maithic.council.session/1"
let coordinator = "coordinator"
let status t = Option.value (Council_board.status t.board t.claim) ~default:Council_board.Conjectured

let run ?(settings = default_settings) ?(clock = Unix.gettimeofday) ?model ~tools ~(policy : Trust_policy.t) (problem : Council_problem.t) =
  let budget = Council_tools.budget tools in
  let board = Council_board.create ~clock () and transcript = Council_transcript.create ~clock () in
  (* Intake: the statement and the claim go on the board, and everyone is told what the problem is. *)
  let statement = Council_board.post board ~author:coordinator Council_board.Statement ~title:problem.Council_problem.theorem problem.Council_problem.statement in
  let claim = Council_board.post board ~author:coordinator Council_board.Claim ~title:"the statement holds" ~refers:[ statement.Council_board.id ] problem.Council_problem.statement in
  let asked =
    if problem.Council_problem.goal = "" then []
    else [ (Council_board.post board ~author:coordinator Council_board.Question ~title:"what is asked" ~refers:[ statement.Council_board.id ] problem.Council_problem.goal).Council_board.id ]
  in
  ignore
    (Council_transcript.say transcript ~sender:coordinator ~recipients:Council_roles.names Council_message.Inform
       ~subject:(Printf.sprintf "the problem: %s" problem.Council_problem.theorem)
       ~refers:([ statement.Council_board.id; claim.Council_board.id ] @ asked)
       (problem.Council_problem.statement ^ if problem.Council_problem.goal = "" then "" else "\n\n" ^ problem.Council_problem.goal));
  (* A model as the roles ask it: refused when the budget is used up, and charged for every question. *)
  let ask =
    Option.map
      (fun (model : model) : Council_role.ask ->
        fun ~purpose ~schema bindings ->
         match Council_budget.blocked budget Council_budget.Model with
         | Some why -> Error ("the budget of the session is used up: " ^ why)
         | None ->
           (match model ~purpose ~schema bindings with
            | Ok (text, input_tokens, output_tokens) ->
              Council_budget.charge_model budget ~input_tokens ~output_tokens;
              Ok text
            | Error why ->
              Council_budget.charge_model budget ~input_tokens:0 ~output_tokens:0;
              Error why))
      model
  in
  let belt = { Council_roles.ask; tools; policy; candidates = settings.candidates } in
  let settled () = match Council_board.status board claim.Council_board.id with Some (Council_board.Verified | Council_board.Refuted) -> true | _ -> false in
  (* Without a model there is nobody to propose a proof: the examination is the session. *)
  let turns = Council_schedule.turns ~rounds:(if model = None then 0 else settings.rounds) in
  let rec take results = function
    | [] -> (List.rev results, if settled () then Settled else Rounds_over)
    | (turn : Council_schedule.turn) :: rest ->
      if settled () then (List.rev results, Settled)
      else
        (* When Lean may no longer be asked, nothing more can be established. When a model may no longer be
           asked, the proofs that are on the board can still be given to Lean. *)
        let asks_model = match Council_roles.find turn.Council_schedule.role with Ok role -> role.Council_role.uses_model | Error _ -> false in
        let blocked =
          match Council_budget.blocked budget Council_budget.Lean with
          | Some why -> Some why
          | None -> if asks_model then Council_budget.blocked budget Council_budget.Model else None
        in
        (match blocked with
         | Some why -> (List.rev results, Out_of_budget why)
         | None ->
           Council_board.set_round board turn.Council_schedule.round;
           Council_transcript.set_round transcript turn.Council_schedule.round;
           ignore
             (Council_transcript.say transcript ~sender:coordinator ~recipients:[ turn.Council_schedule.role ] Council_message.Request
                ~subject:(Council_plan.task_name turn.Council_schedule.task)
                (Council_plan.describe turn.Council_schedule.task));
           let before = Council_budget.spent budget and since = clock () in
           let work =
             match Council_roles.instantiate belt turn.Council_schedule.role with
             | Error why -> { Council_role.status = Council_role.Failed why; summary = why; posted = [] }
             | Ok act ->
               act
                 { Council_role.problem; board; transcript; statement = statement.Council_board.id; claim = claim.Council_board.id; round = turn.Council_schedule.round;
                   task = turn.Council_schedule.task }
           in
           let after = Council_budget.spent budget in
           (* Collection: what the member did, what it posted and what it cost. *)
           let result =
             { turn; status = Council_role.status_name work.Council_role.status; summary = work.Council_role.summary; posted = work.Council_role.posted;
               model_calls = after.Council_budget.model_calls - before.Council_budget.model_calls;
               lean_calls = after.Council_budget.lean_calls - before.Council_budget.lean_calls; seconds = Float.max 0.0 (clock () -. since) }
           in
           (* A statement that does not elaborate, and a prover that cannot be asked, end the session:
              nothing that could follow would have anything to work on. *)
           (match (work.Council_role.status, turn.Council_schedule.task) with
            | Council_role.Failed why, (Council_plan.Examine | Council_plan.Prove | Council_plan.Verify) when not (settled ()) ->
              (List.rev (result :: results), match Council_budget.exhausted budget with Some limit -> Out_of_budget limit | None -> Stopped why)
            | _ -> take (result :: results) rest))
  in
  let turns, ending = take [] turns in
  let answer = Council_synthesis.synthesize problem board ~claim:claim.Council_board.id in
  ignore
    (Council_transcript.say transcript ~sender:coordinator ~recipients:[ "all" ] Council_message.Inform
       ~subject:("the answer: " ^ Council_board.status_name answer.Council_synthesis.status)
       ~refers:(List.map (fun (i : Council_board.item) -> i.Council_board.id) answer.Council_synthesis.components)
       answer.Council_synthesis.text);
  { problem; settings; policy; board; transcript; statement = statement.Council_board.id; claim = claim.Council_board.id; turns; ending; answer; tools;
    with_model = model <> None }

let ending_json = function
  | Settled -> [ ("reason", Json.String "settled") ]
  | Rounds_over -> [ ("reason", Json.String "rounds") ]
  | Out_of_budget why -> [ ("reason", Json.String "budget"); ("detail", Json.String why) ]
  | Stopped why -> [ ("reason", Json.String "stopped"); ("detail", Json.String why) ]

let to_json t =
  let rounded x = Float.round (x *. 1000.0) /. 1000.0 in
  let rounds = if t.with_model then t.settings.rounds else 0 in
  Json.Obj
    [ ("schema", Json.String schema); ("problem", Council_problem.to_json t.problem);
      ( "settings",
        Json.Obj
          [ ("rounds", Json.Int t.settings.rounds); ("candidates", Json.Int t.settings.candidates); ("policy", Json.String t.policy.Trust_policy.name);
            ("model", Json.Bool t.with_model);
            ( "roles",
              Json.List
                (List.map
                   (fun (r : Council_role.t) ->
                     Json.Obj
                       [ ("name", Json.String r.Council_role.name); ("purpose", Json.String r.Council_role.purpose);
                         ("tasks", Json.List (List.map (fun task -> Json.String (Council_plan.task_name task)) r.Council_role.tasks));
                         ("uses_model", Json.Bool r.Council_role.uses_model); ("tools", Json.List (List.map (fun tool -> Json.String tool) r.Council_role.tools)) ])
                   Council_roles.all) );
            ("schedule", Json.String (Council_schedule.describe ~rounds)) ] );
      ("status", Json.String (Council_board.status_name (status t))); ("ending", Json.Obj (ending_json t.ending)); ("answer", Council_synthesis.to_json t.answer);
      ( "turns",
        Json.List
          (List.map
             (fun r ->
               Json.Obj
                 [ ("number", Json.Int r.turn.Council_schedule.number); ("round", Json.Int r.turn.Council_schedule.round);
                   ("task", Json.String (Council_plan.task_name r.turn.Council_schedule.task)); ("role", Json.String r.turn.Council_schedule.role);
                   ("status", Json.String r.status); ("summary", Json.String r.summary); ("posted", Json.List (List.map (fun i -> Json.Int i) r.posted));
                   ("model_calls", Json.Int r.model_calls); ("lean_calls", Json.Int r.lean_calls); ("seconds", Json.Float (rounded r.seconds)) ])
             t.turns) );
      ("board", Council_board.to_json t.board); ("messages", Council_transcript.to_json t.transcript); ("budget", Council_budget.to_json (Council_tools.budget t.tools));
      ("tools", Json.Obj (List.map (fun (name, n) -> (name, Json.Int n)) (Council_tools.uses t.tools))) ]

let ending_text t =
  match t.ending with
  | Settled -> ""
  | Rounds_over -> if t.with_model then Printf.sprintf "the %d round%s are over" t.settings.rounds (if t.settings.rounds = 1 then "" else "s") else "no model was asked, so no round followed the examination"
  | Out_of_budget why -> "the budget ran out: " ^ why
  | Stopped why -> Council_role.first_line why

let summary t =
  let spent = Council_budget.spent (Council_tools.budget t.tools) in
  let cost = Printf.sprintf "%d model call%s, %d call%s to Lean" spent.Council_budget.model_calls (if spent.Council_budget.model_calls = 1 then "" else "s") spent.Council_budget.lean_calls
      (if spent.Council_budget.lean_calls = 1 then "" else "s") in
  match (status t, t.answer.Council_synthesis.proof) with
  | Council_board.Verified, Some p ->
    Printf.sprintf "verified: proof %d, by the %s%s (%s)" p.Council_board.id p.Council_board.author
      (if p.Council_board.round = 0 then " in the examination" else Printf.sprintf " in round %d" p.Council_board.round)
      cost
  | Council_board.Refuted, _ -> Printf.sprintf "refuted: %s (%s)" (match t.answer.Council_synthesis.counterexample with Some c -> c.Council_board.title | None -> "Lean accepted a refutation") cost
  | status, _ -> Printf.sprintf "not proved: the claim is %s; %s (%s)" (Council_board.status_name status) (ending_text t) cost

let to_markdown t =
  let title (id : int) = Option.map (fun (i : Council_board.item) -> Printf.sprintf "%s (%s)" i.Council_board.title (Council_board.kind_name i.Council_board.kind)) (Council_board.find t.board id) in
  let body = Council_transcript.to_markdown ~title:(Printf.sprintf "Council session on `%s`" t.problem.Council_problem.theorem) ~items:title t.transcript in
  let answer =
    [ ""; "## Answer"; ""; Printf.sprintf "**%s.** %s" (String.capitalize_ascii (Council_board.status_name (status t))) t.answer.Council_synthesis.text ]
    @ (match t.answer.Council_synthesis.lean with Some lean -> [ ""; "```lean"; String.trim lean; "```" ] | None -> [])
    @ (match t.answer.Council_synthesis.set_aside with
       | [] -> []
       | aside ->
         [ ""; "Said in the session and not part of the answer: " ^ String.concat ", " (List.map (fun (kind, n) -> Printf.sprintf "%d %s%s" n kind (if n = 1 then "" else "s")) aside) ^ "." ])
  in
  body ^ String.concat "\n" answer ^ "\n"

let render ?(color = false) t =
  let paint code s = if color then Printf.sprintf "\027[%sm%s\027[0m" code s else s in
  let buffer = Buffer.create 2048 in
  let add = Buffer.add_string buffer in
  add (Printf.sprintf "%s %s  %s\n" (paint "1" "council on") (paint "1" t.problem.Council_problem.theorem) (paint "2" (Printf.sprintf "%s:%d" t.problem.Council_problem.file t.problem.Council_problem.line)));
  add (Printf.sprintf "  %s\n" t.problem.Council_problem.statement);
  let round = ref (-1) in
  List.iter
    (fun r ->
      if r.turn.Council_schedule.round <> !round then begin
        round := r.turn.Council_schedule.round;
        add (paint "2" (if !round = 0 then "\nexamination\n" else Printf.sprintf "\nround %d\n" !round))
      end;
      add
        (Printf.sprintf "  %s %s %s\n" (paint "36" (Text.pad_right 11 r.turn.Council_schedule.role)) (paint "2" (Text.pad_right 9 (Council_plan.task_name r.turn.Council_schedule.task)))
           (if r.status = "failed" then paint "31" r.summary else r.summary)))
    t.turns;
  add "\n";
  (match status t with
   | Council_board.Verified -> add (paint "32" "verified" ^ "  " ^ t.answer.Council_synthesis.text ^ "\n")
   | Council_board.Refuted -> add (paint "31" "refuted" ^ "  " ^ t.answer.Council_synthesis.text ^ "\n")
   | _ -> add (paint "33" "not proved" ^ "  " ^ t.answer.Council_synthesis.text ^ (match ending_text t with "" -> "" | why -> " The session ended because " ^ why ^ ".") ^ "\n"));
  (match t.answer.Council_synthesis.lean with Some lean -> add ("\n" ^ Text.indent 2 (String.trim lean) ^ "\n") | None -> ());
  (match t.answer.Council_synthesis.set_aside with
   | [] -> ()
   | aside ->
     add
       (paint "2"
          (Printf.sprintf "\nset aside, not part of the answer: %s\n"
             (String.concat ", " (List.map (fun (kind, n) -> Printf.sprintf "%d %s%s" n kind (if n = 1 then "" else "s")) aside)))));
  Buffer.contents buffer

let exit_code t = match status t with Council_board.Verified -> Exit_code.Success | Council_board.Refuted -> Exit_code.Refuted | _ -> Exit_code.Not_verified
