(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The verifier: asks Lean, and says what Lean decided.

    The verifier asks no model. It is the one role that is handed the tools
    that return evidence, and so the one role through which the status of a
    claim can change. Before the first round it examines the statement: Lean
    is asked whether it is well formed, a counterexample is looked for, and
    the prover's automation has one bounded attempt. In a round it gives
    each proof on the board that Lean has not seen to Lean, in the order of
    posting, and stops at the first that Lean accepts. *)

open Maithic_ledger

let role =
  { Council_role.name = "verifier"; purpose = "Runs Lean and confirms correctness; the only role that can request a verified status.";
    tasks = [ Council_plan.Examine; Council_plan.Verify ]; uses_model = false; tools = [ "lean_check"; "elaborate"; "check_proof"; "prove"; "refute" ]; prompt = "" }

let everyone = [ "coordinator"; "prover"; "critic" ]

type judged = Verified | Not_verified | Unjudged of string

(* Gives one proof to Lean and records what Lean said. A proof Lean could not be asked about is left as it was. *)
let judge ~tools ~(policy : Trust_policy.t) (c : Council_role.context) (proof : Council_board.item) =
  let statement = Council_role.formal_statement c in
  let number = proof.Council_board.id in
  let reject subject why =
    ignore (Council_board.Verifier.rejected c.Council_role.board ~proof:number why);
    Council_role.say c role everyone Council_message.Verdict ~subject ~refers:[ number ] why;
    Not_verified
  in
  match Council_tools.check_proof tools ~policy ~statement proof.Council_board.body with
  | Ok evidence ->
    (match Council_board.Verifier.verified c.Council_role.board ~claim:c.Council_role.claim ~proof:number evidence with
     | Ok () ->
       Council_role.say c role everyone Council_message.Verdict
         ~subject:(Printf.sprintf "proof %d is accepted: the claim is verified" number)
         ~refers:[ number; c.Council_role.claim ]
         (match Council_evidence.axioms evidence with
          | [] -> "Lean's kernel accepted the proof. It rests on no axiom."
          | axioms -> "Lean's kernel accepted the proof. It rests on the axioms " ^ String.concat ", " axioms ^ ".");
       Verified
     | Error why -> reject (Printf.sprintf "proof %d is not counted" number) why)
  | Error (Council_tools.Rejected why) -> reject (Printf.sprintf "proof %d is rejected" number) why
  | Error (Council_tools.Unavailable why) ->
    Council_role.say c role [ "coordinator" ] Council_message.Decline ~subject:(Printf.sprintf "proof %d was not given to Lean" number) ~refers:[ number ] why;
    Unjudged why

(* Before the first round. *)
let examine ~tools ~policy (c : Council_role.context) : Council_role.work =
  match Council_tools.elaborate tools with
  | Error why ->
    Council_role.say c role [ "coordinator" ] Council_message.Verdict ~subject:"the statement is not well formed" ~refers:[ c.Council_role.statement ] why;
    { Council_role.status = Council_role.Failed ("the statement does not elaborate: " ^ Council_role.first_line why); summary = "the statement does not elaborate"; posted = [] }
  | Ok evidence ->
    (match Council_board.Verifier.formalized c.Council_role.board ~claim:c.Council_role.claim evidence with
     | Error why -> { Council_role.status = Council_role.Failed why; summary = why; posted = [] }
     | Ok () ->
       let statement = Council_evidence.statement evidence in
       Council_role.say c role [ "coordinator" ] Council_message.Verdict ~subject:"the statement is well formed" ~refers:[ c.Council_role.claim ] statement;
       (match Council_tools.refute tools ~statement with
        | Some (point, refutation) ->
          let at = if point = [] then "as it stands" else "at " ^ String.concat ", " (List.map (fun (name, value) -> name ^ " = " ^ value) point) in
          let item =
            Council_board.post c.Council_role.board ~author:role.Council_role.name Council_board.Counterexample ~title:("the statement fails " ^ at)
              ~refers:[ c.Council_role.claim ] ~checked:true
              (String.concat "\n" (List.map (fun (name, value) -> name ^ " := " ^ value) point))
          in
          (match Council_board.Verifier.refuted c.Council_role.board ~claim:c.Council_role.claim ~counterexample:item.Council_board.id refutation with
           | Ok () ->
             Council_role.say c role everyone Council_message.Verdict ~subject:"the claim is refuted" ~refers:[ item.Council_board.id; c.Council_role.claim ]
               (Printf.sprintf "The statement fails %s, and Lean accepted a proof of that." at);
             { Council_role.status = Council_role.Done; summary = "the statement is false: it fails " ^ at; posted = [ item.Council_board.id ] }
           | Error why -> { Council_role.status = Council_role.Failed why; summary = why; posted = [ item.Council_board.id ] })
        | None ->
          let so_far = "the statement is well formed" ^ if Council_tools.can_refute tools then "; no counterexample was found" else "" in
          if not (Council_tools.can_prove tools) then { Council_role.status = Council_role.Done; summary = so_far; posted = [] }
          else
            (match Council_tools.prove tools with
             | Error why ->
               Council_role.say c role [ "coordinator" ] Council_message.Inform ~subject:"automation found no proof" why;
               { Council_role.status = Council_role.Done; summary = so_far ^ "; automation found no proof"; posted = [] }
             | Ok (proof, proposer) ->
               let item =
                 Council_board.post c.Council_role.board ~author:role.Council_role.name Council_board.Proof ~title:(Printf.sprintf "from the prover's automation (%s)" proposer)
                   ~refers:[ c.Council_role.claim ] proof
               in
               (match judge ~tools ~policy c item with
                | Verified -> { Council_role.status = Council_role.Done; summary = "automation found a proof, and Lean accepted it"; posted = [ item.Council_board.id ] }
                | Not_verified -> { Council_role.status = Council_role.Done; summary = "automation proposed a proof that was not accepted"; posted = [ item.Council_board.id ] }
                | Unjudged why -> { Council_role.status = Council_role.Failed why; summary = "Lean could not be asked about the proof automation found"; posted = [ item.Council_board.id ] }))))

(* In a round. *)
let verify ~tools ~policy (c : Council_role.context) : Council_role.work =
  let waiting =
    List.filter
      (fun (i : Council_board.item) -> List.mem c.Council_role.claim i.Council_board.refers && Council_board.examined c.Council_role.board i.Council_board.id = Council_board.Unexamined)
      (Council_board.of_kind c.Council_role.board Council_board.Proof)
  in
  if waiting = [] then { Council_role.status = Council_role.Idle; summary = "no proof to give to Lean"; posted = [] }
  else if Council_role.formal_statement c = "" then { Council_role.status = Council_role.Failed "the claim was never formalized"; summary = "the claim was never formalized"; posted = [] }
  else begin
    let rec through examined = function
      | [] -> (examined, Not_verified)
      | proof :: rest ->
        (match judge ~tools ~policy c proof with
         | Verified -> (examined + 1, Verified)
         | Not_verified -> through (examined + 1) rest
         | Unjudged why -> (examined, Unjudged why))
    in
    match through 0 waiting with
    | examined, Verified -> { Council_role.status = Council_role.Done; summary = Printf.sprintf "Lean accepted a proof (%d examined)" examined; posted = [] }
    | examined, Not_verified ->
      { Council_role.status = Council_role.Done; summary = Printf.sprintf "Lean rejected %s" (if examined = 1 then "the proof" else Printf.sprintf "all %d proofs" examined); posted = [] }
    | examined, Unjudged why ->
      { Council_role.status = Council_role.Failed why; summary = Printf.sprintf "Lean could not be asked about %d of %d proofs" (List.length waiting - examined) (List.length waiting);
        posted = [] }
  end

let act ~tools ~policy (c : Council_role.context) : Council_role.work =
  match c.Council_role.task with Council_plan.Examine -> examine ~tools ~policy c | _ -> verify ~tools ~policy c
