(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The prover: proposes proofs.

    A model is shown the statement, its context, the lemmas the researcher
    found, the proofs that Lean rejected with what Lean said about each, and
    what the critic made of them. It answers with proofs. They are posted as
    candidates: the prover has no tool that runs Lean, and nothing it posts
    changes the status of the claim. *)

let role =
  { Council_role.name = "prover"; purpose = "Generates candidate proofs."; tasks = [ Council_plan.Prove ]; uses_model = true; tools = []; prompt = "council.prove" }

let schema = ("proof_candidates", Council_role.object_schema [ ("candidates", Council_role.text_list) ])

(* A proof as one line of words, to tell two layouts of the same proof apart from two proofs. *)
let key proof = String.concat " " (List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.concat " " (String.split_on_char '\n' proof))))

let lemmas (c : Council_role.context) =
  match List.filter (fun (i : Council_board.item) -> i.Council_board.checked) (Council_board.of_kind c.Council_role.board Council_board.Definition) with
  | [] -> "(none)"
  | found -> String.concat "\n" (List.map (fun (i : Council_board.item) -> i.Council_board.body) found)

let attempts (c : Council_role.context) =
  let rejected =
    List.filter_map
      (fun (i : Council_board.item) ->
        match Council_board.examined c.Council_role.board i.Council_board.id with
        | Council_board.Rejected said -> Some (Printf.sprintf "%s\n-- Lean: %s" i.Council_board.body (Maithic_base.Text.truncate 600 (String.trim said)))
        | _ -> None)
      (Council_board.of_kind c.Council_role.board Council_board.Proof)
  in
  if rejected = [] then "(none)" else String.concat "\n\n" rejected

let advice (c : Council_role.context) =
  match List.filter (fun (i : Council_board.item) -> i.Council_board.author <> role.Council_role.name) (Council_board.of_kind c.Council_role.board Council_board.Note) with
  | [] -> "(none)"
  | notes -> String.concat "\n\n" (List.map (fun (i : Council_board.item) -> Printf.sprintf "%s (%s): %s" i.Council_board.author i.Council_board.title i.Council_board.body) notes)

let bindings ~candidates (c : Council_role.context) =
  [ ("count", string_of_int candidates); ("statement", c.Council_role.problem.Council_problem.statement);
    ("context", (match Council_problem.excerpt c.Council_role.problem with "" -> "(nothing)" | text -> text));
    ("goal", (match c.Council_role.problem.Council_problem.goal with "" -> "(none given)" | goal -> goal));
    ("lemmas", lemmas c); ("attempts", attempts c); ("advice", advice c) ]

let act ~(ask : Council_role.ask) ~candidates (c : Council_role.context) : Council_role.work =
  match ask ~purpose:role.Council_role.prompt ~schema (bindings ~candidates c) with
  | Error why ->
    Council_role.say c role [ "coordinator" ] Council_message.Decline ~subject:"no proof to propose" why;
    { Council_role.status = Council_role.Failed why; summary = "the model could not be asked: " ^ Council_role.first_line why; posted = [] }
  | Ok text ->
    let tried = List.map (fun (i : Council_board.item) -> key i.Council_board.body) (Council_board.of_kind c.Council_role.board Council_board.Proof) in
    let proposed = match Council_role.answer text with Some answer -> Council_role.strings "candidates" answer | None -> [] in
    let fresh, _ =
      List.fold_left
        (fun (fresh, seen) proof -> if List.mem (key proof) seen || List.length fresh >= candidates then (fresh, seen) else (fresh @ [ proof ], key proof :: seen))
        ([], tried) proposed
    in
    let posted =
      List.mapi
        (fun i proof ->
          (Council_board.post c.Council_role.board ~author:role.Council_role.name Council_board.Proof
             ~title:(Printf.sprintf "candidate %d of round %d" (i + 1) c.Council_role.round)
             ~refers:[ c.Council_role.claim ] proof)
            .Council_board.id)
        fresh
    in
    if posted = [] then begin
      let why = if proposed = [] then "the model returned no proof" else "the model returned only proofs that were tried before" in
      Council_role.say c role [ "coordinator" ] Council_message.Decline ~subject:"no new proof to propose" why;
      { Council_role.status = Council_role.Idle; summary = why; posted = [] }
    end
    else begin
      let summary = Printf.sprintf "%d candidate proof%s" (List.length posted) (if List.length posted = 1 then "" else "s") in
      Council_role.say c role [ "verifier" ] Council_message.Propose ~subject:summary ~refers:posted (String.concat "\n\n" fresh);
      { Council_role.status = Council_role.Done; summary; posted }
    end
