(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The critic: says what went wrong.

    When Lean rejects the proofs of a round, a model is shown each of them
    with Lean's own words about it, and asked for a diagnosis and for what to
    try instead. Its answer is posted as a note for the prover. The critic
    reads Lean's errors and has no tool of its own: its notes are opinions
    about failed proofs, and the board files them as that. *)

let role =
  { Council_role.name = "critic"; purpose = "Identifies the errors in proofs that Lean rejected, and suggests alternatives."; tasks = [ Council_plan.Critique ];
    uses_model = true; tools = []; prompt = "council.critique" }

let schema = ("critique", Council_role.object_schema [ ("diagnosis", Maithic_base.Json.Obj [ ("type", Maithic_base.Json.String "string") ]); ("suggestions", Council_role.text_list) ])

(** The proofs Lean rejected that nobody has commented on yet, with what Lean said. *)
let open_failures (c : Council_role.context) =
  List.filter_map
    (fun (i : Council_board.item) ->
      match Council_board.examined c.Council_role.board i.Council_board.id with
      | Council_board.Rejected said
        when not (List.exists (fun (n : Council_board.item) -> n.Council_board.author = role.Council_role.name) (Council_board.about c.Council_role.board i.Council_board.id)) ->
        Some (i, said)
      | _ -> None)
    (Council_board.of_kind c.Council_role.board Council_board.Proof)

let bindings (c : Council_role.context) failures =
  [ ("statement", c.Council_role.problem.Council_problem.statement);
    ("context", (match Council_problem.excerpt ~limit:3000 c.Council_role.problem with "" -> "(nothing)" | text -> text));
    ("lemmas", Council_prover.lemmas c);
    ( "attempts",
      String.concat "\n\n"
        (List.map
           (fun ((i : Council_board.item), said) -> Printf.sprintf "%s\n-- Lean: %s" i.Council_board.body (Maithic_base.Text.truncate 800 (String.trim said)))
           failures) ) ]

let act ~(ask : Council_role.ask) (c : Council_role.context) : Council_role.work =
  match open_failures c with
  | [] -> { Council_role.status = Council_role.Idle; summary = "no rejected proof to comment on"; posted = [] }
  | failures ->
    (match ask ~purpose:role.Council_role.prompt ~schema (bindings c failures) with
     | Error why ->
       Council_role.say c role [ "coordinator" ] Council_message.Decline ~subject:"no critique" why;
       { Council_role.status = Council_role.Failed why; summary = "the model could not be asked: " ^ Council_role.first_line why; posted = [] }
     | Ok text ->
       let answer = Option.value (Council_role.answer text) ~default:(Maithic_base.Json.Obj []) in
       let diagnosis = String.trim (Maithic_base.Json.string_or "" "diagnosis" answer) in
       let suggestions = Council_role.strings "suggestions" answer in
       if diagnosis = "" && suggestions = [] then { Council_role.status = Council_role.Idle; summary = "the model had nothing to say"; posted = [] }
       else begin
         let body = String.concat "\n" ((if diagnosis = "" then [] else [ diagnosis ]) @ List.map (fun s -> "Try: " ^ s) suggestions) in
         let refers = List.map (fun ((i : Council_board.item), _) -> i.Council_board.id) failures in
         let note =
           Council_board.post c.Council_role.board ~author:role.Council_role.name Council_board.Note
             ~title:(Printf.sprintf "why the proofs of round %d failed" c.Council_role.round)
             ~refers body
         in
         let summary =
           Printf.sprintf "a diagnosis of %d rejected proof%s, with %d suggestion%s" (List.length failures) (if List.length failures = 1 then "" else "s")
             (List.length suggestions) (if List.length suggestions = 1 then "" else "s")
         in
         Council_role.say c role [ "prover" ] Council_message.Critique ~subject:summary ~refers:(note.Council_board.id :: refers) body;
         { Council_role.status = Council_role.Done; summary; posted = [ note.Council_board.id ] }
       end)
