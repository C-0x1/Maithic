(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The researcher: finds what the library already has.

    A model is asked which definitions and lemmas bear on the statement. It
    answers with names, and names are cheap to invent, so each is looked up:
    a name Lean knows is posted with the signature Lean gives it, and a name
    Lean does not know is dropped and counted. What the prover later reads as
    "the library has this lemma" is therefore always true. Ideas for a proof
    are posted as notes, and a note is a remark and nothing more. *)

open Maithic_base

let role =
  { Council_role.name = "researcher"; purpose = "Finds the definitions, lemmas and prior results that bear on the statement.";
    tasks = [ Council_plan.Research ]; uses_model = true; tools = [ "lookup" ]; prompt = "council.research" }

(** The most names of one answer that are looked up. *)
let max_names = 12

(** The most ideas of one answer that are posted. *)
let max_ideas = 3

let schema = ("research_findings", Council_role.object_schema [ ("names", Council_role.text_list); ("ideas", Council_role.text_list) ])

(** Whether a text is a name Lean could know: parts of letters, digits, the underscore and the prime, with dots between them. *)
let well_formed name =
  let letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let part p = p <> "" && letter p.[0] && String.for_all (fun c -> letter c || (c >= '0' && c <= '9') || c = '\'' || c = '!' || c = '?') p in
  name <> "" && List.for_all part (String.split_on_char '.' name)

let bindings (c : Council_role.context) =
  [ ("statement", c.Council_role.problem.Council_problem.statement);
    ("context", (match Council_problem.excerpt c.Council_role.problem with "" -> "(nothing)" | text -> text));
    ("goal", (match c.Council_role.problem.Council_problem.goal with "" -> "(none given)" | goal -> goal)) ]

(** The names of an answer that are worth looking up: well formed, each once, [max_names] at most. *)
let names_of answer =
  let unique = List.fold_left (fun seen name -> if List.mem name seen then seen else seen @ [ name ]) [] (List.filter well_formed (Council_role.strings "names" answer)) in
  List.filteri (fun i _ -> i < max_names) unique

let act ~(ask : Council_role.ask) ~(lookup : string list -> (string * string) list) (c : Council_role.context) : Council_role.work =
  match ask ~purpose:role.Council_role.prompt ~schema (bindings c) with
  | Error why ->
    Council_role.say c role [ "coordinator" ] Council_message.Decline ~subject:"no research" why;
    { Council_role.status = Council_role.Failed why; summary = "the model could not be asked: " ^ Council_role.first_line why; posted = [] }
  | Ok text ->
    let answer = Option.value (Council_role.answer text) ~default:(Json.Obj []) in
    let names = names_of answer in
    let found = lookup names in
    let unknown = List.filter (fun name -> not (List.mem_assoc name found)) names in
    let definitions =
      List.map
        (fun (name, signature) ->
          (Council_board.post c.Council_role.board ~author:role.Council_role.name Council_board.Definition ~title:name ~refers:[ c.Council_role.statement ] ~checked:true signature)
            .Council_board.id)
        found
    in
    let ideas =
      List.map
        (fun idea ->
          (Council_board.post c.Council_role.board ~author:role.Council_role.name Council_board.Note ~title:"an idea for the proof" ~refers:[ c.Council_role.claim ] idea)
            .Council_board.id)
        (List.filteri (fun i _ -> i < max_ideas) (Council_role.strings "ideas" answer))
    in
    let summary =
      Printf.sprintf "%d of %d names exist in the library%s" (List.length found) (List.length names)
        (match List.length ideas with 0 -> "" | 1 -> "; 1 idea" | n -> Printf.sprintf "; %d ideas" n)
    in
    let content =
      String.concat "\n"
        ((if found = [] then [ "Lean knows none of the names." ] else List.map (fun (_, signature) -> signature) found)
         @ if unknown = [] then [] else [ "Lean does not know: " ^ String.concat ", " unknown ])
    in
    Council_role.say c role [ "prover"; "coordinator" ] Council_message.Inform ~subject:summary ~refers:(definitions @ ideas) content;
    { Council_role.status = (if names = [] && ideas = [] then Council_role.Idle else Council_role.Done); summary; posted = definitions @ ideas }
