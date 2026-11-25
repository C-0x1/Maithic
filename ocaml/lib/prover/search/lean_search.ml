(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

type driver = {
  try_tactics : state:int -> goal:int -> string list -> (Bridge_types.tactic_outcome list, Bridge_error.t) result;
  release : int -> unit;
}

let driver_of_bridge ?limits bridge =
  { try_tactics = (fun ~state ~goal tactics -> Bridge_client.try_tactics ~goal ?limits bridge ~state tactics);
    release = (fun state -> ignore (Bridge_client.release_state bridge state)) }

type focus = { state : int; goals : Bridge_types.goal list; goal : Bridge_types.goal option }

let goal_key (g : Bridge_types.goal) =
  let line (h : Bridge_types.hypothesis) =
    h.Bridge_types.hyp_name ^ " : " ^ h.Bridge_types.hyp_type
    ^ match h.Bridge_types.hyp_value with Some v -> " := " ^ v | None -> ""
  in
  String.concat "\n" (List.sort compare (List.map line g.Bridge_types.hypotheses) @ [ "\xE2\x8A\xA2 " ^ g.Bridge_types.target ])

type proposer = Bridge_types.goal -> (string * float) list

let vocabulary tactics _ = tactics

(* A name that a tactic can refer to: Lean prints names it made up with a dagger. *)
let usable name = name <> "" && not (Text.contains name "\xE2\x9C\x9D")

(* The words of a target that may name definitions of the file: they start with a small letter,
   are not variables of the goal, and are not words of the language. *)
let definition_names (g : Bridge_types.goal) =
  let target = g.Bridge_types.target in
  (* Bytes of other scripts and marks count as parts of a word, so that a name which Lean made
     up, as "n" followed by a dagger, stays one word and is recognised as a variable. *)
  let is_word c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '.' || c = '\'' || Char.code c >= 0x80
  in
  let words = ref [] and start = ref (-1) in
  String.iteri
    (fun i c ->
      if is_word c then (if !start < 0 then start := i)
      else if !start >= 0 then begin
        words := String.sub target !start (i - !start) :: !words;
        start := -1
      end)
    (target ^ " ");
  let reserved = [ "fun"; "if"; "then"; "else"; "match"; "with"; "let"; "in"; "do"; "have"; "show"; "from"; "at"; "by"; "true"; "false" ] in
  let locals = List.map (fun (h : Bridge_types.hypothesis) -> h.Bridge_types.hyp_name) g.Bridge_types.hypotheses in
  List.rev !words
  |> List.filter (fun w ->
         w.[0] >= 'a' && w.[0] <= 'z'
         && String.for_all (fun c -> Char.code c < 0x80) w
         && (not (List.mem w reserved))
         && not (List.mem w locals))
  |> List.fold_left (fun seen w -> if List.mem w seen then seen else seen @ [ w ]) []
  |> List.filteri (fun i _ -> i < 2)

let shape_proposer (g : Bridge_types.goal) =
  let target = g.Bridge_types.target in
  let has = Text.contains target in
  let starts prefix = String.length target >= String.length prefix && String.sub target 0 (String.length prefix) = prefix in
  let closing =
    [ ("rfl", 0.9); ("assumption", 0.9); ("omega", 0.8); ("simp", 0.7); ("simp_all", 0.6); ("decide", 0.5); ("grind", 0.5) ]
  in
  let opening =
    (if starts "\xE2\x88\x80" || has " \xE2\x86\x92 " || starts "\xC2\xAC" then [ ("intros", 0.9) ] else [])
    @ (if has " \xE2\x88\xA7 " || has " \xE2\x86\x94 " then [ ("constructor", 0.8) ] else [])
    @ if has " \xE2\x88\xA8 " then [ ("left", 0.3); ("right", 0.3) ] else []
  in
  let from_hypotheses =
    List.concat_map
      (fun (h : Bridge_types.hypothesis) ->
        let name = h.Bridge_types.hyp_name and ty = h.Bridge_types.hyp_type in
        if not (usable name) then []
        else if Text.contains ty " \xE2\x88\xA7 " || Text.contains ty " \xE2\x88\xA8 " || Text.contains ty "\xE2\x88\x83" then
          [ ("cases " ^ name, 0.6) ]
        else if ty = "Nat" || (String.length ty >= 5 && String.sub ty 0 5 = "List ") then [ ("induction " ^ name, 0.4) ]
        else [])
      g.Bridge_types.hypotheses
  in
  let unfolding = List.map (fun name -> ("simp [" ^ name ^ "]", 0.5)) (definition_names g) in
  closing @ opening @ from_hypotheses @ unfolding

let failure_class = function
  | Bridge_types.Refused_by_guard -> "guard"
  | Bridge_types.Parse_failure -> "parse"
  | Bridge_types.Tactic_failure -> "tactic"

let position p xs =
  let rec go i = function [] -> None | x :: rest -> if p x then Some i else go (i + 1) rest in
  go 0 xs

let expand driver (proposer : proposer) (node : focus Search_tree.node) =
  let focus = node.Search_tree.payload in
  match focus.goal with
  | None -> Ok { Search_kernel.tried = []; lean_calls = 0; tokens = 0 }
  | Some goal ->
    let id = goal.Bridge_types.goal_id in
    (match position (fun (g : Bridge_types.goal) -> g.Bridge_types.goal_id = id) focus.goals with
     | None -> Ok { Search_kernel.tried = []; lean_calls = 0; tokens = 0 }
     | Some index ->
       let candidates = proposer goal in
       if candidates = [] then Ok { Search_kernel.tried = []; lean_calls = 0; tokens = 0 }
       else
         Result.map
           (fun outcomes ->
             let before = List.map (fun (g : Bridge_types.goal) -> g.Bridge_types.goal_id) focus.goals in
             let tried =
               List.mapi
                 (fun i (tactic, prior) ->
                   let step =
                     match List.nth_opt outcomes i with
                     | None -> Search_tree.Fails { error_class = "bridge"; message = "the Bridge returned no outcome for this tactic" }
                     | Some (Bridge_types.Stopped { kind; error; _ }) -> Search_tree.Fails { error_class = failure_class kind; message = error }
                     | Some (Bridge_types.Stepped { state; goals; _ }) ->
                       (* What the tactic left of the goal: the goal itself if it is still there,
                          and every goal that was not in the state before. *)
                       let left =
                         List.filter
                           (fun (g : Bridge_types.goal) -> g.Bridge_types.goal_id = id || not (List.mem g.Bridge_types.goal_id before))
                           goals
                       in
                       Search_tree.Leaves (List.map (fun g -> (goal_key g, { state; goals; goal = Some g })) left)
                   in
                   { Search_kernel.tactic; prior; step })
                 candidates
             in
             { Search_kernel.tried; lean_calls = 1; tokens = 0 })
           (driver.try_tactics ~state:focus.state ~goal:index (List.map fst candidates)))

type found = {
  proof : string option;
  stop : Search_kernel.stop;
  expansions : int;
  lean_calls : int;
  states : int;
  seconds : float;
  record : Search_record.t option;
}

let describe (node : focus Search_tree.node) =
  match node.Search_tree.payload.goal with
  | None -> ("", [])
  | Some g ->
    ( g.Bridge_types.target,
      List.map (fun (h : Bridge_types.hypothesis) -> h.Bridge_types.hyp_name ^ " : " ^ h.Bridge_types.hyp_type) g.Bridge_types.hypotheses )

let default_budget = { Search_kernel.unlimited with Search_kernel.expansions = 60; seconds = 60.0 }
let script_to_proof lines = "by\n" ^ String.concat "\n" (List.map (fun line -> "  " ^ line) lines)

let search ?now ?strategy ?(budget = default_budget) ?(proposer = shape_proposer) ?statement driver ~state goals =
  let root = { state; goals; goal = None } in
  let leaves = List.map (fun g -> (goal_key g, { state; goals; goal = Some g })) goals in
  (* Every tactic that works leaves a state behind in the Bridge, whether or not the search goes
     on from it. All of them are written down, and given back at the end. *)
  let made = Hashtbl.create 64 in
  let recording =
    { driver with
      try_tactics =
        (fun ~state ~goal tactics ->
          let outcomes = driver.try_tactics ~state ~goal tactics in
          Result.iter
            (List.iter (function Bridge_types.Stepped { state = after; _ } -> Hashtbl.replace made after () | Bridge_types.Stopped _ -> ()))
            outcomes;
          outcomes) }
  in
  let result = Search_kernel.search ?now ?strategy ~budget ~expand:(expand recording proposer) ~root leaves in
  Hashtbl.iter (fun after () -> if after <> state then driver.release after) made;
  Result.map
    (fun (result : focus Search_kernel.result) ->
      { proof = Option.map script_to_proof result.Search_kernel.script; stop = result.Search_kernel.stop;
        expansions = result.Search_kernel.expansions; lean_calls = result.Search_kernel.lean_calls; states = Hashtbl.length made;
        seconds = result.Search_kernel.seconds; record = Some (Search_record.of_result ?statement ~describe result) })
    result

let prove_statement ?strategy ?budget ?proposer ?limits bridge ~env statement =
  let ( let* ) = Result.bind in
  let* _report, opened = Bridge_client.start_proof ?limits bridge ~env (`Statement statement) in
  match opened with
  | [] -> Ok { proof = None; stop = Search_kernel.Exhausted; expansions = 0; lean_calls = 1; states = 0; seconds = 0.0; record = None }
  | first :: _ ->
    let driver = driver_of_bridge ?limits bridge in
    let found = search ?strategy ?budget ?proposer ~statement driver ~state:first.Bridge_types.state first.Bridge_types.goals in
    List.iter (fun (o : Bridge_types.opened_proof) -> driver.release o.Bridge_types.state) opened;
    found
