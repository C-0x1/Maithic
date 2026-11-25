(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type turn = { number : int; round : int; task : Council_plan.task; role : string }

let order = [ "researcher"; "prover"; "verifier"; "critic" ]

(* The tasks of a round in the order of the roles; a role with two tasks keeps the order of the plan. *)
let of_round ~round ~last =
  let place role = match List.find_index (fun r -> r = role) order with Some i -> i | None -> List.length order in
  List.stable_sort (fun (_, a) (_, b) -> compare (place a) (place b)) (Council_plan.assign ~round ~last)

let turns ~rounds =
  let rounds = max 0 rounds in
  List.concat (List.init (rounds + 1) (fun round -> List.map (fun (task, role) -> (round, task, role)) (of_round ~round ~last:(round = rounds))))
  |> List.mapi (fun i (round, task, role) -> { number = i + 1; round; task; role })

let after turns turn = List.find_opt (fun t -> t.number = turn.number + 1) turns

let describe ~rounds =
  let rounds = max 0 rounds in
  String.concat " | "
    (List.init (rounds + 1) (fun round ->
         Printf.sprintf "%s: %s" (if round = 0 then "examine" else Printf.sprintf "round %d" round)
           (String.concat ", " (List.map snd (of_round ~round ~last:(round = rounds))))))
