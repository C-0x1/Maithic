(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_bridge
open Maithic_prover

let least = 1e-12

let reweigh base priors =
  let usable = List.length priors = List.length base && List.for_all (fun p -> (not (Float.is_nan p)) && p >= 0.0) priors in
  let top = List.fold_left Float.max 0.0 priors in
  if (not usable) || top <= 0.0 || top = Float.infinity then base
  else
    (* A candidate the model rules out keeps a prior above zero: it is still a candidate. *)
    List.map2 (fun (tactic, b) p -> (tactic, if b <= 0.0 then b else Float.max least (b *. (p /. top)))) base priors

let state_text (g : Bridge_types.goal) =
  if String.trim g.Bridge_types.pretty <> "" then g.Bridge_types.pretty
  else
    String.concat "\n"
      (List.map
         (fun (h : Bridge_types.hypothesis) ->
           h.Bridge_types.hyp_name ^ " : " ^ h.Bridge_types.hyp_type ^ match h.Bridge_types.hyp_value with Some v -> " := " ^ v | None -> "")
         g.Bridge_types.hypotheses
       @ [ "\xE2\x8A\xA2 " ^ g.Bridge_types.target ])

let proposer advice ~(base : Lean_search.proposer) : Lean_search.proposer =
 fun goal ->
  match base goal with
  | [] -> []
  | candidates -> reweigh candidates (Guide_advice.priors advice ~state:(state_text goal) (List.map fst candidates))
