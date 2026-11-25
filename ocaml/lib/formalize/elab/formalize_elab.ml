(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type compiled = { ok : bool; statement : string; said : string }
type compile = Formalize_candidate.t -> compiled

type repair = candidate:Formalize_candidate.t -> error:Formalize_error.t -> said:string -> failures:(string * string) list -> (string * Formalize_candidate.provenance, string) result

type attempt = { round : int; signature : string; elaborated : bool; error_class : string; said : string }
type result = { candidate : Formalize_candidate.t; elaborated : bool; statement : string; history : attempt list; note : string }

let default_rounds = 2
let first_try r = match r.history with (first : attempt) :: _ -> first.elaborated | [] -> false
let repairs r = max 0 (List.length r.history - 1)

let run ?(rounds = default_rounds) ~(compile : compile) ?(repair : repair option) (candidate : Formalize_candidate.t) =
  let attempt round (c : Formalize_candidate.t) (compiled : compiled) =
    { round; signature = c.Formalize_candidate.signature; elaborated = compiled.ok;
      error_class = (if compiled.ok then "" else Formalize_error.name (Formalize_error.classify compiled.said)); said = (if compiled.ok then "" else compiled.said) }
  in
  let rec loop round (c : Formalize_candidate.t) earlier =
    let compiled = compile c in
    let history = earlier @ [ attempt round c compiled ] in
    let stop note = { candidate = c; elaborated = false; statement = ""; history; note } in
    if compiled.ok then { candidate = c; elaborated = true; statement = compiled.statement; history; note = "" }
    else if round >= rounds then stop (if rounds = 0 then "it does not elaborate, and no repair was asked for" else Printf.sprintf "it does not elaborate after %d repair round(s)" rounds)
    else
      match repair with
      | None -> stop "it does not elaborate, and there is no model to repair it"
      | Some repair ->
        let failures = List.filter_map (fun (a : attempt) -> if a.elaborated then None else Some (a.signature, a.said)) earlier in
        (match repair ~candidate:c ~error:(Formalize_error.classify compiled.said) ~said:compiled.said ~failures with
         | Error why -> stop ("it does not elaborate, and the model was not asked for a repair: " ^ why)
         | Ok (raw, provenance) ->
           let signature = Formalize_candidate.signature_of raw in
           let next = { c with Formalize_candidate.signature; provenance = { provenance with Formalize_candidate.origin = "repair"; round = round + 1 } } in
           if signature = "" then stop "it does not elaborate, and the model returned no repair"
           else if List.exists (fun (a : attempt) -> Formalize_candidate.key { c with Formalize_candidate.signature = a.signature } = Formalize_candidate.key next) history then
             stop "it does not elaborate, and the model repeated a statement that had failed"
           else loop (round + 1) next history)
  in
  loop 0 candidate []

let to_json r =
  Json.Obj
    ([ ("status", Json.String (if r.elaborated then "elaborated" else "failed")); ("first_try", Json.Bool (first_try r)); ("repairs", Json.Int (repairs r)) ]
     @ (if r.elaborated then [ ("statement", Json.String r.statement) ] else [])
     @ (if r.note = "" then [] else [ ("note", Json.String r.note) ])
     @ [ ( "history",
           Json.List
             (List.map
                (fun (a : attempt) ->
                  Json.Obj
                    ([ ("round", Json.Int a.round); ("statement", Json.String a.signature); ("elaborated", Json.Bool a.elaborated) ]
                     @ if a.elaborated then [] else [ ("error_class", Json.String a.error_class); ("error", Json.String a.said) ]))
                r.history) ) ])
