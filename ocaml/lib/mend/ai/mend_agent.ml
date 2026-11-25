(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_bridge
open Maithic_prover

type settings = { rounds : int; candidates : int }

let default_settings = { rounds = 2; candidates = 3 }

type attempt = { round : int; proof : string; accepted : bool; said : string }
type stage = Model | Prove of string

type result =
  | Fixed of { stage : stage; proof : string; round : int; changed_lines : int; axioms : string list; attempts : attempt list }
  | Unfixed of { reason : string; attempts : attempt list }

type ask = round:int -> failures:(string * string) list -> (string list, string) Stdlib.result
type reprove = unit -> (string * string, string) Stdlib.result

let stage_name = function Model -> "model" | Prove _ -> "prove"

let reference ~(check : Mend_check.t) (decl : Lean_source.decl) =
  let open_ = check ~untrusted:"" (Lean_source.assemble decl "sorry") in
  if open_.Mend_check.statement = "" || (not open_.Mend_check.accepted && String.trim open_.Mend_check.error <> "") then
    Error
      (Printf.sprintf "the statement does not elaborate, and a change to the proof cannot repair that: %s"
         (match String.trim open_.Mend_check.error with "" -> "Lean produced no declaration" | e -> Mend_detect.first_line e))
  else Ok open_

(* One proof, judged: screened, put in the place of the old proof, elaborated, and compared with
   what the theorem stated. *)
let judge ~(check : Mend_check.t) ~reference ~round (decl : Lean_source.decl) proof =
  match Screen.violations proof with
  | finding :: _ -> ({ round; proof; accepted = false; said = "refused by the guard: " ^ finding.Screen.reason }, None)
  | [] ->
    let verdict = check ~untrusted:proof (Lean_source.assemble decl proof) in
    if Mend_check.proves ~reference verdict then ({ round; proof; accepted = true; said = "" }, Some verdict)
    else ({ round; proof; accepted = false; said = Mend_check.why_not ~reference verdict }, None)

let repair_with ~(ask : ask) ~check ~reference settings (decl : Lean_source.decl) =
  let rec round n failures attempts =
    if n > settings.rounds then Unfixed { reason = Printf.sprintf "no repair after %d round(s) with the model" settings.rounds; attempts = List.rev attempts }
    else
      match ask ~round:n ~failures with
      | Error why -> Unfixed { reason = "the model was not asked: " ^ why; attempts = List.rev attempts }
      | Ok proofs ->
        (* A proof that was already tried is not sent to Lean again. *)
        let fresh =
          List.fold_left
            (fun kept proof -> if List.mem proof kept || List.exists (fun (a : attempt) -> a.proof = proof) attempts then kept else kept @ [ proof ])
            []
            (List.filteri (fun i _ -> i < settings.candidates) (List.map String.trim proofs))
        in
        let judged = List.map (judge ~check ~reference ~round:n decl) fresh in
        let attempts = List.rev_append (List.map fst judged) attempts in
        let accepted = List.filter_map (fun ((a : attempt), verdict) -> Option.map (fun v -> (a.proof, v)) verdict) judged in
        (match accepted with
         | [] ->
           let failures = failures @ List.filter_map (fun ((a : attempt), _) -> if a.accepted then None else Some (a.proof, a.said)) judged in
           if fresh = [] && proofs = [] then
             Unfixed { reason = Printf.sprintf "the model had no repair to propose in round %d" n; attempts = List.rev attempts }
           else round (n + 1) failures attempts
         | _ ->
           (* The smallest change among the proofs Lean accepted; the model's own order decides a tie. *)
           let changed (proof, _) = Mend_edit.changed_lines decl.Lean_source.proof proof in
           let best = List.fold_left (fun best candidate -> if changed candidate < changed best then candidate else best) (List.hd accepted) accepted in
           let proof, (verdict : Mend_check.verdict) = best in
           Fixed { stage = Model; proof; round = n; changed_lines = changed best; axioms = verdict.Mend_check.axioms; attempts = List.rev attempts })
  in
  round 1 [] []

let repair ~ask ~check settings decl =
  match reference ~check decl with
  | Error reason -> Unfixed { reason; attempts = [] }
  | Ok reference -> repair_with ~ask ~check ~reference settings decl

let mend ?ask ?(reprove : reprove option) ~check settings (decl : Lean_source.decl) =
  match reference ~check decl with
  | Error reason -> Unfixed { reason; attempts = [] }
  | Ok reference ->
    let first =
      match ask with
      | Some ask -> repair_with ~ask ~check ~reference settings decl
      | None -> Unfixed { reason = "no model was asked"; attempts = [] }
    in
    (match first, reprove with
     | Fixed _, _ | Unfixed _, None -> first
     | Unfixed { reason; attempts }, Some reprove ->
       (match reprove () with
        | Error why -> Unfixed { reason = Printf.sprintf "%s; the prover did not prove the statement either: %s" reason why; attempts }
        | Ok (proof, proposer) ->
          let attempt, verdict = judge ~check ~reference ~round:0 decl proof in
          let attempts = attempts @ [ attempt ] in
          (match verdict with
           | Some verdict ->
             Fixed
               { stage = Prove proposer; proof; round = 0; changed_lines = Mend_edit.changed_lines decl.Lean_source.proof proof;
                 axioms = verdict.Mend_check.axioms; attempts }
           | None -> Unfixed { reason = Printf.sprintf "%s; the proof of the prover was not accepted in the file: %s" reason attempt.said; attempts })))
