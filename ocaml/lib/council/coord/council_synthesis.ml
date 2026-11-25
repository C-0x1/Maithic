(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_prover

type t = {
  status : Council_board.status;
  statement : string;
  proof : Council_board.item option;
  axioms : string list;
  counterexample : Council_board.item option;
  components : Council_board.item list;
  set_aside : (string * int) list;
  lean : string option;
  text : string;
}

let mentions text name =
  let n = String.length text and len = String.length name in
  let part c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '\'' || c = '.' in
  let rec go from =
    match Text.find_sub ~from text name with
    | None -> false
    | Some at -> ((at = 0 || not (part text.[at - 1])) && (at + len >= n || not (part text.[at + len]))) || go (at + 1)
  in
  len > 0 && go 0

let synthesize (problem : Council_problem.t) board ~claim =
  let status = Option.value (Council_board.status board claim) ~default:Council_board.Conjectured in
  let evidence = Council_board.evidence board claim in
  let of_kind kind = List.find_opt (fun e -> Council_evidence.kind e = kind) evidence in
  let statement = match of_kind Council_evidence.Elaborated with Some e -> Council_evidence.statement e | None -> problem.Council_problem.statement in
  let claim_item = Option.to_list (Council_board.find board claim) in
  (* A proof is part of the answer only as the proof by which the claim was verified. *)
  let proof = if status = Council_board.Verified then Council_board.accepted_proof board claim else None in
  let axioms = match (proof, of_kind Council_evidence.Proved) with Some _, Some e -> Council_evidence.axioms e | _ -> [] in
  let counterexample =
    if status <> Council_board.Refuted then None
    else List.find_opt (fun (i : Council_board.item) -> i.Council_board.kind = Council_board.Counterexample && i.Council_board.checked) (Council_board.about board claim)
  in
  let lemmas =
    match proof with
    | None -> []
    | Some p ->
      List.filter
        (fun (i : Council_board.item) -> i.Council_board.checked && mentions p.Council_board.body i.Council_board.title)
        (Council_board.of_kind board Council_board.Definition)
  in
  let components = claim_item @ Option.to_list proof @ lemmas @ Option.to_list counterexample in
  let rest =
    List.filter
      (fun (i : Council_board.item) -> i.Council_board.kind <> Council_board.Statement && not (List.exists (fun (c : Council_board.item) -> c.Council_board.id = i.Council_board.id) components))
      (Council_board.items board)
  in
  let set_aside =
    List.filter_map
      (fun kind -> match List.length (List.filter (fun (i : Council_board.item) -> i.Council_board.kind = kind) rest) with 0 -> None | n -> Some (Council_board.kind_name kind, n))
      Council_board.kinds
  in
  let lean = Option.map (fun (p : Council_board.item) -> Lean_source.assemble_by problem.Council_problem.decl p.Council_board.body) proof in
  let text =
    match (status, proof, counterexample) with
    | Council_board.Verified, Some p, _ ->
      Printf.sprintf "Lean's kernel accepted proof %d, posted by the %s in %s. %s" p.Council_board.id p.Council_board.author
        (if p.Council_board.round = 0 then "the examination" else Printf.sprintf "round %d" p.Council_board.round)
        (if axioms = [] then "It rests on no axiom." else Printf.sprintf "It rests on the axioms %s." (String.concat ", " axioms))
    | Council_board.Refuted, _, Some c -> Printf.sprintf "%s, and Lean accepted a proof of that." (String.capitalize_ascii c.Council_board.title)
    | Council_board.Refuted, _, None -> "Lean accepted a proof that the statement is false."
    | Council_board.Formalized, _, _ -> "The statement is well formed, and Lean accepted no proof of it. Nothing else that was said in the session is established."
    | Council_board.Conjectured, _, _ -> "Lean did not elaborate the statement, so nothing about it is established."
    | Council_board.Verified, None, _ -> "Lean's kernel accepted a proof."
  in
  { status; statement; proof; axioms; counterexample; components; set_aside; lean; text }

let to_json a =
  Json.Obj
    ([ ("status", Json.String (Council_board.status_name a.status)); ("statement", Json.String a.statement); ("text", Json.String a.text) ]
     @ (match a.proof with Some p -> [ ("proof", Json.String p.Council_board.body); ("proof_item", Json.Int p.Council_board.id); ("axioms", Json.List (List.map (fun x -> Json.String x) a.axioms)) ] | None -> [])
     @ (match a.counterexample with Some c -> [ ("counterexample", Json.String c.Council_board.body); ("counterexample_item", Json.Int c.Council_board.id) ] | None -> [])
     @ [ ("components", Json.List (List.map (fun (i : Council_board.item) -> Json.Int i.Council_board.id) a.components));
         ("set_aside", Json.Obj (List.map (fun (kind, n) -> (kind, Json.Int n)) a.set_aside)) ]
     @ match a.lean with Some text -> [ ("lean", Json.String text) ] | None -> [])
