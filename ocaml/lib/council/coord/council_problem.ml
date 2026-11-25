(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_prover

type t = { file : string; theorem : string; statement : string; line : int; context : string; goal : string; decl : Lean_source.decl; source : string }

let of_source ?theorem ?(goal = "") ~file source =
  let found =
    match theorem with
    | Some name -> Lean_source.find source name
    | None ->
      (match List.filter Lean_source.has_sorry (Lean_source.declarations source) with
       | [ decl ] -> Ok decl
       | [] -> Error (Printf.sprintf "%s has no theorem with an unfinished proof; name one with --theorem" file)
       | several ->
         Error
           (Printf.sprintf "%s has %d theorems with an unfinished proof (%s); name one with --theorem" file (List.length several)
              (String.concat ", " (List.map (fun (d : Lean_source.decl) -> d.Lean_source.name) several))))
  in
  Result.map
    (fun (decl : Lean_source.decl) ->
      { file; theorem = decl.Lean_source.name; statement = Lean_source.statement decl; line = decl.Lean_source.line; context = decl.Lean_source.prelude;
        goal = String.trim goal; decl; source })
    found

let excerpt ?(limit = 6000) t =
  let context = String.trim t.context in
  let n = String.length context in
  if n <= limit then context
  else
    let from = n - limit in
    let start = match String.index_from_opt context from '\n' with Some i -> i + 1 | None -> from in
    String.sub context start (n - start)

let with_proof t proof = Lean_source.with_proof_by t.decl proof

let to_json t =
  Json.Obj
    ([ ("file", Json.String t.file); ("theorem", Json.String t.theorem); ("statement", Json.String t.statement); ("line", Json.Int t.line) ]
     @ if t.goal = "" then [] else [ ("goal", Json.String t.goal) ])
