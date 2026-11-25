(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge
open Maithic_prover
open Maithic_gateway

let name = "mend.repair"

let errors_text ~first_line diagnostics =
  diagnostics
  |> List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error")
  |> List.map (fun (d : Bridge_types.diagnostic) -> Printf.sprintf "line %d: %s" (d.Bridge_types.line - first_line + 1) (String.trim d.Bridge_types.text))
  |> String.concat "\n\n"

let attempts_text = function
  | [] -> "(none)"
  | failures -> String.concat "\n\n" (List.map (fun (proof, said) -> Printf.sprintf "%s\n-- Lean: %s" proof (Text.truncate 600 said)) failures)

let bindings ~count ~(decl : Lean_source.decl) ~(breakage : Mend_breakage.t) ~failures =
  [ ("count", string_of_int count); ("theorem", breakage.Mend_breakage.context); ("class", Mend_class.name breakage.Mend_breakage.class_);
    ("errors", (match errors_text ~first_line:breakage.Mend_breakage.first_line breakage.Mend_breakage.diagnostics with "" -> "(none)" | e -> e));
    ("context", (match String.trim (Model_proposer.context_excerpt decl.Lean_source.prelude) with "" -> "(nothing)" | c -> c));
    ("attempts", attempts_text failures) ]

let ask gateway ~max_tokens ~count ~decl ~breakage ~failures =
  let said (e : Gateway_types.error) = Gateway_types.error_to_string e in
  match Gateway.prompt gateway name with
  | Error e -> Error e
  | Ok prompt ->
    (match Prompt.render prompt (bindings ~count ~decl ~breakage ~failures) with
     | Error e -> Error e
     | Ok (system, user) ->
       let request = Gateway_types.request ~purpose:name ~system ~max_tokens ~schema:("proof_candidates", Model_proposer.schema) [ Gateway_types.user user ] in
       (match Gateway.complete gateway request with
        | Error e -> Error (said e)
        | Ok response -> Ok (List.filteri (fun i _ -> i < count) (Model_proposer.parse_candidates response.Gateway_types.text))))
