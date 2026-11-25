(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_ledger

type belt = { ask : Council_role.ask option; tools : Council_tools.t; policy : Trust_policy.t; candidates : int }

let all = [ Council_researcher.role; Council_prover.role; Council_critic.role; Council_verifier.role ]
let names = List.map (fun (r : Council_role.t) -> r.Council_role.name) all

let find name =
  match List.find_opt (fun (r : Council_role.t) -> r.Council_role.name = name) all with
  | Some role -> Ok role
  | None -> Error (Printf.sprintf "there is no role '%s'; the roles are %s" name (String.concat ", " names))

let for_task task =
  match List.find_opt (fun (r : Council_role.t) -> List.mem task r.Council_role.tasks) all with
  | Some role -> role
  | None -> Council_verifier.role

let instantiate belt name =
  Result.bind (find name) (fun (role : Council_role.t) ->
      match (role.Council_role.name, belt.ask) with
      | "verifier", _ -> Ok (Council_verifier.act ~tools:belt.tools ~policy:belt.policy)
      | _, None -> Error (Printf.sprintf "the role %s asks a model, and the session has none" role.Council_role.name)
      | "researcher", Some ask -> Ok (Council_researcher.act ~ask ~lookup:(Council_tools.lookup belt.tools))
      | "prover", Some ask -> Ok (Council_prover.act ~ask ~candidates:belt.candidates)
      | _, Some ask -> Ok (Council_critic.act ~ask))
