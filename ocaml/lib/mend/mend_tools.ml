(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The tools of a repair, made from a running Bridge, a gateway and the prover.

    The logic of Mend asks its questions through functions, so that it can
    be tested without Lean and without a model. This module makes those
    functions from the real things. *)

open Maithic_schema
open Maithic_bridge
open Maithic_ledger
open Maithic_prover

let said (e : Bridge_error.t) = Bridge_error.to_string e

(** Elaborates the text of a whole file. *)
let file_check ?limits bridge : Mend_detect.check = fun text -> Result.map fst (Result.map_error said (Bridge_client.check_file ?limits bridge text))

(** A check of one theorem in the environment of what stands before it. The
    environment is built once, when the first proof is checked; if what
    stands before the theorem has errors, every check says so. *)
let theorem_check ?limits bridge (decl : Lean_source.decl) : Mend_check.t =
  let env =
    lazy
      (match Bridge_client.check_file ~keep:true ?limits bridge decl.Lean_source.prelude with
       | Ok (_, Some env) -> Ok env
       | Ok (report, None) ->
         Error
           (Printf.sprintf "the file has errors before the theorem: %s"
              (match Mend_detect.first_line (Bridge_types.error_text report) with "" -> "Lean kept no environment" | line -> line))
       | Error e -> Error (said e))
  in
  fun ~untrusted source ->
    match Lazy.force env with
    | Error why -> Mend_check.failed why
    | Ok env -> Mend_check.on_bridge ?limits bridge ~env ~name:decl.Lean_source.name ~untrusted source

(** The model as the agent asks it: the repair prompt through the gateway. *)
let model gateway ~max_tokens ~candidates ~decl ~breakage : Mend_agent.ask =
 fun ~round:_ ~failures -> Mend_prompt.ask gateway ~max_tokens ~count:candidates ~decl ~breakage ~failures

(** The prover as the agent's last resort: its automation, and its step-wise
    search when the settings switch that on. No model is asked here: the
    model has had its rounds with the proof. *)
let prover ?gateway ~policy (settings : Engine.settings) bridge ~source (decl : Lean_source.decl) : Mend_agent.reprove =
 fun () ->
  match Engine.prove ?gateway ~bridge ~policy { settings with Engine.use_ai = false } ~source decl.Lean_source.name with
  | Error (f : Fault.t) -> Error f.Fault.message
  | Ok ({ Engine.proof = Some proof; status = Status.Verified | Status.Checked; _ } as result) -> Ok (proof, Option.value result.Engine.proposer ~default:"prover")
  | Ok _ -> Error "its automation found no proof"

(** The tools of {!Mend_repair.run}. Without [gateway] no model is asked;
    without [prove] no statement is proved again. *)
let tools ?limits ?gateway ?(max_tokens = 2048) ?(candidates = Mend_agent.default_settings.Mend_agent.candidates) ?prove bridge =
  { Mend_repair.check_file = file_check ?limits bridge;
    check_theorem = (fun _ decl -> theorem_check ?limits bridge decl);
    ask = Option.map (fun gateway ~decl ~breakage -> model gateway ~max_tokens ~candidates ~decl ~breakage) gateway;
    reprove = Option.map (fun ((policy : Trust_policy.t), settings) ~source decl -> prover ~policy settings bridge ~source decl) prove }
