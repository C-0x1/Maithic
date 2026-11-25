(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** One verification job: a verifier that is not trusted decides a property
    of a network, and Lean's kernel checks what it claims. *)

open Maithic_base
open Maithic_schema
open Maithic_ledger

(** What the verifier claims. *)
type claim = Claims_verified | Claims_violated | Claims_unknown

let claim_name = function Claims_verified -> "verified" | Claims_violated -> "violated" | Claims_unknown -> "unknown"

(** What Lean said about the theorem that states the claim. *)
type check = {
  theorem : string;
  statement : string;  (** The theorem as Lean prints it; empty when it was not produced. *)
  accepted : bool;  (** The file elaborated without error and the theorem is there. *)
  axioms : string list;
  error : string;  (** Lean's first error; empty when there is none. *)
  decision : Trust_policy.decision;
  elapsed_ms : int;
}

type t = {
  model : string;
  property : string;
  claim : claim;
  verdict : Json.t;  (** What the verifier returned, as it returned it. *)
  check : check option;  (** [None] when the verifier claimed nothing. *)
  status : Status.t;
  reason : string;  (** Why the status is not [verified] or [refuted]; empty when it is. *)
  lean : string option;  (** The Lean file that was checked. *)
  verify_ms : int;
}

(** How Lean is asked: the source and the name of the theorem in it. *)
type checker = source:string -> theorem:string -> (Recheck.outcome, string) result

(** Runs a job: first the verifier, then the checker on what it claims.

    The status is [verified] when the verifier claims that the property
    holds and the kernel accepted the theorem that says so within the trust
    policy; [refuted] when the same is true of the theorem that says it
    fails; [unknown] when the verifier claims nothing; and [failed] when a
    claim was made and Lean did not accept it, which means that the verifier
    is wrong or its certificate is, and never that the property is settled. *)
let run ~(verify : unit -> (Json.t, string) result) ~(check : checker) ~policy ~model ~property =
  let started = Unix.gettimeofday () in
  match verify () with
  | Error why -> Error ("the verifier gave no verdict: " ^ why)
  | Ok verdict ->
    let verify_ms = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
    if Json.string_or "" "schema" verdict <> "maithic.nnv.verdict/1" then Error "the verifier returned something that is not a verdict"
    else (
      match Nnv_text.of_verdict verdict with
      | Error why -> Error ("the verdict cannot be put to Lean: " ^ why)
      | Ok None ->
        Ok
          { model; property; claim = Claims_unknown; verdict; check = None; status = Status.Unknown;
            reason = (match Json.string_or "" "reason" verdict with "" -> "the verifier could not decide the property" | r -> r); lean = None; verify_ms }
      | Ok (Some lean) ->
        let claim = if Json.string_or "" "verdict" verdict = "verified" then Claims_verified else Claims_violated in
        (match check ~source:lean.Nnv_text.source ~theorem:lean.Nnv_text.theorem with
         | Error why -> Error ("Lean could not be asked: " ^ why)
         | Ok (outcome : Recheck.outcome) ->
           let decision =
             Trust_policy.decide policy
               { Trust_policy.declaration = lean.Nnv_text.theorem; found = outcome.Recheck.passed; kind = (if outcome.Recheck.kind = "" then "theorem" else outcome.Recheck.kind);
                 axioms = outcome.Recheck.axioms; errors = outcome.Recheck.errors; rechecked = true }
           in
           let accepted = outcome.Recheck.passed && decision.Trust_policy.status = Status.Verified in
           let checked =
             { theorem = lean.Nnv_text.theorem; statement = outcome.Recheck.statement; accepted; axioms = outcome.Recheck.axioms;
               error = (if outcome.Recheck.passed then "" else outcome.Recheck.first_error); decision; elapsed_ms = outcome.Recheck.elapsed_ms }
           in
           let status, reason =
             if accepted then ((if claim = Claims_verified then Status.Verified else Status.Refuted), "")
             else
               ( Status.Failed,
                 Printf.sprintf "the verifier claims that the property %s, and Lean did not accept the theorem that says so%s"
                   (if claim = Claims_verified then "holds" else "fails")
                   (match (checked.error, decision.Trust_policy.reasons) with
                    | "", [] -> ""
                    | "", reasons -> ": " ^ String.concat "; " reasons
                    | error, _ -> ": " ^ List.hd (String.split_on_char '\n' error)) )
           in
           Ok { model; property; claim; verdict; check = Some checked; status; reason; lean = Some lean.Nnv_text.source; verify_ms }))

let exit_code job = Exit_code.of_status job.status

(* ---- output ---- *)

let schema = "maithic.nnv.job/1"

let to_json job =
  let strings items = Json.List (List.map (fun s -> Json.String s) items) in
  Json.Obj
    ([ ("schema", Json.String schema); ("model", Json.String job.model); ("property", Json.String job.property); ("status", Json.String (Status.to_string job.status));
       ("claim", Json.String (claim_name job.claim)) ]
    @ (if job.reason = "" then [] else [ ("reason", Json.String job.reason) ])
    @ (match job.check with
       | None -> []
       | Some c ->
         [ ( "check",
             Json.Obj
               ([ ("theorem", Json.String c.theorem); ("statement", Json.String c.statement); ("accepted", Json.Bool c.accepted); ("axioms", strings c.axioms) ]
               @ (if c.error = "" then [] else [ ("error", Json.String c.error) ])
               @ [ ("policy", Trust_policy.decision_to_json c.decision); ("elapsed_ms", Json.Int c.elapsed_ms) ]) ) ])
    @ [ ("verify_ms", Json.Int job.verify_ms); ("verdict", job.verdict) ])

let summary job =
  match (job.status, job.check) with
  | Status.Verified, Some c ->
    Printf.sprintf "verified: Lean's kernel accepted %s (axioms: %s)" c.theorem (if c.axioms = [] then "none" else String.concat ", " c.axioms)
  | Status.Refuted, Some c ->
    let point = match Json.member "counterexample" job.verdict with Some ce -> String.concat ", " (Json.strings (Json.List (Json.list_or_empty "point" ce))) | None -> "" in
    let output = match Json.member "counterexample" job.verdict with Some ce -> String.concat ", " (Json.strings (Json.List (Json.list_or_empty "output" ce))) | None -> "" in
    Printf.sprintf "refuted: the input (%s) is mapped to (%s), outside the claimed box; Lean's kernel accepted %s" point output c.theorem
  | Status.Unknown, _ -> "unknown: " ^ job.reason
  | _ -> Status.to_string job.status ^ ": " ^ job.reason

(** The job in a few lines for a terminal. *)
let render job =
  let count name = List.length (Json.list_or_empty name job.verdict) in
  let neurons = List.fold_left (fun acc layer -> acc + List.length (Json.list_or_empty "neurons" layer)) 0 (Json.list_or_empty "network" job.verdict) in
  let box name =
    String.concat " x "
      (List.map (function Json.List [ Json.String lo; Json.String hi ] -> Printf.sprintf "[%s, %s]" lo hi | _ -> "?") (Json.list_or_empty name job.verdict))
  in
  String.concat "\n"
    ([ Printf.sprintf "network   %s: %d layer(s), %d neuron(s)" job.model (count "network") neurons; Printf.sprintf "property  %s" job.property;
       Printf.sprintf "  inputs in   %s" (box "input"); Printf.sprintf "  outputs in  %s" (box "output");
       Printf.sprintf "verifier  claims %s (%d ms)" (claim_name job.claim) job.verify_ms ]
    @ (match job.check with
       | Some c ->
         [ Printf.sprintf "Lean      %s %s (%d ms)" (if c.accepted then "accepted" else "did not accept") c.theorem c.elapsed_ms ]
         @ if c.statement = "" then [] else [ "  " ^ c.statement ]
       | None -> [ "Lean      was not asked: there is no claim to check" ])
    @ [ summary job ])
