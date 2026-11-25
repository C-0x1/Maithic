(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_gateway
open Maithic_ledger

type settings = {
  use_ai : bool;
  rounds : int;
  candidates : int;
  max_tokens : int;
  limits : Bridge_types.limits;
  search : bool;  (* search one tactic at a time when the portfolio fails *)
  search_expansions : int;
}

let default_settings =
  { use_ai = true; rounds = 3; candidates = 4; max_tokens = 2048; limits = Bridge_types.no_limits; search = false;
    search_expansions = 60 }

(* The name under which proofs found by the step-wise search are recorded. *)
let search_name = "search"

type result = {
  theorem : string;
  statement : string;
  line : int;
  status : Status.t;
  proof : string option;
  proposer : string option;
  axioms : string list;
  decision : Trust_policy.decision;
  recheck : Recheck.outcome option;
  attempts : Attempts.attempt list;
  rounds_used : int;
  ai_note : string;
  elapsed_ms : int;
}

let prove_fault number ?hint ~exit message = Fault.make ?hint ~area:"PROVE" ~number ~exit message
let bridge_fault e = Bridge_error.to_fault e
let ( let* ) = Result.bind

type advisor = string * (env:int -> Lean_source.decl -> string list)
type accepted = { proof : string; by : string; decl : Bridge_types.decl }

(* Sends one batch of candidates to Lean and records every outcome. Returns
   the first candidate that Lean accepts and the policy does not reject. *)
let try_batch ~bridge ~env ~policy ~settings ~memory ~on_attempt ~round ~proposer (decl : Lean_source.decl) proofs =
  let record attempt =
    Attempts.record memory attempt;
    on_attempt attempt
  in
  let screened, sendable =
    List.partition_map
      (fun proof ->
        match Screen.violations proof with
        | [] -> Either.Right proof
        | finding :: _ -> Either.Left (proof, finding.Screen.reason))
      proofs
  in
  List.iter
    (fun (proof, reason) ->
      record
        { Attempts.round; proposer; proof; accepted = false; error_class = Some Attempts.Guard_rejected; error = reason; elapsed_ms = 0 })
    screened;
  if sendable = [] then Ok None
  else begin
    let candidates =
      List.map (fun proof -> { Bridge_types.source = Lean_source.assemble decl proof; untrusted = proof }) sendable
    in
    let* reports =
      Result.map_error bridge_fault
        (Bridge_client.try_candidates ~stop_on_first:true ~limits:settings.limits bridge ~env candidates)
    in
    let winner = ref None in
    List.iter
      (fun (cr : Bridge_types.candidate_report) ->
        match List.nth_opt sendable cr.Bridge_types.index with
        | None -> ()
        | Some proof ->
          let failed ?(elapsed_ms = 0) error_class error =
            record { Attempts.round; proposer; proof; accepted = false; error_class = Some error_class; error; elapsed_ms }
          in
          if not cr.Bridge_types.admitted then failed Attempts.Guard_rejected cr.Bridge_types.rejection
          else
            match cr.Bridge_types.report with
            | None -> failed Attempts.Other_error "the Bridge returned no report"
            | Some report ->
              let elapsed_ms = report.Bridge_types.elapsed_ms in
              if not report.Bridge_types.ok then begin
                let text = Bridge_types.error_text report in
                failed ~elapsed_ms (Attempts.classify text) text
              end
              else
                (match Recheck.find_decl report decl.Lean_source.name with
                 | None -> failed ~elapsed_ms Attempts.Other_error "Lean accepted the text but did not produce the declaration"
                 | Some d ->
                   let facts =
                     { Trust_policy.declaration = d.Bridge_types.name; found = true; kind = d.Bridge_types.kind;
                       axioms = d.Bridge_types.axioms; errors = report.Bridge_types.errors; rechecked = false }
                   in
                   let decision = Trust_policy.decide policy facts in
                   if decision.Trust_policy.status = Status.Failed then
                     failed ~elapsed_ms
                       (if d.Bridge_types.uses_sorry then Attempts.Not_closed else Attempts.Other_error)
                       (String.concat "; " decision.Trust_policy.reasons)
                   else begin
                     record { Attempts.round; proposer; proof; accepted = true; error_class = None; error = ""; elapsed_ms };
                     if !winner = None then winner := Some { proof; by = proposer; decl = d }
                   end))
      reports;
    Ok !winner
  end

let prove ?recheck ?gateway ?(advisors = []) ?proposer ?(on_attempt = fun _ -> ()) ?(on_search = fun _ -> ()) ~bridge ~policy settings ~source
    name =
  let started = Unix.gettimeofday () in
  let elapsed () = int_of_float ((Unix.gettimeofday () -. started) *. 1000.0) in
  let* decl =
    Result.map_error (fun e -> prove_fault 1 ~exit:Exit_code.Usage e) (Lean_source.find source name)
  in
  (* The environment the theorem is stated in: everything before it. *)
  let* env =
    let* report, env =
      Result.map_error bridge_fault (Bridge_client.check_file ~keep:true ~limits:settings.limits bridge decl.Lean_source.prelude)
    in
    match env with
    | Some env when report.Bridge_types.ok -> Ok env
    | _ ->
      Error
        (prove_fault 2 ~exit:Exit_code.Failure
           ~hint:"Fix the errors above the theorem first; `maithic check FILE` lists them."
           (Printf.sprintf "the file has errors before theorem '%s':\n%s" decl.Lean_source.name
              (Text.truncate 800 (Bridge_types.error_text report))))
  in
  let memory = Attempts.create () in
  let batch = try_batch ~bridge ~env ~policy ~settings ~memory ~on_attempt decl in
  let* winner = batch ~round:0 ~proposer:Portfolio.name (Attempts.fresh memory (Portfolio.candidates decl.Lean_source.header)) in
  (* Advisors: sources of proofs outside the prover, such as an oracle with a certificate. What
     they propose is checked like everything else. *)
  let* winner =
    List.fold_left
      (fun acc (advisor_name, advise) ->
        let* winner = acc in
        match winner with
        | Some _ -> Ok winner
        | None ->
          (match Attempts.fresh memory (advise ~env decl) with
           | [] -> Ok None
           | proposed -> batch ~round:0 ~proposer:advisor_name proposed))
      (Ok winner) advisors
  in
  (* The step-wise search. What it finds is a proposal like any other: it goes through the same
     screen, kernel check and policy as a proof written out in full. *)
  let* winner =
    match winner with
    | Some _ -> Ok winner
    | None when settings.search ->
      let budget = { Lean_search.default_budget with Search_kernel.expansions = settings.search_expansions } in
      let* found =
        Result.map_error bridge_fault (Lean_search.prove_statement ~budget ?proposer ~limits:settings.limits bridge ~env decl.Lean_source.header)
      in
      Option.iter on_search found.Lean_search.record;
      (match found.Lean_search.proof with
       | Some proof -> batch ~round:0 ~proposer:search_name (Attempts.fresh memory [ proof ])
       | None -> Ok None)
    | None -> Ok None
  in
  let rounds_used = ref 0 and ai_note = ref "" in
  let* winner =
    match winner with
    | Some _ -> Ok winner
    | None ->
      if not settings.use_ai then begin
        ai_note := "the model was not asked (prove.use_ai is false)";
        Ok None
      end
      else
        (match gateway with
         | None -> ai_note := "the model was not asked (no gateway)"; Ok None
         | Some gateway ->
           (match Gateway.ready gateway with
            | Error e -> ai_note := "the model was not asked: " ^ Gateway_types.error_to_string e; Ok None
            | Ok () ->
              let rec round n =
                if n > settings.rounds then Ok None
                else begin
                  rounds_used := n;
                  let failures =
                    if n = 1 then ""
                    else
                      let shown = Attempts.create () in
                      List.iter (fun (a : Attempts.attempt) -> if a.Attempts.round >= 1 then Attempts.record shown a) (Attempts.all memory);
                      Attempts.render_failures shown
                  in
                  match
                    Model_proposer.propose gateway ~max_tokens:settings.max_tokens ~count:settings.candidates
                      ~statement:(Lean_source.statement decl) ~prelude:decl.Lean_source.prelude ~failures
                  with
                  | Error e -> ai_note := "the model call failed: " ^ Gateway_types.error_to_string e; Ok None
                  | Ok proposed ->
                    (match Attempts.fresh memory proposed with
                     | [] -> ai_note := Printf.sprintf "the model proposed nothing new in round %d" n; Ok None
                     | fresh ->
                       let* winner = batch ~round:n ~proposer:Model_proposer.name fresh in
                       (match winner with Some _ -> Ok winner | None -> round (n + 1)))
                end
              in
              round 1))
  in
  ignore (Bridge_client.release_env bridge env);
  let failed reasons =
    { theorem = decl.Lean_source.name; statement = Lean_source.statement decl; line = decl.Lean_source.line; status = Status.Failed;
      proof = None; proposer = None; axioms = [];
      decision = { Trust_policy.status = Status.Failed; reasons; violations = [] };
      recheck = None; attempts = Attempts.all memory; rounds_used = !rounds_used; ai_note = !ai_note; elapsed_ms = elapsed () }
  in
  match winner with
  | None ->
    let tried = List.length (Attempts.all memory) in
    Ok (failed [ Printf.sprintf "no proof was found in %d attempt(s)" tried ])
  | Some { proof; by; decl = d } ->
    let facts rechecked axioms errors =
      { Trust_policy.declaration = d.Bridge_types.name; found = true; kind = d.Bridge_types.kind; axioms; errors; rechecked }
    in
    let finish ?recheck_outcome decision axioms =
      { theorem = d.Bridge_types.name; statement = d.Bridge_types.type_; line = decl.Lean_source.line;
        status = decision.Trust_policy.status; proof = Some proof; proposer = Some by; axioms; decision;
        recheck = recheck_outcome; attempts = Attempts.all memory; rounds_used = !rounds_used; ai_note = !ai_note;
        elapsed_ms = elapsed () }
    in
    (match recheck with
     | Some run when policy.Trust_policy.require_recheck ->
       let assembled = decl.Lean_source.prelude ^ Lean_source.assemble decl proof in
       (match run ~source:assembled decl.Lean_source.name with
        | Error e ->
          Ok
            (finish
               { Trust_policy.status = Status.Checked;
                 reasons = [ "the fresh-process re-check could not run: " ^ Bridge_error.to_string e ]; violations = [] }
               d.Bridge_types.axioms)
        | Ok outcome ->
          if not outcome.Recheck.passed then
            Ok
              { (finish ~recheck_outcome:outcome
                   { Trust_policy.status = Status.Failed;
                     reasons = [ "the fresh-process re-check rejected the proof: " ^ Text.truncate 400 outcome.Recheck.first_error ];
                     violations = [] }
                   outcome.Recheck.axioms)
                with proof = Some proof }
          else if outcome.Recheck.statement <> d.Bridge_types.type_ then
            Ok
              (finish ~recheck_outcome:outcome
                 { Trust_policy.status = Status.Failed;
                   reasons =
                     [ Printf.sprintf "the re-check proved a different statement: '%s' instead of '%s'" outcome.Recheck.statement
                         d.Bridge_types.type_ ];
                   violations = [] }
                 outcome.Recheck.axioms)
          else
            Ok
              (finish ~recheck_outcome:outcome
                 (Trust_policy.decide policy (facts true outcome.Recheck.axioms outcome.Recheck.errors))
                 outcome.Recheck.axioms))
     | _ -> Ok (finish (Trust_policy.decide policy (facts false d.Bridge_types.axioms 0)) d.Bridge_types.axioms))

let result_to_json ?file r =
  Json.Obj
    ([ ("schema", Json.String "maithic.prove.result/1") ]
     @ (match file with Some f -> [ ("file", Json.String f) ] | None -> [])
     @ [ ("theorem", Json.String r.theorem); ("statement", Json.String r.statement); ("line", Json.Int r.line);
         ("status", Status.to_json r.status) ]
     @ (match r.proof with Some p -> [ ("proof", Json.String p) ] | None -> [])
     @ (match r.proposer with Some p -> [ ("proposer", Json.String p) ] | None -> [])
     @ [ ("axioms", Json.List (List.map (fun a -> Json.String a) r.axioms));
         ("decision", Trust_policy.decision_to_json r.decision) ]
     @ (match r.recheck with Some o -> [ ("recheck", Recheck.to_json o) ] | None -> [])
     @ [ ("attempts", Json.List (List.map Attempts.attempt_to_json r.attempts));
         ("rounds_used", Json.Int r.rounds_used) ]
     @ (if r.ai_note = "" then [] else [ ("ai_note", Json.String r.ai_note) ])
     @ [ ("elapsed_ms", Json.Int r.elapsed_ms) ])
