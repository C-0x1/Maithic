(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [council] command: a team of roles on one theorem, with Lean as the only judge. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_council
open Cli_args
open Commands

let council_fault number ~exit message = Fault.make ~area:"COUNCIL" ~number ~exit message

let whole parsed name ~default ~least =
  match get parsed name with
  | None -> Ok default
  | Some text ->
    (match int_of_string_opt text with
     | Some n when n >= least -> Ok n
     | _ -> Error (usage_fault (Printf.sprintf "--%s needs a whole number, %d or more, and got %s" name least text)))

let amount parsed name ~default =
  match get parsed name with
  | None -> Ok default
  | Some text ->
    (match float_of_string_opt text with
     | Some x when x >= 0.0 -> Ok x
     | _ -> Error (usage_fault (Printf.sprintf "--%s needs a number that is not negative, and got %s" name text)))

let council_cmd =
  { path = [ "council" ]; summary = "Put a team of roles to work on a theorem, with Lean as the only judge."; synopsis = "FILE";
    details =
      "The problem is the theorem of FILE that is named with --theorem, or the one theorem whose proof is unfinished. First the \
       verifier examines it without any model: Lean is asked whether the statement is well formed, a counterexample is looked for, \
       and the prover's automation has one attempt. Then rounds follow. In a round the researcher names lemmas of the library, and \
       each name is looked up in Lean; the prover proposes proofs; the verifier gives every proof to Lean; and the critic says why \
       the rejected ones failed, for the prover of the next round. Everything is posted on a shared blackboard and said in a \
       transcript. A claim is verified when Lean's kernel has accepted a proof of it within the trust policy, and by nothing a \
       member says. The answer is built from what Lean confirmed; the rest is set aside and counted. A budget limits tokens, cost, \
       time and the calls to Lean and to models; a limit of 0 is no limit. With --no-ai only the examination is made. The exit \
       status is 0 when the claim is verified, 8 when it is refuted, and 7 otherwise.";
    options =
      [ value "theorem" "NAME" "The theorem to work on.";
        value "goal" "TEXT" "What you are after, in words; passed on to every role that asks a model.";
        value "rounds" "N" "Rounds after the examination (3).";
        value "candidates" "N" "Proofs the prover asks for in one turn (3).";
        switch "no-ai" "Ask no model: the verifier examines the statement and automation tries.";
        value "budget-tokens" "N" "The most tokens all model calls together may use (200000).";
        value "budget-cost" "USD" "The most the tokens may cost, at the prices given (no limit).";
        value "budget-seconds" "S" "The longest the session may take (600).";
        value "budget-lean-calls" "N" "The most texts that may be given to Lean (200).";
        value "budget-model-calls" "N" "The most questions that may be put to a model (24).";
        value "price-input" "USD" "The price of a million tokens read, for --budget-cost (0).";
        value "price-output" "USD" "The price of a million tokens written, for --budget-cost (0).";
        value "transcript" "FILE" "Write the transcript as Markdown to FILE.";
        value "out" "FILE" "Write the session as JSON to FILE as well.";
        switch "write" "Write a verified proof back into FILE.";
        value "project" "DIR" "A Lake project whose modules the file may import.";
        value "policy" "FILE" "A trust policy file." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "council needs exactly one FILE argument") in
            let* source = read_source file in
            let* problem =
              Result.map_error (council_fault 1 ~exit:Exit_code.Usage) (Council_problem.of_source ?theorem:(get parsed "theorem") ?goal:(get parsed "goal") ~file source)
            in
            let standard = Council_session.default_settings and limits_ = Council_budget.default_limits in
            let* rounds = whole parsed "rounds" ~default:standard.Council_session.rounds ~least:0 in
            let* candidates = whole parsed "candidates" ~default:standard.Council_session.candidates ~least:1 in
            let* tokens = whole parsed "budget-tokens" ~default:limits_.Council_budget.tokens ~least:0 in
            let* cost = amount parsed "budget-cost" ~default:limits_.Council_budget.cost in
            let* seconds = amount parsed "budget-seconds" ~default:limits_.Council_budget.seconds in
            let* lean_calls = whole parsed "budget-lean-calls" ~default:limits_.Council_budget.lean_calls ~least:0 in
            let* model_calls = whole parsed "budget-model-calls" ~default:limits_.Council_budget.model_calls ~least:0 in
            let* input = amount parsed "price-input" ~default:0.0 in
            let* output = amount parsed "price-output" ~default:0.0 in
            let* policy = load_policy ctx in
            let use_ai = Config.bool ctx.config "prove.use_ai" in
            let* gateway = if use_ai then Result.map Option.some (Gateway.create ctx.config) else Ok None in
            let* located = locate_bridge ctx in
            let* bridge, _ = start_bridge ctx in
            let recheck = recheck_with ctx located in
            let budget = Council_budget.create ~prices:{ Council_budget.input; output } { Council_budget.tokens; cost; seconds; lean_calls; model_calls } in
            let prove = Council_real.prover ~recheck ~bridge ~policy (engine_settings ctx) problem in
            let* raw =
              match Council_real.raw ~limits:(limits ctx) ~recheck ~prove ~policy bridge problem with
              | Ok raw -> Ok raw
              | Error why ->
                Bridge_client.shutdown bridge;
                Error (council_fault 2 ~exit:Exit_code.Failure why)
            in
            let session =
              Council_session.run ~settings:{ Council_session.rounds; candidates }
                ?model:(Option.map (fun gateway -> Council_real.model gateway ~max_tokens:(Config.int ctx.config "gateway.max_tokens")) gateway)
                ~tools:(Council_tools.make ~budget raw) ~policy problem
            in
            Bridge_client.shutdown bridge;
            let proof = Option.map (fun (p : Council_board.item) -> p.Council_board.body) session.Council_session.answer.Council_synthesis.proof in
            let* written =
              match (has parsed "write", proof) with
              | true, Some proof -> Result.map (fun () -> true) (Result.map_error usage_fault (Fs.write file (Council_problem.with_proof problem proof)))
              | _ -> Ok false
            in
            let* () =
              match get parsed "transcript" with
              | Some path -> Result.map_error usage_fault (Fs.write path (Council_session.to_markdown session))
              | None -> Ok ()
            in
            let document =
              match Council_session.to_json session with
              | Json.Obj members ->
                Json.Obj (members @ [ ("written", Json.Bool written) ] @ match gateway with Some gateway -> [ ("gateway", Gateway.account_to_json gateway) ] | None -> [])
              | other -> other
            in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else begin
              if not ctx.quiet then print_string (Council_session.render ~color:!Term.colour_enabled session ^ "\n");
              print_endline (Council_session.summary session);
              if not ctx.quiet then begin
                (match get parsed "transcript" with Some path -> print_endline (Term.dim (Printf.sprintf "wrote the transcript to %s" path)) | None -> ());
                if written then print_endline (Term.dim (Printf.sprintf "wrote the proof into %s" file))
                else if proof <> None then print_endline (Term.dim "the file was not changed; --write writes the proof into it")
              end
            end;
            Ok (Exit_code.to_int (Council_session.exit_code session)))) }

let all = [ council_cmd ]
