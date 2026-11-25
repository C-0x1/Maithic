(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [formalize] command: a problem in words, stated in Lean and checked. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_formalize
open Cli_args
open Commands

let formalize_fault number ~exit message = Fault.make ~area:"FORMALIZE" ~number ~exit message

(* A name Lean takes for a theorem: letters, digits, the underscore and the prime, not beginning with a digit. *)
let lean_name text =
  let letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  text <> "" && letter text.[0] && String.for_all (fun c -> letter c || (c >= '0' && c <= '9') || c = '\'') text

let module_name text = text <> "" && List.for_all lean_name (String.split_on_char '.' text)

let number parsed name ~default ~least ~most =
  match get parsed name with
  | None -> Ok default
  | Some text ->
    (match int_of_string_opt text with
     | Some n when n >= least && n <= most -> Ok n
     | _ -> Error (usage_fault (Printf.sprintf "--%s needs a whole number from %d to %d, and got %s" name least most text)))

let formalize_cmd =
  { path = [ "formalize" ]; summary = "State a problem written in words as a Lean theorem, and check that the statement holds up."; synopsis = "[PROBLEM]";
    details =
      "The problem is taken from the argument, from --file, or from standard input, in plain words, Markdown or LaTeX. A model drafts \
       several statements that read the problem in different ways. Lean compiles each; one that does not compile is repaired by the \
       model, with Lean's error before it, within --rounds. Of two statements that are the same once definitions are unfolded, the \
       later is dropped. Each of the rest is checked: Lean looks for a contradiction among its hypotheses and for a counterexample, and a statement with \
       either is rejected. A statement that `trivial`, `rfl` or `simp` closes, one that leaves out a number of the problem, has a \
       variable nothing mentions, or uses an operation that does not mean what its symbol suggests, is flagged. A model then says in \
       English what the statement claims, seeing the statement alone, and a second call scores how well that agrees with the \
       problem. The best statement is chosen and printed with `sorry` for a proof: nothing here proves it. With --statement the \
       statements are yours and none is drafted; with --no-ai as well, only Lean is asked. The exit status is 0 for a statement \
       that no check flagged, and 7 when the chosen statement is flagged or no candidate is left.";
    options =
      [ value "file" "FILE" "Read the problem from FILE; - is standard input.";
        value "statement" "TEXT" "A statement to check instead of drafting: binders, a colon and the claim. May be given several times.";
        value "candidates" "N" "Statements asked of the model (3).";
        value "rounds" "N" "Repair rounds for a statement that does not compile (2).";
        value "name" "NAME" "The name the theorem is given (problem).";
        value "import" "MODULE" "A module the statements may use, besides the core library. May be given several times.";
        switch "no-ai" "Ask no model: check the statements given with --statement, with Lean alone.";
        value "write" "FILE" "Write the chosen statement to FILE as a Lean file.";
        value "out" "FILE" "Write the run as JSON to FILE as well.";
        value "project" "DIR" "A Lake project whose modules the statements may import." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* argument =
              match parsed.positional with
              | [] -> Ok None
              | [ text ] -> Ok (Some text)
              | _ -> Error (usage_fault "formalize takes one PROBLEM: put it in quotes, or give it with --file")
            in
            let* intake = Result.map_error (formalize_fault 1 ~exit:Exit_code.Usage) (Formalize_intake.read ~argument ~file:(get parsed "file") ()) in
            let standard = Formalize_run.default_settings in
            let* candidates = number parsed "candidates" ~default:standard.Formalize_run.candidates ~least:1 ~most:8 in
            let* rounds = number parsed "rounds" ~default:standard.Formalize_run.rounds ~least:0 ~most:5 in
            let name = Option.value (get parsed "name") ~default:standard.Formalize_run.name in
            let* () = if lean_name name then Ok () else Error (usage_fault (Printf.sprintf "--name needs a name Lean takes for a theorem, and got %s" name)) in
            let imports = get_all parsed "import" in
            let* () =
              match List.find_opt (fun m -> not (module_name m)) imports with
              | Some m -> Error (usage_fault (Printf.sprintf "--import needs the name of a module, such as Mathlib.Tactic, and got %s" m))
              | None -> Ok ()
            in
            let statements = get_all parsed "statement" in
            let use_ai = Config.bool ctx.config "prove.use_ai" in
            let* () =
              if (not use_ai) && statements = [] then Error (usage_fault "without a model there is nothing to draft: give the statements to check with --statement")
              else Ok ()
            in
            let* gateway = if use_ai then Result.map Option.some (Gateway.create ctx.config) else Ok None in
            let* bridge, _ = start_bridge ctx in
            let* session =
              match Formalize_tools.open_session ~limits:(limits ctx) bridge ~imports with
              | Ok session -> Ok session
              | Error problem ->
                Bridge_client.shutdown bridge;
                Error
                  (match problem with
                   | `Imports why -> formalize_fault 2 ~exit:Exit_code.Failure ("Lean could not load what the statements may import: " ^ why)
                   | `Commands why when Formalize_tools.library_missing why ->
                     formalize_fault 3 ~exit:Exit_code.Unavailable
                       "the statement checks are not on Lean's search path. They are part of the Maithic Lean package: run the Bridge from a \
                        build of the source tree, or give --project a Lake project that depends on Maithic"
                   | `Commands why -> formalize_fault 3 ~exit:Exit_code.Unavailable ("Lean could not load the statement checks: " ^ why))
            in
            let tools =
              Formalize_tools.tools ?gateway ~max_tokens:(Config.int ctx.config "gateway.max_tokens")
                ?statements:(if statements = [] then None else Some statements)
                ~name ~imports ~intake session
            in
            let outcome = Formalize_run.run ~settings:{ Formalize_run.candidates; rounds; name; imports } tools intake in
            Bridge_client.shutdown bridge;
            let* run = Result.map_error (fun why -> formalize_fault 4 ~exit:Exit_code.Provider ("no statement could be drafted: " ^ why)) outcome in
            let lean = Formalize_run.lean_text run in
            let* written =
              match (get parsed "write", lean) with
              | Some path, Some text -> Result.map (fun () -> Some path) (Result.map_error usage_fault (Fs.write path text))
              | _ -> Ok None
            in
            let document =
              match Formalize_run.to_json run with
              | Json.Obj members ->
                Json.Obj
                  (members
                   @ (match written with Some path -> [ ("written", Json.String path) ] | None -> [])
                   @ match gateway with Some gateway -> [ ("gateway", Gateway.account_to_json gateway) ] | None -> [])
              | other -> other
            in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else begin
              if not ctx.quiet then print_string (Formalize_run.render ~color:!Term.colour_enabled run ^ "\n");
              print_endline (Formalize_run.summary run);
              (match Formalize_run.chosen run with
               | Some entry when not ctx.quiet ->
                 let candidate = Formalize_run.final entry in
                 print_endline "";
                 print_endline (Printf.sprintf "theorem %s %s" name candidate.Formalize_candidate.signature);
                 print_endline
                   (Term.dim
                      (match written with
                       | Some path -> Printf.sprintf "wrote %s, with sorry for a proof" path
                       | None -> "nothing here proves it; --write FILE writes it as a Lean file, with sorry for a proof"))
               | _ -> ())
            end;
            Ok (Exit_code.to_int (Formalize_run.exit_code run)))) }

let all = [ formalize_cmd ]
