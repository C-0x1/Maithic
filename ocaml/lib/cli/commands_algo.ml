(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [algo] commands: an implementation drafted by a model and judged by
    Lean, and the verified library tested from outside. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_algolab
open Cli_args
open Commands

let algo_fault number ~exit message = Fault.make ~area:"ALGOLAB" ~number ~exit message

let number parsed name ~least ~default =
  match get parsed name with
  | None -> Ok default
  | Some text ->
    (match int_of_string_opt text with
     | Some n when n >= least -> Ok n
     | _ -> Error (usage_fault (Printf.sprintf "--%s needs a whole number, %d or more, and got %s" name least text)))

(* ---- algo synth ---- *)

let synth_cmd =
  { path = [ "algo"; "synth" ]; summary = "Have a model write a Lean function, and let Lean and your examples judge it."; synopsis = "TASK";
    details =
      "TASK is a TOML file: the `name` of the function, a `description` of what it should do, and when you have them its \
       `signature`, a list of `constraints`, and a table [[example]] for every example, with a `call` and what the call should \
       give under `expect`. A model drafts the definition. A draft that contains anything but one definition, or that could \
       reach outside a computation, is refused unseen by Lean. Otherwise Lean compiles it and runs every example. What went \
       wrong, an error of Lean or an example with the wrong answer, goes back to the model with the draft, and it is asked for a \
       correction, --rounds times at most. The function is accepted when it compiles and every example gives what was expected: \
       that is a test, and nothing is proved. With --given the first implementation is one you wrote, and with --no-ai as well \
       no model is asked at all. The command exits with status 7 when nothing was accepted.";
    options =
      [ value "rounds" "N" "Corrections the model may be asked for (3).";
        value "given" "FILE" "Start from the implementation in FILE instead of a draft.";
        switch "no-ai" "Ask no model: judge the implementation of --given, once.";
        value "import" "MODULE" "A module the function may use (repeatable).";
        value "write" "FILE" "Write the accepted function, with its examples as checks, to FILE.";
        value "out" "FILE" "Write the record as JSON to FILE as well.";
        value "project" "DIR" "A Lake project whose modules may be imported." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "algo synth needs exactly one TASK argument") in
            let* task = Result.map_error (algo_fault 1 ~exit:Exit_code.Usage) (Algolab_task.read file) in
            let* rounds = number parsed "rounds" ~least:0 ~default:Algolab_synth.default_rounds in
            let use_ai = Config.bool ctx.config "prove.use_ai" in
            let* given = match get parsed "given" with Some path -> Result.map (fun text -> Some (Text.trim text)) (read_source path) | None -> Ok None in
            let* () = if (not use_ai) && given = None then Error (usage_fault "--no-ai needs --given: there is nothing to judge") else Ok () in
            let* gateway = Gateway.create ctx.config in
            (* The first Bridge is started here, so that a missing Lean is reported as such. *)
            let* first, _ = start_bridge ctx in
            let pending = ref (Some first) in
            let start () =
              match !pending with
              | Some bridge ->
                pending := None;
                Ok bridge
              | None -> Result.map fst (Result.map_error (fun (f : Fault.t) -> f.Fault.message) (start_bridge ctx))
            in
            let session = Algolab_lean.session ~limits:(limits ctx) ~imports:(get_all parsed "import") start in
            let tools = Algolab_lean.tools ?gateway:(if use_ai then Some gateway else None) ~max_tokens:(Config.int ctx.config "gateway.max_tokens") session task in
            let synth = Algolab_synth.run ~rounds:(if use_ai then rounds else 0) ?given tools task in
            Algolab_lean.close session;
            (match !pending with Some bridge -> Bridge_client.shutdown bridge | None -> ());
            let* () =
              match (synth.Algolab_synth.attempts, synth.Algolab_synth.status) with
              | [], Algolab_synth.Failed why -> Error (algo_fault 2 ~exit:Exit_code.Provider why)
              | _ -> Ok ()
            in
            let written = match (get parsed "write", Algolab_synth.lean_text synth) with Some path, Some text -> Some (path, text) | _ -> None in
            let* () = match written with Some (path, text) -> Result.map_error usage_fault (Fs.write path text) | None -> Ok () in
            let document =
              match Algolab_synth.to_json synth with Json.Obj members -> Json.Obj (members @ [ ("gateway", Gateway.account_to_json gateway) ]) | other -> other
            in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty document ^ "\n")) | None -> Ok () in
            if ctx.json then Term.print_json document
            else begin
              if not ctx.quiet then print_endline (Algolab_synth.render synth);
              print_endline ((if Algolab_synth.accepted synth then Term.green else Term.coral) (Algolab_synth.summary synth));
              if not ctx.quiet then
                match (written, get parsed "write") with
                | Some (path, _), _ -> print_endline (Term.dim (Printf.sprintf "wrote %s; `maithic check %s` runs the examples again" path path))
                | None, Some _ -> print_endline (Term.dim "nothing was written: no implementation was accepted")
                | None, None -> ()
            end;
            Ok (if Algolab_synth.accepted synth then 0 else Exit_code.to_int Exit_code.Not_verified))) }

(* ---- algo differential ---- *)

let differential_cmd =
  { path = [ "algo"; "differential" ]; summary = "Test the compiled verified algorithms against independent references."; synopsis = "";
    details =
      "The algorithms of the verified library are compiled by Lake into one program, `maithic-algo`, that answers requests such \
       as `merge_sort 3 1 2`. This command generates inputs from a seed, sends each request to that program, and holds the \
       answer against a reference implementation in Julia that shares no code with the Lean one. Where several answers are \
       right, as for a path in a graph, the reference checks the answer instead of comparing it. A proof says the Lean \
       definition is right; this test says the compiled program, the parser of requests and the printer of answers are too. \
       The command exits with status 7 when an answer is not accepted.";
    options =
      [ value "exe" "FILE" "The program to test (the `maithic-algo` of the Lean project).";
        value "seed" "N" "The seed of the generated inputs (1).";
        value "count" "N" "Cases in all, shared out over the algorithms (190).";
        value "size" "N" "How large an input may be (12).";
        value "algorithm" "NAME" "Test this algorithm only (repeatable).";
        value "out" "FILE" "Write the report as JSON to FILE as well." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* () = if parsed.positional <> [] then Error (usage_fault "algo differential takes no argument") else Ok () in
            let standard = Algolab_differential.default_settings in
            let* seed = number parsed "seed" ~least:0 ~default:standard.Algolab_differential.seed in
            let* count = number parsed "count" ~least:1 ~default:standard.Algolab_differential.count in
            let* size = number parsed "size" ~least:1 ~default:standard.Algolab_differential.size in
            let* executable =
              match get parsed "exe" with
              | Some path -> Ok path
              | None ->
                (* Lake builds the program next to the Bridge. *)
                Result.map (fun (located : Bridge_locate.located) -> Algolab_differential.beside located.Bridge_locate.path) (locate_bridge ctx)
            in
            let* () =
              if Fs.exists executable then Ok ()
              else Error (algo_fault 3 ~exit:Exit_code.Toolchain (Printf.sprintf "%s is not there: build it with `lake build maithic-algo` in the Lean project" executable))
            in
            let project = Config.string ctx.config "algolab.project" in
            let* engine =
              Result.map_error
                (algo_fault 4 ~exit:Exit_code.Toolchain)
                (Algolab_differential.locate ~julia:(Config.string ctx.config "oracle.julia")
                   ~project:(if project = "" then "" else Config.resolve_path ctx.config project)
                   ~cwd:(Sys.getcwd ()) ())
            in
            let* () = Result.map_error (algo_fault 4 ~exit:Exit_code.Toolchain) (Algolab_differential.prepare engine) in
            let settings = { Algolab_differential.seed; count; size; algorithms = get_all parsed "algorithm" } in
            let* report = Result.map_error (algo_fault 5 ~exit:Exit_code.Failure) (Algolab_differential.run ~settings engine ~executable ()) in
            let* () = match get parsed "out" with Some out -> Result.map_error usage_fault (Fs.write out (Json.pretty report ^ "\n")) | None -> Ok () in
            let agrees = Algolab_differential.agrees report in
            if ctx.json then Term.print_json report
            else begin
              if not ctx.quiet then begin
                let rows =
                  List.map
                    (fun (name, cases, bad) -> [ name; string_of_int cases; (if bad = 0 then Term.green "0" else Term.coral (string_of_int bad)) ])
                    (Algolab_differential.rows report)
                in
                print_string (Term.table ([ "algorithm"; "cases"; "differ" ] :: rows));
                List.iter (fun line -> print_endline (Term.coral line)) (Algolab_differential.failures report)
              end;
              print_endline ((if agrees then Term.green else Term.coral) (Algolab_differential.summary report))
            end;
            Ok (if agrees then 0 else Exit_code.to_int Exit_code.Not_verified))) }

let all = [ synth_cmd; differential_cmd ]
