(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [guide] commands: train a tactic model, look into one, and ask one
    about a goal. `prove --guide` is where a model guides a search. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_prover
open Maithic_guide
open Cli_args
open Commands

let ranking_schema = "maithic.guide.ranking/1"
let model_fault message = guide_fault 2 ~exit:Exit_code.Usage message
let percent share = Printf.sprintf "%.1f%%" (100.0 *. share)

(* ---- guide info ---- *)

let info_cmd =
  { path = [ "guide"; "info" ]; summary = "Show what a tactic model is made of."; synopsis = "MODEL";
    details =
      "MODEL is a file written by `maithic guide train`. The command prints the shape of the model and the tactic templates it \
       predicts, with how often each was seen in training and how often its tactic closed a goal. It reads the file and runs \
       nothing: neither Julia nor Lean is needed.";
    options = [ value "top" "N" "The number of templates shown, most frequent first (15)." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault "guide info needs exactly one MODEL argument") in
            let* top =
              match get parsed "top" with
              | None -> Ok 15
              | Some text ->
                (match int_of_string_opt text with Some n when n >= 0 -> Ok n | _ -> Error (usage_fault ("--top needs a number, and got " ^ text)))
            in
            let* model = Result.map_error model_fault (Guide_model.load file) in
            if ctx.json then Term.print_json (Guide_model.to_json model) else print_string (Guide_model.render ~color:!Term.colour_enabled ~top model);
            Ok 0)) }

(* ---- guide rank ---- *)

(* The first goal of a theorem of a Lean file, as the search would meet it. *)
let goal_of_theorem ctx file name =
  let* source = read_source file in
  let* decl = Result.map_error (fun e -> Fault.make ~area:"PROVE" ~number:1 ~exit:Exit_code.Usage e) (Lean_source.find source name) in
  let* bridge, _ = start_bridge ctx in
  let bridge_fault = Bridge_error.to_fault in
  let result =
    let* report, env = Result.map_error bridge_fault (Bridge_client.check_file ~keep:true ~limits:(limits ctx) bridge decl.Lean_source.prelude) in
    match env with
    | Some env when report.Bridge_types.ok ->
      let* _, opened = Result.map_error bridge_fault (Bridge_client.start_proof ~limits:(limits ctx) bridge ~env (`Statement decl.Lean_source.header)) in
      (match opened with
       | { Bridge_types.goals = goal :: _; _ } :: _ -> Ok goal
       | _ -> Error (guide_fault 3 ~exit:Exit_code.Failure (Printf.sprintf "the statement of '%s' opens no goal" name)))
    | _ -> Error (Fault.make ~area:"PROVE" ~number:2 ~exit:Exit_code.Failure (Printf.sprintf "the file has errors before theorem '%s'" name))
  in
  Bridge_client.shutdown bridge;
  result

type row = { tactic : string; template : string; prior : float; base : float option; guided : float option }

let rank_cmd =
  { path = [ "guide"; "rank" ]; summary = "Ask a tactic model which tactics fit a goal."; synopsis = "MODEL [TACTIC...]";
    details =
      "The goal is given as text with --goal or --goal-file, as Lean prints it, or taken from a theorem of a Lean file with \
       --file and --theorem. Each TACTIC gets the probability the model gives it among the tactics named. Without a TACTIC the \
       command ranks the candidates the step-wise search would try on the goal, when the goal comes from a Lean file, and shows \
       the prior of the search beside the prior the guide turns it into; for a goal given as text it ranks the templates the \
       model knows. The value is the model's estimate of the chance that the next tactic closes a goal. What a model says is \
       advice on the order of a search: nothing here is checked by Lean, and nothing needs to be.";
    options =
      [ value "goal" "TEXT" "The goal, as Lean prints it: hypotheses, then the target after the turnstile.";
        value "goal-file" "FILE" "A file that holds the goal.";
        value "file" "FILE" "A Lean file; the goal is that of the theorem named with --theorem.";
        value "theorem" "NAME" "The theorem of the Lean file whose goal is ranked.";
        value "top" "N" "The number of candidates shown, most probable first (10).";
        value "project" "DIR" "A Lake project whose modules the Lean file may import." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file, tactics =
              match parsed.positional with file :: tactics -> Ok (file, tactics) | [] -> Error (usage_fault "guide rank needs a MODEL argument")
            in
            let* top =
              match get parsed "top" with
              | None -> Ok 10
              | Some text ->
                (match int_of_string_opt text with Some n when n >= 1 -> Ok n | _ -> Error (usage_fault ("--top needs a number, and got " ^ text)))
            in
            let* model = Result.map_error model_fault (Guide_model.load file) in
            let* state, searched =
              match get parsed "goal", get parsed "goal-file", get parsed "file", get parsed "theorem" with
              | Some text, None, None, None -> Ok (text, None)
              | None, Some path, None, None -> Result.map (fun text -> (String.trim text, None)) (read_source path)
              | None, None, Some lean, Some name ->
                Result.map (fun goal -> (Guide_prior.state_text goal, Some (Lean_search.shape_proposer goal))) (goal_of_theorem ctx lean name)
              | None, None, Some _, None -> Error (usage_fault "--file needs --theorem NAME")
              | None, None, None, None -> Error (usage_fault "guide rank needs a goal: --goal TEXT, --goal-file FILE, or --file FILE with --theorem NAME")
              | _ -> Error (usage_fault "give the goal once: with --goal, with --goal-file, or with --file and --theorem")
            in
            (* What is ranked: the tactics named; or what the search would try; or the model's own templates. *)
            let base =
              match tactics, searched with
              | _ :: _, _ -> List.map (fun t -> (t, None)) tactics
              | [], Some candidates -> List.map (fun (t, prior) -> (t, Some prior)) candidates
              | [], None -> List.filteri (fun i _ -> i > 0) model.Guide_model.templates |> List.map (fun (c : Guide_model.template) -> (c.Guide_model.name, None))
            in
            let* () = if base = [] then Error (guide_fault 3 ~exit:Exit_code.Failure "there is nothing to rank: the search has no candidate for this goal") else Ok () in
            let* client = Result.map_error (guide_fault 3 ~exit:Exit_code.Failure) (guide_connect ctx file) in
            let asked =
              let* ranking = Guide_client.tactic client ~state (List.map fst base) in
              let* value = Guide_client.value client ~state in
              Ok (ranking, value)
            in
            Guide_client.close client;
            let* ranking, value = Result.map_error (guide_fault 3 ~exit:Exit_code.Failure) asked in
            let guided =
              if List.for_all (fun (_, prior) -> prior <> None) base then
                Some (Guide_prior.reweigh (List.map (fun (t, prior) -> (t, Option.get prior)) base) ranking.Guide_client.priors)
              else None
            in
            let rows =
              List.mapi
                (fun i (tactic, prior) ->
                  { tactic; template = List.nth ranking.Guide_client.templates i; prior = List.nth ranking.Guide_client.priors i; base = prior;
                    guided = Option.bind guided (List.assoc_opt tactic) })
                base
              |> List.stable_sort (fun a b -> compare (b.guided, b.prior) (a.guided, a.prior))
            in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ("schema", Json.String ranking_schema); ("model", Json.String file); ("goal", Json.String state); ("value", Json.Float value);
                     ( "candidates",
                       Json.List
                         (List.map
                            (fun r ->
                              Json.Obj
                                ([ ("tactic", Json.String r.tactic); ("template", Json.String r.template); ("prior", Json.Float r.prior) ]
                                 @ (match r.base with Some b -> [ ("search_prior", Json.Float b) ] | None -> [])
                                 @ match r.guided with Some g -> [ ("guided_prior", Json.Float g) ] | None -> []))
                            rows) ) ])
            else begin
              print_endline (Term.bold "goal");
              print_endline (Text.indent 2 state);
              print_newline ();
              let shown = List.filteri (fun i _ -> i < top) rows in
              let with_search = guided <> None in
              let number = Printf.sprintf "%.2f" in
              print_string
                (Term.table
                   (([ "tactic"; "read as"; "model" ] @ if with_search then [ "search"; "guided" ] else [])
                    :: List.map
                         (fun r ->
                           [ r.tactic; r.template; percent r.prior ]
                           @ if with_search then [ number (Option.value r.base ~default:0.0); number (Option.value r.guided ~default:0.0) ] else [])
                         shown));
              if List.length rows > top then print_endline (Term.dim (Printf.sprintf "and %d more" (List.length rows - top)));
              print_newline ();
              Printf.printf "%s  %s  %s\n" (Term.bold "value") (percent value) (Term.dim "the model's estimate that the next tactic closes a goal")
            end;
            Ok 0)) }

(* ---- guide train ---- *)

let train_options =
  [ ("name", "NAME", "The name of the run in its record."); ("seed", "N", "The seed of the run: it decides the split of the data and the first weights (0).");
    ("epochs", "N", "Passes over the training examples (30)."); ("batch", "N", "Examples in one step of the optimizer (16).");
    ("dim", "N", "The size of the model's vectors (32)."); ("max-length", "N", "The longest sequence of tokens the model reads (128).");
    ("pooling", "KIND", "How a sequence becomes one vector: mean or first."); ("mixer", "KIND", "A layer before the pooling: none or attention.");
    ("rate", "X", "The learning rate (0.01)."); ("weight-decay", "X", "The weight decay (0.0001)."); ("k", "N", "The k of the top-k accuracy that is reported (5).") ]

let train_cmd =
  { path = [ "guide"; "train" ]; summary = "Train a tactic model on a dataset of proofs."; synopsis = "DATASET OUT";
    details =
      "DATASET is a directory written by `maithic corpus harvest`. The state-and-tactic pairs of its complete proofs are split by \
       theorem into a training, a validation and a test part; a small classifier is trained to predict the template of the next \
       tactic from the goal, and the model that did best on the validation part is written to OUT/model.json with the record of \
       the run in OUT/run.json. The record gives the accuracy on the test part beside a baseline that knows no goal. Training \
       runs in Julia, takes seconds, and is decided by the seed: the same command writes the same files.";
    options = List.map (fun (long, metavar, doc) -> value long metavar doc) train_options;
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* dataset, out =
              match parsed.positional with [ dataset; out ] -> Ok (dataset, out) | _ -> Error (usage_fault "guide train needs a DATASET and an OUT directory")
            in
            let* () =
              if Fs.exists Fs.(dataset / "manifest.json") then Ok ()
              else Error (usage_fault (Printf.sprintf "%s is not a dataset: it has no manifest.json; `maithic corpus harvest` writes one" dataset))
            in
            let* engine = guide_engine ctx in
            let* () = Result.map_error (guide_fault 1 ~exit:Exit_code.Toolchain) (Julia_package.prepare engine) in
            let options =
              List.concat_map (fun (long, _, _) -> match get parsed long with Some v -> [ "--" ^ long; v ] | None -> []) train_options
            in
            let program, args = Guide_service.train_command engine ~dataset ~out options in
            let* output =
              Result.map_error (fun e -> guide_fault 4 ~exit:Exit_code.Failure ("the training could not be run: " ^ e))
                (Proc.run ~env:Julia_package.offline ~timeout:(Float.max 3600.0 (Config.float ctx.config "guide.timeout_s")) program args)
            in
            if output.Proc.status <> 0 then
              Error
                (guide_fault 4 ~exit:Exit_code.Failure
                   (match String.trim output.Proc.stderr with "" -> Printf.sprintf "the training stopped with status %d" output.Proc.status | said -> Text.truncate 600 said))
            else begin
              if ctx.json then
                Term.print_json
                  (match Result.bind (Fs.read Fs.(out / "run.json")) Json.parse with
                   | Ok record -> record
                   | Error _ -> Json.Obj [ ("model", Json.String Fs.(out / "model.json")) ])
              else if not ctx.quiet then print_string output.Proc.stdout;
              Ok 0
            end)) }

let all = [ info_cmd; rank_cmd; train_cmd ]
