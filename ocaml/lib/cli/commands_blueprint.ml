(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [blueprint] commands: the plan of a formalization as a graph, its Lean
    skeleton, and where the work stands. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_gateway
open Maithic_blueprint
open Cli_args
open Commands

let blueprint_fault number ~exit message = Fault.make ~area:"BLUEPRINT" ~number ~exit message

(* A blueprint that parses; what it says is not checked yet. *)
let read_blueprint file =
  let* text = read_source file in
  Result.map_error (fun e -> blueprint_fault 1 ~exit:Exit_code.Usage (Printf.sprintf "%s is not a blueprint: %s" file e)) (Blueprint_file.parse text)

(* A blueprint the other commands can build on. *)
let sound_blueprint file =
  let* doc = read_blueprint file in
  match Blueprint_check.validate doc with
  | [] -> Ok doc
  | problems ->
    Error
      (blueprint_fault 2 ~exit:Exit_code.Usage
         (Printf.sprintf "%s has %d problem(s): %s" file (List.length problems) (String.concat "; " (List.map Blueprint_check.describe problems))))

let one_file parsed what = match parsed.positional with [ file ] -> Ok file | _ -> Error (usage_fault (Printf.sprintf "%s needs exactly one FILE argument" what))
let write_text path text = Result.map_error usage_fault (Fs.write path text)

(* What Lean reports about the declarations of a file. *)
let lean_status ctx ~file source =
  let* bridge, _ = start_bridge ctx in
  let extracted = Bridge_client.extract ~file ~limits:(limits ctx) bridge source in
  Bridge_client.shutdown bridge;
  let* extraction = Result.map_error Bridge_error.to_fault extracted in
  let decls =
    List.map
      (fun (d : Extraction.declaration) ->
        { Blueprint_status.name = d.Extraction.name; own_sorry = Extraction.contains_sorry d; uses_sorry = d.Extraction.uses_sorry;
          deps = d.Extraction.type_deps @ d.Extraction.value_deps })
      extraction.Extraction.declarations
  in
  Ok (decls, extraction.Extraction.errors)

(* The Lean file of a blueprint: the one named, or the one that stands beside it. *)
let lean_file_of parsed file =
  match get parsed "lean" with
  | Some path -> Some path
  | None ->
    let beside = Filename.remove_extension file ^ ".lean" in
    if Fs.exists beside then Some beside else None

let status_of ctx parsed file =
  let* doc = sound_blueprint file in
  match lean_file_of parsed file with
  | None -> Ok (Blueprint_status.compute ?label:(get parsed "label") doc [])
  | Some lean ->
    let* source = read_source lean in
    let* decls, errors = lean_status ctx ~file:lean source in
    Ok (Blueprint_status.compute ~lean_file:lean ~lean_errors:errors ?label:(get parsed "label") doc decls)

let lean_options =
  [ value "lean" "FILE" "The Lean file of the formalization; by default the file beside the blueprint with the extension .lean.";
    value "project" "DIR" "A Lake project whose modules the Lean file may import." ]

let check_cmd =
  { path = [ "blueprint"; "check" ]; summary = "Check that a blueprint is well formed."; synopsis = "FILE";
    details =
      "Reads a blueprint and checks what the other commands rely on: identifiers are unique, every reference names a node, no node \
       depends on itself through others, a Lean name is given once, a definition has no proof, and every node says something. \
       The exit status is 7 when a problem is found.";
    options = [];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = one_file parsed "blueprint check" in
            let* doc = read_blueprint file in
            let problems = Blueprint_check.validate doc in
            let nodes = List.length doc.Blueprint_doc.nodes and edges = List.length (Blueprint_doc.edges doc) in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ("file", Json.String file); ("title", Json.String doc.Blueprint_doc.title); ("nodes", Json.Int nodes); ("edges", Json.Int edges);
                     ("problems", Json.List (List.map (fun p -> Json.String (Blueprint_check.describe p)) problems)) ])
            else begin
              List.iter (fun p -> print_endline ("  " ^ Blueprint_check.describe p)) problems;
              print_endline
                (if problems = [] then Printf.sprintf "%s: %d node(s), %d edge(s), no problem" file nodes edges
                 else Printf.sprintf "%s: %d node(s), %d edge(s), %d problem(s)" file nodes edges (List.length problems))
            end;
            Ok (if problems = [] then 0 else Exit_code.to_int Exit_code.Not_verified))) }

let plan_cmd =
  { path = [ "blueprint"; "plan" ]; summary = "Take an argument in words apart into a blueprint."; synopsis = "ARGUMENT";
    details =
      "ARGUMENT is a text file with an informal argument: definitions, claims and proofs as a person would write them. A model \
       names the definitions and the lemmas, says what each rests on, and states in Lean what it is sure of. The answer is put in \
       order before it is written: identifiers are made unique, references to nothing are dropped, and a plan whose nodes depend \
       on each other in a circle is refused. What comes out is a plan and proves nothing: `maithic blueprint skeleton` has Lean \
       compile its statements.";
    options =
      [ value "out" "FILE" "Where the blueprint is written (the argument's name with the extension .blueprint.md).";
        value "title" "TEXT" "The title of the blueprint, instead of the one the model gives.";
        value "namespace" "NAME" "The Lean namespace of the declarations.";
        value "import" "MODULE" "A module the Lean statements may use; may be given several times." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = one_file parsed "blueprint plan" in
            let* text = read_source file in
            let* gateway = Gateway.create ctx.config in
            let asked =
              Blueprint_plan.ask gateway ~max_tokens:(Config.int ctx.config "gateway.max_tokens") ?namespace:(get parsed "namespace")
                ~imports:(get_all parsed "import") ?title:(get parsed "title") text
            in
            let* outcome = Result.map_error (fun why -> blueprint_fault 3 ~exit:Exit_code.Provider ("no plan could be made: " ^ why)) asked in
            let out = match get parsed "out" with Some path -> path | None -> Filename.remove_extension file ^ ".blueprint.md" in
            let* () = Result.map_error usage_fault (Blueprint_plan.write ~path:out outcome) in
            let doc = outcome.Blueprint_plan.doc in
            let stated = List.length (List.filter (fun (n : Blueprint_node.t) -> n.Blueprint_node.formal <> None) doc.Blueprint_doc.nodes) in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ("file", Json.String out); ("graph", Blueprint_graph.to_json doc);
                     ("notes", Json.List (List.map (fun n -> Json.String n) outcome.Blueprint_plan.notes)); ("gateway", Gateway.account_to_json gateway) ])
            else begin
              if not ctx.quiet then begin
                List.iter
                  (fun (n : Blueprint_node.t) ->
                    Printf.printf "  %s  %s  %s%s\n"
                      (Text.pad_right 11 (Blueprint_node.kind_name n.Blueprint_node.kind))
                      (Text.pad_right 18 n.Blueprint_node.id) n.Blueprint_node.title
                      (match Blueprint_node.dependencies n with [] -> "" | uses -> Term.dim ("  <- " ^ String.concat ", " uses)))
                  doc.Blueprint_doc.nodes;
                List.iter (fun note -> print_endline (Term.dim ("  note: " ^ note))) outcome.Blueprint_plan.notes
              end;
              Printf.printf "wrote %s: %d node(s), %d edge(s), %d stated in Lean\n" out (List.length doc.Blueprint_doc.nodes)
                (List.length (Blueprint_doc.edges doc)) stated
            end;
            Ok 0)) }

let skeleton_cmd =
  { path = [ "blueprint"; "skeleton" ]; summary = "Write the Lean skeleton of a blueprint and have Lean compile it."; synopsis = "FILE";
    details =
      "Writes every node that has a Lean statement as a declaration, with sorry where its proof will stand, in an order in which \
       everything a node uses comes before it. Each declaration carries the identifier of its node in a comment. The skeleton is \
       then given to Lean: a statement that does not compile is reported with its node, and the exit status is 7. A node without \
       a Lean statement is named in a comment and counted as not stated.";
    options =
      [ value "out" "FILE" "Where the skeleton is written (the blueprint's name with the extension .lean).";
        switch "no-check" "Write the skeleton and do not give it to Lean.";
        switch "force" "Write over a file that exists.";
        value "project" "DIR" "A Lake project whose modules the skeleton may import." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = one_file parsed "blueprint skeleton" in
            let* doc = sound_blueprint file in
            let* skeleton = Result.map_error (blueprint_fault 2 ~exit:Exit_code.Usage) (Blueprint_skeleton.emit doc) in
            let out = match get parsed "out" with Some path -> path | None -> Filename.remove_extension file ^ ".lean" in
            let* () =
              if Fs.exists out && not (has parsed "force") then
                Error (usage_fault (Printf.sprintf "%s exists; --force writes over it, and --out names another file" out))
              else Ok ()
            in
            let* outcome =
              if has parsed "no-check" then Ok None
              else
                let* bridge, _ = start_bridge ctx in
                let elaborate text =
                  match Bridge_client.check_file ~limits:(limits ctx) bridge text with
                  | Error e -> Error (Bridge_error.to_string e)
                  | Ok (report, _) ->
                    Ok
                      (List.map
                         (fun (d : Bridge_types.diagnostic) -> { Blueprint_skeleton.severity = d.Bridge_types.severity; line = d.Bridge_types.line; message = d.Bridge_types.text })
                         report.Bridge_types.diagnostics)
                in
                let compiled = Blueprint_skeleton.compile ~elaborate skeleton in
                Bridge_client.shutdown bridge;
                Result.map Option.some (Result.map_error (blueprint_fault 4 ~exit:Exit_code.Failure) compiled)
            in
            let* () = write_text out skeleton.Blueprint_skeleton.text in
            let stated = List.length skeleton.Blueprint_skeleton.entries and unstated = List.length skeleton.Blueprint_skeleton.unstated in
            let compiles = match outcome with Some o -> o.Blueprint_skeleton.compiles | None -> true in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   ([ ("file", Json.String out); ("stated", Json.Int stated); ("unstated", Json.List (List.map (fun id -> Json.String id) skeleton.Blueprint_skeleton.unstated)) ]
                   @ match outcome with
                     | None -> []
                     | Some o ->
                       [ ("compiles", Json.Bool o.Blueprint_skeleton.compiles);
                         ( "problems",
                           Json.List
                             (List.map
                                (fun (node, line, message) ->
                                  Json.Obj
                                    ((match node with Some id -> [ ("node", Json.String id) ] | None -> []) @ [ ("line", Json.Int line); ("message", Json.String message) ]))
                                o.Blueprint_skeleton.problems) ) ]))
            else begin
              (match outcome with
               | Some o ->
                 List.iter
                   (fun (node, line, message) ->
                     Printf.printf "  %s  line %d: %s\n" (match node with Some id -> id | None -> "(outside any node)") line
                       (List.hd (String.split_on_char '\n' message)))
                   o.Blueprint_skeleton.problems
               | None -> ());
              Printf.printf "wrote %s: %d declaration(s)%s; %s\n" out stated
                (if unstated = 0 then "" else Printf.sprintf ", %d node(s) not stated yet" unstated)
                (match outcome with
                 | None -> "Lean was not asked"
                 | Some o when o.Blueprint_skeleton.compiles -> "Lean compiles the skeleton"
                 | Some o -> Printf.sprintf "Lean reports %d error(s)" (List.length o.Blueprint_skeleton.problems))
            end;
            Ok (if compiles then 0 else Exit_code.to_int Exit_code.Not_verified))) }

let status_cmd =
  { path = [ "blueprint"; "status" ]; summary = "Show where a formalization stands, node by node."; synopsis = "FILE";
    details =
      "Reads the blueprint and asks Lean about the declarations it names. A node is planned when Lean does not have it, stated \
       when its own text contains sorry, proved when its own text is finished and something it rests on is not, and done when \
       nothing below it is open. The report lists every node with what it waits for, counts the nodes by status and by kind, and \
       notes where the blueprint and the Lean text disagree about what a node uses. With --strict the exit status is 7 unless \
       every node is done.";
    options =
      lean_options
      @ [ value "label" "TEXT" "A name for this snapshot, such as a date or a commit; it is written into the report.";
          value "out" "FILE" "Write the report as JSON to FILE as well.";
          value "svg" "FILE" "Draw the graph, coloured by status, to FILE.";
          switch "strict" "Exit with status 7 unless every node is done." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = one_file parsed "blueprint status" in
            let* status = status_of ctx parsed file in
            let document = Blueprint_status.to_json status in
            let* () = match get parsed "out" with Some out -> write_text out (Json.pretty document ^ "\n") | None -> Ok () in
            let* () = match get parsed "svg" with Some path -> write_text path (Blueprint_svg.render status) | None -> Ok () in
            if ctx.json then Term.print_json document
            else begin
              if ctx.quiet then print_endline (Blueprint_status.summary status)
              else print_endline (Blueprint_status.render ~color:!Term.colour_enabled status);
              if (not ctx.quiet) && status.Blueprint_status.lean_file = None then
                print_endline (Term.dim "no Lean file was given and none stands beside the blueprint: every node is planned")
            end;
            Ok (if has parsed "strict" && not (Blueprint_status.complete status) then Exit_code.to_int Exit_code.Not_verified else 0))) }

let next_cmd =
  { path = [ "blueprint"; "next" ]; summary = "List what can be worked on now, the most useful first."; synopsis = "FILE";
    details =
      "Lists the nodes that are not settled and wait for nothing: the frontier of the formalization. The node that the most \
       unsettled nodes rest on comes first; among equals, one that is already stated comes before one that is not, and then the \
       order of the blueprint decides. With --blocked the nodes that wait are listed too, each with what it waits for.";
    options = lean_options @ [ value "limit" "N" "Show at most N nodes."; switch "blocked" "List the nodes that wait as well." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = one_file parsed "blueprint next" in
            let* limit =
              match get parsed "limit" with
              | None -> Ok max_int
              | Some text -> (match int_of_string_opt text with Some n when n >= 1 -> Ok n | _ -> Error (usage_fault ("--limit needs a whole number, 1 or more, and got " ^ text)))
            in
            let* status = status_of ctx parsed file in
            let ready = List.filteri (fun i _ -> i < limit) (Blueprint_status.frontier status) in
            let shown = { status with Blueprint_status.entries = List.map (fun (e : Blueprint_status.entry) -> if List.memq e ready then e else { e with Blueprint_status.ready = false }) status.Blueprint_status.entries } in
            if ctx.json then
              Term.print_json
                (Json.Obj
                   [ ("frontier", Json.List (List.map (fun (e : Blueprint_status.entry) -> Json.String e.Blueprint_status.node.Blueprint_node.id) ready));
                     ( "blocked",
                       Json.List
                         (List.map
                            (fun (e : Blueprint_status.entry) ->
                              Json.Obj
                                [ ("id", Json.String e.Blueprint_status.node.Blueprint_node.id);
                                  ("blockers", Json.List (List.map (fun b -> Json.String b) e.Blueprint_status.blockers)) ])
                            (Blueprint_status.blocked status)) ) ])
            else begin
              print_endline (Blueprint_status.render_next ~color:!Term.colour_enabled shown);
              if has parsed "blocked" then
                List.iter
                  (fun (e : Blueprint_status.entry) ->
                    Printf.printf "   %s waits for %s\n" e.Blueprint_status.node.Blueprint_node.id (String.concat ", " e.Blueprint_status.blockers))
                  (Blueprint_status.blocked status)
            end;
            Ok 0)) }

let graph_cmd =
  { path = [ "blueprint"; "graph" ]; summary = "Export the graph of a blueprint."; synopsis = "FILE";
    details =
      "Writes the nodes and the edges of a blueprint for another program: as JSON, in the DOT language of Graphviz, or as a \
       drawing. An edge says that a node uses another, in its statement or only in its proof.";
    options = [ value "format" "FORMAT" "json (the default), dot or svg."; value "out" "FILE" "Write to FILE instead of the standard output." ] @ lean_options;
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* file = one_file parsed "blueprint graph" in
            let* text =
              match Option.value ~default:"json" (get parsed "format") with
              | "json" -> Result.map (fun doc -> Json.pretty (Blueprint_graph.to_json doc) ^ "\n") (sound_blueprint file)
              | "dot" -> Result.map Blueprint_graph.to_dot (sound_blueprint file)
              | "svg" -> Result.map Blueprint_svg.render (status_of ctx parsed file)
              | other -> Error (usage_fault (Printf.sprintf "--format is json, dot or svg, and got %s" other))
            in
            (match get parsed "out" with
             | Some out -> Result.map (fun () -> say ctx (Printf.sprintf "wrote %s" out); 0) (write_text out text)
             | None ->
               print_string text;
               Ok 0))) }

let all = [ check_cmd; plan_cmd; skeleton_cmd; status_cmd; next_cmd; graph_cmd ]
