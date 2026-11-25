(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [intel] commands: a Lean code base as a graph, and questions to it. *)

open Maithic_base
open Maithic_schema
open Maithic_config
open Maithic_bridge
open Maithic_intel
open Cli_args
open Commands

let intel_fault number ~exit message = Fault.make ~area:"INTEL" ~number ~exit message
let default_dir = ".maithic/intel"
let dir_option = value "dir" "DIR" "Where the graph is kept (.maithic/intel)."
let dir_of parsed = Option.value ~default:default_dir (get parsed "dir")

(* Extracts every Lean file among the paths. Used by the other commands that read whole files.
   Each file gets a Bridge process of its own: the environment of a file's imports stays in the
   memory of the process that loaded it, and a project has about as many import lists as files. *)
let extract_files ?(trace = false) ?(duplicates = false) ?(lint = false) ?(rules = []) ?(convention = []) ctx paths =
  let* files = Result.map_error usage_fault (Lean_files.expand paths) in
  let* () = if files = [] then Error (usage_fault "no Lean file among the paths") else Ok () in
  let project = match Config.string ctx.config "bridge.project" with "" -> None | dir -> Some dir in
  let limits = limits ctx in
  let result =
    List.fold_left
      (fun acc file ->
        let* done_ = acc in
        let* text = Result.map_error usage_fault (Fs.read file) in
        let* bridge, _ = start_bridge ctx in
        let extraction =
          Result.map_error Bridge_error.to_fault
            (Bridge_client.extract ~file ~module_name:(Lean_files.module_of ?project file) ~trace ~duplicates ~lint ~rules ~convention
               ~limits bridge text)
        in
        Bridge_client.shutdown bridge;
        let* extraction = extraction in
        if ctx.verbose then
          Printf.eprintf "%s: %d declaration(s)%s\n%!" file (List.length extraction.Extraction.declarations)
            (if extraction.Extraction.ok then "" else Printf.sprintf ", %d error(s)" extraction.Extraction.errors);
        Ok (extraction :: done_))
      (Ok []) files
  in
  Result.map List.rev result

let load_graph parsed =
  let file = Fs.(dir_of parsed / "graph.json") in
  let* text =
    Result.map_error
      (fun _ -> intel_fault 1 ~exit:Exit_code.Usage (Printf.sprintf "%s is not there; `maithic intel scan PATH` writes it" file))
      (Fs.read file)
  in
  Result.map_error
    (fun why -> intel_fault 2 ~exit:Exit_code.Usage (Printf.sprintf "%s is not a code graph: %s" file why))
    (Result.bind (Json.parse text) Intel_graph.of_json)

let where (n : Intel_graph.node) = if n.Intel_graph.internal then Printf.sprintf "%s:%d" n.Intel_graph.file n.Intel_graph.name_line else "outside"
let kind_of (n : Intel_graph.node) = if n.Intel_graph.internal then n.Intel_graph.decl_kind else "-"

let scan_cmd =
  { path = [ "intel"; "scan" ]; summary = "Read Lean files into a code graph and a health report."; synopsis = "PATH...";
    details =
      "Every Lean file among the paths is elaborated, and its declarations, their dependencies, documentation and axioms are \
       read from the environment the kernel checked. The graph and the health report are written as JSON; the other intel \
       commands read them and need no Lean.";
    options = [ value "project" "DIR" "A Lake project whose modules the files may import."; dir_option ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* extractions = extract_files ctx parsed.positional in
            let graph = Intel_graph.build extractions in
            let health = Intel_health.snapshot extractions in
            let dir = dir_of parsed in
            let* () = Result.map_error usage_fault (Fs.mkdir_p dir) in
            let* () = Result.map_error usage_fault (Fs.write Fs.(dir / "graph.json") (Json.pretty (Intel_graph.to_json graph) ^ "\n")) in
            let* () = Result.map_error usage_fault (Fs.write Fs.(dir / "health.json") (Json.pretty health ^ "\n")) in
            let stats = Intel_graph.statistics graph in
            if ctx.json then
              Term.print_json
                (Json.Obj [ ("files", Json.Int (List.length extractions)); ("graph", Intel_graph.statistics_to_json stats); ("health", health) ])
            else begin
              let declarations = List.fold_left (fun acc (_, n) -> acc + n) 0 stats.Intel_graph.declarations_by_kind in
              Printf.printf "scanned %d file(s): %d declaration(s), %d resting on sorry, %d file(s) with errors\n" (List.length extractions)
                declarations (Json.int_or 0 "restingOnSorry" health) (Json.int_or 0 "filesWithErrors" health);
              Printf.printf "  %s\n"
                (String.concat "  " (List.map (fun (kind, n) -> Printf.sprintf "%s %d" kind n) stats.Intel_graph.declarations_by_kind));
              Printf.printf "wrote %s and %s\n" Fs.(dir / "graph.json") Fs.(dir / "health.json")
            end;
            Ok 0)) }

let extract_cmd =
  { path = [ "intel"; "extract" ]; summary = "Write what Lean knows about the declarations of files, as JSON."; synopsis = "PATH...";
    details =
      "For every Lean file among the paths, one document with its declarations, imports and documentation is written to the output directory, and all declarations together are written as JSON lines, one declaration on each line.";
    options =
      [ value "project" "DIR" "A Lake project whose modules the files may import."; value "out" "DIR" "Where to write (.maithic/extract).";
        switch "trace" "Include the tactic steps of every proof, with the goals before and after.";
        switch "lint" "Include the findings of the lint rules.";
        switch "duplicates" "Include, for every theorem, the theorems elsewhere that state the same." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* extractions =
              extract_files ~trace:(has parsed "trace") ~lint:(has parsed "lint") ~duplicates:(has parsed "duplicates") ctx parsed.positional
            in
            let dir = Option.value ~default:".maithic/extract" (get parsed "out") in
            let* () = Result.map_error usage_fault (Fs.mkdir_p dir) in
            let lines = Buffer.create 65536 in
            let* () =
              List.fold_left
                (fun acc (e : Extraction.t) ->
                  let* () = acc in
                  List.iter
                    (fun (d : Extraction.declaration) ->
                      let record =
                        match Extraction.declaration_to_json d with
                        | Json.Obj fields -> Json.Obj (("file", Json.String e.Extraction.file) :: fields)
                        | other -> other
                      in
                      Buffer.add_string lines (Json.to_string record);
                      Buffer.add_char lines '\n')
                    e.Extraction.declarations;
                  let name = if e.Extraction.module_name = "" then Fs.basename e.Extraction.file else e.Extraction.module_name in
                  Result.map_error usage_fault (Fs.write Fs.(dir / (name ^ ".json")) (Json.pretty (Extraction.to_json e) ^ "\n")))
                (Ok ()) extractions
            in
            let* () = Result.map_error usage_fault (Fs.write Fs.(dir / "declarations.jsonl") (Buffer.contents lines)) in
            let count = List.fold_left (fun acc (e : Extraction.t) -> acc + List.length e.Extraction.declarations) 0 extractions in
            if ctx.json then
              Term.print_json (Json.Obj [ ("files", Json.Int (List.length extractions)); ("declarations", Json.Int count); ("out", Json.String dir) ])
            else Printf.printf "%d file(s), %d declaration(s) written to %s\n" (List.length extractions) count dir;
            Ok 0)) }

let names nodes = List.map (fun (n : Intel_graph.node) -> n.Intel_graph.name) nodes

let show_cmd =
  { path = [ "intel"; "show" ]; summary = "Show one declaration: where it is, what it uses and what uses it."; synopsis = "NAME";
    details = "NAME is the full name of a declaration, or the end of it when that is enough to tell which is meant.";
    options = [ dir_option ];
    run =
      (fun parsed ->
        guarded parsed (fun _ctx ->
            let* name = match parsed.positional with [ name ] -> Ok name | _ -> Error (usage_fault "intel show needs exactly one NAME") in
            let* graph = load_graph parsed in
            let* node = Result.map_error (intel_fault 3 ~exit:Exit_code.Usage) (Intel_graph.find graph name) in
            let uses = Intel_graph.dependencies graph node.Intel_graph.name and used_by = Intel_graph.dependents graph node.Intel_graph.name in
            let inside, outside = List.partition (fun (n : Intel_graph.node) -> n.Intel_graph.internal) uses in
            if has parsed "json" then
              Term.print_json
                (Json.Obj
                   [ ("name", Json.String node.Intel_graph.name); ("kind", Json.String node.Intel_graph.decl_kind);
                     ("file", Json.String node.Intel_graph.file); ("line", Json.Int node.Intel_graph.name_line);
                     ("documented", Json.Bool node.Intel_graph.documented); ("usesSorry", Json.Bool node.Intel_graph.uses_sorry);
                     ("containsSorry", Json.Bool node.Intel_graph.contains_sorry);
                     ("axioms", Json.List (List.map (fun a -> Json.String a) node.Intel_graph.axioms));
                     ("uses", Json.List (List.map (fun n -> Json.String n) (names uses)));
                     ("usedBy", Json.List (List.map (fun n -> Json.String n) (names used_by))) ])
            else begin
              Printf.printf "%s  %s  %s\n" node.Intel_graph.name (kind_of node) (where node);
              Printf.printf "  documented: %s\n" (if node.Intel_graph.documented then "yes" else "no");
              Printf.printf "  rests on sorry: %s\n"
                (if node.Intel_graph.contains_sorry then "yes, in its own text"
                 else if node.Intel_graph.uses_sorry then "yes, through what it uses"
                 else "no");
              Printf.printf "  axioms: %s\n" (if node.Intel_graph.axioms = [] then "none" else String.concat ", " node.Intel_graph.axioms);
              Printf.printf "  uses (%d here, %d from outside): %s\n" (List.length inside) (List.length outside)
                (Text.truncate 400 (String.concat ", " (names inside @ names outside)));
              Printf.printf "  used by (%d): %s\n" (List.length used_by) (Text.truncate 400 (String.concat ", " (names used_by)))
            end;
            Ok 0)) }

let reach_cmd ~which ~summary ~details ~follow =
  { path = [ "intel"; which ]; summary; synopsis = "NAME"; details;
    options =
      [ value "depth" "N" "How far to follow: 1 for the direct ones only. Default: all the way."; dir_option ]
      @ if which = "deps" then [ switch "all" "Include declarations from outside the scanned files." ] else [];
    run =
      (fun parsed ->
        guarded parsed (fun _ctx ->
            let* name =
              match parsed.positional with [ name ] -> Ok name | _ -> Error (usage_fault (Printf.sprintf "intel %s needs exactly one NAME" which))
            in
            let* depth =
              match get parsed "depth" with
              | None -> Ok None
              | Some text ->
                (match int_of_string_opt text with
                 | Some n when n >= 1 -> Ok (Some n)
                 | _ -> Error (usage_fault "--depth needs a number, one or more"))
            in
            let* graph = load_graph parsed in
            let* node = Result.map_error (intel_fault 3 ~exit:Exit_code.Usage) (Intel_graph.find graph name) in
            let found = follow ?depth ~all:(has parsed "all") graph node.Intel_graph.name in
            if has parsed "json" then
              Term.print_json
                (Json.Obj
                   [ ("name", Json.String node.Intel_graph.name);
                     ( "found",
                       Json.List
                         (List.map
                            (fun ((n : Intel_graph.node), distance) ->
                              Json.Obj [ ("name", Json.String n.Intel_graph.name); ("distance", Json.Int distance); ("internal", Json.Bool n.Intel_graph.internal) ])
                            found) ) ])
            else if found = [] then Printf.printf "%s: none\n" node.Intel_graph.name
            else
              print_string
                (Term.table
                   ([ "distance"; "declaration"; "kind"; "where" ]
                   :: List.map (fun ((n : Intel_graph.node), distance) -> [ string_of_int distance; n.Intel_graph.name; kind_of n; where n ]) found));
            Ok 0)) }

let deps_cmd =
  reach_cmd ~which:"deps" ~summary:"List what a declaration depends on, directly and through others."
    ~details:"Follows the uses edges of the graph from NAME. Only declarations of the scanned files are followed unless --all is given."
    ~follow:(fun ?depth ~all graph name -> Intel_query.dependencies ?depth ~internal_only:(not all) graph name)

let dependents_cmd =
  reach_cmd ~which:"dependents" ~summary:"List what depends on a declaration, directly and through others."
    ~details:"Follows the uses edges of the graph backwards from NAME: everything a change to NAME can reach."
    ~follow:(fun ?depth ~all:_ graph name -> Intel_query.dependents ?depth graph name)

let health_cmd =
  { path = [ "intel"; "health" ]; summary = "List unfinished proofs with their goals, and the axioms in use."; synopsis = "";
    details = "Reads the health report that `intel scan` wrote.";
    options = [ dir_option ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let file = Fs.(dir_of parsed / "health.json") in
            let* health =
              Result.map_error
                (fun _ -> intel_fault 1 ~exit:Exit_code.Usage (Printf.sprintf "%s is not there; `maithic intel scan PATH` writes it" file))
                (Result.bind (Fs.read file) Json.parse)
            in
            if ctx.json then Term.print_json health
            else begin
              let unfinished = Json.list_or_empty "unfinished" health and axioms = Json.list_or_empty "axioms" health in
              Printf.printf "%d file(s), %d declaration(s), %d documented, %d resting on sorry\n" (Json.int_or 0 "files" health)
                (Json.int_or 0 "declarations" health) (Json.int_or 0 "documented" health) (Json.int_or 0 "restingOnSorry" health);
              Printf.printf "\nunfinished (%d)\n" (List.length unfinished);
              List.iter
                (fun g ->
                  Printf.printf "  %s:%d:%d  %s%s\n" (Json.string_or "" "file" g) (Json.int_or 0 "line" g) (Json.int_or 0 "column" g)
                    (Json.string_or "" "name" g)
                    (match Json.member "through" g with Some (Json.String t) -> "  (through " ^ t ^ ")" | _ -> "");
                  List.iter (fun goal -> print_endline (Text.indent 6 goal)) (Json.strings (Option.value ~default:(Json.List []) (Json.member "goals" g))))
                unfinished;
              let users a = Json.list_or_empty "usedBy" a in
              let other, standard = List.partition (fun a -> not (Json.bool_or false "standard" a)) axioms in
              Printf.printf "\naxioms beyond Lean's logic (%d)\n" (List.length other);
              List.iter
                (fun a ->
                  Printf.printf "  %s  used by %s\n" (Json.string_or "" "axiom" a)
                    (Text.truncate 300 (String.concat ", " (List.map (fun u -> Json.string_or "" "name" u) (users a)))))
                other;
              Printf.printf "\naxioms of Lean's logic\n";
              List.iter (fun a -> Printf.printf "  %s  %d declaration(s)\n" (Text.pad_right 16 (Json.string_or "" "axiom" a)) (List.length (users a))) standard
            end;
            Ok 0)) }

let stats_cmd =
  { path = [ "intel"; "stats" ]; summary = "Count the nodes and edges of the graph and name the most used declarations."; synopsis = "";
    details = "Reads the graph that `intel scan` wrote."; options = [ dir_option ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* graph = load_graph parsed in
            let stats = Intel_graph.statistics graph in
            if ctx.json then Term.print_json (Intel_graph.statistics_to_json stats)
            else begin
              let line title pairs = Printf.printf "%s  %s\n" (Text.pad_right 13 title) (String.concat "  " (List.map (fun (k, n) -> Printf.sprintf "%s %d" k n) pairs)) in
              line "nodes" stats.Intel_graph.nodes_by_kind;
              line "edges" stats.Intel_graph.edges_by_kind;
              line "declarations" stats.Intel_graph.declarations_by_kind;
              Printf.printf "%s  %d\n" (Text.pad_right 13 "from outside") stats.Intel_graph.external_declarations;
              let degrees title (d : Intel_graph.degrees) =
                Printf.printf "%s  least %d, greatest %d, mean %.2f, median %.1f\n" (Text.pad_right 13 title) d.Intel_graph.least
                  d.Intel_graph.greatest d.Intel_graph.mean d.Intel_graph.median
              in
              degrees "uses" stats.Intel_graph.uses;
              degrees "used by" stats.Intel_graph.used_by;
              print_endline "most used, of the scanned files";
              List.iter (fun (name, by) -> Printf.printf "  %4d  %s\n" by name) stats.Intel_graph.most_used_internal;
              print_endline "most used, from anywhere";
              List.iter (fun (name, by) -> Printf.printf "  %4d  %s\n" by name) stats.Intel_graph.most_used
            end;
            Ok 0)) }

let all = [ scan_cmd; extract_cmd; show_cmd; deps_cmd; dependents_cmd; health_cmd; stats_cmd ]
