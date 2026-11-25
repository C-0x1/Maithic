(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The [search] command: declarations by the words of their names, their
    statements and their documentation. *)

open Maithic_base
open Maithic_schema
open Maithic_bridge
open Maithic_lookup
open Cli_args
open Commands

let search_fault number ~exit message = Fault.make ~area:"SEARCH" ~number ~exit message

(* The declarations to search: an export on disk, or Lean files read now. *)
let documents ctx parsed =
  match (get parsed "from", get_all parsed "in") with
  | Some _, _ :: _ -> Error (usage_fault "give the declarations with --from or with --in, not with both")
  | None, [] -> Error (usage_fault "search needs declarations: --from DIR, as written by `maithic intel extract`, or --in PATH for Lean files")
  | Some from, [] ->
    let file = if Fs.is_dir from then Fs.(from / "declarations.jsonl") else from in
    let* text = Result.map_error (fun e -> search_fault 1 ~exit:Exit_code.Usage (Printf.sprintf "the declarations cannot be read: %s" e)) (Fs.read file) in
    Result.map_error (fun e -> search_fault 1 ~exit:Exit_code.Usage (Printf.sprintf "%s does not hold declarations: %s" file e)) (Decl_document.of_jsonl text)
  | None, paths ->
    let* extractions = Commands_intel.extract_files ctx paths in
    Ok (Decl_document.of_extractions extractions)

(* Lean, asked about everything in scope with #maithic_find. *)
let lean_retriever ctx ~imports =
  let* bridge, _ = start_bridge ctx in
  match Bridge_client.load_header bridge ("Maithic.Search.Find" :: imports) with
  | Error e ->
    Bridge_client.shutdown bridge;
    Error (search_fault 2 ~exit:Exit_code.Unavailable ("Lean could not load the search command or the modules named with --import: " ^ Bridge_error.to_string e))
  | Ok (env, _) ->
    let ask query ~limit =
      let parts = List.filter (fun w -> w <> "") (String.split_on_char ' ' (Text.replace_all ~sub:"\"" ~by:"" query)) in
      if parts = [] then Ok []
      else
        let command = Printf.sprintf "#maithic_find json (limit := %d) %s" limit (String.concat " " (List.map (fun w -> "\"" ^ w ^ "\"") parts)) in
        match Bridge_client.run_command ~limits:(limits ctx) bridge ~env command with
        | Error e -> Error (Bridge_error.to_string e)
        | Ok (report, _) ->
          (match Maithic_refute.Refute_probe.printed report with
           | Error e -> Error e
           | Ok json ->
             Ok
               (List.filter_map
                  (fun hit ->
                    match Json.string_or "" "name" hit with
                    | "" -> None
                    | name ->
                      Some
                        { Decl_document.name; kind = Json.string_or "" "kind" hit; statement = Json.string_or "" "type" hit; doc = ""; module_ = ""; attributes = [];
                          file = ""; line = 0 })
                  (Json.list_or_empty "hits" json)))
    in
    Ok (bridge, ask)

let search_cmd =
  { path = [ "search" ]; summary = "Find declarations by the words of their names, statements and documentation."; synopsis = "QUERY...";
    details =
      "Searches the declarations of a project. They come from --from DIR, a folder that `maithic intel extract` wrote, or from --in \
       PATH, Lean files and folders that are read now. Two retrievers answer by default. The lexical one ranks declarations by the \
       words they share with the query, with BM25: identifiers are taken apart, so that `add comm` finds `Nat.add_comm`, and \
       notation counts as its name, so that it also finds a statement with `a + b = b + a`. The other takes the query for a name \
       or a part of one. With --lean, Lean itself is asked about everything in scope, the libraries included. The answers are \
       merged by rank, and each hit says who found it. With --queries FILE the queries of a query set are answered instead, and \
       the rankings are written for evaluation.";
    options =
      [ value "from" "DIR" "A folder with declarations.jsonl, as `maithic intel extract --out DIR` writes it, or such a file.";
        value "in" "PATH" "A Lean file or a folder of them to read now; may be given several times.";
        value "project" "DIR" "A Lake project whose modules the Lean files may import.";
        value "limit" "N" "The number of hits (10).";
        value "kind" "KIND" "Only declarations of this kind, such as theorem or definition; may be given several times.";
        value "module" "PREFIX" "Only declarations of modules whose names begin with PREFIX.";
        value "only" "RETRIEVER" "Ask one retriever only: lexical or name.";
        switch "lean" "Ask Lean about everything in scope as well.";
        value "import" "MODULE" "A module that is in scope when Lean is asked; may be given several times.";
        value "queries" "FILE" "A query set to answer instead of QUERY.";
        value "rankings" "FILE" "Where the rankings for --queries are written (rankings.json)." ];
    run =
      (fun parsed ->
        guarded parsed (fun ctx ->
            let* limit =
              match get parsed "limit" with
              | None -> Ok Decl_search.default_options.Decl_search.limit
              | Some text -> (match int_of_string_opt text with Some n when n >= 1 -> Ok n | _ -> Error (usage_fault ("--limit needs a whole number, 1 or more, and got " ^ text)))
            in
            let* retrievers =
              match get parsed "only" with
              | None -> Ok (Decl_search.default_options.Decl_search.retrievers @ if has parsed "lean" then [ Decl_search.Lean ] else [])
              | Some name ->
                (match Decl_search.retriever_of_name name with
                 | Some Decl_search.Lean | None -> Error (usage_fault (Printf.sprintf "--only is lexical or name, and got %s" name))
                 | Some r -> Ok [ r ])
            in
            let options = { Decl_search.default_options with Decl_search.limit; kinds = get_all parsed "kind"; module_prefix = get parsed "module"; retrievers } in
            let* docs = documents ctx parsed in
            let index = Inverted_index.build docs in
            match get parsed "queries" with
            | Some file ->
              let* text = read_source file in
              let* queryset = Result.map_error (fun e -> usage_fault (Printf.sprintf "%s is not JSON: %s" file e)) (Json.parse text) in
              let* rankings = Result.map_error usage_fault (Decl_search.rankings ~options index queryset) in
              let out = Option.value ~default:"rankings.json" (get parsed "rankings") in
              let* () = Result.map_error usage_fault (Fs.write out (Json.pretty rankings ^ "\n")) in
              if ctx.json then Term.print_json rankings
              else Printf.printf "answered %d query(ies) over %d declaration(s); wrote %s\n" (List.length (Json.list_or_empty "results" rankings)) (Inverted_index.size index) out;
              Ok 0
            | None ->
              let query = String.concat " " parsed.positional in
              let* () = if Text.is_blank query then Error (usage_fault "search needs a QUERY") else Ok () in
              let* lean = if List.mem Decl_search.Lean retrievers then Result.map Option.some (lean_retriever ctx ~imports:(get_all parsed "import")) else Ok None in
              let hits, note = Decl_search.search ~options ?lean:(Option.map snd lean) index query in
              Option.iter (fun (bridge, _) -> Bridge_client.shutdown bridge) lean;
              if ctx.json then Term.print_json (Decl_search.to_json ~query ~options ~declarations:(Inverted_index.size index) ?note hits)
              else begin
                print_endline (Decl_search.render hits);
                if not ctx.quiet then begin
                  Option.iter (fun note -> print_endline (Term.dim note)) note;
                  print_endline (Term.dim (Printf.sprintf "%d hit(s) among %d declaration(s) and %d word(s)" (List.length hits) (Inverted_index.size index) (Inverted_index.vocabulary index)))
                end
              end;
              Ok (if hits = [] then Exit_code.to_int Exit_code.Not_verified else 0))) }

let all = [ search_cmd ]
