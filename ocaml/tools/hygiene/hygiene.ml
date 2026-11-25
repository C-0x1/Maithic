(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Repository hygiene: the checks that keep the repository publishable.

    - {b placeholders}: code contains no unfinished-work markers, and the
      Lean libraries contain no [sorry], no [axiom] and no native evaluation;
    - {b secrets}: no file contains a key-shaped string or private key;
    - {b neutrality}: no file says which tools or services were used to
      write the repository;
    - {b versions}: every package carries the version of [PROJECT.toml], and the
      two toolchain files agree;
    - {b links}: every relative link of a Markdown file leads somewhere;
    - {b headers}: every source file starts with the copyright notice.

    This file lists the phrases the neutrality and placeholder scans look
    for, so it is the one file those two scans skip. *)

open Maithic_base

type finding = { check : string; file : string; line : int; detail : string }

let finding check file line detail = { check; file; line; detail }
let self = "ocaml/tools/hygiene/hygiene.ml"
let source_extensions = [ ".ml"; ".mli"; ".mll"; ".mly"; ".lean"; ".jl"; ".r"; ".py"; ".sh"; ".js"; ".ts"; ".css" ]
let is_source rel = List.mem (Fs.extension rel) source_extensions

let is_word_char = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false

(* Offsets at which [word] occurs as a whole word. *)
let word_offsets text word =
  let n = String.length word in
  let rec go from acc =
    match Text.find_sub ~from text word with
    | None -> List.rev acc
    | Some i ->
      let before = i = 0 || not (is_word_char text.[i - 1]) in
      let after = i + n >= String.length text || not (is_word_char text.[i + n]) in
      go (i + 1) (if before && after then i :: acc else acc)
  in
  go 0 []

let has_word text word = word_offsets text word <> []

let numbered_lines text = List.mapi (fun i line -> (i + 1, line)) (String.split_on_char '\n' text)

(* ---- placeholders ---- *)

let marker_words = [ "TODO"; "FIXME"; "XXX"; "HACK" ]
let marker_phrases = [ "not implemented"; "not yet implemented"; "unimplemented"; "to be implemented"; "lorem ipsum" ]

let placeholder_findings rel text =
  if not (is_source rel) || rel = self then []
  else
    List.concat_map
      (fun (number, line) ->
        let lower = String.lowercase_ascii line in
        (List.filter_map
           (fun word -> if has_word line word then Some (finding "placeholder" rel number ("unfinished-work marker " ^ word)) else None)
           marker_words)
        @ List.filter_map
            (fun phrase -> if Text.contains lower phrase then Some (finding "placeholder" rel number ("phrase '" ^ phrase ^ "'")) else None)
            marker_phrases)
      (numbered_lines text)

(** Lean libraries that ship: no unfinished proof and no added axiom. *)
let lean_library_roots = [ "lean/Maithic/"; "lean/Maithic.lean"; "lean/MaithicBridge/"; "lean/MaithicBridge.lean"; "lean/MaithicBench/"; "lean/MaithicBench.lean" ]

(* The Bridge's guard and the audit name these words as data; they are code
   only where they stand outside strings and comments. *)
let lean_findings rel text =
  if Fs.extension rel <> ".lean" || not (List.exists (fun prefix -> Text.starts_with ~prefix rel) lean_library_roots) then []
  else begin
    let masked = Maithic_prover.Lean_source.mask text in
    List.concat_map
      (fun (number, line) ->
        List.filter_map
          (fun word ->
            let declares_axiom = word = "axiom" && Text.starts_with ~prefix:"axiom " (String.trim line) in
            let uses = word <> "axiom" && has_word line word in
            if declares_axiom || uses then Some (finding "placeholder" rel number (Printf.sprintf "'%s' in a Lean library" word)) else None)
          [ "sorry"; "admit"; "axiom"; "native_decide" ])
      (numbered_lines masked)
  end

(* ---- secrets ---- *)

let private_key_markers = [ "BEGIN PRIVATE KEY"; "BEGIN RSA PRIVATE KEY"; "BEGIN EC PRIVATE KEY"; "BEGIN OPENSSH PRIVATE KEY" ]

let secret_findings rel text =
  List.concat_map
    (fun (number, line) ->
      (match Maithic_gateway.Keys.find_key_shaped line with
       | [] -> []
       | found -> [ finding "secret" rel number (Printf.sprintf "%d key-shaped token(s)" (List.length found)) ])
      @ List.filter_map
          (fun marker -> if rel <> self && Text.contains line marker then Some (finding "secret" rel number "private key material") else None)
          private_key_markers)
    (numbered_lines text)

(* ---- neutrality ---- *)

let verbs = [ "generated"; "written"; "created"; "authored"; "produced"; "drafted"; "built"; "made"; "coded"; "developed"; "implemented"; "designed"; "assisted" ]
let adverbs = [ "entirely"; "partly"; "partially"; "mostly"; "largely"; "fully"; "automatically"; "primarily" ]
let prepositions = [ "by"; "with"; "using"; "via"; "through" ]
let articles = [ "a"; "an"; "the" ]

let tool_words =
  [ "ai"; "llm"; "llms"; "chatbot"; "copilot"; "chatgpt"; "claude"; "gpt"; "gemini"; "grok"; "codex"; "cursor"; "anthropic"; "openai"; "assistant"; "agent" ]

let tool_phrases = [ "artificial intelligence"; "language model"; "large language model"; "ai assistant"; "coding assistant"; "ai model"; "ai tool"; "ai agent" ]

(* Words after a tool name that show the sentence is about the product. *)
let product_nouns =
  [ "adapter"; "api"; "provider"; "gateway"; "key"; "sdk"; "endpoint"; "client"; "request"; "response"; "responses"; "shape"; "shapes";
    "compatible"; "messages"; "model"; "models"; "proposer"; "tier"; "registry"; "call"; "calls"; "error"; "errors" ]

let fixed_phrases =
  [ "co-authored-by:"; "generated with claude"; "as an ai";
    "as a language model"; "this repository was generated"; "this project was generated"; "this code was generated";
    "this file was generated by an"; "this document was generated by an" ]

let split_words line =
  let acc = ref [] and current = Buffer.create 16 in
  let flush () =
    if Buffer.length current > 0 then begin
      acc := Buffer.contents current :: !acc;
      Buffer.clear current
    end
  in
  String.iter (fun c -> if is_word_char c || c = '-' then Buffer.add_char current (Char.lowercase_ascii c) else flush ()) line;
  flush ();
  List.rev !acc

(* A verb of making, a preposition and a tool, in that order: a statement
   about who or what made something. *)
let authorship_statement words =
  let rec after_verb = function
    | w :: rest when List.mem w adverbs -> after_verb rest
    | w :: rest when List.mem w prepositions -> after_preposition rest
    | _ -> false
  and after_preposition = function
    | w :: rest when List.mem w articles -> after_preposition rest
    | a :: b :: c :: rest when List.mem (a ^ " " ^ b ^ " " ^ c) tool_phrases -> not_product rest
    | a :: b :: rest when List.mem (a ^ " " ^ b) tool_phrases -> not_product rest
    | w :: rest when List.mem w tool_words -> not_product rest
    | _ -> false
  and not_product = function next :: _ when List.mem next product_nouns -> false | _ -> true in
  let rec scan = function
    | [] -> false
    | w :: rest -> (List.mem w verbs && after_verb rest) || scan rest
  in
  scan words

let neutrality_findings rel text =
  if rel = self then []
  else
    List.concat_map
      (fun (number, line) ->
        let lower = String.lowercase_ascii line in
        (match List.find_opt (fun phrase -> Text.contains lower phrase) fixed_phrases with
         | Some phrase -> [ finding "neutrality" rel number ("phrase '" ^ phrase ^ "'") ]
         | None -> [])
        @ if authorship_statement (split_words line) then [ finding "neutrality" rel number "statement about the tools used to write the repository" ] else [])
      (numbered_lines text)

(* ---- machine paths ---- *)

(* The directories under which the home of a user lives. *)
let home_roots = [ "/home/"; "/Users/" ]

(* A path below the home directory of a user: it names a machine and a person, and it is in a
   file only because some output was recorded with it. Fixtures, pictures of terminal sessions and
   documentation are written from what commands print, so this is where such a path would come in. *)
let machine_path_findings rel text =
  let name_char c = is_word_char c || c = '.' || c = '-' in
  let below_home line =
    List.exists
      (fun root ->
        let n = String.length line and r = String.length root in
        let rec from i =
          match Text.find_sub ~from:i line root with
          | None -> false
          | Some at ->
            let rec stop j = if j < n && name_char line.[j] then stop (j + 1) else j in
            let after = stop (at + r) in
            (after > at + r && after < n && line.[after] = '/') || from (at + r)
        in
        from 0)
      home_roots
  in
  List.filter_map
    (fun (number, line) -> if below_home line then Some (finding "path" rel number "a path below the home directory of a user") else None)
    (numbered_lines text)

(* ---- headers ---- *)

let notice = "Copyright (c) 2026 The Maithic Authors"
let header_extensions = [ ".ml"; ".mli"; ".lean"; ".jl"; ".r" ]

let header_findings rel text =
  if not (List.mem (Fs.extension rel) header_extensions) || Text.starts_with ~prefix:"fixtures/" rel
     || Text.starts_with ~prefix:"examples/" rel
  then []
  else begin
    let head = String.concat "\n" (List.filteri (fun i _ -> i < 6) (String.split_on_char '\n' text)) in
    if Text.contains head notice then [] else [ finding "header" rel 1 "missing copyright notice" ]
  end

(* ---- versions ---- *)

(* The text between [before] and the next [after] on the first line that contains [before]. *)
let extract ~before ~after text =
  match Text.find_sub text before with
  | None -> None
  | Some i ->
    let start = i + String.length before in
    (match Text.find_sub ~from:start text after with
     | Some stop -> Some (String.trim (String.sub text start (stop - start)))
     | None -> None)

let version_sources root files =
  let read = Repo.read root in
  let fixed =
    [ ("ocaml/dune-project", "(version ", ")"); ("ocaml/lib/schema/version.ml", "let version = \"", "\"");
      ("lean/lakefile.toml", "version = \"", "\""); ("lean/Maithic/Version.lean", "def version : String := \"", "\"");
      ("lean/MaithicBridge/Serialize/Types.lean", "def bridgeVersion : String := \"", "\""); ("CITATION.cff", "\nversion: ", "\n") ]
  in
  let julia =
    List.filter (fun f -> Text.starts_with ~prefix:"julia/" f && Filename.basename f = "Project.toml") files
    |> List.map (fun f -> (f, "version = \"", "\""))
  in
  let r =
    List.filter (fun f -> Text.starts_with ~prefix:"r/" f && Filename.basename f = "DESCRIPTION") files
    |> List.map (fun f -> (f, "Version: ", "\n"))
  in
  List.filter_map
    (fun (file, before, after) ->
      if List.mem file files then Some (file, extract ~before ~after (read file)) else None)
    (fixed @ julia @ r)

let version_findings root files =
  match Result.to_option (Repo.project root) with
  | None -> [ finding "version" "PROJECT.toml" 1 "cannot be read" ]
  | Some doc ->
    (match Option.bind (Json.path [ "project"; "version" ] doc) Json.string_opt with
     | None -> [ finding "version" "PROJECT.toml" 1 "missing [project] version" ]
     | Some version ->
       List.filter_map
         (fun (file, found) ->
           match found with
           | Some v when v = version -> None
           | Some v -> Some (finding "version" file 1 (Printf.sprintf "version %s differs from PROJECT.toml (%s)" v version))
           | None -> Some (finding "version" file 1 "no version found"))
         (version_sources root files))

(* ---- toolchain pin ---- *)

(* The toolchain is pinned at the root, for commands run anywhere in the
   repository, and in the Lean package. The two files must agree. *)
let toolchain_findings root =
  let root_pin = String.trim (Repo.read root "lean-toolchain") and package_pin = String.trim (Repo.read root "lean/lean-toolchain") in
  if root_pin = "" then [ finding "version" "lean-toolchain" 1 "missing or empty" ]
  else if root_pin <> package_pin then [ finding "version" "lean-toolchain" 1 "differs from lean/lean-toolchain" ]
  else if not (Text.ends_with ~suffix:Maithic_schema.Version.lean_toolchain root_pin) then
    [ finding "version" "lean-toolchain" 1 "differs from the Lean version the command expects" ]
  else []

(* ---- links ---- *)

(* The targets of the Markdown links of one line: the text between "](" and ")". *)
let link_targets line =
  let rec go from acc =
    match Text.find_sub ~from line "](" with
    | None -> List.rev acc
    | Some i ->
      (match String.index_from_opt line (i + 2) ')' with
       | Some stop -> go (stop + 1) (String.sub line (i + 2) (stop - i - 2) :: acc)
       | None -> List.rev acc)
  in
  go 0 []

(** Every relative link of a Markdown file must lead to a file or directory
    of the repository. *)
let link_findings root rel text =
  if Fs.extension rel <> ".md" then []
  else
    List.concat_map
      (fun (number, line) ->
        List.filter_map
          (fun target ->
            let target = match String.index_opt target ' ' with Some sp -> String.sub target 0 sp | None -> target in
            let path = match String.index_opt target '#' with Some h -> String.sub target 0 h | None -> target in
            let external_ = List.exists (fun prefix -> Text.starts_with ~prefix target) [ "http://"; "https://"; "mailto:"; "#" ] in
            if external_ || path = "" then None
            else if Fs.exists Fs.(root / Filename.concat (Filename.dirname rel) path) then None
            else Some (finding "link" rel number (Printf.sprintf "link target %s does not exist" path)))
          (link_targets line))
      (numbered_lines text)

(* ---- all checks ---- *)

(** Runs every check over the repository. *)
let run root =
  let files = Repo.files root in
  let per_file =
    List.concat_map
      (fun rel ->
        if not (Repo.is_text rel) then []
        else begin
          let text = Repo.read root rel in
          placeholder_findings rel text @ lean_findings rel text @ secret_findings rel text @ neutrality_findings rel text
          @ machine_path_findings rel text @ header_findings rel text @ link_findings root rel text
        end)
      files
  in
  per_file @ version_findings root files @ toolchain_findings root

let render findings =
  String.concat "" (List.map (fun f -> Printf.sprintf "%s:%d: [%s] %s\n" f.file f.line f.check f.detail) findings)
