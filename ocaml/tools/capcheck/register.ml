(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The capability register: one TOML file per pillar under
    [registry/capabilities/].

    The register is the map of the product: every capability has an
    identifier, a title, a specification sentence, a status and the test
    files that cover it. This module reads the register, writes it back
    in a canonical form and checks it against the test suites. *)

open Maithic_base
open Maithic_schema

type capability = { id : string; title : string; spec : string; status : string; tests : string list }

type group = {
  group_id : string;
  group_title : string;
  language : string;
  path : string;
  purpose : string;
  capabilities : capability list;
}

type pillar = {
  pillar_id : string;
  name : string;
  pillar_title : string;
  mission : string;
  groups : group list;
}

let statuses = [ "planned"; "in_progress"; "done" ]
let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x :: rest ->
    let* y = f x in
    let* ys = map_result f rest in
    Ok (y :: ys)

let capability_of_json j =
  let* id = Json.get_string "id" j in
  let* title = Json.get_string "title" j in
  let* status = Json.get_string "status" j in
  if not (List.mem status statuses) then Error (Printf.sprintf "%s: unknown status '%s'" id status)
  else
    Ok
      { id; title; status;
        spec = Json.string_or "" "spec" j;
        tests = Json.strings (Json.List (Json.list_or_empty "tests" j)) }

let group_of_json j =
  let* group_id = Json.get_string "id" j in
  let* group_title = Json.get_string "title" j in
  let* capabilities = map_result capability_of_json (Json.list_or_empty "capability" j) in
  Ok
    { group_id; group_title; capabilities;
      language = Json.string_or "" "language" j;
      path = Json.string_or "" "path" j;
      purpose = Json.string_or "" "purpose" j }

let pillar_of_json doc =
  let* head = match Json.member "pillar" doc with Some p -> Ok p | None -> Error "missing [pillar] table" in
  let* pillar_id = Json.get_string "id" head in
  let* name = Json.get_string "name" head in
  let* groups = map_result group_of_json (Json.list_or_empty "group" doc) in
  Ok
    { pillar_id; name; groups;
      pillar_title = Json.string_or "" "title" head;
      mission = Json.string_or "" "mission" head }

let directory root = Fs.(root / "registry" / "capabilities")
let file_of pillar = String.lowercase_ascii pillar.pillar_id ^ ".toml"

(** Loads every pillar file, in order. *)
let load root =
  let dir = directory root in
  let files = List.filter (fun f -> Fs.extension f = ".toml") (Fs.list_dir dir) in
  if files = [] then Error ("no register files in " ^ dir)
  else
    map_result
      (fun file ->
        match Result.bind (Toml.parse_file Fs.(dir / file)) pillar_of_json with
        | Ok pillar -> Ok pillar
        | Error e -> Error (file ^ ": " ^ e))
      files

(** The canonical text of a pillar file. *)
let emit pillar =
  let q = Toml.escape_string in
  let buf = Buffer.create 65536 in
  let line text = Buffer.add_string buf text; Buffer.add_char buf '\n' in
  line (Printf.sprintf "# Capability register for %s %s." pillar.pillar_id pillar.name);
  line "# Status and tests are kept in step with the test suites by `make cap-sync`.";
  line "";
  line "[pillar]";
  line ("id = " ^ q pillar.pillar_id);
  line ("name = " ^ q pillar.name);
  line ("title = " ^ q pillar.pillar_title);
  line ("mission = " ^ q pillar.mission);
  List.iter
    (fun g ->
      line "";
      line "[[group]]";
      line ("id = " ^ q g.group_id);
      line ("title = " ^ q g.group_title);
      line ("language = " ^ q g.language);
      line ("path = " ^ q g.path);
      line ("purpose = " ^ q g.purpose);
      List.iter
        (fun c ->
          line "";
          line "  [[group.capability]]";
          line ("  id = " ^ q c.id);
          line ("  title = " ^ q c.title);
          line ("  spec = " ^ q c.spec);
          line ("  status = " ^ q c.status);
          line ("  tests = [" ^ String.concat ", " (List.map q c.tests) ^ "]"))
        g.capabilities)
    pillar.groups;
  Buffer.contents buf

let all_capabilities pillars = List.concat_map (fun p -> List.concat_map (fun g -> g.capabilities) p.groups) pillars

(** The digest that pins identifiers and titles: SHA-256 over one line
    [id TAB title] per capability, in register order. *)
let titles_digest pillars =
  Sha256.hex (String.concat "" (List.map (fun c -> c.id ^ "\t" ^ c.title ^ "\n") (all_capabilities pillars)))

let digest_file root = Fs.(root / "registry" / "titles.sha256")

(* ---- finding the tests that name a capability ---- *)

(** Where tests live and how each language tags a test with a capability. *)
let test_locations =
  [ ("ocaml/test", [ ".ml" ], [ "\"" ]);
    ("lean/MaithicTest", [ ".lean" ], [ "-- cap: " ]);
    ("julia", [ ".jl" ], [ "@testset \"" ]);
    ("r", [ ".r" ], [ "test_that(\"" ]);
    ("ocaml/test/cram", [ ".t" ], [ "# cap: " ]) ]

(* Identifiers that directly follow one of the markers in a text and are
   followed by a space or the end of the line: the start of a test name, or a
   tag comment. An identifier in quotes on its own is data, not a tag. *)
let tagged_ids markers text =
  List.concat_map
    (fun marker ->
      let n = String.length marker in
      let rec go from acc =
        match Text.find_sub ~from text marker with
        | None -> List.rev acc
        | Some i ->
          let start = i + n in
          let acc =
            if start + 9 <= String.length text then
              match Cap_id.of_string (String.sub text start 9) with
              | Some id
                when start + 9 = String.length text || (match text.[start + 9] with ' ' | '\n' | '\r' -> true | _ -> false) ->
                Cap_id.to_string id :: acc
              | _ -> acc
            else acc
          in
          go (i + 1) acc
      in
      go 0 [])
    markers

(** Every (capability, test file) pair found in the test suites. *)
let scan_tests root =
  List.concat_map
    (fun (dir, extensions, markers) ->
      let base = Fs.(root / dir) in
      if not (Fs.is_dir base) then []
      else
        Fs.walk ~skip:Repo.skip base
        |> List.filter (fun rel -> List.mem (Fs.extension rel) extensions)
        |> List.filter (fun rel ->
               (* Julia and R tests live in the test directories of their packages. *)
               match dir with
               | "julia" -> Text.contains rel "/test/"
               | "r" -> Text.contains rel "/tests/"
               | _ -> true)
        |> List.concat_map (fun rel ->
               let file = Fs.(dir / rel) in
               List.map (fun id -> (id, file)) (List.sort_uniq compare (tagged_ids markers (Repo.read root file)))))
    test_locations
  |> List.sort_uniq compare

(** Sets status and tests from what the suites actually contain: a
    capability named by a test is done and lists the files that name it; a
    capability that was done and is no longer named goes back to planned. *)
let sync pillars tags =
  let files_of id = List.filter_map (fun (tag, file) -> if tag = id then Some file else None) tags in
  let update c =
    match files_of c.id with
    | [] -> if c.status = "done" then { c with status = "planned"; tests = [] } else { c with tests = [] }
    | files -> { c with status = "done"; tests = files }
  in
  List.map (fun p -> { p with groups = List.map (fun g -> { g with capabilities = List.map update g.capabilities }) p.groups }) pillars

(* ---- checking ---- *)

let path_alternatives path =
  Text.split_on ", " path |> List.concat_map (Text.split_on " and ") |> List.map String.trim |> List.filter (fun p -> p <> "")

let path_exists root pattern =
  match String.index_opt pattern '*' with
  | None -> Fs.exists Fs.(root / pattern)
  | Some star ->
    let dir = Filename.dirname pattern in
    let prefix = Filename.basename (String.sub pattern 0 star) in
    let suffix = String.sub pattern (star + 1) (String.length pattern - star - 1) in
    List.exists (fun name -> Text.starts_with ~prefix name && Text.ends_with ~suffix name) (Fs.list_dir Fs.(root / dir))

(** Everything that is wrong with the register; empty when it is sound. *)
let check ?(min_per_pillar = 0) root pillars tags =
  let caps = all_capabilities pillars in
  let ids = List.map (fun c -> c.id) caps in
  let problems = ref [] in
  let problem fmt = Printf.ksprintf (fun text -> problems := text :: !problems) fmt in
  (* Identifiers are well formed, stand in order, and sit in the group and pillar they name. *)
  let rec ordered = function
    | a :: (b :: _ as rest) ->
      if compare a b >= 0 then problem "%s stands before %s: identifiers must rise and may not repeat" a b;
      ordered rest
    | _ -> ()
  in
  ordered ids;
  List.iter
    (fun p ->
      List.iter
        (fun g ->
          if not (Text.starts_with ~prefix:(p.pillar_id ^ "-") g.group_id) then problem "group %s stands in the file of %s" g.group_id p.pillar_id;
          List.iter
            (fun c ->
              if Cap_id.of_string c.id = None then problem "%s is not a capability identifier" c.id
              else if not (Text.starts_with ~prefix:(g.group_id ^ "-") c.id) then problem "%s stands in group %s" c.id g.group_id)
            g.capabilities)
        p.groups;
      let held = List.length (List.concat_map (fun g -> g.capabilities) p.groups) in
      if held < min_per_pillar then
        problem "%s %s holds %d capabilities; every pillar must hold at least %d" p.pillar_id p.name held min_per_pillar)
    pillars;
  (match Fs.read (digest_file root) with
   | Ok recorded when String.trim recorded = titles_digest pillars -> ()
   | Ok _ -> problem "identifiers or titles differ from registry/titles.sha256"
   | Error _ -> problem "registry/titles.sha256 is missing");
  List.iter
    (fun c ->
      if c.status = "done" then begin
        if c.tests = [] then problem "%s is done but lists no test" c.id;
        List.iter
          (fun file ->
            if not (Fs.exists Fs.(root / file)) then problem "%s lists a test file that does not exist: %s" c.id file
            else if not (List.mem (c.id, file) tags) then problem "%s is not named by a test in %s" c.id file)
          c.tests
      end
      else if c.tests <> [] then problem "%s lists tests but its status is '%s'" c.id c.status)
    caps;
  List.iter (fun (id, file) -> if not (List.mem id ids) then problem "%s names an unknown capability %s" file id) tags;
  List.iter
    (fun (id, file) ->
      match List.find_opt (fun c -> c.id = id) caps with
      | Some c when c.status <> "done" -> problem "%s is named by %s but its status is '%s' (run `make cap-sync`)" id file c.status
      | Some c when not (List.mem file c.tests) -> problem "%s is named by %s, which the register does not list (run `make cap-sync`)" id file
      | _ -> ())
    tags;
  List.iter
    (fun p ->
      List.iter
        (fun g ->
          if List.exists (fun c -> c.status = "done") g.capabilities
             && not (List.exists (path_exists root) (path_alternatives g.path))
          then problem "%s has done capabilities but its path does not exist: %s" g.group_id g.path)
        p.groups)
    pillars;
  List.rev !problems

(* ---- reporting ---- *)

type counts = { done_ : int; in_progress : int; planned : int }

let count caps =
  let n status = List.length (List.filter (fun c -> c.status = status) caps) in
  { done_ = n "done"; in_progress = n "in_progress"; planned = n "planned" }

let pillar_caps p = List.concat_map (fun g -> g.capabilities) p.groups

(** The status page, [registry/STATUS.md]. *)
let status_markdown pillars =
  let total = count (all_capabilities pillars) in
  let all = all_capabilities pillars in
  let rows =
    List.map
      (fun p ->
        let caps = pillar_caps p in
        let c = count caps in
        Printf.sprintf "| %s | %s | %s | %d | %d | %d |" p.pillar_id p.name p.pillar_title (List.length caps) c.done_ (c.planned + c.in_progress))
      pillars
  in
  let groups =
    List.concat_map
      (fun p ->
        List.map
          (fun g ->
            Printf.sprintf "### %s %s\n\n%s\n\n%s\n" g.group_id g.group_title g.purpose
              (String.concat "\n"
                 (List.map (fun c -> Printf.sprintf "- [%s] `%s` %s" (if c.status = "done" then "x" else " ") c.id c.title) g.capabilities)))
          p.groups)
      pillars
  in
  String.concat "\n"
    ([ "# Capability status"; "";
       "This page is written by `make cap-sync` from the capability register and the test suites. A capability is counted";
       "as done only when a test that names it exists; `make hygiene` fails if the register and the tests disagree."; "";
       Printf.sprintf "**%d of %d capabilities are done** (%d to do)." total.done_ (List.length all) (total.in_progress + total.planned);
       ""; "| Pillar | Name | What it is | Capabilities | Done | To do |"; "|---|---|---|---:|---:|---:|" ]
     @ rows
     @ [ ""; "## Capabilities by group"; "" ]
     @ groups)

(** The four languages a group can be written in, in the order they are reported. *)
let languages = [ "OCaml"; "Lean"; "Julia"; "R" ]

(** The language a group is counted under: the first one it names, so that a
    group in "OCaml, shell" counts as OCaml. *)
let primary_language g =
  match String.split_on_char ',' g.language with first :: _ -> String.trim first | [] -> ""

(** The capabilities of a pillar by the language of their group. *)
let capabilities_by_language p =
  List.map
    (fun language ->
      ( language,
        List.fold_left (fun n g -> if primary_language g = language then n + List.length g.capabilities else n) 0 p.groups ))
    languages

let status_json pillars =
  let all = all_capabilities pillars in
  let c = count all in
  Json.Obj
    [ ("total", Json.Int (List.length all)); ("done", Json.Int c.done_); ("in_progress", Json.Int c.in_progress);
      ("planned", Json.Int c.planned);
      ( "pillars",
        Json.List
          (List.map
             (fun p ->
               let pc = count (pillar_caps p) in
               Json.Obj
                 [ ("id", Json.String p.pillar_id); ("name", Json.String p.name); ("done", Json.Int pc.done_);
                   ("in_progress", Json.Int pc.in_progress); ("planned", Json.Int pc.planned);
                   ("languages", Json.Obj (List.map (fun (language, n) -> (language, Json.Int n)) (capabilities_by_language p))) ])
             pillars) ) ]
