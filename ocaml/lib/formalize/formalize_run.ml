(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_schema

type tools = {
  draft : count:int -> Formalize_intake.t -> (Formalize_candidate.t list, string) result;
  compile : Formalize_elab.compile;
  repair : Formalize_elab.repair option;
  lean : Formalize_faith.lean;
  model : Formalize_faith.model option;
  same : Formalize_candidate.t -> Formalize_candidate.t -> string option;
}

type settings = { candidates : int; rounds : int; name : string; imports : string list }

let default_settings = { candidates = 3; rounds = Formalize_elab.default_rounds; name = "problem"; imports = [] }

type verdict = Chosen | Kept | Duplicate of int * string | Rejected of string | Failed of string

let verdict_name = function Chosen -> "chosen" | Kept -> "kept" | Duplicate _ -> "duplicate" | Rejected _ -> "rejected" | Failed _ -> "failed"

type entry = { drafted : Formalize_candidate.t; elaboration : Formalize_elab.result; faith : Formalize_faith.t option; verdict : verdict; rank : int option }
type status = Formalized | Flagged | Nothing

let status_name = function Formalized -> "formalized" | Flagged -> "flagged" | Nothing -> "none"

type t = { intake : Formalize_intake.t; settings : settings; entries : entry list; status : status; note : string }

let schema = "maithic.formalize.run/1"
let final entry = entry.elaboration.Formalize_elab.candidate
let chosen t = List.find_opt (fun e -> e.verdict = Chosen) t.entries

let given ~name ~imports statements =
  let kept, _ =
    List.fold_left
      (fun (kept, seen) statement ->
        let id = List.length kept + 1 in
        let candidate =
          { Formalize_candidate.id; name = Printf.sprintf "%s_%d" name id; signature = Formalize_candidate.signature_of statement; imports; reading = "";
            provenance = Formalize_candidate.given }
        in
        let key = Formalize_candidate.key candidate in
        if candidate.Formalize_candidate.signature = "" || List.mem key seen then (kept, seen) else (kept @ [ candidate ], key :: seen))
      ([], []) statements
  in
  kept

(* Names in a row: "a", "a and b", "a, b and c". *)
let listed names =
  match List.rev names with
  | [] -> ""
  | [ only ] -> only
  | last :: rest -> String.concat ", " (List.rev rest) ^ " and " ^ last

let run ?(settings = default_settings) tools intake =
  match tools.draft ~count:settings.candidates intake with
  | Error why -> Error why
  | Ok drafted ->
    (* Compile each, and repair what does not compile. *)
    let compiled = List.map (fun c -> (c, Formalize_elab.run ~rounds:settings.rounds ~compile:tools.compile ?repair:tools.repair c)) drafted in
    (* Of two statements Lean shows to be one statement, the later is dropped, before a model is asked about it. *)
    let entries, _ =
      List.fold_left
        (fun (entries, originals) ((c : Formalize_candidate.t), (e : Formalize_elab.result)) ->
          let entry verdict faith = { drafted = c; elaboration = e; faith; verdict; rank = None } in
          if not e.Formalize_elab.elaborated then (entries @ [ entry (Failed e.Formalize_elab.note) None ], originals)
          else
            let candidate = e.Formalize_elab.candidate in
            match List.find_map (fun (o : Formalize_candidate.t) -> Option.map (fun relation -> (o.Formalize_candidate.id, relation)) (tools.same o candidate)) originals with
            | Some (id, relation) -> (entries @ [ entry (Duplicate (id, relation)) None ], originals)
            | None ->
              let faith = Formalize_faith.validate ?model:tools.model ~lean:tools.lean intake candidate ~statement:e.Formalize_elab.statement in
              let verdict = match faith.Formalize_faith.rejected with Some why -> Rejected why | None -> Kept in
              (entries @ [ entry verdict (Some faith) ], originals @ [ candidate ]))
        ([], []) compiled
    in
    (* The best score first; of two with the same score, the one the model put first. *)
    let score entry = match entry.faith with Some f -> f.Formalize_faith.score | None -> 0.0 in
    let ranked =
      List.stable_sort (fun a b -> compare (score b) (score a)) (List.filter (fun e -> e.verdict = Kept) entries)
      |> List.mapi (fun i e -> (e.drafted.Formalize_candidate.id, i + 1))
    in
    let entries =
      List.map
        (fun e ->
          match List.assoc_opt e.drafted.Formalize_candidate.id ranked with
          | Some 1 -> { e with verdict = Chosen; rank = Some 1 }
          | Some rank -> { e with rank = Some rank }
          | None -> e)
        entries
    in
    let status, note =
      match List.find_opt (fun e -> e.verdict = Chosen) entries with
      | Some { faith = Some f; _ } when Formalize_faith.flagged f <> [] -> (Flagged, "flagged by " ^ listed (Formalize_faith.flagged f))
      | Some _ -> (Formalized, "")
      | None ->
        ( Nothing,
          if drafted = [] then "the model returned no statement"
          else if List.for_all (fun e -> match e.verdict with Failed _ -> true | _ -> false) entries then "no candidate elaborates"
          else "no candidate passed the checks" )
    in
    Ok { intake; settings; entries; status; note }

let count p entries = List.length (List.filter p entries)

let counts t =
  let elab p = count (fun e -> p e.elaboration) t.entries in
  [ ("drafted", List.length t.entries); ("elaborated_first_try", elab Formalize_elab.first_try);
    ("repaired", elab (fun r -> r.Formalize_elab.elaborated && not (Formalize_elab.first_try r))); ("elaborated", elab (fun r -> r.Formalize_elab.elaborated));
    ("failed", count (fun e -> match e.verdict with Failed _ -> true | _ -> false) t.entries);
    ("duplicates", count (fun e -> match e.verdict with Duplicate _ -> true | _ -> false) t.entries);
    ("rejected", count (fun e -> match e.verdict with Rejected _ -> true | _ -> false) t.entries);
    ("kept", count (fun e -> e.verdict = Kept || e.verdict = Chosen) t.entries) ]

let reason = function
  | Chosen | Kept -> ""
  | Duplicate (id, relation) ->
    Printf.sprintf "it says what candidate %d says (%s)" id (if relation = "identical" then "the same statement" else "the same once definitions are unfolded")
  | Rejected why -> why
  | Failed why -> why

(* Text set off by two spaces, as in a comment. *)
let set_off text = String.concat "\n" (List.map (fun line -> if line = "" then "" else "  " ^ line) (String.split_on_char '\n' (String.trim text)))

(* The checks as lines of a comment: the name, the outcome, and what was found. *)
let check_lines (f : Formalize_faith.t) =
  let width = List.fold_left (fun w (c : Formalize_faith.check) -> max w (String.length c.Formalize_faith.name)) 0 f.Formalize_faith.checks in
  List.map
    (fun (c : Formalize_faith.check) ->
      if c.Formalize_faith.note = "" then Printf.sprintf "%s  %s" (Text.pad_right width c.Formalize_faith.name) c.Formalize_faith.outcome
      else Printf.sprintf "%s  %s  %s" (Text.pad_right width c.Formalize_faith.name) (Text.pad_right 7 c.Formalize_faith.outcome) c.Formalize_faith.note)
    f.Formalize_faith.checks

let lean_text t =
  match chosen t with
  | None -> None
  | Some entry ->
    let candidate = { (final entry) with Formalize_candidate.name = t.settings.name } in
    let total = List.length t.entries in
    let checks = match entry.faith with Some f -> check_lines f | None -> [] in
    let english = match entry.faith with Some f when f.Formalize_faith.english <> "" -> [ ""; "Read back into English, the statement says:"; ""; set_off f.Formalize_faith.english ] | _ -> [] in
    let comment =
      [ "/-"; "The problem, as it was posed:"; ""; set_off t.intake.Formalize_intake.text; "";
        Printf.sprintf "The statement below was chosen from %d candidate%s. Lean elaborates it; nothing here proves it." total (if total = 1 then "" else "s") ]
      @ (if candidate.Formalize_candidate.reading = "" then [] else [ "Its reading of the problem: " ^ candidate.Formalize_candidate.reading ])
      @ english
      @ [ ""; "What the checks found:"; "" ]
      @ List.map (fun line -> "  " ^ line) checks
      @ [ ""; "That a statement says what was meant is a matter of evidence. Read it before you rely on it."; "-/" ]
    in
    (* The option that makes an undeclared name an error is part of how the statement was checked, and stays with it. *)
    let header = Formalize_candidate.header candidate in
    Some
      (String.concat ""
         [ header; (if header = "" then "" else "\n"); String.concat "\n" comment; "\n";
           Printf.sprintf "set_option autoImplicit false in\ntheorem %s %s := by\n  sorry\n" candidate.Formalize_candidate.name candidate.Formalize_candidate.signature ])

let entry_to_json entry =
  let candidate = final entry in
  let faith =
    match entry.faith with
    | None -> []
    | Some f -> (match Formalize_faith.to_json f with Json.Obj members -> members | _ -> [])
  in
  Json.Obj
    ([ ("id", Json.Int entry.drafted.Formalize_candidate.id); ("name", Json.String entry.drafted.Formalize_candidate.name);
       ("drafted", Json.String entry.drafted.Formalize_candidate.signature); ("statement", Json.String candidate.Formalize_candidate.signature);
       ("reading", Json.String entry.drafted.Formalize_candidate.reading) ]
     @ (match Formalize_candidate.to_json entry.drafted with Json.Obj members -> List.filter (fun (k, _) -> k = "provenance") members | _ -> [])
     @ [ ("elaboration", Formalize_elab.to_json entry.elaboration) ]
     @ faith
     @ [ ("verdict", Json.String (verdict_name entry.verdict)) ]
     @ (match entry.verdict with Duplicate (id, relation) -> [ ("duplicate_of", Json.Obj [ ("id", Json.Int id); ("relation", Json.String relation) ]) ] | _ -> [])
     @ (match reason entry.verdict with "" -> [] | why -> [ ("reason", Json.String why) ])
     @ match entry.rank with Some rank -> [ ("rank", Json.Int rank) ] | None -> [])

let to_json t =
  Json.Obj
    ([ ("schema", Json.String schema); ("problem", Formalize_intake.to_json t.intake);
       ( "settings",
         Json.Obj
           [ ("candidates", Json.Int t.settings.candidates); ("rounds", Json.Int t.settings.rounds); ("name", Json.String t.settings.name);
             ("imports", Json.List (List.map (fun m -> Json.String m) t.settings.imports)) ] );
       ("status", Json.String (status_name t.status)) ]
     @ (if t.note = "" then [] else [ ("note", Json.String t.note) ])
     @ (match chosen t with Some e -> [ ("chosen", Json.Int e.drafted.Formalize_candidate.id) ] | None -> [])
     @ [ ("candidates", Json.List (List.map entry_to_json t.entries)); ("summary", Json.Obj (List.map (fun (name, n) -> (name, Json.Int n)) (counts t))) ]
     @ match lean_text t with Some text -> [ ("lean", Json.String text) ] | None -> [])

let summary t =
  let n name = List.assoc name (counts t) in
  let parts =
    List.filter_map
      (fun (n, text) -> if n = 0 then None else Some (Printf.sprintf "%d %s" n text))
      [ (n "repaired", "repaired"); (n "failed", "did not elaborate"); (n "duplicates", if n "duplicates" = 1 then "duplicate" else "duplicates"); (n "rejected", "rejected") ]
  in
  let detail = if parts = [] then "" else Printf.sprintf " (%s)" (String.concat ", " parts) in
  match (t.status, chosen t) with
  | Formalized, Some e -> Printf.sprintf "formalized: candidate %d of %d%s" e.drafted.Formalize_candidate.id (n "drafted") detail
  | Flagged, Some e -> Printf.sprintf "chosen with a flag: candidate %d of %d, %s%s" e.drafted.Formalize_candidate.id (n "drafted") t.note detail
  | _ -> Printf.sprintf "no statement: %s%s" t.note detail

let render ?(color = false) t =
  let paint code s = if color then Printf.sprintf "\027[%sm%s\027[0m" code s else s in
  let buffer = Buffer.create 2048 in
  let add = Buffer.add_string buffer in
  let label text = paint "2" (Text.pad_right 11 text) in
  List.iteri
    (fun i line -> add (if line = "" then "\n" else Printf.sprintf "%s%s\n" (label (if i = 0 then "problem" else "")) line))
    (String.split_on_char '\n' (String.trim t.intake.Formalize_intake.text));
  (match String.split_on_char '\n' (Formalize_intake.frame t.intake) with
   | [] | [ "" ] -> ()
   | lines -> List.iter (fun line -> add (Printf.sprintf "%s%s\n" (label "") (paint "2" line))) lines);
  List.iter
    (fun entry ->
      let candidate = final entry in
      let e = entry.elaboration in
      add "\n";
      add
        (Printf.sprintf "%s %s\n" (paint "1" (Printf.sprintf "candidate %d" entry.drafted.Formalize_candidate.id))
           (String.concat "\n            " (String.split_on_char '\n' candidate.Formalize_candidate.signature)));
      if entry.drafted.Formalize_candidate.reading <> "" then add (Printf.sprintf "  %s%s\n" (label "reading") entry.drafted.Formalize_candidate.reading);
      List.iter
        (fun (a : Formalize_elab.attempt) ->
          if not a.Formalize_elab.elaborated then begin
            add
              (Printf.sprintf "  %s%s  %s\n" (label (if a.Formalize_elab.round = 0 then "drafted" else Printf.sprintf "repair %d" a.Formalize_elab.round)) (paint "31" "does not elaborate")
                 (paint "2" a.Formalize_elab.error_class));
            if a.Formalize_elab.signature <> candidate.Formalize_candidate.signature then add (Printf.sprintf "  %s%s\n" (label "") (paint "2" a.Formalize_elab.signature));
            match String.split_on_char '\n' (String.trim a.Formalize_elab.said) with
            | first :: _ when first <> "" -> add (Printf.sprintf "  %s%s %s\n" (label "") (paint "2" "Lean:") (Text.truncate 160 first))
            | _ -> ()
          end)
        e.Formalize_elab.history;
      if e.Formalize_elab.elaborated then
        add
          (Printf.sprintf "  %s%s\n" (label "elaborates")
             (if Formalize_elab.first_try e then "as drafted" else Printf.sprintf "after %d repair%s" (Formalize_elab.repairs e) (if Formalize_elab.repairs e = 1 then "" else "s")));
      (match entry.faith with
       | None -> ()
       | Some f ->
         let mark (c : Formalize_faith.check) =
           (match c.Formalize_faith.outcome with "passed" -> paint "32" "✓" | "failed" -> paint "31" "✗" | "flagged" -> paint "33" "!" | _ -> paint "2" "·")
           ^ " "
         in
         List.iter
           (fun (c : Formalize_faith.check) ->
             if not (c.Formalize_faith.outcome = "skipped" && c.Formalize_faith.note = "the statement was rejected") then
               add
                 (Printf.sprintf "  %s%s  %s%s\n" (mark c) (Text.pad_right 16 c.Formalize_faith.name)
                    (paint
                       (match c.Formalize_faith.outcome with "passed" -> "32" | "failed" -> "31" | "flagged" -> "33" | _ -> "2")
                       (if c.Formalize_faith.note = "" then c.Formalize_faith.outcome else Text.pad_right 7 c.Formalize_faith.outcome))
                    (if c.Formalize_faith.note = "" then "" else "  " ^ c.Formalize_faith.note)))
           f.Formalize_faith.checks;
         if f.Formalize_faith.english <> "" then add (Printf.sprintf "  %s%s\n" (label "says") f.Formalize_faith.english));
      add
        (Printf.sprintf "  %s %s\n" (paint "2" "→")
           (match entry.verdict with
            | Chosen -> paint "32" "chosen" ^ (match entry.faith with Some f -> Printf.sprintf "  score %.2f" f.Formalize_faith.score | None -> "")
            | Kept ->
              "kept"
              ^ (match (entry.rank, entry.faith) with Some rank, Some f -> Printf.sprintf "  rank %d, score %.2f" rank f.Formalize_faith.score | _ -> "")
            | Duplicate _ -> paint "33" "dropped" ^ "  " ^ reason entry.verdict
            | Rejected _ -> paint "31" "rejected" ^ "  " ^ reason entry.verdict
            | Failed _ -> paint "31" "failed" ^ "  " ^ reason entry.verdict)))
    t.entries;
  Buffer.contents buffer

let exit_code t = match t.status with Formalized -> Exit_code.Success | Flagged | Nothing -> Exit_code.Not_verified
