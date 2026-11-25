(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_prover

type metrics = { lines : int; steps : int; chars : int }

let measure proof =
  let lines = List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' proof) in
  { lines = List.length lines; steps = List.length (List.filter (fun l -> String.trim l <> "by") lines); chars = Text.utf8_length (String.trim proof) }

type result = {
  theorem : string;
  line : int;
  before : string;
  after : string;
  size_before : metrics;
  size_after : metrics;
  how : string;
  removed : string list;
  checks : int;
  axioms : string list;
  note : string;
}

let automation = [ "by rfl"; "by simp"; "by omega"; "by decide"; "by grind"; "by simp_all" ]

let indent line =
  let rec go i = if i < String.length line && line.[i] = ' ' then go (i + 1) else i in
  go 0

let blank line = String.trim line = ""

(* A line that goes on from the step before it at the same depth: an alternative of a case split,
   or the closing of a bracket. *)
let continues line =
  let line = String.trim line in
  List.exists (fun prefix -> Text.starts_with ~prefix line) [ "|"; ")"; "]"; "}"; "\xE2\x9F\xA9"; "with" ]

(* The lines a step owns: the line itself, the deeper lines that follow it, and the lines that go
   on from it at its own depth, such as the cases of an induction. *)
let subtree lines i =
  let own = indent (List.nth lines i) in
  let rec go j =
    match List.nth_opt lines j with
    | Some line when blank line || indent line > own || (indent line = own && continues line) -> go (j + 1)
    | _ -> j
  in
  let stop = go (i + 1) in
  (* Blank lines at the end belong to what follows. *)
  let rec back j = if j > i + 1 && blank (List.nth lines (j - 1)) then back (j - 1) else j in
  back stop

(* A line that cannot be taken out on its own: an alternative of a case split, the closing of a
   bracket, or the [by] that opens the proof. *)
let fixed line = String.trim line = "by" || continues line

(* A line that opens a block, which must not be left empty. *)
let opens line =
  let line = String.trim line in
  List.exists (fun suffix -> Text.ends_with ~suffix line) [ "by"; "=>"; ":="; "do"; "then"; "else"; "from" ]

(* Whether every line that opens a block is followed by a deeper line. Taking out the only step
   of a block would leave a line that is not. The first line of a proof holds its [by], and
   whatever follows is its body. *)
let well_formed kept =
  let lines = List.filter (fun l -> not (blank l)) kept in
  let rec go first = function
    | [] -> true
    | [ last ] -> not (opens last)
    | line :: (next :: _ as rest) -> ((not (opens line)) || first || indent next > indent line) && go false rest
  in
  go true lines

let step_removals proof =
  let lines = String.split_on_char '\n' proof in
  let n = List.length lines in
  (* The first line holds the [by], and a step beside it when the proof begins on that line. *)
  List.filter_map
    (fun i ->
      let line = List.nth lines i in
      if i = 0 || blank line || fixed line then None
      else
        let stop = subtree lines i in
        let kept = List.filteri (fun j _ -> j < i || j >= stop) lines in
        if List.for_all (fun l -> blank l || String.trim l = "by") kept || not (well_formed kept) then None
        else Some (String.concat "\n" kept, String.trim line))
    (List.rev (List.init n Fun.id))

(* The lemma lists of a line: for each, where its bracket opens and closes and its items. A list
   counts when it follows a tactic that takes lemmas. *)
let lemma_lists line =
  let takers = [ "simp only"; "simp_all only"; "simp_all"; "simp"; "simpa"; "rw"; "rwa"; "grind"; "dsimp only"; "dsimp"; "unfold" ] in
  let n = String.length line in
  let word_before i =
    (* The text before the bracket, without what a configuration in brackets or an arrow adds. *)
    let before = String.trim (String.sub line 0 i) in
    List.find_opt
      (fun taker ->
        Text.ends_with ~suffix:taker before
        && (String.length before = String.length taker
            || not (let c = before.[String.length before - String.length taker - 1] in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_')))
      takers
  in
  let rec closing i depth = if i >= n then None else match line.[i] with '[' | '(' | '{' -> closing (i + 1) (depth + 1) | ']' | ')' | '}' -> if depth = 0 then Some i else closing (i + 1) (depth - 1) | _ -> closing (i + 1) depth in
  let rec go i acc =
    if i >= n then List.rev acc
    else if line.[i] = '[' then
      match word_before i, closing (i + 1) 0 with
      | Some taker, Some stop ->
        let inner = String.sub line (i + 1) (stop - i - 1) in
        (* Items are split at the commas that stand in no bracket. *)
        let items =
          let buffer = Buffer.create 32 and depth = ref 0 and out = ref [] in
          String.iter
            (fun c ->
              (match c with '[' | '(' | '{' -> incr depth | ']' | ')' | '}' -> decr depth | _ -> ());
              if c = ',' && !depth = 0 then begin
                out := Buffer.contents buffer :: !out;
                Buffer.clear buffer
              end
              else Buffer.add_char buffer c)
            inner;
          List.rev_map String.trim (Buffer.contents buffer :: !out)
        in
        go (stop + 1) ((taker, i, stop, items) :: acc)
      | _ -> go (i + 1) acc
    else go (i + 1) acc
  in
  go 0 []

let argument_removals proof =
  let lines = String.split_on_char '\n' proof in
  List.concat
    (List.rev
       (List.mapi
          (fun at line ->
            List.concat_map
              (fun (taker, opens, closes, items) ->
                let count = List.length items in
                List.filter_map
                  (fun k ->
                    let rest = List.filteri (fun j _ -> j <> k) items in
                    let replaced =
                      if rest = [] then
                        (* [simp \[a\]] without its lemma is [simp]; a rewrite without a rule is nothing, and is a step to delete. *)
                        if List.mem taker [ "simp"; "simp_all"; "simpa"; "grind"; "dsimp" ] then
                          Some (String.trim (String.sub line 0 opens) |> fun head -> String.make (indent line) ' ' ^ String.trim head ^ String.sub line (closes + 1) (String.length line - closes - 1))
                        else None
                      else Some (String.sub line 0 (opens + 1) ^ String.concat ", " rest ^ String.sub line closes (String.length line - closes))
                    in
                    Option.map
                      (fun changed ->
                        ( String.concat "\n" (List.mapi (fun j l -> if j = at then changed else l) lines),
                          Printf.sprintf "%s in `%s`" (List.nth items k) (String.trim line) ))
                      replaced)
                  (List.rev (List.init count Fun.id)))
              (List.rev (lemma_lists line)))
          lines))

let candidates proof =
  let proof = String.trim proof in
  if not (Text.starts_with ~prefix:"by" proof) then [] else step_removals proof @ argument_removals proof

let keeps ~(reference : Mend_check.verdict) (v : Mend_check.verdict) =
  Mend_check.proves ~reference v && List.for_all (fun axiom -> List.mem axiom reference.Mend_check.axioms) v.Mend_check.axioms

let calls = automation

let minimize ?(automation = false) ?(max_checks = 200) ~(check : Mend_check.t) (decl : Lean_source.decl) =
  let before = String.trim decl.Lean_source.proof in
  let checks = ref 0 and at_a_price = ref [] in
  let ask proof =
    incr checks;
    check ~untrusted:"" (Lean_source.assemble decl proof)
  in
  (* Whether a shorter proof may stand. One that Lean accepts and that needs new axioms may not,
     and the axioms are remembered for the report. *)
  let keeps_noting ~reference proof =
    let verdict = ask proof in
    if keeps ~reference verdict then true
    else begin
      if Mend_check.proves ~reference verdict then
        at_a_price := List.sort_uniq compare (!at_a_price @ List.filter (fun a -> not (List.mem a reference.Mend_check.axioms)) verdict.Mend_check.axioms);
      false
    end
  in
  let unchanged ?(axioms = []) note =
    { theorem = decl.Lean_source.name; line = decl.Lean_source.line; before; after = before; size_before = measure before; size_after = measure before;
      how = "unchanged"; removed = []; checks = !checks; axioms; note }
  in
  let reference = ask before in
  if not (reference.Mend_check.accepted && not reference.Mend_check.uses_sorry) then
    unchanged
      (if reference.Mend_check.uses_sorry then "the proof is not finished: it rests on sorry"
       else "the proof does not compile as it stands: " ^ Mend_detect.first_line (Mend_check.why_not ~reference reference))
  else begin
    let finish how after removed =
      { theorem = decl.Lean_source.name; line = decl.Lean_source.line; before; after; size_before = measure before; size_after = measure after; how;
        removed; checks = !checks; axioms = reference.Mend_check.axioms; note = "" }
    in
    let by_automation =
      if automation && (measure before).steps >= 2 && not (List.mem before calls) then
        List.find_opt (fun call -> !checks < max_checks && keeps_noting ~reference call) calls
      else None
    in
    match by_automation with
    | Some call -> finish "automation" call [ "the whole proof, for `" ^ call ^ "`" ]
    | None ->
      (* One removal at a time; after each that Lean accepts, the search starts again from the shorter proof. *)
      let rec shrink proof removed =
        let rec first = function
          | [] -> None
          | (shorter, what) :: rest -> if !checks >= max_checks then None else if keeps_noting ~reference shorter then Some (shorter, what) else first rest
        in
        match first (candidates proof) with Some (shorter, what) -> shrink shorter (what :: removed) | None -> (proof, List.rev removed)
      in
      let after, removed = shrink before [] in
      if removed <> [] then finish "deletion" after removed
      else
        unchanged ~axioms:reference.Mend_check.axioms
          (match !at_a_price with
           | [] -> ""
           | axioms -> Printf.sprintf "Lean accepts a shorter proof, and it needs what this proof does without: %s" (String.concat ", " axioms))
  end

let changed r = r.how <> "unchanged"
let schema = "maithic.mend.minimization/1"
let metrics_to_json m = Json.Obj [ ("lines", Json.Int m.lines); ("steps", Json.Int m.steps); ("chars", Json.Int m.chars) ]

let result_to_json r =
  Json.Obj
    ([ ("theorem", Json.String r.theorem); ("line", Json.Int r.line); ("how", Json.String r.how); ("before", Json.String r.before);
       ("after", Json.String r.after); ("size_before", metrics_to_json r.size_before); ("size_after", metrics_to_json r.size_after);
       ("removed", Json.List (List.map (fun s -> Json.String s) r.removed)); ("checks", Json.Int r.checks);
       ("axioms", Json.List (List.map (fun s -> Json.String s) r.axioms)) ]
     @ if r.note = "" then [] else [ ("note", Json.String r.note) ])

let total f results = List.fold_left (fun n r -> n + f r) 0 results

let to_json ~file results =
  Json.Obj
    [ ("schema", Json.String schema); ("file", Json.String file);
      ( "summary",
        Json.Obj
          [ ("theorems", Json.Int (List.length results)); ("shortened", Json.Int (List.length (List.filter changed results)));
            ("lines_before", Json.Int (total (fun r -> r.size_before.lines) results)); ("lines_after", Json.Int (total (fun r -> r.size_after.lines) results));
            ("chars_before", Json.Int (total (fun r -> r.size_before.chars) results)); ("chars_after", Json.Int (total (fun r -> r.size_after.chars) results));
            ("checks", Json.Int (total (fun r -> r.checks) results)) ] );
      ("results", Json.List (List.map result_to_json results)) ]

let render ?(color = false) ~file results =
  let paint code s = if color then Printf.sprintf "\027[%sm%s\027[0m" code s else s in
  let buffer = Buffer.create 1024 in
  let add = Buffer.add_string buffer in
  List.iter
    (fun r ->
      if changed r then begin
        add
          (Printf.sprintf "%s  %s  %d to %d line(s), %d to %d character(s)  (%s, %d check(s))\n" (paint "1" r.theorem) (paint "32" "shorter")
             r.size_before.lines r.size_after.lines r.size_before.chars r.size_after.chars r.how r.checks);
        List.iter (fun what -> add (Printf.sprintf "  %s %s\n" (paint "31" "-") what)) r.removed;
        add (Text.indent 4 r.after ^ "\n")
      end
      else
        add
          (Printf.sprintf "%s  %s  %d line(s)%s\n" (paint "1" r.theorem) (paint "2" "as short as it gets") r.size_before.lines
             (if r.note = "" then Printf.sprintf "  (%d check(s))" r.checks else "  " ^ paint "33" r.note)))
    results;
  let shortened = List.length (List.filter changed results) in
  add
    (Printf.sprintf "%s: %d of %d proof(s) shortened, %d line(s) to %d, every shorter proof checked by Lean\n" file shortened (List.length results)
       (total (fun r -> r.size_before.lines) results) (total (fun r -> r.size_after.lines) results));
  Buffer.contents buffer
