(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_prover

type outcome = By_rule of string | By_model of int | By_prover of string | Unrepaired of string

type entry = { breakage : Mend_breakage.t; outcome : outcome; description : string; after : string; tried : int; changed_lines : int }
type t = { file : string; entries : entry list; remaining : Mend_breakage.t list; source : string; checks : int }

type tools = {
  check_file : Mend_detect.check;
  check_theorem : string -> Lean_source.decl -> Mend_check.t;
  ask : (decl:Lean_source.decl -> breakage:Mend_breakage.t -> Mend_agent.ask) option;
  reprove : (source:string -> Lean_source.decl -> Mend_agent.reprove) option;
}

let sanitize source ~except broken =
  List.fold_left
    (fun text name ->
      if name = except then text else match Lean_source.find text name with Ok decl -> Lean_source.with_proof decl "sorry" | Error _ -> text)
    source
    (List.sort_uniq compare broken)

let repaired e = match e.outcome with Unrepaired _ -> false | By_rule _ | By_model _ | By_prover _ -> true
let clean t = t.remaining = []
let outcome_stage = function By_rule _ -> "rule" | By_model _ -> "model" | By_prover _ -> "prove" | Unrepaired _ -> "none"

let declaration_text source (decl : Lean_source.decl) =
  let start = String.length decl.Lean_source.prelude and stop = String.length source - String.length decl.Lean_source.rest in
  String.trim (String.sub source start (max 0 (stop - start)))

let run ?rules ?renames ?(settings = Mend_agent.default_settings) tools ~file source =
  let ( let* ) = Result.bind in
  let* by_rules = Mend_apply.run ?rules ?renames ~check:tools.check_file ~file source in
  let from_rules =
    List.map
      (fun (r : Mend_apply.repair) ->
        { breakage = r.Mend_apply.breakage; outcome = By_rule r.Mend_apply.edit.Mend_edit.rule; description = r.Mend_apply.edit.Mend_edit.description;
          after = r.Mend_apply.after; tried = r.Mend_apply.tried;
          changed_lines = Mend_edit.changed_lines r.Mend_apply.breakage.Mend_breakage.context r.Mend_apply.after })
      by_rules.Mend_apply.repairs
  in
  let broken = List.filter_map (fun (b : Mend_breakage.t) -> if b.Mend_breakage.declaration <> "" && Mend_breakage.has_error b then Some b.Mend_breakage.declaration else None) by_rules.Mend_apply.remaining in
  (* What the rules left: proofs go to the agent, one theorem at a time, each checked with the
     broken proofs around it left open. *)
  let source, later =
    List.fold_left
      (fun (source, entries) (b : Mend_breakage.t) ->
        let unrepaired ?(tried = 0) reason = (source, entries @ [ { breakage = b; outcome = Unrepaired reason; description = ""; after = ""; tried; changed_lines = 0 } ]) in
        if b.Mend_breakage.declaration = "" then unrepaired "no rule has an edit for it, and it is outside every theorem: a model is asked about proofs only"
        else if not (Mend_breakage.has_error b) then unrepaired "no rule has an edit that Lean accepts"
        else
          let around = sanitize source ~except:b.Mend_breakage.declaration broken in
          match Lean_source.find around b.Mend_breakage.declaration, Lean_source.find source b.Mend_breakage.declaration with
          | Ok decl, Ok real ->
            let check = tools.check_theorem around decl in
            let ask = Option.map (fun ask -> ask ~decl ~breakage:b) tools.ask in
            let reprove = Option.map (fun reprove -> reprove ~source:around decl) tools.reprove in
            (match Mend_agent.mend ?ask ?reprove ~check settings decl with
             | Mend_agent.Fixed { stage; proof; round; changed_lines; attempts; _ } ->
               let changed = Mend_edit.put_proof real proof in
               let after = match Lean_source.find changed b.Mend_breakage.declaration with Ok d -> declaration_text changed d | Error _ -> "" in
               let outcome, description =
                 match stage with
                 | Mend_agent.Model -> (By_model round, Printf.sprintf "a model's repair from round %d, which changes %d line(s) of the proof" round changed_lines)
                 | Mend_agent.Prove proposer -> (By_prover proposer, Printf.sprintf "the statement was proved again by the %s" proposer)
               in
               (changed, entries @ [ { breakage = b; outcome; description; after; tried = List.length attempts; changed_lines } ])
             | Mend_agent.Unfixed { reason; attempts } -> unrepaired ~tried:(List.length attempts) reason)
          | _ -> unrepaired "the theorem was not found in the file again")
      (by_rules.Mend_apply.source, []) by_rules.Mend_apply.remaining
  in
  (* The last word is Lean's, on the whole file. *)
  let* remaining, _ = Mend_detect.collect ~check:tools.check_file ~file source in
  let still = List.map Mend_breakage.key remaining in
  let entries =
    List.map
      (fun e ->
        if repaired e && List.mem (Mend_breakage.key e.breakage) still then
          { e with outcome = Unrepaired "the repair was made, and Lean still reports the breakage when the whole file is checked" }
        else e)
      (from_rules @ later)
  in
  Ok { file; entries; remaining; source; checks = by_rules.Mend_apply.checks + 1 }

let schema = "maithic.mend.report/1"

let entry_to_json e =
  let b = e.breakage in
  Json.Obj
    ([ ("declaration", Json.String b.Mend_breakage.declaration); ("line", Json.Int b.Mend_breakage.line);
       ("class", Json.String (Mend_class.name b.Mend_breakage.class_)) ]
     @ (match Mend_class.detail b.Mend_breakage.class_ with "" -> [] | detail -> [ ("detail", Json.String detail) ])
     @ [ ("severity", Json.String b.Mend_breakage.severity); ("error", Json.String b.Mend_breakage.error);
         ("outcome", Json.String (if repaired e then "repaired" else "unrepaired")); ("stage", Json.String (outcome_stage e.outcome)) ]
     @ (match e.outcome with
        | By_rule rule -> [ ("rule", Json.String rule) ]
        | By_model round -> [ ("round", Json.Int round) ]
        | By_prover proposer -> [ ("proposer", Json.String proposer) ]
        | Unrepaired reason -> [ ("reason", Json.String reason) ])
     @ (if e.description = "" then [] else [ ("description", Json.String e.description) ])
     @ [ ("tried", Json.Int e.tried); ("changed_lines", Json.Int e.changed_lines); ("before", Json.String b.Mend_breakage.context) ]
     @ if e.after = "" then [] else [ ("after", Json.String e.after) ])

let count p entries = List.length (List.filter p entries)

let to_json t =
  let stage name = count (fun e -> outcome_stage e.outcome = name) t.entries in
  Json.Obj
    [ ("schema", Json.String schema); ("file", Json.String t.file);
      ( "summary",
        Json.Obj
          [ ("breakages", Json.Int (List.length t.entries)); ("repaired", Json.Int (count repaired t.entries));
            ("unrepaired", Json.Int (count (fun e -> not (repaired e)) t.entries)); ("by_rule", Json.Int (stage "rule"));
            ("by_model", Json.Int (stage "model")); ("by_prover", Json.Int (stage "prove")); ("remaining", Json.Int (List.length t.remaining));
            ("clean", Json.Bool (clean t)); ("checks", Json.Int t.checks) ] );
      ("repairs", Json.List (List.map entry_to_json t.entries)); ("remaining", Json.List (List.map Mend_breakage.to_json t.remaining)) ]

let render ?(color = false) t =
  let paint code s = if color then Printf.sprintf "\027[%sm%s\027[0m" code s else s in
  let buffer = Buffer.create 1024 in
  let add = Buffer.add_string buffer in
  let where (b : Mend_breakage.t) = if b.Mend_breakage.declaration = "" then Printf.sprintf "line %d" b.Mend_breakage.line else b.Mend_breakage.declaration in
  let width = List.fold_left (fun w e -> max w (Text.utf8_length (where e.breakage))) 12 t.entries in
  let classes = List.fold_left (fun w e -> max w (String.length (Mend_class.name e.breakage.Mend_breakage.class_))) 10 t.entries in
  List.iter
    (fun e ->
      let how =
        match e.outcome with
        | By_rule rule -> paint "32" "repaired" ^ "  by the rule " ^ paint "36" rule
        | By_model round -> paint "32" "repaired" ^ Printf.sprintf "  by a model, round %d" round
        | By_prover proposer -> paint "32" "repaired" ^ Printf.sprintf "  by the prover (%s)" proposer
        | Unrepaired _ -> paint "31" "left    "
      in
      add
        (Printf.sprintf "%s  %s  %s\n" (paint "1" (Text.pad_right width (where e.breakage)))
           (paint "2" (Text.pad_right classes (Mend_class.name e.breakage.Mend_breakage.class_)))
           how);
      add (Printf.sprintf "    %s %s\n" (paint "2" "Lean:") e.breakage.Mend_breakage.error);
      (match e.outcome with
       | Unrepaired reason -> add (Printf.sprintf "    %s %s\n" (paint "33" "why:") reason)
       | _ -> if e.description <> "" then add (Printf.sprintf "    %s %s\n" (paint "2" "done:") e.description)))
    t.entries;
  let stage name = count (fun e -> outcome_stage e.outcome = name) t.entries in
  let total = List.length t.entries in
  add
    (if total = 0 then Printf.sprintf "%s: nothing is broken\n" t.file
     else
       Printf.sprintf "%s: %d breakage(s), %d repaired (%d by a rule, %d by a model, %d by the prover), %d left; %s\n" t.file total
         (count repaired t.entries) (stage "rule") (stage "model") (stage "prove")
         (count (fun e -> not (repaired e)) t.entries)
         (if clean t then "Lean accepts the whole file" else Printf.sprintf "Lean still reports %d problem(s)" (List.length t.remaining)));
  Buffer.contents buffer
