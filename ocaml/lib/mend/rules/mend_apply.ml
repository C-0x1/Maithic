(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_bridge

type repair = { breakage : Mend_breakage.t; edit : Mend_edit.t; after : string; tried : int }
type outcome = { source : string; repairs : repair list; remaining : Mend_breakage.t list; checks : int }

let default_rules = Mend_rule.registry [ Mend_rename.rule; Mend_suggest.rule ]
let max_steps = 50
let admits text = Screen.admits text
let errors (report : Bridge_types.report) = List.length (List.filter (fun (d : Bridge_types.diagnostic) -> d.Bridge_types.severity = "error") report.Bridge_types.diagnostics)

(* The text a breakage stood in, after an edit: the declaration of the same name, or the line. *)
let context_after ~file ~source (b : Mend_breakage.t) =
  if b.Mend_breakage.declaration = "" then
    match List.nth_opt (String.split_on_char '\n' source) (b.Mend_breakage.first_line - 1) with Some line -> line | None -> ""
  else
    match Maithic_prover.Lean_source.find source b.Mend_breakage.declaration with
    | Ok decl ->
      let start = String.length decl.Maithic_prover.Lean_source.prelude and stop = String.length source - String.length decl.Maithic_prover.Lean_source.rest in
      String.trim (String.sub source start (max 0 (stop - start)))
    | Error _ -> ignore file; ""

let run ?(max_steps = max_steps) ?(rules = default_rules) ?(renames = []) ~check ~file source =
  let ( let* ) = Result.bind in
  let checks = ref 0 in
  let detect source =
    incr checks;
    Mend_detect.collect ~check ~file source
  in
  let rec loop source (breakages, report) repairs steps =
    let finish () = Ok { source; repairs = List.rev repairs; remaining = breakages; checks = !checks } in
    if steps >= max_steps then finish ()
    else
      (* The first breakage with an edit Lean accepts. *)
      let rec first = function
        | [] -> Ok None
        | (b : Mend_breakage.t) :: rest ->
          let rec try_edits tried = function
            | [] -> Ok None
            | (edit : Mend_edit.t) :: later ->
              if not (admits edit.Mend_edit.untrusted) then try_edits tried later
              else
                (match Mend_edit.apply source edit.Mend_edit.replacements with
                 | Error _ -> try_edits tried later
                 | Ok changed ->
                   let* after, after_report = detect changed in
                   (* Kept when the breakage is gone, nothing is broken that was not broken before, and
                      Lean reports no more errors than it did. *)
                   let before = List.map Mend_breakage.key breakages in
                   let gone = not (List.exists (fun other -> Mend_breakage.key other = Mend_breakage.key b) after) in
                   let nothing_new = List.for_all (fun other -> List.mem (Mend_breakage.key other) before) after in
                   if gone && nothing_new && errors after_report <= errors report then Ok (Some (b, edit, changed, (after, after_report), tried + 1))
                   else try_edits (tried + 1) later)
          in
          let* kept = try_edits 0 (Mend_rule.proposals rules { Mend_rule.source; renames } b) in
          (match kept with Some found -> Ok (Some found) | None -> first rest)
      in
      let* found = first breakages in
      match found with
      | None -> finish ()
      | Some (b, edit, changed, state, tried) ->
        loop changed state ({ breakage = b; edit; after = context_after ~file ~source:changed b; tried } :: repairs) (steps + 1)
  in
  let* state = detect source in
  loop source state [] 0
