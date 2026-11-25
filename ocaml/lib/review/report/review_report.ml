(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** The report of a review, as JSON, as Markdown and for the terminal. *)

open Maithic_base

let schema = "maithic.review.report/1"

type file_result = {
  file : string;
  source : string;
  declarations : int;
  findings : Review_finding.t list;  (** In report order, after the policy was applied. *)
  suppressed : int;
  note : string;  (** Something the reader should know about how the file was reviewed; usually empty. *)
}

type t = { profile : string; files : file_result list }

let all_findings t = List.concat_map (fun f -> f.findings) t.files
let count severity t = List.length (List.filter (fun (f : Review_finding.t) -> f.Review_finding.severity = severity) (all_findings t))
let suppressed t = List.fold_left (fun acc f -> acc + f.suppressed) 0 t.files

(** The most severe finding of a review, if it has any. *)
let worst t =
  List.fold_left
    (fun acc (f : Review_finding.t) ->
      match acc with
      | Some s when Review_finding.severity_rank s >= Review_finding.severity_rank f.Review_finding.severity -> acc
      | _ -> Some f.Review_finding.severity)
    None (all_findings t)

let summary_line t =
  Printf.sprintf "%d file(s), %d finding(s): %d error(s), %d warning(s), %d note(s)%s" (List.length t.files) (List.length (all_findings t))
    (count Review_finding.Error t) (count Review_finding.Warning t) (count Review_finding.Info t)
    (if suppressed t = 0 then "" else Printf.sprintf ", %d suppressed" (suppressed t))

let to_json t =
  Json.Obj
    [ ("schema", Json.String schema); ("profile", Json.String t.profile);
      ( "summary",
        Json.Obj
          [ ("files", Json.Int (List.length t.files)); ("findings", Json.Int (List.length (all_findings t)));
            ("error", Json.Int (count Review_finding.Error t)); ("warning", Json.Int (count Review_finding.Warning t));
            ("info", Json.Int (count Review_finding.Info t)); ("suppressed", Json.Int (suppressed t)) ] );
      ( "files",
        Json.List
          (List.map
             (fun f ->
               Json.Obj
                 ([ ("file", Json.String f.file); ("declarations", Json.Int f.declarations); ("suppressed", Json.Int f.suppressed);
                    ("findings", Json.List (List.map Review_finding.to_json f.findings)) ]
                 @ if f.note = "" then [] else [ ("note", Json.String f.note) ]))
             t.files) ) ]

let cell text = Text.replace_all ~sub:"|" ~by:"\\|" (Text.replace_all ~sub:"\n" ~by:" " text)

(** The whole review as a Markdown document: a line of counts, then for
    every file a table of findings and the verified changes as diffs. *)
let markdown t =
  let buf = Buffer.create 4096 in
  let add fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  add "# Review\n\n%s. Profile: %s.\n" (summary_line t) t.profile;
  List.iter
    (fun f ->
      add "\n## %s\n\n" f.file;
      if f.note <> "" then add "%s\n\n" f.note;
      if f.findings = [] then add "Nothing to report in %d declaration(s).\n" f.declarations
      else begin
        add "| Line | Severity | Rule | Declaration | Finding |\n|---:|---|---|---|---|\n";
        List.iter
          (fun (x : Review_finding.t) ->
            add "| %d | %s | %s | %s | %s |\n" x.Review_finding.line (Review_finding.severity_to_string x.Review_finding.severity) x.Review_finding.rule
              (if x.Review_finding.declaration = "" then "" else "`" ^ x.Review_finding.declaration ^ "`")
              (cell x.Review_finding.message))
          f.findings;
        let edits = List.filter (fun (x : Review_finding.t) -> x.Review_finding.edit <> None) f.findings in
        if edits <> [] then begin
          add "\n### Changes that Lean has checked\n";
          List.iter
            (fun (x : Review_finding.t) ->
              match x.Review_finding.edit with
              | None -> ()
              | Some e ->
                add "\n`%s`, line %d:\n\n```diff\n" x.Review_finding.declaration x.Review_finding.line;
                List.iter (fun line -> add "- %s\n" line) (String.split_on_char '\n' e.Review_finding.before);
                List.iter (fun line -> add "+ %s\n" line) (String.split_on_char '\n' e.Review_finding.after);
                add "```\n")
            edits
        end
      end)
    t.files;
  Buffer.contents buf

(** The review for a terminal: every finding with the line of the source it
    is about. With [color], severities are painted. *)
let terminal ?(color = false) t =
  let paint code text = if color then Printf.sprintf "\027[%sm%s\027[0m" code text else text in
  let severity (s : Review_finding.severity) =
    let word = Text.pad_right 7 (Review_finding.severity_to_string s) in
    match s with Review_finding.Error -> paint "31" word | Review_finding.Warning -> paint "33" word | Review_finding.Info -> paint "2" word
  in
  let buf = Buffer.create 4096 in
  let add fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  List.iter
    (fun f ->
      let lines = Array.of_list (String.split_on_char '\n' f.source) in
      if f.note <> "" then add "%s: %s\n" f.file f.note;
      List.iter
        (fun (x : Review_finding.t) ->
          add "%s:%d  %s  %s%s\n" f.file x.Review_finding.line (severity x.Review_finding.severity) (paint "1" x.Review_finding.rule)
            (if x.Review_finding.declaration = "" then "" else "  " ^ x.Review_finding.declaration);
          add "    %s\n" x.Review_finding.message;
          if x.Review_finding.line >= 1 && x.Review_finding.line <= Array.length lines then
            add "    %s\n" (paint "2" (Printf.sprintf "%4d | %s" x.Review_finding.line (Text.truncate 110 lines.(x.Review_finding.line - 1))));
          match x.Review_finding.edit with
          | Some e -> List.iter (fun line -> add "    %s\n" (paint "32" ("+ " ^ line))) (String.split_on_char '\n' e.Review_finding.after)
          | None -> ())
        f.findings)
    t.files;
  add "%s\n" (summary_line t);
  Buffer.contents buf
