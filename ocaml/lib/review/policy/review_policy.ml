(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What a review reports, and how seriously.

    A policy switches rules on and off and gives each a severity. It starts
    from a profile and is changed by a configuration file. A finding can
    also be suppressed where it stands, by a comment that says why. *)

open Maithic_base

type t = { profile : string; switches : (string * bool) list; severities : (string * Review_finding.severity) list }

(** Every rule there is, with the severity it has unless a policy says otherwise. *)
let rules =
  [ ("sorry", Review_finding.Error); ("axiom", Review_finding.Warning); ("docstring", Review_finding.Info); ("naming", Review_finding.Info);
    ("duplicate", Review_finding.Warning); ("automation", Review_finding.Info); ("unneeded-step", Review_finding.Info);
    ("suppression", Review_finding.Warning); ("statement", Review_finding.Info); ("proof", Review_finding.Info);
    ("definition", Review_finding.Info) ]

let profiles = [ "default"; "strict"; "minimal" ]

(** The policy of a profile. [default] reports everything at the usual
    severities. [strict] also treats axioms and duplicates as errors and
    missing documentation and names out of convention as warnings.
    [minimal] reports unfinished work and axioms only. *)
let of_profile = function
  | "default" -> Ok { profile = "default"; switches = []; severities = [] }
  | "strict" ->
    Ok
      { profile = "strict"; switches = [];
        severities =
          [ ("axiom", Review_finding.Error); ("duplicate", Review_finding.Error); ("docstring", Review_finding.Warning);
            ("naming", Review_finding.Warning) ] }
  | "minimal" ->
    Ok { profile = "minimal"; switches = List.filter_map (fun (rule, _) -> if List.mem rule [ "sorry"; "axiom"; "suppression" ] then None else Some (rule, false)) rules; severities = [] }
  | other -> Error (Printf.sprintf "there is no profile '%s'; the profiles are %s" other (String.concat ", " profiles))

let default = { profile = "default"; switches = []; severities = [] }
let known rule = List.mem_assoc rule rules
let enabled t rule = Option.value ~default:true (List.assoc_opt rule t.switches)

let severity t rule fallback =
  match List.assoc_opt rule t.severities with Some s -> s | None -> (match List.assoc_opt rule rules with Some s -> s | None -> fallback)

(** Reads a policy from a configuration document:
    {v
    [review]
    profile = "strict"

    [review.rules]
    naming = false

    [review.severity]
    duplicate = "error"
    v}
    A rule or a severity that does not exist is an error. *)
let of_document doc =
  let ( let* ) = Result.bind in
  let review = Option.value ~default:(Json.Obj []) (Json.member "review" doc) in
  let* base = of_profile (Json.string_or "default" "profile" review) in
  let members name = match Json.member name review with Some (Json.Obj fields) -> fields | _ -> [] in
  let* switches =
    List.fold_left
      (fun acc (rule, value) ->
        let* done_ = acc in
        match value with
        | Json.Bool on when known rule -> Ok ((rule, on) :: done_)
        | Json.Bool _ -> Error (Printf.sprintf "[review.rules] names a rule that does not exist: %s" rule)
        | _ -> Error (Printf.sprintf "[review.rules] %s must be true or false" rule))
      (Ok []) (members "rules")
  in
  let* severities =
    List.fold_left
      (fun acc (rule, value) ->
        let* done_ = acc in
        match Option.bind (Json.string_opt value) Review_finding.severity_of_string with
        | Some s when known rule -> Ok ((rule, s) :: done_)
        | Some _ -> Error (Printf.sprintf "[review.severity] names a rule that does not exist: %s" rule)
        | None -> Error (Printf.sprintf "[review.severity] %s must be \"info\", \"warning\" or \"error\"" rule))
      (Ok []) (members "severity")
  in
  Ok { base with switches = List.rev switches @ base.switches; severities = List.rev severities @ base.severities }

let of_file file = Result.bind (Toml.parse_file file) of_document

(* ---- suppression in the source ---- *)

type suppression = { at_line : int; rules : string list; reason : string }

(** The suppression comments of a source text. A comment of the form
    [-- maithic: allow RULE, RULE (reason)] suppresses the named rules for
    the declaration that begins on the next line that is not such a comment.
    The reason is what stands in the parentheses. *)
let suppressions source =
  let marker = "-- maithic: allow" in
  List.concat
    (List.mapi
       (fun i line ->
         let line = String.trim line in
         if not (Text.starts_with ~prefix:marker line) then []
         else begin
           let rest = String.trim (String.sub line (String.length marker) (String.length line - String.length marker)) in
           let listed, reason =
             match String.index_opt rest '(' with
             | Some open_ ->
               let close = match String.rindex_opt rest ')' with Some c when c > open_ -> c | _ -> String.length rest in
               (String.sub rest 0 open_, String.trim (String.sub rest (open_ + 1) (close - open_ - 1)))
             | None -> (rest, "")
           in
           let rules = List.filter (fun r -> r <> "") (List.map String.trim (String.split_on_char ',' listed)) in
           [ { at_line = i + 1; rules; reason } ]
         end)
       (String.split_on_char '\n' source))

(** Applies a policy to the findings of one file. Findings of rules that are
    switched off are dropped; the others get the severity the policy gives
    their rule. A finding is also dropped when a suppression comment with a
    reason stands directly above its declaration and names its rule. A
    suppression without a reason suppresses nothing and is reported itself,
    and so is one that names a rule which does not exist. Returns the
    findings in report order and the number that were suppressed.

    A finding points at the name of its declaration, and a comment stands
    above the whole declaration, its documentation included; [starts] gives
    the first line of each declaration by name, so that the two can be
    matched. A finding whose declaration is not listed is matched by its own
    line. *)
let apply t ~source ?(starts = []) (findings : Review_finding.t list) =
  let found = suppressions source in
  let lines = Array.of_list (String.split_on_char '\n' source) in
  (* The line of the declaration a comment stands above: the next line that is no suppression. *)
  let target (s : suppression) =
    let rec next line = if line <= Array.length lines && List.exists (fun (o : suppression) -> o.at_line = line) found then next (line + 1) else line in
    next (s.at_line + 1)
  in
  let file = match findings with f :: _ -> f.Review_finding.file | [] -> "" in
  let faults =
    List.concat_map
      (fun (s : suppression) ->
        let fault message =
          { Review_finding.rule = "suppression"; origin = Review_finding.Semantic; severity = Review_finding.Warning; file; line = s.at_line;
            declaration = ""; message; edit = None }
        in
        (if s.reason = "" then [ fault "a suppression needs a reason in parentheses, and suppresses nothing without one" ] else [])
        @ List.filter_map (fun rule -> if known rule then None else Some (fault (Printf.sprintf "a suppression names a rule that does not exist: %s" rule))) s.rules
        @ if s.rules = [] then [ fault "a suppression names no rule" ] else [])
      found
  in
  let suppressed (f : Review_finding.t) =
    let first = Option.value ~default:f.Review_finding.line (List.assoc_opt f.Review_finding.declaration starts) in
    List.exists (fun (s : suppression) -> s.reason <> "" && List.mem f.Review_finding.rule s.rules && target s = first) found
  in
  let kept, dropped = List.partition (fun f -> not (suppressed f)) findings in
  let reported =
    List.filter_map
      (fun (f : Review_finding.t) ->
        if enabled t f.Review_finding.rule then Some { f with Review_finding.severity = severity t f.Review_finding.rule f.Review_finding.severity } else None)
      (kept @ faults)
  in
  (List.sort Review_finding.compare reported, List.length dropped)
