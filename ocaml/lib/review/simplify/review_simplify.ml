(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Simplifications of proofs that Lean has checked.

    A suggestion is made by trying it: the proof is replaced, or a step is
    taken out, and the changed theorem is given to Lean where the original
    stood. Only a change that Lean accepts is reported, so a suggestion
    never has to be taken on trust.

    The trials work on the text of a proof and ask a [check] function
    whether a proof compiles; in the command, that function is the Bridge. *)

open Maithic_base

(** The single calls of automation that are tried in place of a proof. *)
let automation = [ "by rfl"; "by simp"; "by omega"; "by decide"; "by grind"; "by simp_all" ]

(* The lines of a tactic proof after its [by], and the indentation of its steps. *)
let body proof =
  let proof = String.trim proof in
  if not (Text.starts_with ~prefix:"by" proof) then None
  else begin
    let rest = String.sub proof 2 (String.length proof - 2) in
    let lines = List.filter (fun l -> String.trim l <> "") (String.split_on_char '\n' rest) in
    let indent line =
      let n = String.length line in
      let rec go i = if i < n && line.[i] = ' ' then go (i + 1) else i in
      go 0
    in
    match lines with
    | [] -> None
    | first :: _ when String.trim rest = String.trim first && not (String.contains rest '\n') -> Some ([ [ String.trim first ] ], 2)
    | _ ->
      let least = List.fold_left (fun acc l -> min acc (indent l)) max_int lines in
      (* A step is a line at the least indentation with the lines that belong to it: the deeper
         lines that follow, and the lines that continue it at the same depth, such as the
         alternatives of an induction, which begin with a bar. *)
      let continues line =
        let line = String.trim line in
        List.exists (fun prefix -> Text.starts_with ~prefix line) [ "|"; "<;>"; ")"; "]"; "}"; "\xE2\x9F\xA9" ]
      in
      let steps =
        List.rev
          (List.map List.rev
             (List.fold_left
                (fun acc line ->
                  match acc with
                  | current :: rest when indent line > least || continues line -> (line :: current) :: rest
                  | _ -> [ line ] :: acc)
                [] lines))
      in
      Some (steps, least)
  end

(** The steps of a tactic proof: each is a tactic of the proof's own
    sequence with the lines that belong to it. A term proof has none. *)
let steps proof = match body proof with Some (steps, _) -> List.map (String.concat "\n") steps | None -> []

let rebuild steps = "by\n" ^ String.concat "\n" (List.concat steps)

(** Tries to replace a proof by one call of automation. Only a proof of two
    steps or more, or a term proof of some length, is worth the try; a proof
    that already is such a call is left alone. Returns the first call that
    [check] accepts. *)
let replace_by_automation ~check proof =
  let proof = String.trim proof in
  let worth =
    match body proof with
    | Some (steps, _) -> List.length steps >= 2 || List.exists (fun step -> List.length step >= 2) steps
    | None -> String.length proof > 40
  in
  if (not worth) || List.mem proof automation then None else List.find_opt check automation

(** Takes steps out of a tactic proof, one at a time from the last to the
    first, and keeps every removal after which [check] still accepts the
    proof. Returns the shorter proof and the steps that were removed, in
    their order; nothing when every step is needed. The last step left is
    never removed: a proof needs one. *)
let delete_steps ~check proof =
  match body proof with
  | None -> None
  | Some (steps, _) when List.length steps < 2 -> None
  | Some (steps, _) ->
    let indexed = List.mapi (fun i step -> (i, step)) steps in
    let kept, removed =
      List.fold_left
        (fun (kept, removed) (i, step) ->
          let without = List.filter (fun (j, _) -> j <> i) kept in
          if without <> [] && check (rebuild (List.map snd without)) then (without, (i, step) :: removed) else (kept, removed))
        (indexed, []) (List.rev indexed)
    in
    if removed = [] then None
    else Some (rebuild (List.map snd kept), List.map (fun (_, step) -> String.trim (String.concat "\n" step)) (List.sort compare removed))

(** The findings for one theorem: an automation call that can stand in for
    its proof, or else the steps its proof does not need. Every edit in a
    finding has been accepted by [check]. *)
let suggestions ~check ~file ~line ~declaration proof =
  let finding rule message after =
    { Review_finding.rule; origin = Review_finding.Suggestion; severity = Review_finding.Info; file; line; declaration; message;
      edit = Some { Review_finding.before = String.trim proof; after } }
  in
  match replace_by_automation ~check proof with
  | Some call -> [ finding "automation" (Printf.sprintf "the proof can be replaced by `%s`; Lean accepts it" call) call ]
  | None ->
    (match delete_steps ~check proof with
     | Some (shorter, removed) ->
       [ finding "unneeded-step"
           (Printf.sprintf "%s not needed: Lean accepts the proof without %s"
              (if List.length removed = 1 then "one step is" else Printf.sprintf "%d steps are" (List.length removed))
              (String.concat ", " (List.map (fun step -> "`" ^ List.hd (String.split_on_char '\n' step) ^ "`") removed)))
           shorter ]
     | None -> [])
