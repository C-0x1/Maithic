(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_bridge

let indent_to columns text =
  match String.split_on_char '\n' text with
  | [] -> text
  | first :: rest -> String.concat "\n" (first :: List.map (fun line -> String.make columns ' ' ^ line) rest)

let propose (context : Mend_rule.context) (b : Mend_breakage.t) =
  let source = context.Mend_rule.source in
  let asked =
    List.filter_map
      (fun (d : Bridge_types.diagnostic) ->
        match Mend_class.of_diagnostic d, Mend_edit.span source d with
        | Some (Mend_class.Suggestion (_ :: _ as texts)), Some (start, stop) -> Some (d, start, stop, texts)
        | _ -> None)
      b.Mend_breakage.diagnostics
  in
  match asked with
  | [] -> []
  | _ ->
    (* The first edit takes Lean's first suggestion everywhere. When Lean gave a choice for one
       call, a further edit for each other choice follows, should the first not compile. *)
    let edit pick =
      let chosen = List.mapi (fun i (d, start, stop, texts) -> (d, start, stop, pick i texts)) asked in
      { Mend_edit.rule = "suggestion";
        description =
          "write what Lean suggests: "
          ^ String.concat "; " (List.map (fun (_, start, stop, text) -> Printf.sprintf "%s for %s" (List.hd (String.split_on_char '\n' text)) (String.sub source start (stop - start))) chosen);
        replacements =
          List.map (fun ((d : Bridge_types.diagnostic), start, stop, text) -> { Mend_edit.start; stop; text = indent_to d.Bridge_types.column text }) chosen;
        untrusted = String.concat "\n" (List.map (fun (_, _, _, text) -> text) chosen) }
    in
    let first = edit (fun _ texts -> List.hd texts) in
    let others =
      List.concat
        (List.mapi
           (fun at (_, _, _, texts) -> List.filteri (fun k _ -> k > 0) texts |> List.map (fun alternative -> edit (fun i texts -> if i = at then alternative else List.hd texts)))
           asked)
    in
    first :: others

let rule =
  { Mend_rule.name = "suggestion"; priority = 20; summary = "Writes what Lean suggests in place of a call such as exact? or simp?."; propose }
