(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base
open Maithic_bridge

type exported = { goal : Oracle_goal.t; skipped : (string * string) list }

type token = Number of string | Name of string | Symbol of string

let symbols = [ ("\xE2\x89\xA4", "<="); ("\xE2\x89\xA5", ">="); ("\xE2\x89\xA0", "!=") ]

(* Splits a statement into numbers, names and operators. Anything else is not arithmetic. *)
let tokenize text =
  let n = String.length text in
  let is_digit c = c >= '0' && c <= '9' in
  let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let rec go i acc =
    if i >= n then Ok (List.rev acc)
    else
      let c = text.[i] in
      if c = ' ' || c = '\n' then go (i + 1) acc
      else if is_digit c then begin
        let j = ref i in
        while !j < n && is_digit text.[!j] do incr j done;
        go !j (Number (String.sub text i (!j - i)) :: acc)
      end
      else if is_letter c then begin
        let j = ref i in
        while !j < n && (is_letter text.[!j] || is_digit text.[!j] || text.[!j] = '\'') do incr j done;
        go !j (Name (String.sub text i (!j - i)) :: acc)
      end
      else
        match List.find_opt (fun (utf8, _) -> i + String.length utf8 <= n && String.sub text i (String.length utf8) = utf8) symbols with
        | Some (utf8, ascii) -> go (i + String.length utf8) (Symbol ascii :: acc)
        | None ->
          if i + 1 < n && List.mem (String.sub text i 2) [ "<="; ">="; "!=" ] then go (i + 2) (Symbol (String.sub text i 2) :: acc)
          else if String.contains "+-*/^()=<>" c then go (i + 1) (Symbol (String.make 1 c) :: acc)
          else
            (* The whole character, for the message: it may take several bytes. *)
            let width = if Char.code c < 0x80 then 1 else if Char.code c < 0xE0 then 2 else if Char.code c < 0xF0 then 3 else 4 in
            Error (Printf.sprintf "it contains \"%s\", which is not arithmetic" (String.sub text i (min width (n - i))))
  in
  go 0 []

let comparisons = [ "="; "<"; ">"; "<="; ">="; "!=" ]
let text_of = function Number s | Name s | Symbol s -> s

let translate ~domain ~variables text =
  let ( let* ) = Result.bind in
  let* tokens = tokenize text in
  let* () =
    match List.find_opt (function Name v -> not (List.mem v variables) | _ -> false) tokens with
    | Some (Name v) -> Error (Printf.sprintf "the name %s is not one of the variables" v)
    | _ -> Ok ()
  in
  let* () =
    match List.length (List.filter (function Symbol s -> List.mem s comparisons | _ -> false) tokens) with
    | 1 -> Ok ()
    | 0 -> Error "it is not a comparison"
    | _ -> Error "it has more than one comparison"
  in
  let* () =
    if domain = Oracle_goal.Integers && List.mem (Symbol "/") tokens then
      Error "it divides integers, which Lean rounds and the oracles do not"
    else Ok ()
  in
  Ok (String.concat " " (List.map text_of tokens))

let is_linear statement =
  let words = Array.of_list (List.filter (fun w -> w <> "") (String.split_on_char ' ' statement)) in
  let is_number w = w <> "" && String.for_all (fun c -> c >= '0' && c <= '9') w in
  let ok = ref true in
  Array.iteri
    (fun i w ->
      let before = if i > 0 then words.(i - 1) else "" and after = if i + 1 < Array.length words then words.(i + 1) else "" in
      if w = "^" then ok := false
      else if w = "*" && not (is_number before || is_number after) then ok := false
      else if w = "/" && not (is_number after) then ok := false)
    words;
  !ok

let export (g : Bridge_types.goal) =
  let ( let* ) = Result.bind in
  let typed ty = List.filter_map (fun (h : Bridge_types.hypothesis) -> if h.Bridge_types.hyp_type = ty then Some h.Bridge_types.hyp_name else None) g.Bridge_types.hypotheses in
  let integers = typed "Int" and rationals = typed "Rat" and naturals = typed "Nat" in
  let* domain, variables =
    match integers, rationals, naturals with
    | _, _, _ :: _ -> Error "variables over the natural numbers are not exported: their subtraction stops at zero"
    | [], [], [] -> Error "the goal has no integer or rational variable"
    | vs, [], [] -> Ok (Oracle_goal.Integers, vs)
    | [], vs, [] -> Ok (Oracle_goal.Rationals, vs)
    | _ -> Error "the goal mixes integer and rational variables"
  in
  let* target =
    if g.Bridge_types.target = "False" then Ok "false"
    else Result.map_error (fun e -> "the target cannot be exported: " ^ e) (translate ~domain ~variables g.Bridge_types.target)
  in
  let hypotheses, skipped =
    List.fold_left
      (fun (kept, skipped) (h : Bridge_types.hypothesis) ->
        let name = h.Bridge_types.hyp_name in
        if List.mem name variables then (kept, skipped)
        else if Text.contains name "\xE2\x9C\x9D" then (kept, skipped @ [ (name, "it has no name that a proof can refer to") ])
        else
          match translate ~domain ~variables h.Bridge_types.hyp_type with
          | Ok statement -> (kept @ [ { Oracle_goal.name; statement } ], skipped)
          | Error reason -> (kept, skipped @ [ (name, reason) ]))
      ([], []) g.Bridge_types.hypotheses
  in
  let statements = (if target = "false" then [] else [ target ]) @ List.map (fun (h : Oracle_goal.hypothesis) -> h.Oracle_goal.statement) hypotheses in
  let kind = if List.for_all is_linear statements then Oracle_goal.Linear else Oracle_goal.Polynomial in
  Ok { goal = { Oracle_goal.kind; domain; variables; hypotheses; goal = target }; skipped }
