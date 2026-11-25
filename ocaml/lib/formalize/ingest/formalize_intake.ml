(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type kind = Formula | Number | Symbol | Quantifier | Constraint | Task
type element = { kind : kind; text : string; start : int; stop : int }
type t = { origin : string; source : string; text : string; elements : element list }

let kind_name = function
  | Formula -> "formula"
  | Number -> "number"
  | Symbol -> "symbol"
  | Quantifier -> "quantifier"
  | Constraint -> "constraint"
  | Task -> "task"

let kinds = [ Formula; Number; Symbol; Quantifier; Constraint; Task ]

(* Phrases, longest first within each kind, matched as whole words without regard to case. *)
let quantifiers = [ "for all"; "for every"; "for each"; "for any"; "there exists"; "there exist"; "there is"; "there are"; "for some"; "every"; "each"; "any"; "all"; "some"; "no" ]

let constraints =
  [ "at least"; "at most"; "greater than or equal to"; "less than or equal to"; "greater than"; "less than"; "strictly"; "positive"; "negative"; "nonnegative";
    "non-negative"; "nonzero"; "non-zero"; "even"; "odd"; "prime"; "distinct"; "divisible by"; "divides"; "multiple of"; "natural numbers"; "natural number";
    "integers"; "integer"; "real numbers"; "real number"; "rational numbers"; "rational number"; "if and only if"; "between"; "exactly"; "unique" ]

(* Words that head a numbered item: the number after one of them counts nothing. *)
let labels = [ "problem"; "exercise"; "question"; "theorem"; "lemma"; "proposition"; "claim"; "example"; "part" ]

let tasks = [ "prove that"; "prove"; "show that"; "show"; "find all"; "find"; "determine"; "compute"; "calculate"; "evaluate"; "how many"; "what is" ]

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_word c = is_letter c || is_digit c || c = '_' || c = '\'' || Char.code c >= 0x80

(* The places where a phrase stands as whole words. *)
let phrase_places lower phrase =
  let n = String.length lower and len = String.length phrase in
  let rec go from acc =
    match Text.find_sub ~from lower phrase with
    | None -> List.rev acc
    | Some at ->
      let whole = (at = 0 || not (is_word lower.[at - 1])) && (at + len >= n || not (is_word lower.[at + len])) in
      go (at + 1) (if whole then (at, at + len) :: acc else acc)
  in
  go 0 []

(* The code point at a byte, and its width in bytes. *)
let decode text i =
  let n = String.length text in
  let c = Char.code text.[i] in
  let continuation j = if i + j < n then Char.code text.[i + j] land 0x3F else 0 in
  if c < 0x80 then (c, 1)
  else if c < 0xE0 then (((c land 0x1F) lsl 6) lor continuation 1, min 2 (n - i))
  else if c < 0xF0 then (((c land 0x0F) lsl 12) lor (continuation 1 lsl 6) lor continuation 2, min 3 (n - i))
  else (((c land 0x07) lsl 18) lor (continuation 1 lsl 12) lor (continuation 2 lsl 6) lor continuation 3, min 4 (n - i))

(* Greek letters and the blackboard letters stand for variables and for sets of numbers. *)
let is_math_letter code = (code >= 0x370 && code <= 0x3FF) || List.mem code [ 0x2102; 0x2115; 0x211A; 0x211D; 0x2124 ]

(* The signs of mathematics beyond ASCII: operators, relations and arrows. Letters with accents,
   dashes and quotation marks are not among them. *)
let is_math_sign code =
  (code >= 0x2190 && code <= 0x21FF) || (code >= 0x2200 && code <= 0x22FF) || (code >= 0x27F0 && code <= 0x27FF) || (code >= 0x2A00 && code <= 0x2AFF)
  || List.mem code [ 0xAC; 0xB1; 0xD7; 0xF7 ]

(* What a formula is made of. *)
type piece = Operand | Operator | Bracket

(* Words of two letters that are words of the language and not products of two variables. *)
let short_words = [ "an"; "is"; "of"; "if"; "in"; "be"; "to"; "or"; "so"; "we"; "as"; "at"; "by"; "it"; "no"; "on"; "do"; "up"; "us"; "he"; "me"; "my" ]

(* The formulas of a normalized text: the longest stretches of numbers, variables, signs and
   brackets, with single spaces between them, that hold both something to operate on and a sign
   that operates. *)
let formula_places text =
  let n = String.length text in
  let after_space i = if i < n && text.[i] = ' ' then i + 1 else i in
  let sign_at i = i < n && (List.mem text.[i] [ '+'; '-'; '*'; '/'; '='; '<'; '>'; '^' ] || (Char.code text.[i] >= 0x80 && is_math_sign (fst (decode text i)))) in
  (* The piece that starts at [i], and where it stops. [previous] is the piece before it in the stretch. *)
  let piece previous i =
    if i >= n then None
    else
      let c = text.[i] in
      if is_digit c then
        let rec go j = if j < n && (is_digit text.[j] || (text.[j] = '.' && j + 1 < n && is_digit text.[j + 1])) then go (j + 1) else j in
        Some (go i, Operand)
      else if is_letter c then
        if i > 0 && is_letter text.[i - 1] then None
        else
          let rec letters j = if j < n && is_letter text.[j] then letters (j + 1) else j in
          let rec digits j = if j < n && (is_digit text.[j] || text.[j] = '\'') then digits (j + 1) else j in
          let stop = letters i in
          let word = Text.lowercase (String.sub text i (stop - i)) in
          (* "a" and "I" are words of the language unless a sign or a bracket stands beside them. *)
          let beside_sign = previous = Some Operator || previous = Some Bracket || sign_at (after_space stop) || (stop < n && text.[stop] = ')') in
          if stop - i = 1 && (beside_sign || not (List.mem word [ "a"; "i" ] && c <> 'i')) then Some (digits stop, Operand)
          else if stop - i = 2 && not (List.mem word short_words) then Some (stop, Operand)
          else None
      else if List.mem c [ '+'; '-'; '*'; '/'; '='; '<'; '>'; '^' ] then Some (i + 1, Operator)
      else if List.mem c [ '('; ')'; '|'; '!'; '_' ] then Some (i + 1, Bracket)
      else if Char.code c >= 0x80 then
        let code, width = decode text i in
        if is_math_sign code then Some (i + width, Operator) else if is_math_letter code then Some (i + width, Operand) else None
      else None
  in
  let rec scan i acc =
    if i >= n then List.rev acc
    else
      match piece None i with
      | None -> scan (i + 1) acc
      | Some _ ->
        (* Grows a stretch over pieces and the single spaces between them. *)
        let rec grow j previous pieces =
          match piece previous (if pieces = [] then j else after_space j) with
          | Some (stop, kind) -> grow stop (Some kind) ((stop, kind) :: pieces)
          | None -> pieces
        in
        (* A sign at the end of a stretch operates on nothing: "x-axis" holds no formula. *)
        let rec trimmed = function (_, Operator) :: rest -> trimmed rest | pieces -> pieces in
        let all = grow i None [] in
        let reach = match all with (stop, _) :: _ -> stop | [] -> i + 1 in
        (match trimmed all with
         | (stop, _) :: _ as pieces when List.exists (fun (_, kind) -> kind = Operator) pieces && List.exists (fun (_, kind) -> kind = Operand) pieces ->
           scan reach ((i, stop) :: acc)
         | _ -> scan reach acc)
  in
  scan 0 []

let of_text ?(origin = "argument") source =
  if not (Text.is_valid_utf8 source) then Error "the problem is not valid UTF-8 text"
  else if Text.is_blank source then Error "the problem is empty"
  else begin
    let normalized = Formalize_normalize.normalize source in
    (* Blank space at the end is not part of the problem. *)
    let text =
      let whole = normalized.Formalize_normalize.text in
      let rec stop i = if i > 0 && List.mem whole.[i - 1] [ ' '; '\n'; '\t'; '\r' ] then stop (i - 1) else i in
      String.sub whole 0 (stop (String.length whole))
    in
    let lower = Text.lowercase text in
    let back (start, stop) = (Formalize_normalize.source_offset normalized start, Formalize_normalize.source_end normalized (max start (stop - 1))) in
    let element kind (start, stop) =
      let source_start, source_stop = back (start, stop) in
      { kind; text = String.sub text start (stop - start); start = source_start; stop = max source_stop (source_start + 1) }
    in
    (* Phrases: a place taken by a longer phrase is not taken again by a shorter one. *)
    let phrases kind table =
      let table = List.stable_sort (fun a b -> compare (String.length b) (String.length a)) table in
      List.fold_left
        (fun taken phrase ->
          List.fold_left
            (fun taken (start, stop) -> if List.exists (fun (s, e) -> start < e && s < stop) taken then taken else taken @ [ (start, stop) ])
            taken (phrase_places lower phrase))
        [] table
      |> List.map (element kind)
    in
    let formulas = formula_places text in
    let inside (start, stop) = List.exists (fun (s, e) -> start >= s && stop <= e) formulas in
    (* Numbers and symbols, wherever they stand. A symbol is a single letter in a formula, or a
       single letter that is not a word of the language. *)
    let n = String.length text in
    let rec tokens i numbers symbols =
      if i >= n then (List.rev numbers, List.rev symbols)
      else if is_digit text.[i] && (i = 0 || not (is_letter text.[i - 1] || is_digit text.[i - 1] || text.[i - 1] = '_' || text.[i - 1] = '\'')) then
        (* A digit that follows a letter or an underscore is part of a name or an index, as the 1 of x_1. *)
        let rec go j = if j < n && (is_digit text.[j] || (text.[j] = '.' && j + 1 < n && is_digit text.[j + 1])) then go (j + 1) else j in
        let stop = go i in
        (* The number of an exercise is its label, not a number of the problem. *)
        let label =
          let before = String.trim (String.sub lower 0 i) in
          List.exists (fun word -> Text.ends_with ~suffix:word before && (String.length before = String.length word || not (is_letter before.[String.length before - String.length word - 1]))) labels
        in
        tokens stop (if label then numbers else (i, stop) :: numbers) symbols
      else if is_letter text.[i] && (i = 0 || not (is_word text.[i - 1])) then
        let rec go j = if j < n && is_word text.[j] && Char.code text.[j] < 0x80 then go (j + 1) else j in
        let stop = go i in
        let word = String.sub text i (stop - i) in
        let single = stop - i = 1 || (stop - i = 2 && is_digit text.[i + 1]) in
        (* A letter of an abbreviation, as in "i.e.", is not a variable. *)
        let abbreviated = (stop + 1 < n && text.[stop] = '.' && is_letter text.[stop + 1]) || (i >= 2 && text.[i - 1] = '.' && is_letter text.[i - 2]) in
        (* "a" is the article unless it stands where only a variable can: before a mark or a sign, or before "and", "or", "is". *)
        let next = if stop < n && text.[stop] = ' ' then stop + 1 else stop in
        let next_word =
          let rec go j = if j < n && is_letter text.[j] then go (j + 1) else j in
          Text.lowercase (String.sub text next (go next - next))
        in
        let as_variable = next_word = "" || List.mem next_word [ "and"; "or"; "is"; "are"; "be"; "divides"; "equals" ] in
        let article = List.mem word [ "a"; "A"; "I" ] && not (word <> "I" && as_variable) in
        if single && (not abbreviated) && (inside (i, stop) || not article) then tokens stop numbers ((i, stop) :: symbols) else tokens stop numbers symbols
      else tokens (i + 1) numbers symbols
    in
    let numbers, symbols = tokens 0 [] [] in
    (* A word that is part of what is asked, as "all" is in "find all", does not quantify. *)
    let asked = phrases Task tasks in
    let apart (e : element) = not (List.exists (fun (t : element) -> e.start < t.stop && t.start < e.stop) asked) in
    let elements =
      List.map (element Formula) formulas @ List.map (element Number) numbers @ List.map (element Symbol) symbols
      @ List.filter apart (phrases Quantifier quantifiers)
      @ phrases Constraint constraints @ asked
    in
    let rank e = match List.find_index (fun k -> k = e.kind) kinds with Some i -> i | None -> 0 in
    Ok { origin; source; text; elements = List.stable_sort (fun a b -> compare (a.start, rank a, a.stop) (b.start, rank b, b.stop)) elements }
  end

let read ?(stdin = fun () -> In_channel.input_all In_channel.stdin) ~argument ~file () =
  match argument, file with
  | Some _, Some _ -> Error "give the problem once: as an argument, or as a file"
  | Some text, None -> of_text ~origin:"argument" text
  | None, Some "-" | None, None -> of_text ~origin:"stdin" (stdin ())
  | None, Some path -> Result.bind (Fs.read path) (of_text ~origin:path)

let of_kind t kind =
  List.fold_left (fun seen e -> if e.kind = kind && not (List.mem e.text seen) then seen @ [ e.text ] else seen) [] t.elements

let numbers t = of_kind t Number
let symbols t = of_kind t Symbol

let task t =
  match List.find_opt (fun e -> e.kind = Task) t.elements with
  | Some e when List.exists (fun prefix -> Text.starts_with ~prefix (Text.lowercase e.text)) [ "find"; "determine"; "compute"; "calculate"; "evaluate"; "how many"; "what is" ] -> "find"
  | _ -> "prove"

let frame t =
  let line kind label = match of_kind t kind with [] -> [] | items -> [ Printf.sprintf "%s: %s" label (String.concat "; " items) ] in
  String.concat "\n"
    ([ "task: " ^ task t ] @ line Formula "formulas" @ line Symbol "symbols" @ line Number "numbers" @ line Quantifier "quantifiers" @ line Constraint "constraints")

let to_json t =
  Json.Obj
    [ ("origin", Json.String t.origin); ("source", Json.String t.source); ("text", Json.String t.text); ("task", Json.String (task t));
      ( "elements",
        Json.List
          (List.map
             (fun e -> Json.Obj [ ("kind", Json.String (kind_name e.kind)); ("text", Json.String e.text); ("start", Json.Int e.start); ("stop", Json.Int e.stop) ])
             t.elements) ) ]
