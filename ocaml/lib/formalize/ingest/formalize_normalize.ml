(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)


type change = { start : int; stop : int; given : string; written : string }
type t = { source : string; text : string; changes : change list }

(* Symbols with one canonical form. The canonical form of an operator is the symbol Lean uses. *)
let symbols =
  [ ("\xE2\x88\x92", "-"); ("\xE2\x80\x93", "-"); ("\xC3\x97", "*"); ("\xE2\x8B\x85", "*"); ("\xC2\xB7", "*"); ("\xE2\x88\x97", "*"); ("\xC3\xB7", "/");
    ("\xE2\x88\x95", "/"); ("\xE2\x89\xA6", "\xE2\x89\xA4"); ("\xE2\xA9\xBD", "\xE2\x89\xA4"); ("\xE2\x89\xA7", "\xE2\x89\xA5"); ("\xE2\xA9\xBE", "\xE2\x89\xA5");
    ("<=", "\xE2\x89\xA4"); (">=", "\xE2\x89\xA5"); ("!=", "\xE2\x89\xA0"); ("=/=", "\xE2\x89\xA0"); ("\xE2\x87\x92", "\xE2\x86\x92"); ("\xE2\x9F\xB9", "\xE2\x86\x92");
    ("\xE2\x9F\xB6", "\xE2\x86\x92"); ("=>", "\xE2\x86\x92"); ("->", "\xE2\x86\x92"); ("\xE2\x87\x94", "\xE2\x86\x94"); ("\xE2\x9F\xBA", "\xE2\x86\x94"); ("<=>", "\xE2\x86\x94");
    ("<->", "\xE2\x86\x94"); ("\xE2\x88\xA3", "\xE2\x88\xA3"); ("\xCF\xB5", "\xCE\xB5"); ("\xCF\x95", "\xCF\x86"); ("\xCF\x91", "\xCE\xB8"); ("\xCF\xB1", "\xCF\x81");
    ("\xCF\x96", "\xCF\x80"); ("\xE2\x84\x95", "\xE2\x84\x95"); ("\xE2\x80\x98", "'"); ("\xE2\x80\x99", "'"); ("\xE2\x80\x9C", "\""); ("\xE2\x80\x9D", "\"");
    ("\xC2\xA0", " "); ("\xE2\x80\x89", " ") ]

(* Superscript and subscript digits and signs, as what they stand for. *)
let superscripts =
  [ ("\xE2\x81\xB0", "0"); ("\xC2\xB9", "1"); ("\xC2\xB2", "2"); ("\xC2\xB3", "3"); ("\xE2\x81\xB4", "4"); ("\xE2\x81\xB5", "5"); ("\xE2\x81\xB6", "6");
    ("\xE2\x81\xB7", "7"); ("\xE2\x81\xB8", "8"); ("\xE2\x81\xB9", "9"); ("\xE2\x81\xBA", "+"); ("\xE2\x81\xBB", "-"); ("\xE2\x81\xBF", "n") ]

let subscripts =
  [ ("\xE2\x82\x80", "0"); ("\xE2\x82\x81", "1"); ("\xE2\x82\x82", "2"); ("\xE2\x82\x83", "3"); ("\xE2\x82\x84", "4"); ("\xE2\x82\x85", "5"); ("\xE2\x82\x86", "6");
    ("\xE2\x82\x87", "7"); ("\xE2\x82\x88", "8"); ("\xE2\x82\x89", "9") ]

(* Commands of LaTeX, without the backslash. *)
let commands =
  [ ("leq", "\xE2\x89\xA4"); ("le", "\xE2\x89\xA4"); ("geq", "\xE2\x89\xA5"); ("ge", "\xE2\x89\xA5"); ("neq", "\xE2\x89\xA0"); ("ne", "\xE2\x89\xA0"); ("cdot", "*");
    ("times", "*"); ("div", "/"); ("pm", "\xC2\xB1"); ("to", "\xE2\x86\x92"); ("rightarrow", "\xE2\x86\x92"); ("implies", "\xE2\x86\x92"); ("Rightarrow", "\xE2\x86\x92");
    ("iff", "\xE2\x86\x94"); ("Leftrightarrow", "\xE2\x86\x94"); ("leftrightarrow", "\xE2\x86\x94"); ("land", "\xE2\x88\xA7"); ("wedge", "\xE2\x88\xA7"); ("lor", "\xE2\x88\xA8");
    ("vee", "\xE2\x88\xA8"); ("lnot", "\xC2\xAC"); ("neg", "\xC2\xAC"); ("forall", "\xE2\x88\x80"); ("exists", "\xE2\x88\x83"); ("in", "\xE2\x88\x88"); ("notin", "\xE2\x88\x89");
    ("subseteq", "\xE2\x8A\x86"); ("subset", "\xE2\x8A\x82"); ("cup", "\xE2\x88\xAA"); ("cap", "\xE2\x88\xA9"); ("emptyset", "\xE2\x88\x85"); ("mid", "\xE2\x88\xA3");
    ("infty", "\xE2\x88\x9E"); ("sum", "\xE2\x88\x91"); ("prod", "\xE2\x88\x8F"); ("sqrt", "\xE2\x88\x9A"); ("alpha", "\xCE\xB1"); ("beta", "\xCE\xB2"); ("gamma", "\xCE\xB3");
    ("delta", "\xCE\xB4"); ("epsilon", "\xCE\xB5"); ("varepsilon", "\xCE\xB5"); ("zeta", "\xCE\xB6"); ("eta", "\xCE\xB7"); ("theta", "\xCE\xB8"); ("vartheta", "\xCE\xB8");
    ("lambda", "\xCE\xBB"); ("mu", "\xCE\xBC"); ("nu", "\xCE\xBD"); ("xi", "\xCE\xBE"); ("pi", "\xCF\x80"); ("rho", "\xCF\x81"); ("sigma", "\xCF\x83"); ("tau", "\xCF\x84");
    ("phi", "\xCF\x86"); ("varphi", "\xCF\x86"); ("chi", "\xCF\x87"); ("psi", "\xCF\x88"); ("omega", "\xCF\x89"); ("Gamma", "\xCE\x93"); ("Delta", "\xCE\x94");
    ("Sigma", "\xCE\xA3"); ("Omega", "\xCE\xA9"); ("ldots", "..."); ("dots", "..."); ("cdots", "..."); ("left", ""); ("right", ""); ("quad", " "); (",", " ");
    ("!", ""); (";", " "); (" ", " ") ]

(* The blackboard letters of LaTeX. *)
let blackboard = [ ("N", "\xE2\x84\x95"); ("Z", "\xE2\x84\xA4"); ("Q", "\xE2\x84\x9A"); ("R", "\xE2\x84\x9D"); ("C", "\xE2\x84\x82") ]

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let starts source i prefix = i + String.length prefix <= String.length source && String.sub source i (String.length prefix) = prefix

(* The text between a brace at [i] and the brace that closes it, and the byte after that. *)
let braced source i =
  let n = String.length source in
  if i >= n || source.[i] <> '{' then None
  else
    let rec go j depth = if j >= n then None else match source.[j] with '{' -> go (j + 1) (depth + 1) | '}' -> if depth = 0 then Some j else go (j + 1) (depth - 1) | _ -> go (j + 1) depth in
    Option.map (fun close -> (String.sub source (i + 1) (close - i - 1), close + 1)) (go (i + 1) 0)

(* A run of superscript or subscript characters at [i], as plain text, and the byte after it. *)
let run table source i =
  let rec go j acc =
    match List.find_opt (fun (given, _) -> starts source j given) table with
    | Some (given, written) -> go (j + String.length given) (acc ^ written)
    | None -> (acc, j)
  in
  go i ""

let rec rewrite source =
  let n = String.length source in
  let buffer = Buffer.create (n + 16) in
  let changes = ref [] in
  let ends_blank () = Buffer.length buffer > 0 && (let last = Buffer.nth buffer (Buffer.length buffer - 1) in last = ' ' || last = '\n') in
  let change start stop written =
    (* A space that would follow a space is left out: markup that is removed leaves no gap behind. *)
    let written = if written = " " && ends_blank () then "" else written in
    if String.sub source start (stop - start) <> written then changes := { start; stop; given = String.sub source start (stop - start); written } :: !changes;
    Buffer.add_string buffer written
  in
  let wrap exponent = if String.length exponent = 1 || String.for_all (fun c -> c >= '0' && c <= '9') exponent then exponent else "(" ^ exponent ^ ")" in
  (* Whether nothing but blank space stands between the start of the line and [i]. *)
  let head_of_line i =
    let rec back j = j < 0 || source.[j] = '\n' || ((source.[j] = ' ' || source.[j] = '\t') && back (j - 1)) in
    back (i - 1)
  in
  let rec go i =
    if i < n then begin
      let c = source.[i] in
      if c = '\\' then begin
        (* A command: its name is letters, or one character that is not a letter. *)
        let stop =
          if i + 1 < n && is_letter source.[i + 1] then
            let rec word j = if j < n && is_letter source.[j] then word (j + 1) else j in
            word (i + 1)
          else min n (i + 2)
        in
        let name = String.sub source (i + 1) (stop - i - 1) in
        match name with
        | "frac" | "dfrac" | "tfrac" ->
          (match braced source stop with
           | Some (top, after) ->
             (match braced source after with
              | Some (bottom, last) ->
                change i last (Printf.sprintf "(%s)/(%s)" (rewrite_text top) (rewrite_text bottom));
                go last
              | None -> change i stop ""; go stop)
           | None -> change i stop ""; go stop)
        | "mathbb" ->
          (match braced source stop with
           | Some (letter, after) -> change i after (Option.value ~default:letter (List.assoc_opt letter blackboard)); go after
           | None -> change i stop ""; go stop)
        | "text" | "mathrm" | "operatorname" | "mathit" | "mathbf" ->
          (match braced source stop with Some (inner, after) -> change i after inner; go after | None -> change i stop ""; go stop)
        | "(" | ")" | "[" | "]" -> change i stop ""; go stop
        | _ ->
          (match List.assoc_opt name commands with
           | Some written ->
             (* A command that became a word of symbols is followed by a space when a letter follows it. *)
             change i stop written;
             go stop
           | None -> Buffer.add_string buffer (String.sub source i (stop - i)); go stop)
      end
      else if c = ' ' || c = '\t' then begin
        change i (i + 1) " ";
        go (i + 1)
      end
      else if c = '$' then begin
        let stop = if i + 1 < n && source.[i + 1] = '$' then i + 2 else i + 1 in
        change i stop "";
        go stop
      end
      else if c = '`' then begin
        (* The marks of Markdown around code. *)
        change i (i + 1) "";
        go (i + 1)
      end
      else if c = '*' && i + 1 < n && source.[i + 1] = '*' then begin
        (* The marks of Markdown around strong words. *)
        change i (i + 2) "";
        go (i + 2)
      end
      else if (c = '#' || c = '>') && head_of_line i then begin
        (* The marks of Markdown at the head of a line: a heading, a quotation. *)
        let rec marks j = if j < n && (source.[j] = c || source.[j] = ' ') then marks (j + 1) else j in
        let stop = marks i in
        change i stop "";
        go stop
      end
      else if c = '^' || c = '_' then begin
        match braced source (i + 1) with
        | Some (inner, after) -> change i after (String.make 1 c ^ wrap (rewrite_text inner)); go after
        | None -> Buffer.add_char buffer c; go (i + 1)
      end
      else
        match run superscripts source i with
        | written, stop when stop > i -> change i stop ("^" ^ wrap written); go stop
        | _ ->
          (match run subscripts source i with
           | written, stop when stop > i -> change i stop ("_" ^ written); go stop
           | _ ->
             (* The longest spelling that stands here: "<=>" is one sign, not "<=" and then ">". *)
             let longest best (given, written) =
               if starts source i given && (match best with Some (g, _) -> String.length given > String.length g | None -> true) then Some (given, written) else best
             in
             (match List.fold_left longest None symbols with
              | Some (given, written) -> change i (i + String.length given) written; go (i + String.length given)
              | None -> Buffer.add_char buffer c; go (i + 1)))
    end
  in
  go 0;
  (Buffer.contents buffer, List.rev !changes)

and rewrite_text source = fst (rewrite source)

let normalize source =
  let text, changes = rewrite source in
  { source; text; changes }

let text source = (normalize source).text

let source_end t at =
  let rec go changes source_at written_at =
    match changes with
    | [] -> source_at + (at - written_at) + 1
    | change :: rest ->
      let copied = change.start - source_at in
      if at < written_at + copied then source_at + (at - written_at) + 1
      else if at < written_at + copied + String.length change.written then change.stop
      else go rest change.stop (written_at + copied + String.length change.written)
  in
  max 0 (min (String.length t.source) (go t.changes 0 0))

let source_offset t at =
  (* Walks the changes, keeping count of how far the normalized text has run ahead or behind. *)
  let rec go changes source_at written_at =
    match changes with
    | [] -> source_at + (at - written_at)
    | change :: rest ->
      let copied = change.start - source_at in
      if at < written_at + copied then source_at + (at - written_at)
      else if at < written_at + copied + String.length change.written then change.start
      else go rest change.stop (written_at + copied + String.length change.written)
  in
  max 0 (min (String.length t.source) (go t.changes 0 0))
