(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

let is_ident_char = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '.' | '!' | '?' -> true | c -> Char.code c >= 0x80

let mask source =
  let n = String.length source in
  let out = Bytes.of_string source in
  let blank i = if Bytes.get out i <> '\n' then Bytes.set out i ' ' in
  let rec code i =
    if i < n then
      match source.[i] with
      | '-' when i + 1 < n && source.[i + 1] = '-' -> line_comment i
      | '/' when i + 1 < n && source.[i + 1] = '-' -> block_comment (i + 2) 1 i
      | '"' -> string_literal (i + 1) i
      | _ -> code (i + 1)
  and line_comment i =
    if i < n && source.[i] <> '\n' then begin blank i; line_comment (i + 1) end else code i
  and block_comment i depth start =
    for k = start to min (n - 1) (i - 1) do blank k done;
    if i >= n then ()
    else if depth = 0 then code i
    else if i + 1 < n && source.[i] = '-' && source.[i + 1] = '/' then block_comment (i + 2) (depth - 1) i
    else if i + 1 < n && source.[i] = '/' && source.[i + 1] = '-' then block_comment (i + 2) (depth + 1) i
    else block_comment (i + 1) depth i
  and string_literal i start =
    (* The quotes stay; the contents become spaces. *)
    ignore start;
    if i >= n then ()
    else
      match source.[i] with
      | '\\' when i + 1 < n -> blank i; blank (i + 1); string_literal (i + 2) start
      | '"' -> code (i + 1)
      | _ -> blank i; string_literal (i + 1) start
  in
  code 0;
  Bytes.to_string out

type decl = {
  name : string;
  keyword : string;
  line : int;
  prelude : string;
  doc : string;
  header : string;
  proof : string;
  rest : string;
}

(* Words that begin a new command when they start a line. *)
let command_words =
  [ "theorem"; "lemma"; "def"; "example"; "instance"; "structure"; "inductive"; "class"; "abbrev"; "axiom"; "opaque";
    "namespace"; "section"; "end"; "open"; "universe"; "variable"; "set_option"; "attribute"; "private"; "protected";
    "noncomputable"; "mutual"; "macro"; "syntax"; "notation"; "elab"; "initialize"; "export"; "import"; "unsafe"; "partial";
    "local"; "scoped"; "infix"; "infixl"; "infixr"; "prefix"; "postfix"; "deriving"; "macro_rules"; "elab_rules";
    "declare_syntax_cat"; "omit"; "include"; "public"; "meta"; "module" ]

let word_at text i =
  let n = String.length text in
  let j = ref i in
  while !j < n && (match text.[!j] with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false) do incr j done;
  String.sub text i (!j - i)

let line_start text i =
  match String.rindex_from_opt text (max 0 (i - 1)) '\n' with
  | Some k when i > 0 -> k + 1
  | _ -> 0

let line_number text i =
  let count = ref 1 in
  String.iteri (fun k c -> if k < i && c = '\n' then incr count) text;
  !count

(* Offset of the first line at or after [from] that starts a new command. *)
let next_command ~source ~masked from =
  let n = String.length masked in
  let rec at_line i =
    if i >= n then n
    else begin
      let starts =
        (i + 1 < n && source.[i] = '@' && source.[i + 1] = '[')
        || (i + 2 < n && source.[i] = '/' && source.[i + 1] = '-' && source.[i + 2] = '-')
        || (masked.[i] = '#' && i + 1 < n && (match masked.[i + 1] with 'a' .. 'z' -> true | _ -> false))
        || List.mem (word_at masked i) command_words
      in
      if starts then i
      else match String.index_from_opt masked i '\n' with Some k -> at_line (k + 1) | None -> n
    end
  in
  match String.index_from_opt masked from '\n' with Some k -> at_line (k + 1) | None -> n

(* Words that may stand before the keyword of a declaration, on the line of the keyword or on
   lines of their own. *)
let modifier_words = [ "private"; "protected"; "noncomputable"; "unsafe"; "partial"; "public"; "meta"; "nonrec" ]

(* The offset just after the block comment that opens at [k]. *)
let comment_end source k =
  let n = String.length source in
  let rec go i depth =
    if i + 1 >= n then None
    else if source.[i] = '-' && source.[i + 1] = '/' then if depth = 1 then Some (i + 2) else go (i + 2) (depth - 1)
    else if source.[i] = '/' && source.[i + 1] = '-' then go (i + 2) (depth + 1)
    else go (i + 1) depth
  in
  go (k + 2) 1

(* Where a declaration really begins, given the start of the line of its keyword: the lines just
   before it that hold only attributes or modifiers are part of it, and so is a documentation
   comment that ends just before those. Returns the start of the comment, or of the lines when
   there is none, and the start of the lines. *)
let head ~source ~masked start =
  let rec lines s =
    if s = 0 then s
    else begin
      let previous = line_start masked (s - 1) in
      let text = String.trim (String.sub masked previous (s - previous)) in
      let n = String.length text in
      let attributes = n >= 3 && text.[0] = '@' && text.[1] = '[' && text.[n - 1] = ']' in
      let modifiers = text <> "" && List.for_all (fun w -> w = "" || List.mem w modifier_words) (String.split_on_char ' ' text) in
      if attributes || modifiers then lines previous else s
    end
  in
  let s = lines start in
  let blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let last = ref (s - 1) in
  while !last >= 0 && blank source.[!last] do decr last done;
  if !last < 4 || source.[!last] <> '/' || source.[!last - 1] <> '-' then (s, s)
  else begin
    (* The comment may not reach back past the code before it. *)
    let code = ref (!last - 2) in
    while !code >= 0 && blank masked.[!code] do decr code done;
    let rec opening k =
      if k <= !code then None
      else if source.[k] = '/' && source.[k + 1] = '-' && source.[k + 2] = '-' && comment_end source k = Some (!last + 1) then Some k
      else opening (k - 1)
    in
    match opening (!last - 3) with
    | None -> (s, s)
    | Some k ->
      let line = line_start source k in
      ((if String.trim (String.sub source line (k - line)) = "" then line else k), s)
  end

(* A command that ends in [in], as [set_option maxHeartbeats 400000 in] and [open Nat in] do,
   applies to the one declaration that follows it, and is part of that declaration: without the
   declaration it is not a command at all. Given where a declaration begins, returns where it
   begins with such lines before it counted in. *)
let prefixes ~masked begins =
  let rec back b =
    if b = 0 then b
    else begin
      let previous = line_start masked (b - 1) in
      let words = List.filter (fun w -> w <> "") (String.split_on_char ' ' (String.trim (String.sub masked previous (b - previous)))) in
      match (words, List.rev words) with
      | first :: _ :: _, "in" :: _ when List.mem first [ "set_option"; "open"; "omit"; "include"; "variable"; "universe" ] -> back previous
      | _ -> b
    end
  in
  back begins

(* Offset of the first [:=] at bracket depth zero in [masked] between [from]
   and [limit]. *)
let find_assign masked from limit =
  let depth = ref 0 and found = ref None and i = ref from in
  while !found = None && !i < limit do
    (match masked.[!i] with
     | '(' | '[' | '{' -> incr depth
     | ')' | ']' | '}' -> decr depth
     | '\xE2' when !i + 2 < limit && masked.[!i + 1] = '\x9F' && masked.[!i + 2] = '\xA8' -> incr depth
     | '\xE2' when !i + 2 < limit && masked.[!i + 1] = '\x9F' && masked.[!i + 2] = '\xA9' -> decr depth
     | ':' when !depth <= 0 && !i + 1 < limit && masked.[!i + 1] = '=' -> found := Some !i
     | _ -> ());
    incr i
  done;
  !found

let declarations source =
  let masked = mask source in
  let n = String.length masked in
  let rec scan i acc =
    if i >= n then List.rev acc
    else begin
      let boundary = i = 0 || not (is_ident_char masked.[i - 1]) in
      let keyword = if boundary then word_at masked i else "" in
      if keyword = "theorem" || keyword = "lemma" then begin
        let after = i + String.length keyword in
        let name_start = ref after in
        while !name_start < n && (masked.[!name_start] = ' ' || masked.[!name_start] = '\t') do incr name_start done;
        let name_end = ref !name_start in
        while !name_end < n && is_ident_char masked.[!name_end] do incr name_end done;
        let name = String.sub source !name_start (!name_end - !name_start) in
        let stop =
          (* The declaration ends with its last line of code: comments and blank
             lines before the next command belong to what follows. *)
          let limit = next_command ~source ~masked i in
          let last = ref (limit - 1) in
          while !last > i && (match masked.[!last] with ' ' | '\t' | '\n' | '\r' -> true | _ -> false) do decr last done;
          match String.index_from_opt masked !last '\n' with Some k when k < limit -> k + 1 | _ -> limit
        in
        match (if name = "" || !name_start = after then None else find_assign masked !name_end stop) with
        | None -> scan (i + String.length keyword) acc
        | Some assign ->
          let begins, start = head ~source ~masked (line_start masked i) in
          let begins = prefixes ~masked begins in
          let decl =
            { name; keyword;
              line = line_number source i;
              prelude = String.sub source 0 begins;
              doc = String.sub source begins (start - begins);
              header = String.sub source start (i - start) ^ String.trim (String.sub source i (assign - i));
              proof = String.trim (String.sub source (assign + 2) (stop - assign - 2));
              rest = String.sub source stop (n - stop) }
          in
          scan stop (decl :: acc)
      end
      else scan (i + max 1 (String.length keyword)) acc
    end
  in
  scan 0 []

let short_name name = match List.rev (String.split_on_char '.' name) with last :: _ -> last | [] -> name

let find source name =
  let all = declarations source in
  let exact = List.filter (fun d -> d.name = name) all in
  let loose = List.filter (fun d -> short_name d.name = short_name name) all in
  match exact, loose with
  | d :: _, _ -> Ok d
  | [], [ d ] -> Ok d
  | [], _ :: _ :: _ -> Error (Printf.sprintf "several theorems are called '%s'; give the full name" name)
  | [], [] ->
    (match all with
     | [] -> Error "the file contains no theorem with a `:=` proof"
     | _ ->
       Error
         (Printf.sprintf "no theorem called '%s' in the file (found: %s)" name
            (String.concat ", " (List.map (fun d -> d.name) all))))

let has_sorry decl =
  let masked = mask decl.proof in
  let n = String.length masked in
  let word_is w i =
    let l = String.length w in
    i + l <= n && String.sub masked i l = w
    && (i = 0 || not (is_ident_char masked.[i - 1]))
    && (i + l = n || not (is_ident_char masked.[i + l]))
  in
  let rec any i = i < n && (word_is "sorry" i || word_is "admit" i || any (i + 1)) in
  any 0

let assemble decl proof =
  let proof = String.trim proof in
  let last_line = match List.rev (String.split_on_char '\n' decl.header) with line :: _ -> line | [] -> "" in
  if String.contains proof '\n' || String.length last_line + String.length proof > 96 then
    decl.doc ^ decl.header ^ " :=\n" ^ Text.indent 2 proof ^ "\n"
  else decl.doc ^ decl.header ^ " := " ^ proof ^ "\n"

let assemble_by decl proof =
  let proof = String.trim proof in
  let lines = String.split_on_char '\n' proof in
  (* Tactics under `by` have to stand to the right of the start of the theorem. *)
  let indented = List.for_all (fun line -> String.trim line = "" || Text.starts_with ~prefix:"  " line) (List.tl lines) in
  if List.hd lines = "by" && List.length lines > 1 && indented then decl.doc ^ decl.header ^ " := " ^ proof ^ "\n" else assemble decl proof

let with_proof_by decl proof =
  let body = assemble_by decl proof in
  let separator = if decl.rest = "" || Text.starts_with ~prefix:"\n" decl.rest then "" else "\n" in
  decl.prelude ^ body ^ separator ^ decl.rest

let with_proof decl proof =
  let body = assemble decl proof in
  let separator = if decl.rest = "" || Text.starts_with ~prefix:"\n" decl.rest then "" else "\n" in
  decl.prelude ^ body ^ separator ^ decl.rest

let statement decl =
  String.split_on_char '\n' decl.header |> List.map String.trim |> List.filter (fun l -> l <> "") |> String.concat " "
  |> String.split_on_char ' '
  |> List.filter (fun w -> w <> "")
  |> String.concat " "
