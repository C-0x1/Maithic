(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

exception Parse_error of int * string

type node =
  | Leaf of Json.t
  | Table of table
  | Tables of table list ref (* most recent first *)

and table = {
  mutable entries : (string * node) list; (* most recent first *)
  mutable defined : bool; (* named by a [header] *)
  mutable closed : bool; (* an inline table: cannot be extended *)
}

type reader = { src : string; mutable pos : int; mutable line : int }

let fail r msg = raise (Parse_error (r.line, msg))
let new_table () = { entries = []; defined = false; closed = false }
let peek r = if r.pos < String.length r.src then Some r.src.[r.pos] else None
let peek_at r k = if r.pos + k < String.length r.src then Some r.src.[r.pos + k] else None

let advance r =
  if r.pos < String.length r.src && r.src.[r.pos] = '\n' then r.line <- r.line + 1;
  r.pos <- r.pos + 1

let looking_at r word =
  let n = String.length word in
  r.pos + n <= String.length r.src && String.sub r.src r.pos n = word

let rec skip_blanks r =
  match peek r with
  | Some (' ' | '\t') -> advance r; skip_blanks r
  | _ -> ()

let skip_comment r =
  if peek r = Some '#' then
    while (match peek r with Some '\n' | None -> false | _ -> true) do advance r done

(* Skips blanks, comments and newlines: used between elements of an array. *)
let rec skip_all r =
  skip_blanks r;
  match peek r with
  | Some '#' -> skip_comment r; skip_all r
  | Some ('\n' | '\r') -> advance r; skip_all r
  | _ -> ()

let end_of_line r =
  skip_blanks r;
  skip_comment r;
  match peek r with
  | None -> ()
  | Some '\r' when peek_at r 1 = Some '\n' -> advance r; advance r
  | Some '\n' -> advance r
  | Some c -> fail r (Printf.sprintf "unexpected '%c' after a value" c)

let add_utf8 buf code =
  let add n = Buffer.add_char buf (Char.chr n) in
  if code < 0x80 then add code
  else if code < 0x800 then (add (0xC0 lor (code lsr 6)); add (0x80 lor (code land 0x3F)))
  else if code < 0x10000 then (
    add (0xE0 lor (code lsr 12));
    add (0x80 lor ((code lsr 6) land 0x3F));
    add (0x80 lor (code land 0x3F)))
  else (
    add (0xF0 lor (code lsr 18));
    add (0x80 lor ((code lsr 12) land 0x3F));
    add (0x80 lor ((code lsr 6) land 0x3F));
    add (0x80 lor (code land 0x3F)))

let hex_digits r count =
  let value = ref 0 in
  for _ = 1 to count do
    let d =
      match peek r with
      | Some ('0' .. '9' as c) -> Char.code c - 48
      | Some ('a' .. 'f' as c) -> Char.code c - 87
      | Some ('A' .. 'F' as c) -> Char.code c - 55
      | _ -> fail r "invalid unicode escape"
    in
    value := (!value lsl 4) lor d;
    advance r
  done;
  !value

let escape r buf =
  advance r;
  match peek r with
  | Some 'b' -> Buffer.add_char buf '\b'; advance r
  | Some 't' -> Buffer.add_char buf '\t'; advance r
  | Some 'n' -> Buffer.add_char buf '\n'; advance r
  | Some 'f' -> Buffer.add_char buf '\012'; advance r
  | Some 'r' -> Buffer.add_char buf '\r'; advance r
  | Some '"' -> Buffer.add_char buf '"'; advance r
  | Some '\\' -> Buffer.add_char buf '\\'; advance r
  | Some 'u' -> advance r; add_utf8 buf (hex_digits r 4)
  | Some 'U' -> advance r; add_utf8 buf (hex_digits r 8)
  | _ -> fail r "invalid escape in string"

let basic_string r =
  advance r;
  let buf = Buffer.create 32 in
  let rec loop () =
    match peek r with
    | None | Some '\n' -> fail r "unterminated string"
    | Some '"' -> advance r
    | Some '\\' -> escape r buf; loop ()
    | Some c -> Buffer.add_char buf c; advance r; loop ()
  in
  loop ();
  Buffer.contents buf

let literal_string r =
  advance r;
  let start = r.pos in
  while (match peek r with Some '\'' -> false | None | Some '\n' -> fail r "unterminated string" | _ -> true) do
    advance r
  done;
  let s = String.sub r.src start (r.pos - start) in
  advance r;
  s

let skip_first_newline r =
  if peek r = Some '\r' && peek_at r 1 = Some '\n' then (advance r; advance r)
  else if peek r = Some '\n' then advance r

let multiline_basic r =
  r.pos <- r.pos + 3;
  skip_first_newline r;
  let buf = Buffer.create 64 in
  let rec loop () =
    if looking_at r "\"\"\"" then begin
      r.pos <- r.pos + 3;
      (* Up to two extra quotes belong to the content. *)
      while peek r = Some '"' do Buffer.add_char buf '"'; advance r done
    end
    else
      match peek r with
      | None -> fail r "unterminated multi-line string"
      | Some '\\' ->
        (match peek_at r 1 with
         | Some ('\n' | '\r' | ' ' | '\t') ->
           advance r;
           while (match peek r with Some (' ' | '\t' | '\n' | '\r') -> true | _ -> false) do advance r done
         | _ -> escape r buf);
        loop ()
      | Some c -> Buffer.add_char buf c; advance r; loop ()
  in
  loop ();
  Buffer.contents buf

let multiline_literal r =
  r.pos <- r.pos + 3;
  skip_first_newline r;
  let buf = Buffer.create 64 in
  let rec loop () =
    if looking_at r "'''" then begin
      r.pos <- r.pos + 3;
      while peek r = Some '\'' do Buffer.add_char buf '\''; advance r done
    end
    else
      match peek r with
      | None -> fail r "unterminated multi-line string"
      | Some c -> Buffer.add_char buf c; advance r; loop ()
  in
  loop ();
  Buffer.contents buf

let is_bare_key_char = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false

let key_part r =
  match peek r with
  | Some '"' -> basic_string r
  | Some '\'' -> literal_string r
  | Some c when is_bare_key_char c ->
    let start = r.pos in
    while (match peek r with Some c -> is_bare_key_char c | None -> false) do advance r done;
    String.sub r.src start (r.pos - start)
  | _ -> fail r "expected a key"

let dotted_key r =
  let rec loop acc =
    skip_blanks r;
    let part = key_part r in
    skip_blanks r;
    if peek r = Some '.' then (advance r; loop (part :: acc)) else List.rev (part :: acc)
  in
  loop []

let scalar_token r =
  let start = r.pos in
  while
    (match peek r with
     | Some (',' | ']' | '}' | '#' | '\n' | '\r' | ' ' | '\t') | None -> false
     | Some _ -> true)
  do
    advance r
  done;
  (* A date and a time may be separated by one space. *)
  if r.pos - start = 10 && peek r = Some ' '
     && (match peek_at r 1 with Some '0' .. '9' -> true | _ -> false)
     && r.src.[start + 4] = '-'
  then begin
    advance r;
    while
      (match peek r with
       | Some (',' | ']' | '}' | '#' | '\n' | '\r' | ' ' | '\t') | None -> false
       | Some _ -> true)
    do
      advance r
    done
  end;
  String.sub r.src start (r.pos - start)

let number_of_token r token =
  let clean = String.concat "" (String.split_on_char '_' token) in
  let digits_only s = s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s in
  let unsigned = if clean <> "" && (clean.[0] = '+' || clean.[0] = '-') then String.sub clean 1 (String.length clean - 1) else clean in
  let negative = clean <> "" && clean.[0] = '-' in
  let is_date = String.length token >= 8 && digits_only (String.sub token 0 2) && (String.contains token ':' || (String.length token >= 10 && token.[4] = '-' && token.[7] = '-')) in
  if is_date then Json.String token
  else if unsigned = "inf" then Json.Float (if negative then Float.neg_infinity else Float.infinity)
  else if unsigned = "nan" then Json.Float Float.nan
  else if String.length unsigned > 2 && unsigned.[0] = '0' && (unsigned.[1] = 'x' || unsigned.[1] = 'o' || unsigned.[1] = 'b') then
    (match int_of_string_opt unsigned with
     | Some n -> Json.Int n
     | None -> fail r (Printf.sprintf "invalid integer '%s'" token))
  else if digits_only unsigned then
    (if String.length unsigned > 1 && unsigned.[0] = '0' then fail r (Printf.sprintf "leading zero in '%s'" token);
     match int_of_string_opt clean with
     | Some n -> Json.Int n
     | None -> fail r (Printf.sprintf "integer out of range: '%s'" token))
  else
    match float_of_string_opt clean with
    | Some f when String.for_all (function '0' .. '9' | '.' | 'e' | 'E' | '+' | '-' -> true | _ -> false) clean -> Json.Float f
    | _ -> fail r (Printf.sprintf "invalid value '%s'" token)

let rec node_to_json = function
  | Leaf v -> v
  | Table t -> table_to_json t
  | Tables ts -> Json.List (List.rev_map table_to_json !ts)

and table_to_json t = Json.Obj (List.rev_map (fun (k, n) -> (k, node_to_json n)) t.entries)

let rec value r : node =
  skip_blanks r;
  match peek r with
  | None -> fail r "expected a value"
  | Some '"' -> if looking_at r "\"\"\"" then Leaf (Json.String (multiline_basic r)) else Leaf (Json.String (basic_string r))
  | Some '\'' -> if looking_at r "'''" then Leaf (Json.String (multiline_literal r)) else Leaf (Json.String (literal_string r))
  | Some '[' ->
    advance r;
    let rec elements acc =
      skip_all r;
      if peek r = Some ']' then (advance r; List.rev acc)
      else begin
        let v = node_to_json (value r) in
        skip_all r;
        match peek r with
        | Some ',' -> advance r; elements (v :: acc)
        | Some ']' -> advance r; List.rev (v :: acc)
        | _ -> fail r "expected ',' or ']' in array"
      end
    in
    Leaf (Json.List (elements []))
  | Some '{' ->
    advance r;
    let t = new_table () in
    skip_blanks r;
    if peek r = Some '}' then advance r
    else begin
      let rec members () =
        let keys = dotted_key r in
        skip_blanks r;
        if peek r <> Some '=' then fail r "expected '=' in inline table";
        advance r;
        let v = value r in
        assign r t keys v;
        skip_blanks r;
        match peek r with
        | Some ',' -> advance r; members ()
        | Some '}' -> advance r
        | _ -> fail r "expected ',' or '}' in inline table"
      in
      members ()
    end;
    t.closed <- true;
    Table t
  | Some _ ->
    let token = scalar_token r in
    (match token with
     | "true" -> Leaf (Json.Bool true)
     | "false" -> Leaf (Json.Bool false)
     | "" -> fail r "expected a value"
     | _ -> Leaf (number_of_token r token))

(* Sets [keys] to [v] below table [t], creating intermediate tables. *)
and assign r t keys v =
  match keys with
  | [] -> fail r "empty key"
  | [ k ] ->
    if List.mem_assoc k t.entries then fail r (Printf.sprintf "key '%s' is defined twice" k);
    t.entries <- (k, v) :: t.entries
  | k :: rest ->
    (match List.assoc_opt k t.entries with
     | None ->
       let sub = new_table () in
       t.entries <- (k, Table sub) :: t.entries;
       assign r sub rest v
     | Some (Table sub) ->
       if sub.closed then fail r (Printf.sprintf "inline table '%s' cannot be extended" k);
       assign r sub rest v
     | Some _ -> fail r (Printf.sprintf "key '%s' is not a table" k))

(* Follows [keys] from [t] for a table header, creating tables on the way. *)
let rec descend r t keys =
  match keys with
  | [] -> t
  | k :: rest ->
    (match List.assoc_opt k t.entries with
     | None ->
       let sub = new_table () in
       t.entries <- (k, Table sub) :: t.entries;
       descend r sub rest
     | Some (Table sub) ->
       if sub.closed then fail r (Printf.sprintf "inline table '%s' cannot be extended" k);
       descend r sub rest
     | Some (Tables ts) ->
       (match !ts with last :: _ -> descend r last rest | [] -> fail r "empty array of tables")
     | Some (Leaf _) -> fail r (Printf.sprintf "key '%s' is not a table" k))

let split_last keys =
  match List.rev keys with
  | last :: rev_parents -> (List.rev rev_parents, last)
  | [] -> invalid_arg "split_last"

let parse_document text =
  let r = { src = text; pos = 0; line = 1 } in
  let root = new_table () in
  let current = ref root in
  let rec loop () =
    skip_all r;
    match peek r with
    | None -> ()
    | Some '[' ->
      let array = peek_at r 1 = Some '[' in
      advance r;
      if array then advance r;
      let keys = dotted_key r in
      skip_blanks r;
      if not (looking_at r (if array then "]]" else "]")) then fail r "unterminated table header";
      advance r;
      if array then advance r;
      let parents, last = split_last keys in
      let parent = descend r root parents in
      (match List.assoc_opt last parent.entries, array with
       | None, false ->
         let t = new_table () in
         t.defined <- true;
         parent.entries <- (last, Table t) :: parent.entries;
         current := t
       | Some (Table t), false ->
         if t.defined || t.closed then fail r (Printf.sprintf "table '%s' is defined twice" (String.concat "." keys));
         t.defined <- true;
         current := t
       | None, true ->
         let t = new_table () in
         parent.entries <- (last, Tables (ref [ t ])) :: parent.entries;
         current := t
       | Some (Tables ts), true ->
         let t = new_table () in
         ts := t :: !ts;
         current := t
       | Some _, _ -> fail r (Printf.sprintf "'%s' is already defined with another type" (String.concat "." keys)));
      end_of_line r;
      loop ()
    | Some _ ->
      let keys = dotted_key r in
      skip_blanks r;
      if peek r <> Some '=' then fail r "expected '=' after a key";
      advance r;
      let v = value r in
      assign r !current keys v;
      end_of_line r;
      loop ()
  in
  loop ();
  table_to_json root

let parse text =
  match parse_document text with
  | doc -> Ok doc
  | exception Parse_error (line, msg) -> Error (Printf.sprintf "TOML error at line %d: %s" line msg)

let parse_file path =
  match Fs.read path with
  | Error e -> Error e
  | Ok text -> (match parse text with Ok _ as ok -> ok | Error e -> Error (path ^ ": " ^ e))

let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 || Char.code c = 0x7F -> Buffer.add_string buf (Printf.sprintf "\\u%04X" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf
