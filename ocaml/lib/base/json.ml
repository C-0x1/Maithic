(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Obj of (string * t) list

exception Parse_error of int * string

(* ---- parsing ---- *)

type reader = { src : string; mutable pos : int }

let fail r msg = raise (Parse_error (r.pos, msg))
let peek r = if r.pos < String.length r.src then Some r.src.[r.pos] else None
let advance r = r.pos <- r.pos + 1

let rec skip_ws r =
  match peek r with
  | Some (' ' | '\t' | '\n' | '\r') -> advance r; skip_ws r
  | _ -> ()

let expect_word r word value =
  let n = String.length word in
  if r.pos + n <= String.length r.src && String.sub r.src r.pos n = word then begin
    r.pos <- r.pos + n;
    value
  end
  else fail r (Printf.sprintf "expected '%s'" word)

let add_utf8 buf code =
  if code < 0x80 then Buffer.add_char buf (Char.chr code)
  else if code < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end
  else if code < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end
  else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (code lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F)))
  end

let hex4 r =
  if r.pos + 4 > String.length r.src then fail r "truncated \\u escape";
  let value = ref 0 in
  for i = 0 to 3 do
    let d =
      match r.src.[r.pos + i] with
      | '0' .. '9' as c -> Char.code c - 48
      | 'a' .. 'f' as c -> Char.code c - 87
      | 'A' .. 'F' as c -> Char.code c - 55
      | _ -> fail r "invalid \\u escape"
    in
    value := (!value lsl 4) lor d
  done;
  r.pos <- r.pos + 4;
  !value

let parse_string r =
  advance r;
  let buf = Buffer.create 32 in
  let rec loop () =
    match peek r with
    | None -> fail r "unterminated string"
    | Some '"' -> advance r
    | Some '\\' ->
      advance r;
      (match peek r with
       | Some '"' -> Buffer.add_char buf '"'; advance r
       | Some '\\' -> Buffer.add_char buf '\\'; advance r
       | Some '/' -> Buffer.add_char buf '/'; advance r
       | Some 'b' -> Buffer.add_char buf '\b'; advance r
       | Some 'f' -> Buffer.add_char buf '\012'; advance r
       | Some 'n' -> Buffer.add_char buf '\n'; advance r
       | Some 'r' -> Buffer.add_char buf '\r'; advance r
       | Some 't' -> Buffer.add_char buf '\t'; advance r
       | Some 'u' ->
         advance r;
         let hi = hex4 r in
         if hi >= 0xD800 && hi <= 0xDBFF then begin
           if r.pos + 2 <= String.length r.src && r.src.[r.pos] = '\\' && r.src.[r.pos + 1] = 'u'
           then begin
             r.pos <- r.pos + 2;
             let lo = hex4 r in
             if lo >= 0xDC00 && lo <= 0xDFFF then
               add_utf8 buf (0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00))
             else fail r "invalid surrogate pair"
           end
           else fail r "lone surrogate"
         end
         else if hi >= 0xDC00 && hi <= 0xDFFF then fail r "lone surrogate"
         else add_utf8 buf hi
       | _ -> fail r "invalid escape");
      loop ()
    | Some c when Char.code c < 0x20 -> fail r "control character in string"
    | Some c -> Buffer.add_char buf c; advance r; loop ()
  in
  loop ();
  Buffer.contents buf

let parse_number r =
  let start = r.pos in
  let is_float = ref false in
  if peek r = Some '-' then advance r;
  let digits () =
    let from = r.pos in
    while (match peek r with Some '0' .. '9' -> true | _ -> false) do advance r done;
    if r.pos = from then fail r "expected a digit"
  in
  (match peek r with
   | Some '0' -> advance r
   | Some '1' .. '9' -> digits ()
   | _ -> fail r "expected a digit");
  if peek r = Some '.' then begin is_float := true; advance r; digits () end;
  (match peek r with
   | Some ('e' | 'E') ->
     is_float := true;
     advance r;
     (match peek r with Some ('+' | '-') -> advance r | _ -> ());
     digits ()
   | _ -> ());
  let text = String.sub r.src start (r.pos - start) in
  if !is_float then Float (float_of_string text)
  else match int_of_string_opt text with
    | Some n -> Int n
    | None -> Float (float_of_string text)

let rec parse_value r depth =
  if depth > 512 then fail r "nesting too deep";
  skip_ws r;
  match peek r with
  | None -> fail r "unexpected end of input"
  | Some '{' ->
    advance r;
    skip_ws r;
    if peek r = Some '}' then begin advance r; Obj [] end
    else begin
      let rec members acc =
        skip_ws r;
        if peek r <> Some '"' then fail r "expected a string key";
        let key = parse_string r in
        skip_ws r;
        if peek r <> Some ':' then fail r "expected ':'";
        advance r;
        let value = parse_value r (depth + 1) in
        skip_ws r;
        match peek r with
        | Some ',' -> advance r; members ((key, value) :: acc)
        | Some '}' -> advance r; Obj (List.rev ((key, value) :: acc))
        | _ -> fail r "expected ',' or '}'"
      in
      members []
    end
  | Some '[' ->
    advance r;
    skip_ws r;
    if peek r = Some ']' then begin advance r; List [] end
    else begin
      let rec elements acc =
        let value = parse_value r (depth + 1) in
        skip_ws r;
        match peek r with
        | Some ',' -> advance r; elements (value :: acc)
        | Some ']' -> advance r; List (List.rev (value :: acc))
        | _ -> fail r "expected ',' or ']'"
      in
      elements []
    end
  | Some '"' -> String (parse_string r)
  | Some 't' -> expect_word r "true" (Bool true)
  | Some 'f' -> expect_word r "false" (Bool false)
  | Some 'n' -> expect_word r "null" Null
  | Some ('-' | '0' .. '9') -> parse_number r
  | Some c -> fail r (Printf.sprintf "unexpected character '%c'" c)

let parse s =
  let r = { src = s; pos = 0 } in
  match
    let v = parse_value r 0 in
    skip_ws r;
    if r.pos <> String.length s then fail r "trailing characters after the value";
    v
  with
  | v -> Ok v
  | exception Parse_error (pos, msg) -> Error (Printf.sprintf "JSON error at offset %d: %s" pos msg)

let parse_exn s = match parse s with Ok v -> v | Error e -> failwith e

(* ---- printing ---- *)

let escape_to buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let float_repr f =
  if Float.is_nan f || Float.abs f = Float.infinity then "null"
  else if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.1f" f
  else
    let short = Printf.sprintf "%.15g" f in
    if float_of_string short = f then short else Printf.sprintf "%.17g" f

let sort_members members = List.stable_sort (fun (a, _) (b, _) -> compare a b) members

let to_string ?(canonical = false) v =
  let buf = Buffer.create 256 in
  let rec go = function
    | Null -> Buffer.add_string buf "null"
    | Bool b -> Buffer.add_string buf (if b then "true" else "false")
    | Int n -> Buffer.add_string buf (string_of_int n)
    | Float f -> Buffer.add_string buf (float_repr f)
    | String s -> escape_to buf s
    | List items ->
      Buffer.add_char buf '[';
      List.iteri (fun i item -> if i > 0 then Buffer.add_char buf ','; go item) items;
      Buffer.add_char buf ']'
    | Obj members ->
      let members = if canonical then sort_members members else members in
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, item) ->
          if i > 0 then Buffer.add_char buf ',';
          escape_to buf k;
          Buffer.add_char buf ':';
          go item)
        members;
      Buffer.add_char buf '}'
  in
  go v;
  Buffer.contents buf

let pretty v =
  let buf = Buffer.create 256 in
  let pad n = Buffer.add_string buf (String.make (2 * n) ' ') in
  let rec go level = function
    | (Null | Bool _ | Int _ | Float _ | String _) as scalar -> Buffer.add_string buf (to_string scalar)
    | List [] -> Buffer.add_string buf "[]"
    | Obj [] -> Buffer.add_string buf "{}"
    | List items ->
      Buffer.add_string buf "[\n";
      List.iteri
        (fun i item ->
          if i > 0 then Buffer.add_string buf ",\n";
          pad (level + 1);
          go (level + 1) item)
        items;
      Buffer.add_char buf '\n';
      pad level;
      Buffer.add_char buf ']'
    | Obj members ->
      Buffer.add_string buf "{\n";
      List.iteri
        (fun i (k, item) ->
          if i > 0 then Buffer.add_string buf ",\n";
          pad (level + 1);
          escape_to buf k;
          Buffer.add_string buf ": ";
          go (level + 1) item)
        members;
      Buffer.add_char buf '\n';
      pad level;
      Buffer.add_char buf '}'
  in
  go 0 v;
  Buffer.contents buf

(* ---- access ---- *)

let member key = function Obj members -> List.assoc_opt key members | _ -> None

let rec path keys v =
  match keys with
  | [] -> Some v
  | k :: rest -> (match member k v with Some inner -> path rest inner | None -> None)

let string_opt = function String s -> Some s | _ -> None
let int_opt = function
  | Int n -> Some n
  | Float f when Float.is_integer f && Float.abs f < 4e18 -> Some (int_of_float f)
  | _ -> None
let float_opt = function Float f -> Some f | Int n -> Some (float_of_int n) | _ -> None
let bool_opt = function Bool b -> Some b | _ -> None
let list_opt = function List l -> Some l | _ -> None
let obj_opt = function Obj m -> Some m | _ -> None

let get what conv key v =
  match member key v with
  | None -> Error (Printf.sprintf "missing member '%s'" key)
  | Some inner ->
    (match conv inner with
     | Some x -> Ok x
     | None -> Error (Printf.sprintf "member '%s' is not %s" key what))

let get_string key v = get "a string" string_opt key v
let get_int key v = get "an integer" int_opt key v
let get_bool key v = get "a boolean" bool_opt key v
let get_list key v = get "a list" list_opt key v

let string_or default key v = match member key v with Some (String s) -> s | _ -> default
let int_or default key v = match Option.bind (member key v) int_opt with Some n -> n | None -> default
let bool_or default key v = match member key v with Some (Bool b) -> b | _ -> default
let list_or_empty key v = match member key v with Some (List l) -> l | _ -> []
let strings = function List l -> List.filter_map string_opt l | _ -> []

(* ---- comparison ---- *)

let rec equal a b =
  match a, b with
  | Null, Null -> true
  | Bool x, Bool y -> x = y
  | Int x, Int y -> x = y
  | Float x, Float y -> x = y
  | Int x, Float y | Float y, Int x -> float_of_int x = y
  | String x, String y -> String.equal x y
  | List x, List y -> List.length x = List.length y && List.for_all2 equal x y
  | Obj x, Obj y ->
    List.length x = List.length y
    && List.for_all (fun (k, v) -> match List.assoc_opt k y with Some w -> equal v w | None -> false) x
  | _ -> false

let contains_sub hay needle =
  let n = String.length needle and h = String.length hay in
  let rec at i = i + n <= h && (String.sub hay i n = needle || at (i + 1)) in
  n = 0 || at 0

let is_subset ~expected actual =
  let rec go where expected actual =
    match expected, actual with
    | Obj [ ("$contains", String needle) ], String text ->
      if contains_sub text needle then Ok ()
      else Error (Printf.sprintf "%s: %S does not contain %S" where text needle)
    | Obj [ ("$contains", _) ], _ -> Error (where ^ ": $contains needs a string on both sides")
    | Obj members, Obj _ ->
      List.fold_left
        (fun acc (k, v) ->
          match acc with
          | Error _ -> acc
          | Ok () ->
            (match member k actual with
             | Some inner -> go (where ^ "." ^ k) v inner
             | None -> Error (Printf.sprintf "%s.%s: missing" where k)))
        (Ok ()) members
    | Obj _, _ -> Error (Printf.sprintf "%s: expected an object, got %s" where (to_string actual))
    | List xs, List ys ->
      if List.length xs <> List.length ys then
        Error (Printf.sprintf "%s: expected %d elements, got %d" where (List.length xs) (List.length ys))
      else
        List.fold_left
          (fun acc (i, (x, y)) ->
            match acc with Error _ -> acc | Ok () -> go (Printf.sprintf "%s[%d]" where i) x y)
          (Ok ())
          (List.mapi (fun i pair -> (i, pair)) (List.combine xs ys))
    | e, a ->
      if equal e a then Ok ()
      else Error (Printf.sprintf "%s: expected %s, got %s" where (to_string e) (to_string a))
  in
  go "$" expected actual
