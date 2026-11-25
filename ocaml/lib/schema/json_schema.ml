(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

(* ---- patterns ---- *)

type node =
  | Byte of char
  | Any
  | Class of bool * (char * char) list  (* negated, ranges *)
  | Start
  | End
  | Group of node list list  (* alternatives, each a sequence *)
  | Repeat of node * int * int option

exception Bad_pattern of string

let parse_pattern (p : string) : node list list =
  let n = String.length p in
  let pos = ref 0 in
  let peek () = if !pos < n then Some p.[!pos] else None in
  let advance () = incr pos in
  let digits = [ ('0', '9') ] and word = [ ('a', 'z'); ('A', 'Z'); ('0', '9'); ('_', '_') ] in
  let space = [ (' ', ' '); ('\t', '\t'); ('\n', '\n'); ('\r', '\r') ] in
  let escape_class c =
    match c with
    | 'd' -> Some digits
    | 'w' -> Some word
    | 's' -> Some space
    | _ -> None
  in
  let escaped_literal c =
    match c with
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'r' -> '\r'
    | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ->
      raise (Bad_pattern (Printf.sprintf "the escape \\%c is not supported" c))
    | c -> c
  in
  let parse_class () =
    (* the opening bracket has been consumed *)
    let negated = peek () = Some '^' in
    if negated then advance ();
    let ranges = ref [] in
    let rec loop first =
      match peek () with
      | None -> raise (Bad_pattern "a character class is not closed")
      | Some ']' when not first -> advance ()
      | Some c ->
        advance ();
        let low =
          if c = '\\' then begin
            match peek () with
            | None -> raise (Bad_pattern "the pattern ends with a backslash")
            | Some e ->
              advance ();
              (match escape_class e with
               | Some rs ->
                 ranges := rs @ !ranges;
                 None
               | None -> Some (escaped_literal e))
          end
          else Some c
        in
        (match low with
         | None -> ()
         | Some low ->
           if peek () = Some '-' && !pos + 1 < n && p.[!pos + 1] <> ']' then begin
             advance ();
             let high = match peek () with Some h -> advance (); if h = '\\' then (match peek () with Some e -> advance (); escaped_literal e | None -> raise (Bad_pattern "the pattern ends with a backslash")) else h | None -> raise (Bad_pattern "a character class is not closed") in
             if high < low then raise (Bad_pattern "a character range runs backwards");
             ranges := (low, high) :: !ranges
           end
           else ranges := (low, low) :: !ranges);
        loop false
    in
    loop true;
    Class (negated, !ranges)
  in
  let parse_bound () =
    (* after '{': n, n, or n,m followed by '}' *)
    let number () =
      let start = !pos in
      while (match peek () with Some c -> c >= '0' && c <= '9' | None -> false) do advance () done;
      if !pos = start then None else Some (int_of_string (String.sub p start (!pos - start)))
    in
    let low = match number () with Some v -> v | None -> raise (Bad_pattern "a repetition count is expected after {") in
    let high =
      if peek () = Some ',' then begin
        advance ();
        number ()
      end
      else Some low
    in
    if peek () <> Some '}' then raise (Bad_pattern "a repetition is not closed");
    advance ();
    (match high with Some h when h < low -> raise (Bad_pattern "a repetition has its bounds the wrong way round") | _ -> ());
    if low > 1000 || (match high with Some h -> h > 1000 | None -> false) then raise (Bad_pattern "a repetition count is too large");
    (low, high)
  in
  let rec parse_alternatives () =
    let rec sequences acc =
      let s = parse_sequence () in
      if peek () = Some '|' then begin
        advance ();
        sequences (s :: acc)
      end
      else List.rev (s :: acc)
    in
    sequences []
  and parse_sequence () =
    let rec items acc =
      match peek () with
      | None | Some '|' | Some ')' -> List.rev acc
      | Some _ ->
        let atom = parse_atom () in
        let rec quantified atom =
          match peek () with
          | Some '*' -> advance (); quantified (Repeat (atom, 0, None))
          | Some '+' -> advance (); quantified (Repeat (atom, 1, None))
          | Some '?' -> advance (); quantified (Repeat (atom, 0, Some 1))
          | Some '{' ->
            advance ();
            let low, high = parse_bound () in
            quantified (Repeat (atom, low, high))
          | _ -> atom
        in
        items (quantified atom :: acc)
    in
    items []
  and parse_atom () =
    match peek () with
    | None -> raise (Bad_pattern "the pattern ends too early")
    | Some c ->
      advance ();
      (match c with
       | '.' -> Any
       | '^' -> Start
       | '$' -> End
       | '[' -> parse_class ()
       | '(' ->
         if !pos + 1 < n && p.[!pos] = '?' && p.[!pos + 1] = ':' then pos := !pos + 2
         else if peek () = Some '?' then raise (Bad_pattern "only (?: ) groups are supported");
         let alternatives = parse_alternatives () in
         if peek () <> Some ')' then raise (Bad_pattern "a group is not closed");
         advance ();
         Group alternatives
       | '\\' ->
         (match peek () with
          | None -> raise (Bad_pattern "the pattern ends with a backslash")
          | Some e ->
            advance ();
            (match escape_class e with Some rs -> Class (false, rs) | None -> Byte (escaped_literal e)))
       | '*' | '+' | '?' | '{' -> raise (Bad_pattern (Printf.sprintf "nothing to repeat before %c" c))
       | ')' -> raise (Bad_pattern "a group is closed that was not opened")
       | c -> Byte c)
  in
  let alternatives = parse_alternatives () in
  if !pos < n then raise (Bad_pattern "a group is closed that was not opened");
  alternatives

(* Matching by backtracking with continuations. [k] receives the position
   after a successful match of the node and says whether the rest matches. *)
let rec match_node text node i (k : int -> bool) : bool =
  let n = String.length text in
  match node with
  | Byte c -> i < n && text.[i] = c && k (i + 1)
  | Any -> i < n && k (i + 1)
  | Class (negated, ranges) -> i < n && List.exists (fun (lo, hi) -> text.[i] >= lo && text.[i] <= hi) ranges <> negated && k (i + 1)
  | Start -> i = 0 && k i
  | End -> i = n && k i
  | Group alternatives -> List.exists (fun sequence -> match_sequence text sequence i k) alternatives
  | Repeat (inner, low, high) ->
    (* Greedy: one more repetition that consumes something is tried first,
       then stopping. A repetition that consumes nothing is never counted in
       a loop, so empty matches cannot spin; when repetitions are still owed
       and the inner pattern can match the empty text here, the ones owed are
       all taken to be empty. *)
    let rec more count i =
      let can_repeat = match high with Some h -> count < h | None -> true in
      (can_repeat && match_node text inner i (fun j -> j > i && more (count + 1) j))
      || (count >= low && k i)
      || (count < low && match_node text inner i (fun j -> j = i && k j))
    in
    more 0 i

and match_sequence text sequence i k =
  match sequence with
  | [] -> k i
  | node :: rest -> match_node text node i (fun j -> match_sequence text rest j k)

let compiled : (string, (node list list, string) result) Hashtbl.t = Hashtbl.create 32

let compile pattern =
  match Hashtbl.find_opt compiled pattern with
  | Some result -> result
  | None ->
    let result = try Ok (parse_pattern pattern) with Bad_pattern reason -> Error (Printf.sprintf "the pattern %S cannot be read: %s" pattern reason) in
    Hashtbl.replace compiled pattern result;
    result

let pattern_matches pattern text =
  match compile pattern with
  | Error e -> Error e
  | Ok alternatives ->
    let n = String.length text in
    let rec from i = i <= n && (match_node text (Group alternatives) i (fun _ -> true) || from (i + 1)) in
    Ok (from 0)

(* ---- schemas ---- *)

type error = { at : string; message : string }

type source = { root : Json.t; dir : string; files : (string, Json.t) Hashtbl.t }

type t = source

let error_to_string e = Printf.sprintf "%s: %s" e.at e.message

let of_json ?(dir = ".") root = { root; dir; files = Hashtbl.create 8 }

let load path =
  match Fs.read path with
  | Error e -> Error e
  | Ok text ->
    (match Json.parse text with
     | Error e -> Error (Printf.sprintf "%s: %s" path e)
     | Ok root -> Ok (of_json ~dir:(Filename.dirname path) root))

let annotations = [ "$schema"; "$id"; "$defs"; "$comment"; "title"; "description"; "examples"; "default" ]

let keywords =
  [ "type"; "enum"; "const"; "allOf"; "anyOf"; "oneOf"; "not"; "if"; "then"; "else"; "$ref"; "properties"; "required";
    "additionalProperties"; "minProperties"; "items"; "prefixItems"; "minItems"; "maxItems"; "uniqueItems"; "minLength";
    "maxLength"; "pattern"; "minimum"; "maximum"; "exclusiveMinimum"; "exclusiveMaximum" ]

let type_names = [ "null"; "boolean"; "object"; "array"; "number"; "integer"; "string" ]

let type_of (j : Json.t) =
  match j with
  | Json.Null -> "null"
  | Json.Bool _ -> "boolean"
  | Json.Int _ -> "integer"
  | Json.Float f -> if Float.is_integer f then "integer" else "number"
  | Json.String _ -> "string"
  | Json.List _ -> "array"
  | Json.Obj _ -> "object"

let has_type name j = let actual = type_of j in actual = name || (name = "number" && actual = "integer")

let number_of (j : Json.t) = match j with Json.Int i -> Some (float_of_int i) | Json.Float f -> Some f | _ -> None

(* Follows a JSON pointer such as /$defs/goal. *)
let rec follow (j : Json.t) (tokens : string list) =
  match tokens, j with
  | [], _ -> Some j
  | token :: rest, Json.Obj _ -> Option.bind (Json.member token j) (fun child -> follow child rest)
  | token :: rest, Json.List items -> Option.bind (int_of_string_opt token) (fun i -> Option.bind (List.nth_opt items i) (fun child -> follow child rest))
  | _ -> None

let unescape token = Text.replace_all ~sub:"~0" ~by:"~" (Text.replace_all ~sub:"~1" ~by:"/" token)

(* Resolves a reference to the schema it names and the source that schema lives in. *)
let resolve (source : source) (reference : string) : (Json.t * source, string) result =
  let file, pointer =
    match String.index_opt reference '#' with
    | Some i -> (String.sub reference 0 i, String.sub reference (i + 1) (String.length reference - i - 1))
    | None -> (reference, "")
  in
  let target =
    if file = "" then Ok source
    else begin
      let path = Filename.concat source.dir file in
      match Hashtbl.find_opt source.files path with
      | Some root -> Ok { source with root; dir = Filename.dirname path }
      | None ->
        (match Fs.read path with
         | Error _ -> Error (Printf.sprintf "the reference %S names a file that cannot be read (%s)" reference path)
         | Ok text ->
           (match Json.parse text with
            | Error e -> Error (Printf.sprintf "the reference %S names a file that is not JSON: %s" reference e)
            | Ok root ->
              Hashtbl.replace source.files path root;
              Ok { source with root; dir = Filename.dirname path }))
    end
  in
  match target with
  | Error e -> Error e
  | Ok target ->
    if pointer = "" then Ok (target.root, target)
    else if pointer.[0] <> '/' then Error (Printf.sprintf "the reference %S does not use a JSON pointer" reference)
    else begin
      let tokens = List.map unescape (List.tl (String.split_on_char '/' pointer)) in
      match follow target.root tokens with
      | Some schema -> Ok (schema, target)
      | None -> Error (Printf.sprintf "the reference %S points at nothing" reference)
    end

let max_depth = 200

let rec check (source : source) (schema : Json.t) (instance : Json.t) (at : string) (depth : int) : error list =
  let fail message = [ { at; message } ] in
  if depth > max_depth then fail "the schema refers to itself without end"
  else
    match schema with
    | Json.Bool true -> []
    | Json.Bool false -> fail "no value is allowed here"
    | Json.Obj members ->
      let sub schema instance at = check source schema instance at (depth + 1) in
      let keyword (name, value) =
        match name, value, instance with
        | "$ref", Json.String reference, _ ->
          (match resolve source reference with
           | Ok (target, target_source) -> check target_source target instance at (depth + 1)
           | Error e -> fail e)
        | "type", Json.String name, _ -> if has_type name instance then [] else fail (Printf.sprintf "expected %s, found %s" name (type_of instance))
        | "type", Json.List names, _ ->
          let names = Json.strings (Json.List names) in
          if List.exists (fun name -> has_type name instance) names then []
          else fail (Printf.sprintf "expected %s, found %s" (String.concat " or " names) (type_of instance))
        | "enum", Json.List allowed, _ ->
          if List.exists (Json.equal instance) allowed then []
          else fail (Printf.sprintf "%s is not one of %s" (Json.to_string instance) (String.concat ", " (List.map Json.to_string allowed)))
        | "const", expected, _ ->
          if Json.equal instance expected then [] else fail (Printf.sprintf "expected %s, found %s" (Json.to_string expected) (Text.truncate 60 (Json.to_string instance)))
        | "allOf", Json.List schemas, _ -> List.concat_map (fun s -> sub s instance at) schemas
        | "anyOf", Json.List schemas, _ ->
          let outcomes = List.map (fun s -> sub s instance at) schemas in
          if List.exists (fun errors -> errors = []) outcomes then []
          else begin
            let closest = List.fold_left (fun best errors -> if List.length errors < List.length best then errors else best) (List.hd outcomes) outcomes in
            { at; message = Printf.sprintf "matches none of the %d alternatives; the closest fails as follows" (List.length schemas) } :: closest
          end
        | "oneOf", Json.List schemas, _ ->
          let matching = List.length (List.filter (fun s -> sub s instance at = []) schemas) in
          if matching = 1 then [] else fail (Printf.sprintf "matches %d of the %d alternatives, and must match exactly one" matching (List.length schemas))
        | "not", schema, _ -> if sub schema instance at = [] then fail "matches a schema it must not match" else []
        | "if", condition, _ ->
          let branch = if sub condition instance at = [] then "then" else "else" in
          (match List.assoc_opt branch members with Some s -> sub s instance at | None -> [])
        | ("then" | "else"), _, _ -> []
        | "properties", Json.Obj properties, Json.Obj _ ->
          List.concat_map
            (fun (name, s) -> match Json.member name instance with Some value -> sub s value (at ^ "." ^ name) | None -> [])
            properties
        | "required", Json.List names, Json.Obj _ ->
          List.filter_map
            (fun name -> if Json.member name instance = None then Some { at; message = Printf.sprintf "the member %S is missing" name } else None)
            (Json.strings (Json.List names))
        | "additionalProperties", extra, Json.Obj present ->
          let declared = match List.assoc_opt "properties" members with Some (Json.Obj properties) -> List.map fst properties | _ -> [] in
          List.concat_map
            (fun (name, value) ->
              if List.mem name declared then []
              else
                match extra with
                | Json.Bool false -> [ { at; message = Printf.sprintf "the member %S is not allowed" name } ]
                | s -> sub s value (at ^ "." ^ name))
            present
        | "minProperties", Json.Int least, Json.Obj present ->
          if List.length present >= least then [] else fail (Printf.sprintf "has %d members, fewer than %d" (List.length present) least)
        | "prefixItems", Json.List schemas, Json.List items ->
          List.concat (List.mapi (fun i s -> match List.nth_opt items i with Some item -> sub s item (Printf.sprintf "%s[%d]" at i) | None -> []) schemas)
        | "items", s, Json.List items ->
          let skip = match List.assoc_opt "prefixItems" members with Some (Json.List prefix) -> List.length prefix | _ -> 0 in
          List.concat (List.mapi (fun i item -> if i < skip then [] else sub s item (Printf.sprintf "%s[%d]" at i)) items)
        | "minItems", Json.Int least, Json.List items ->
          if List.length items >= least then [] else fail (Printf.sprintf "has %d items, fewer than %d" (List.length items) least)
        | "maxItems", Json.Int most, Json.List items ->
          if List.length items <= most then [] else fail (Printf.sprintf "has %d items, more than %d" (List.length items) most)
        | "uniqueItems", Json.Bool true, Json.List items ->
          let rec repeated = function [] -> false | x :: rest -> List.exists (Json.equal x) rest || repeated rest in
          if repeated items then fail "has the same item twice" else []
        | "minLength", Json.Int least, Json.String s ->
          if Text.utf8_length s >= least then [] else fail (Printf.sprintf "is shorter than %d characters" least)
        | "maxLength", Json.Int most, Json.String s ->
          if Text.utf8_length s <= most then [] else fail (Printf.sprintf "is longer than %d characters" most)
        | "pattern", Json.String pattern, Json.String s ->
          (match pattern_matches pattern s with
           | Ok true -> []
           | Ok false -> fail (Printf.sprintf "%S does not match the pattern %s" (Text.truncate 60 s) pattern)
           | Error e -> fail e)
        | ("minimum" | "maximum" | "exclusiveMinimum" | "exclusiveMaximum"), bound, _ ->
          (match number_of bound, number_of instance with
           | Some b, Some v ->
             let holds, relation =
               match name with
               | "minimum" -> (v >= b, "at least")
               | "maximum" -> (v <= b, "at most")
               | "exclusiveMinimum" -> (v > b, "greater than")
               | _ -> (v < b, "less than")
             in
             if holds then [] else fail (Printf.sprintf "must be %s %s" relation (Json.to_string bound))
           | _ -> [])
        | _ -> []
      in
      List.concat_map keyword members
    | _ -> fail "the schema is neither an object nor a boolean"

let validate source instance = check source source.root instance "$" 0

let conforms source instance =
  match validate source instance with
  | [] -> Ok ()
  | errors -> Error (String.concat "\n" (List.map error_to_string errors))

(* ---- linting schemas ---- *)

let lint (source : source) : error list =
  let seen_files = Hashtbl.create 8 in
  let errors = ref [] in
  let report at message = errors := { at; message } :: !errors in
  let rec walk (source : source) (schema : Json.t) at =
    match schema with
    | Json.Bool _ -> ()
    | Json.Obj members ->
      let schema_at name s = walk source s (at ^ "." ^ name) in
      let each_schema name = function
        | Json.List schemas when schemas <> [] -> List.iteri (fun i s -> walk source s (Printf.sprintf "%s.%s[%d]" at name i)) schemas
        | _ -> report at (Printf.sprintf "%s needs a list of schemas that is not empty" name)
      in
      let count name = function Json.Int v when v >= 0 -> () | _ -> report at (Printf.sprintf "%s needs a whole number that is not negative" name) in
      List.iter
        (fun (name, value) ->
          match name, value with
          | "$defs", Json.Obj definitions -> List.iter (fun (key, s) -> walk source s (Printf.sprintf "%s.$defs.%s" at key)) definitions
          | "$defs", _ -> report at "$defs needs an object"
          | name, _ when List.mem name annotations -> ()
          | name, _ when not (List.mem name keywords) -> report at (Printf.sprintf "the keyword %S is not known" name)
          | "$ref", Json.String reference ->
            (match resolve source reference with
             | Error e -> report at e
             | Ok (target, target_source) ->
               (* A referenced file is linted once, from its root. *)
               if target_source.dir <> source.dir || target_source.root != source.root then begin
                 let key = Json.to_string target_source.root in
                 if not (Hashtbl.mem seen_files key) then begin
                   Hashtbl.replace seen_files key ();
                   walk target_source target_source.root (Printf.sprintf "%s -> %s" at reference)
                 end
               end;
               ignore target)
          | "$ref", _ -> report at "$ref needs text"
          | "type", Json.String name -> if not (List.mem name type_names) then report at (Printf.sprintf "%S is not a type" name)
          | "type", Json.List names ->
            List.iter (fun n -> match n with Json.String name when List.mem name type_names -> () | other -> report at (Printf.sprintf "%s is not a type" (Json.to_string other))) names
          | "type", _ -> report at "type needs a type name or a list of them"
          | "enum", Json.List (_ :: _) -> ()
          | "enum", _ -> report at "enum needs a list that is not empty"
          | "const", _ -> ()
          | ("allOf" | "anyOf" | "oneOf" | "prefixItems"), schemas -> each_schema name schemas
          | ("not" | "if" | "then" | "else" | "items" | "additionalProperties"), s -> schema_at name s
          | "properties", Json.Obj properties -> List.iter (fun (key, s) -> walk source s (Printf.sprintf "%s.properties.%s" at key)) properties
          | "properties", _ -> report at "properties needs an object"
          | "required", Json.List names ->
            if not (List.for_all (function Json.String _ -> true | _ -> false) names) then report at "required needs a list of member names";
            (match List.assoc_opt "properties" members, List.assoc_opt "additionalProperties" members with
             | Some (Json.Obj properties), Some (Json.Bool false) ->
               List.iter
                 (fun n -> if not (List.mem_assoc n properties) then report at (Printf.sprintf "the member %S is required and not allowed" n))
                 (Json.strings (Json.List names))
             | _ -> ())
          | "required", _ -> report at "required needs a list of member names"
          | ("minProperties" | "minItems" | "maxItems" | "minLength" | "maxLength"), v -> count name v
          | "uniqueItems", Json.Bool _ -> ()
          | "uniqueItems", _ -> report at "uniqueItems needs true or false"
          | "pattern", Json.String pattern -> (match compile pattern with Ok _ -> () | Error e -> report at e)
          | "pattern", _ -> report at "pattern needs text"
          | ("minimum" | "maximum" | "exclusiveMinimum" | "exclusiveMaximum"), v -> if number_of v = None then report at (Printf.sprintf "%s needs a number" name)
          | _ -> ())
        members
    | _ -> report at "a schema is an object or a boolean"
  in
  walk source source.root "$";
  List.rev !errors
