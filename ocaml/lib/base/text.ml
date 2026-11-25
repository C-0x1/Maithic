(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let ends_with ~suffix s =
  let n = String.length suffix and l = String.length s in
  l >= n && String.sub s (l - n) n = suffix

let find_sub ?(from = 0) hay needle =
  let n = String.length needle and h = String.length hay in
  if n = 0 then Some from
  else begin
    let rec at i =
      if i + n > h then None
      else if String.unsafe_get hay i = String.unsafe_get needle 0 && String.sub hay i n = needle then Some i
      else at (i + 1)
    in
    at (max 0 from)
  end

let contains hay needle = find_sub hay needle <> None

let split_on sep s =
  if sep = "" then invalid_arg "Text.split_on: empty separator";
  let n = String.length sep in
  let rec go from acc =
    match find_sub ~from s sep with
    | Some i -> go (i + n) (String.sub s from (i - from) :: acc)
    | None -> List.rev (String.sub s from (String.length s - from) :: acc)
  in
  go 0 []

let replace_all ~sub ~by s = if sub = "" then s else String.concat by (split_on sub s)

let lines s =
  match List.rev (String.split_on_char '\n' s) with
  | "" :: rest -> List.rev rest
  | all -> List.rev all

let trim = String.trim
let is_blank s = String.trim s = ""
let lowercase = String.lowercase_ascii
let concat_map sep f items = String.concat sep (List.map f items)

let utf8_length s =
  let count = ref 0 in
  String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr count) s;
  !count

let pad_right width s =
  let len = utf8_length s in
  if len >= width then s else s ^ String.make (width - len) ' '

(* Byte offset of the code point with index [n], or the length of the string. *)
let offset_of_codepoint s n =
  let count = ref 0 and result = ref (String.length s) in
  (try
     String.iteri
       (fun i c ->
         if Char.code c land 0xC0 <> 0x80 then begin
           if !count = n then begin result := i; raise Exit end;
           incr count
         end)
       s
   with Exit -> ());
  !result

let truncate n s =
  if utf8_length s <= n then s
  else if n <= 1 then String.sub s 0 (offset_of_codepoint s n)
  else String.sub s 0 (offset_of_codepoint s (n - 1)) ^ "\xE2\x80\xA6"

(** The first words of [s] that fit in [n] characters, with a mark when words were left out. A
    text of one long word is cut as [truncate] cuts it. *)
let truncate_words n s =
  if utf8_length s <= n then s
  else begin
    let cut = truncate n s in
    (* [truncate] left room for the mark: the last space before it ends the last whole word *)
    match String.rindex_opt cut ' ' with
    | Some i when i > 0 -> String.sub cut 0 i ^ " \xE2\x80\xA6"
    | _ -> cut
  end

let indent n s =
  let pad = String.make n ' ' in
  String.concat "\n" (List.map (fun l -> if l = "" then l else pad ^ l) (String.split_on_char '\n' s))

let is_valid_utf8 s =
  let n = String.length s in
  let cont i = i < n && Char.code s.[i] land 0xC0 = 0x80 in
  let rec go i =
    if i >= n then true
    else
      let c = Char.code s.[i] in
      if c < 0x80 then go (i + 1)
      else if c >= 0xC2 && c <= 0xDF then cont (i + 1) && go (i + 2)
      else if c >= 0xE0 && c <= 0xEF then
        cont (i + 1) && cont (i + 2)
        && (c <> 0xE0 || Char.code s.[i + 1] >= 0xA0)
        && (c <> 0xED || Char.code s.[i + 1] < 0xA0)
        && go (i + 3)
      else if c >= 0xF0 && c <= 0xF4 then
        cont (i + 1) && cont (i + 2) && cont (i + 3)
        && (c <> 0xF0 || Char.code s.[i + 1] >= 0x90)
        && (c <> 0xF4 || Char.code s.[i + 1] < 0x90)
        && go (i + 4)
      else false
  in
  go 0

let hex_of_bytes s =
  let buf = Buffer.create (2 * String.length s) in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) s;
  Buffer.contents buf

let iso8601 seconds =
  let t = Unix.gmtime seconds in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.Unix.tm_year + 1900) (t.Unix.tm_mon + 1) t.Unix.tm_mday t.Unix.tm_hour
    t.Unix.tm_min t.Unix.tm_sec
