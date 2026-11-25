(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_bridge

type replacement = { start : int; stop : int; text : string }
type t = { rule : string; description : string; replacements : replacement list; untrusted : string }

(* The bytes of the code point that begins at a byte. *)
let width c = if Char.code c < 0x80 then 1 else if Char.code c < 0xE0 then 2 else if Char.code c < 0xF0 then 3 else 4

let offset source ~line ~column =
  if line < 1 || column < 0 then None
  else begin
    let n = String.length source in
    (* The start of the line. *)
    let rec to_line i remaining = if remaining = 0 then Some i else match String.index_from_opt source i '\n' with Some j -> to_line (j + 1) (remaining - 1) | None -> None in
    match (if n = 0 && line = 1 then Some 0 else to_line 0 (line - 1)) with
    | None -> None
    | Some start ->
      let rec advance i remaining =
        if remaining = 0 then Some i
        else if i >= n || source.[i] = '\n' then None
        else advance (min n (i + width source.[i])) (remaining - 1)
      in
      advance start column
  end

let span source (d : Bridge_types.diagnostic) =
  match d.Bridge_types.end_line, d.Bridge_types.end_column with
  | Some end_line, Some end_column ->
    (match offset source ~line:d.Bridge_types.line ~column:d.Bridge_types.column, offset source ~line:end_line ~column:end_column with
     | Some start, Some stop when stop >= start -> Some (start, stop)
     | _ -> None)
  | _ -> None

let apply source replacements =
  let n = String.length source in
  let ordered = List.sort (fun a b -> compare (a.start, a.stop) (b.start, b.stop)) replacements in
  let rec check last = function
    | [] -> Ok ()
    | r :: rest ->
      if r.start < 0 || r.stop > n || r.stop < r.start then Error "a replacement lies outside the text"
      else if r.start < last then Error "two replacements overlap"
      else check r.stop rest
  in
  Result.map
    (fun () ->
      let buffer = Buffer.create (n + 64) in
      let last =
        List.fold_left
          (fun at r ->
            Buffer.add_string buffer (String.sub source at (r.start - at));
            Buffer.add_string buffer r.text;
            r.stop)
          0 ordered
      in
      Buffer.add_string buffer (String.sub source last (n - last));
      Buffer.contents buffer)
    (check 0 ordered)

(* A byte that can be part of a name: a name that is found must not go on to either side. *)
let name_byte c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '.' || c = '\'' || c = '!' || c = '?' || Char.code c >= 0x80

let find_word source name ~from ~upto =
  let len = String.length name in
  let upto = min upto (String.length source) in
  let rec go i =
    if len = 0 || i + len > upto then None
    else if String.sub source i len = name
            && (i = 0 || not (name_byte source.[i - 1]))
            && (i + len >= String.length source || not (name_byte source.[i + len]) || source.[i + len] = '.' && (i + len + 1 >= String.length source || not (name_byte source.[i + len + 1])))
    then Some i
    else go (i + 1)
  in
  go (max 0 from)

let declaration (decl : Maithic_prover.Lean_source.decl) proof = Maithic_prover.Lean_source.assemble_by decl proof
let put_proof (decl : Maithic_prover.Lean_source.decl) proof = Maithic_prover.Lean_source.with_proof_by decl proof

let changed_lines before after =
  let lines text = List.map String.trim (String.split_on_char '\n' text) in
  let a = lines before and b = lines after in
  (* Lines are counted with their multiplicity: a line that occurs twice and is kept once is one change. *)
  let missing xs ys =
    let pool = ref ys in
    List.fold_left
      (fun count x ->
        let rec take = function [] -> None | y :: rest -> if y = x then Some rest else Option.map (fun r -> y :: r) (take rest) in
        match take !pool with
        | Some rest ->
          pool := rest;
          count
        | None -> count + 1)
      0 xs
  in
  missing a b + missing b a
