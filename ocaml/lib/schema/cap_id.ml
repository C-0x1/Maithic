(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** Capability identifiers, such as [P07-03-02]: pillar 7, group 3,
    capability 2. Every capability of the product has one; tests name the
    capability they cover and the register in [registry/] tracks its status. *)

type t = { pillar : int; group : int; index : int }

let pillars = 19
let two_digits s = String.length s = 2 && String.for_all (function '0' .. '9' -> true | _ -> false) s

let of_string s =
  if String.length s = 9 && s.[0] = 'P' && s.[3] = '-' && s.[6] = '-' then begin
    let p = String.sub s 1 2 and g = String.sub s 4 2 and i = String.sub s 7 2 in
    if two_digits p && two_digits g && two_digits i then begin
      let id = { pillar = int_of_string p; group = int_of_string g; index = int_of_string i } in
      if id.pillar < pillars && id.group >= 1 && id.index >= 1 then Some id else None
    end
    else None
  end
  else None

let to_string id = Printf.sprintf "P%02d-%02d-%02d" id.pillar id.group id.index
let group_id id = Printf.sprintf "P%02d-%02d" id.pillar id.group
let pillar_id id = Printf.sprintf "P%02d" id.pillar

let compare a b = compare (a.pillar, a.group, a.index) (b.pillar, b.group, b.index)

(** Every identifier that occurs in [text], in order of appearance. *)
let scan text =
  let n = String.length text in
  let rec go i acc =
    if i + 9 > n then List.rev acc
    else if text.[i] = 'P' then
      match of_string (String.sub text i 9) with
      | Some id ->
        let boundary_before = i = 0 || not (match text.[i - 1] with 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true | _ -> false) in
        let boundary_after = i + 9 >= n || not (match text.[i + 9] with '0' .. '9' -> true | _ -> false) in
        if boundary_before && boundary_after then go (i + 9) (id :: acc) else go (i + 1) acc
      | None -> go (i + 1) acc
    else go (i + 1) acc
  in
  go 0 []
