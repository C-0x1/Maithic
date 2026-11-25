(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type context = { source : string; renames : (string * string) list }
type t = { name : string; priority : int; summary : string; propose : context -> Mend_breakage.t -> Mend_edit.t list }
type registry = t list

let order rules = List.stable_sort (fun a b -> compare (a.priority, a.name) (b.priority, b.name)) rules
let registry rules = order (List.fold_left (fun kept rule -> List.filter (fun k -> k.name <> rule.name) kept @ [ rule ]) [] rules)
let rules registry = registry
let names registry = List.map (fun rule -> rule.name) registry
let find registry name = List.find_opt (fun rule -> rule.name = name) registry
let add held rule = registry (held @ [ rule ])
let without registry name = List.filter (fun rule -> rule.name <> name) registry
let proposals registry context breakage = List.concat_map (fun rule -> rule.propose context breakage) registry
