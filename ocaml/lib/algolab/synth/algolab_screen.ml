(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

(** What a drafted implementation may contain. A draft is a definition that
    a model wrote, and Lean will run it on the examples: so it is one [def]
    and nothing that could reach outside a computation. *)

open Maithic_base
open Maithic_bridge

(* Words the screen for proofs forbids and a definition needs. *)
let allowed = [ "def" ]

(** Why a draft is not given to Lean; empty for a draft that may be. The
    rules are those for a candidate proof, with [def] allowed: no [partial],
    no [unsafe], no [IO], no [sorry], no command that evaluates or declares
    anything else. And the draft has to define the function that was asked
    for, and only that one at the top. *)
let violations ~name draft =
  let found =
    List.filter_map
      (fun (f : Screen.finding) -> if List.mem f.Screen.construct allowed then None else Some f.Screen.reason)
      (Screen.violations draft)
  in
  let lines = String.split_on_char '\n' draft in
  let top_level = List.filter (fun line -> Text.starts_with ~prefix:"def " line) lines in
  let defines = List.exists (fun line -> Text.starts_with ~prefix:("def " ^ name ^ " ") line || line = "def " ^ name || Text.starts_with ~prefix:("def " ^ name ^ ":") line) top_level in
  List.sort_uniq compare found
  @ (if defines then [] else [ Printf.sprintf "the draft does not define %s" name ])
  @ if List.length top_level > 1 then [ "the draft has more than one definition at the top; helpers go in a where clause" ] else []
