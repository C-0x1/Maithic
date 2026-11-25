(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

open Maithic_base

type t = Unknown_constant of string | Unknown_module of string | Failed_instance | Type_mismatch | Ambiguous | Syntax | Other

let name = function
  | Unknown_constant _ -> "unknown-constant"
  | Unknown_module _ -> "unknown-module"
  | Failed_instance -> "failed-instance"
  | Type_mismatch -> "type-mismatch"
  | Ambiguous -> "ambiguous"
  | Syntax -> "syntax"
  | Other -> "other"

let names = [ "unknown-constant"; "unknown-module"; "failed-instance"; "type-mismatch"; "ambiguous"; "syntax"; "other" ]

(* The first text between backticks or single quotes. *)
let quoted text =
  let n = String.length text in
  let rec go i =
    if i >= n then ""
    else if text.[i] = '`' || text.[i] = '\'' then
      match String.index_from_opt text (i + 1) text.[i] with Some j when j > i + 1 -> String.sub text (i + 1) (j - i - 1) | _ -> go (i + 1)
    else go (i + 1)
  in
  go 0

let classify said =
  (* Lean may report several errors, each set off from the next by an empty line. The first is the
     one a repair has to deal with first: what follows it is often its consequence. *)
  let first =
    let text = String.trim said in
    match Text.find_sub text "\n\n" with Some stop -> String.sub text 0 stop | None -> text
  in
  let said = first in
  let lower = Text.lowercase said in
  let has needle = Text.contains lower needle in
  if has "unknown module prefix" || has "unknown package" || (has "unknown module" && not (has "unknown identifier")) then Unknown_module (quoted said)
  else if has "unknown identifier" || has "unknown constant" || has "unknown namespace" || has "is unknown" then Unknown_constant (quoted said)
  else if has "failed to synthesize" then Failed_instance
  else if has "ambiguous, possible interpretations" || has "ambiguous term" || has "overloaded, errors" || has "ambiguous" then Ambiguous
  else if has "type mismatch" || (has "has type" && has "but is expected to have type") || has "application type mismatch" then Type_mismatch
  else if has "unexpected token" || has "expected token" || has "expected term" || has "expected ':'" || has "unexpected identifier" || has "unexpected end of input" then Syntax
  else Other

let hint = function
  | Unknown_constant _ -> "replace the name by what it stands for, written out with what the library provides"
  | Unknown_module _ -> "do without the import: say what is needed with what the library provides"
  | Failed_instance -> "use the operation on a type that has it, or state the property another way"
  | Type_mismatch -> "make the two sides the same kind of number, with a cast written where one is meant"
  | Ambiguous -> "write the type of a number or a variable beside it"
  | Syntax -> "write the statement as binders, a colon and a claim, in Lean's syntax"
  | Other -> "read what Lean reports and change what it points at"
