(* Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE. *)

type task = Examine | Research | Prove | Verify | Critique

let tasks = [ Examine; Research; Prove; Verify; Critique ]
let task_name = function Examine -> "examine" | Research -> "research" | Prove -> "prove" | Verify -> "verify" | Critique -> "critique"

let describe = function
  | Examine -> "Check that the statement is well formed, look for a counterexample, and let automation try."
  | Research -> "Find the definitions and lemmas of the library that bear on the statement."
  | Prove -> "Propose proofs of the statement."
  | Verify -> "Give each proposed proof to Lean, and say what Lean decided."
  | Critique -> "Say why the proofs Lean rejected fail, and what to try instead."

let role_for = function Examine | Verify -> "verifier" | Research -> "researcher" | Prove -> "prover" | Critique -> "critic"

let subtasks ~round ~last =
  if round <= 0 then [ Examine ]
  else (if round = 1 then [ Research ] else []) @ [ Prove; Verify ] @ if last then [] else [ Critique ]

let assign ~round ~last = List.map (fun task -> (task, role_for task)) (subtasks ~round ~last)
