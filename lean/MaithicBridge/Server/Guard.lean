/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Candidate guard

Candidate proofs come from untrusted proposers. Lean can run arbitrary programs while it elaborates
a file, so before a candidate reaches the elaborator the Bridge rejects any text that tries to leave
the world of proofs: evaluation commands, new axioms, compiler hooks, option changes, meta-programs
and process or file access. The guard is lexical and deliberately conservative. It is the first of
several layers; the axiom audit after elaboration is the second.
-/

namespace Maithic.Bridge.Guard

/-- Characters that may appear inside a Lean identifier or keyword for the purpose of the guard. -/
def isWordChar (c : Char) : Bool :=
  c.isAlphanum || c == '_' || c == '\'' || c == '.' || c == '#' || c == '!' || c == '?'

/-- Splits text into maximal runs of word characters. -/
def words (s : String) : List String :=
  let step (acc : List String × List Char) (c : Char) : List String × List Char :=
    if isWordChar c then (acc.1, c :: acc.2)
    else if acc.2.isEmpty then acc
    else (String.ofList acc.2.reverse :: acc.1, [])
  let (ws, cur) := s.toList.foldl step ([], [])
  let ws := if cur.isEmpty then ws else String.ofList cur.reverse :: ws
  ws.reverse

/-- Words that a candidate proof may not contain. -/
def deniedWords : List String := [
  "axiom", "unsafe", "partial", "extern", "implemented_by", "initialize", "builtin_initialize",
  "run_cmd", "run_elab", "run_meta", "elab", "elab_rules", "macro", "macro_rules", "syntax",
  "notation", "infix", "infixl", "infixr", "prefix", "postfix", "attribute", "instance",
  "set_option", "import", "include", "open", "export", "namespace", "section", "end", "universe",
  "variable", "mutual", "inductive", "structure", "class", "abbrev", "opaque", "def", "theorem",
  "lemma", "example", "deriving", "native_decide", "sorry", "admit", "stop",
  "run_tac", "by_elab", "eval_expr", "csimp", "reduce_bool", "local", "scoped", "private",
  "protected", "noncomputable", "register_option", "register_simp_attr", "declare_syntax_cat",
  "#eval", "#eval!", "#exit", "#print", "#check", "#reduce", "#synth", "#guard", "#guard_msgs",
  "#help", "#html", "#lint", "#find", "#widget", "#time"]

/-- Prefixes of dotted names that a candidate proof may not mention. -/
def deniedPrefixes : List String := [
  "IO.", "System.", "Lean.Elab.", "Lean.Meta.", "Lean.Compiler.", "Lean.IR.", "Lean.Environment.",
  "Lean.ofReduceBool", "Lean.ofReduceNat", "Lean.reduceBool", "Lean.reduceNat", "Lean.trustCompiler",
  "IO", "unsafeCast", "unsafeIO", "unsafeBaseIO", "lcProof", "lcUnreachable", "sorryAx", "ptrAddrUnsafe"]

/-- `true` when `p` is a prefix of `s`. -/
def startsWith (s p : String) : Bool :=
  let rec go : List Char → List Char → Bool
    | _, [] => true
    | [], _ :: _ => false
    | a :: as, b :: bs => a == b && go as bs
  go s.toList p.toList

/-- Removes a leading `_root_.` qualifier, repeatedly. -/
def stripRoot (w : String) : String :=
  let rec go (fuel : Nat) (cs : List Char) : List Char :=
    match fuel with
    | 0 => cs
    | fuel + 1 =>
      if startsWith (String.ofList cs) "_root_." then go fuel (cs.drop 7) else cs
  String.ofList (go w.length w.toList)

/-- The reason a single word is rejected, if it is. -/
def wordViolation (word : String) : Option String :=
  let w := stripRoot word
  if deniedWords.contains w then some s!"forbidden keyword '{w}'"
  else
    let hit (p : String) : Bool :=
      if p.toList.getLast? == some '.' then startsWith w p else w == p
    match deniedPrefixes.find? hit with
    | some p => some s!"forbidden name '{w}' (matches '{p}')"
    | none => none

/-- Characters that a candidate proof may not contain: they allow names to be spelled indirectly. -/
def deniedChars : List Char := ['«', '»']

/-- Upper bound on the size of a candidate, in characters. -/
def maxCandidateChars : Nat := 20000

/--
Checks a candidate proof. Returns `none` when the candidate may be elaborated and the reason for
the rejection otherwise.
-/
def violation (candidate : String) : Option String :=
  if candidate.length > maxCandidateChars then
    some s!"candidate longer than {maxCandidateChars} characters"
  else if candidate.toList.any deniedChars.contains then
    some "forbidden character (quoted identifier)"
  else
    (words candidate).findSome? wordViolation

/-- `true` when the candidate passes the guard. -/
def admits (candidate : String) : Bool := (violation candidate).isNone

end Maithic.Bridge.Guard
