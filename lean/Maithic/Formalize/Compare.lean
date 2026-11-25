/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Refute.Vacuity.Analysis

/-!
# Two statements side by side

When several formal statements are offered for one informal sentence, the first question is which
of them differ. Two candidates that say the same thing in other words are one reading; a candidate
that follows from another is the weaker claim of the two. `compare` answers what it can prove:

* `identical`: the same statement, up to the names of bound variables;
* `definitional`: the same by unfolding definitions;
* `equivalent`: each follows from the other, with a proof Lean's kernel accepted;
* `stronger`: the first implies the second, and the converse was not proved;
* `weaker`: the second implies the first, and the converse was not proved;
* `unrelated`: nothing was proved either way.

One statement follows from another here only if the proof *uses* the other. Any two true
statements imply each other, and automation that proves a statement outright would call every
pair of easy theorems equivalent, which says nothing about whether they say the same. So a proof
of `q` from `p` that never refers to `p` does not count.

Every answer but the last comes with a proof, so it is certain. `unrelated` is the absence of a
proof within a budget. It does not say that the statements differ: two statements that are
equivalent by a deep theorem are `unrelated` here.
-/

namespace Maithic.Formalize

open Lean Meta Elab Maithic.Refute

/-- How one statement stands to another. -/
inductive Relation where
  | identical
  | definitional
  | equivalent (by_ : String)
  | stronger (by_ : String)
  | weaker (by_ : String)
  | unrelated
  deriving Repr, BEq, Inhabited

/-- The relation in a word. -/
def Relation.word : Relation → String
  | .identical => "identical"
  | .definitional => "definitional"
  | .equivalent _ => "equivalent"
  | .stronger _ => "stronger"
  | .weaker _ => "weaker"
  | .unrelated => "unrelated"

/-- What proved the relation; empty where no automation was needed, or nothing was proved. -/
def Relation.provedBy : Relation → String
  | .equivalent by_ | .stronger by_ | .weaker by_ => by_
  | _ => ""

/-- Whether the two statements are one reading: whatever proves one proves the other. -/
def Relation.same : Relation → Bool
  | .identical | .definitional | .equivalent _ => true
  | _ => false

/-- The automation that tries to show that one statement follows from another. The first attempt
covers a statement that differs from the other by the order of its hypotheses. -/
def implicationAttempts : MetaM (List Attempt) := do
  return [
    ⟨"its own hypotheses", ← `(tactic| (intro h; intros; first | exact h | (apply h <;> assumption)))⟩,
    ⟨"simp_all", ← `(tactic| (intro h; simp_all))⟩,
    ⟨"omega", ← `(tactic| (intro h; intros; omega))⟩,
    ⟨"grind", ← `(tactic| grind)⟩]

/-- Whether the second statement follows from the first, and by what. A proof that does not use
the first statement shows only that the second is true, and does not count. -/
def implies (a b : Expr) : MetaM (Option String) := do
  let some (by_, proof) ← proveBy (← mkArrow a b) (← implicationAttempts) | return none
  match ← instantiateMVars proof with
  | .lam _ _ body _ => return if body.hasLooseBVar 0 then some by_ else none
  | _ => return some by_

/-- How the first statement stands to the second. Both are closed propositions. -/
def compare (a b : Expr) : MetaM Relation :=
  withLCtx {} {} do
    let a ← instantiateMVars a
    let b ← instantiateMVars b
    if a == b then return .identical
    if ← withNewMCtxDepth (isDefEq a b) then return .definitional
    let forth ← implies a b
    let back ← implies b a
    match forth, back with
    | some f, some g => return .equivalent (if f == g then f else s!"{f}, and {g} for the converse")
    | some f, none => return .stronger f
    | none, some g => return .weaker g
    | none, none => return .unrelated

/-- The relation as JSON. -/
def Relation.toJson (r : Relation) : Json :=
  Json.mkObj ([("relation", (r.word : Json)), ("same", (r.same : Json))] ++
    (if r.provedBy.isEmpty then [] else [("by", (r.provedBy : Json))]))

/-- The relation in a sentence. -/
def Relation.toString : Relation → String
  | .identical => "identical: the same statement"
  | .definitional => "definitional: the same statement once definitions are unfolded"
  | .equivalent by_ => s!"equivalent: each follows from the other (proved by {by_})"
  | .stronger by_ => s!"stronger: the first implies the second (proved by {by_}); the converse was not proved"
  | .weaker by_ => s!"weaker: the second implies the first (proved by {by_}); the converse was not proved"
  | .unrelated => "unrelated: nothing was proved either way, which does not show that the statements differ"

end Maithic.Formalize
