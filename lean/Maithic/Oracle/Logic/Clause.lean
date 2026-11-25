/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Formula

/-!
# Clauses and the conversion to clause normal form

A clause is a disjunction of literals and a clause set is a conjunction of clauses. `Formula.cnf`
turns a formula, or its negation, into a clause set with the same truth value under every
assignment (`Formula.eval_cnf`). The conversion distributes disjunction over conjunction, so it
can grow exponentially on equivalences nested deeply; that is the price of an equivalent clause
set with no auxiliary atoms. `Cnf.simplify` then removes repeated literals and clauses that
contain a literal together with its negation, again without changing the truth value.

To prove a formula one refutes `Formula.refutation`, the clause set of its negation
(`Formula.valid_of_refutation_unsatisfiable`). Clause sets are printed in the DIMACS format, in
which atom `i` is the variable `i + 1`.
-/

namespace Maithic.Oracle.Logic

/-- A literal: an atom when `positive` is true, the negation of the atom otherwise. -/
structure Literal where
  /-- The number of the atom. -/
  atom : Nat
  /-- Whether the atom occurs without a negation. -/
  positive : Bool
  deriving DecidableEq, Repr, Inhabited

namespace Literal

/-- The truth value of a literal under an assignment. -/
def eval (σ : Assignment) (l : Literal) : Bool := σ l.atom == l.positive

/-- The literal of the same atom with the opposite sign. -/
def negate (l : Literal) : Literal := { l with positive := !l.positive }

@[simp] theorem eval_negate (σ : Assignment) (l : Literal) : l.negate.eval σ = !l.eval σ := by
  simp only [eval, negate]
  cases σ l.atom <;> cases l.positive <;> rfl

@[simp] theorem negate_negate (l : Literal) : l.negate.negate = l := by
  cases l; simp [negate]

/-- The literal as a DIMACS integer: the variable of atom `i` is `i + 1`. -/
def toDimacs (l : Literal) : Int :=
  if l.positive then Int.ofNat (l.atom + 1) else -Int.ofNat (l.atom + 1)

/-- The literal a nonzero DIMACS integer stands for. -/
def ofDimacs (n : Int) : Option Literal :=
  if n = 0 then none else some ⟨n.natAbs - 1, decide (0 < n)⟩

theorem ofDimacs_toDimacs (l : Literal) : ofDimacs l.toDimacs = some l := by
  cases l with
  | mk atom positive => cases positive <;> simp [toDimacs, ofDimacs] <;> omega

end Literal

/-- The words of a line: the longest runs of characters other than blanks. -/
def words (line : String) : List String :=
  let step := fun (state : List String × List Char) (ch : Char) =>
    if ch == ' ' || ch == '\t' || ch == '\r' then
      (if state.2.isEmpty then state.1 else String.ofList state.2.reverse :: state.1, [])
    else (state.1, ch :: state.2)
  let (done, last) := line.toList.foldl step ([], [])
  (if last.isEmpty then done else String.ofList last.reverse :: done).reverse

/-- A clause: the disjunction of its literals. The empty clause is false. -/
abbrev Clause := List Literal

/-- A clause set: the conjunction of its clauses. The empty set is true. -/
abbrev Cnf := List Clause

namespace Clause

/-- The truth value of a clause under an assignment. -/
def eval (σ : Assignment) (c : Clause) : Bool := c.any (·.eval σ)

@[simp] theorem eval_nil (σ : Assignment) : eval σ [] = false := rfl

@[simp] theorem eval_cons (σ : Assignment) (l : Literal) (c : Clause) :
    eval σ (l :: c) = (l.eval σ || eval σ c) := rfl

theorem eval_append (σ : Assignment) (c d : Clause) :
    eval σ (c ++ d) = (eval σ c || eval σ d) := by
  simp [eval, List.any_append]

theorem eval_eq_true_iff {σ : Assignment} {c : Clause} :
    eval σ c = true ↔ ∃ l, l ∈ c ∧ l.eval σ = true := by
  simp [eval, List.any_eq_true]

/-- Clauses with the same literals have the same truth value. -/
theorem eval_congr {σ : Assignment} {c d : Clause} (h : ∀ l, l ∈ c ↔ l ∈ d) :
    eval σ c = eval σ d := by
  apply Bool.eq_iff_iff.mpr
  simp only [eval_eq_true_iff]
  exact ⟨fun ⟨l, hl, hv⟩ => ⟨l, (h l).mp hl, hv⟩, fun ⟨l, hl, hv⟩ => ⟨l, (h l).mpr hl, hv⟩⟩

/-- Whether a clause contains a literal together with its negation, and so is always true. -/
def isTrivial (c : Clause) : Bool := c.any fun l => c.contains l.negate

theorem eval_of_isTrivial {c : Clause} (h : c.isTrivial = true) (σ : Assignment) :
    eval σ c = true := by
  simp only [isTrivial, List.any_eq_true, List.contains_iff_mem] at h
  obtain ⟨l, hl, hn⟩ := h
  cases hv : l.eval σ
  · exact eval_eq_true_iff.mpr ⟨l.negate, hn, by simp [hv]⟩
  · exact eval_eq_true_iff.mpr ⟨l, hl, hv⟩

/-- The clause without repeated literals; of equal literals the last one is kept. -/
def dedup : Clause → Clause
  | [] => []
  | l :: c => if c.contains l then dedup c else l :: dedup c

theorem mem_dedup {l : Literal} : ∀ {c : Clause}, l ∈ dedup c ↔ l ∈ c
  | [] => by simp [dedup]
  | m :: c => by
    simp only [dedup]
    split
    · rename_i h
      have hm : m ∈ c := List.contains_iff_mem.mp h
      rw [mem_dedup, List.mem_cons]
      exact ⟨Or.inr, fun h => h.elim (fun e => e ▸ hm) id⟩
    · simp only [List.mem_cons, mem_dedup]

theorem eval_dedup (σ : Assignment) (c : Clause) : eval σ (dedup c) = eval σ c :=
  eval_congr fun _ => mem_dedup

/-- The clause as a line of a DIMACS file, ended by `0`. -/
def toDimacs (c : Clause) : String :=
  String.intercalate " " (c.map (fun l => toString l.toDimacs) ++ ["0"])

end Clause

namespace Cnf

/-- The truth value of a clause set under an assignment. -/
def eval (σ : Assignment) (φ : Cnf) : Bool := φ.all (Clause.eval σ)

/-- A clause set is unsatisfiable when no assignment makes all of its clauses true. -/
def Unsatisfiable (φ : Cnf) : Prop := ∀ σ : Assignment, eval σ φ = false

@[simp] theorem eval_nil (σ : Assignment) : eval σ [] = true := rfl

@[simp] theorem eval_cons (σ : Assignment) (c : Clause) (φ : Cnf) :
    eval σ (c :: φ) = (Clause.eval σ c && eval σ φ) := rfl

theorem eval_append (σ : Assignment) (φ ψ : Cnf) :
    eval σ (φ ++ ψ) = (eval σ φ && eval σ ψ) := by
  simp [eval, List.all_append]

theorem eval_eq_true_iff {σ : Assignment} {φ : Cnf} :
    eval σ φ = true ↔ ∀ c, c ∈ φ → Clause.eval σ c = true := by
  simp [eval, List.all_eq_true]

/-- A clause set that contains the empty clause is false under every assignment. -/
theorem eval_of_mem_nil {φ : Cnf} (h : [] ∈ φ) (σ : Assignment) : eval σ φ = false := by
  cases hv : eval σ φ
  · rfl
  · have := eval_eq_true_iff.mp hv [] h
    simp at this

/-- The clause set of a disjunction of two clause sets: every clause of the first joined with
every clause of the second. -/
def distribute (φ ψ : Cnf) : Cnf := φ.flatMap fun c => ψ.map fun d => c ++ d

theorem eval_map_append (σ : Assignment) (c : Clause) (ψ : Cnf) :
    eval σ (ψ.map fun d => c ++ d) = (Clause.eval σ c || eval σ ψ) := by
  induction ψ with
  | nil => simp
  | cons d ψ ih =>
    simp only [List.map_cons, eval_cons, ih, Clause.eval_append]
    cases Clause.eval σ c <;> simp

theorem eval_distribute (σ : Assignment) (φ ψ : Cnf) :
    eval σ (distribute φ ψ) = (eval σ φ || eval σ ψ) := by
  induction φ with
  | nil => simp [distribute]
  | cons c φ ih =>
    have : distribute (c :: φ) ψ = (ψ.map fun d => c ++ d) ++ distribute φ ψ := by
      simp [distribute]
    rw [this, eval_append, eval_map_append, ih, eval_cons]
    cases Clause.eval σ c <;> cases eval σ φ <;> simp

/-- The clause set without trivial clauses and without repeated literals. -/
def simplify (φ : Cnf) : Cnf := (φ.filter fun c => !c.isTrivial).map Clause.dedup

theorem eval_simplify (σ : Assignment) (φ : Cnf) : eval σ (simplify φ) = eval σ φ := by
  induction φ with
  | nil => rfl
  | cons c φ ih =>
    have ih' : eval σ ((φ.filter fun c => !c.isTrivial).map Clause.dedup) = eval σ φ := ih
    cases h : c.isTrivial
    · simp [simplify, h, Clause.eval_dedup, ih']
    · simp [simplify, h, Clause.eval_of_isTrivial h σ, ih']

/-- One more than the largest atom of a clause set, and zero when it has none. -/
def atomBound (φ : Cnf) : Nat :=
  φ.foldl (fun n c => c.foldl (fun n l => max n (l.atom + 1)) n) 0

/-- The clause set as a DIMACS file. -/
def toDimacs (φ : Cnf) : String :=
  s!"p cnf {φ.atomBound} {φ.length}\n" ++ String.join (φ.map fun c => c.toDimacs ++ "\n")

/-- The clauses in a list of DIMACS integers: each run of literals up to a `0`. Literals after
the last `0` are an error. -/
def ofDimacsNumbers (numbers : List Int) : Except String Cnf :=
  let (clauses, open_) := numbers.foldl
    (fun (state : Array Clause × Array Literal) n =>
      match Literal.ofDimacs n with
      | none => (state.1.push state.2.toList, #[])
      | some l => (state.1, state.2.push l))
    (#[], #[])
  if open_.isEmpty then .ok clauses.toList else .error "the last clause does not end with 0"

/-- Reads a clause set from a DIMACS file: comment lines start with `c`, the problem line is
`p cnf <variables> <clauses>`, and every clause ends with `0`. When there is a problem line, the
number of clauses must agree with it. -/
def parseDimacs (text : String) : Except String Cnf := do
  let mut numbers : Array Int := #[]
  let mut declared : Option Nat := none
  let mut line := 1
  for content in text.splitOn "\n" do
    match words content with
    | [] => pure ()
    | "c" :: _ => pure ()
    | "p" :: rest =>
      match rest with
      | ["cnf", _, count] =>
        match count.toNat? with
        | some n => declared := some n
        | none => throw s!"line {line}: \"{count}\" is not a number of clauses"
      | _ => throw s!"line {line}: the problem line is `p cnf <variables> <clauses>`"
    | tokens =>
      for token in tokens do
        match token.toInt? with
        | some n => numbers := numbers.push n
        | none => throw s!"line {line}: \"{token}\" is not a number"
    line := line + 1
  let φ ← ofDimacsNumbers numbers.toList
  match declared with
  | some n =>
    if φ.length == n then return φ
    else throw s!"the problem line announces {n} clause(s) and the file has {φ.length}"
  | none => return φ

end Cnf

namespace Formula

/-- The clause set of a formula when `positive` is true, and of its negation otherwise. -/
def cnf : Formula → Bool → Cnf
  | atom i, positive => [[⟨i, positive⟩]]
  | tt, positive => if positive then [] else [[]]
  | ff, positive => if positive then [[]] else []
  | not f, positive => f.cnf !positive
  | and f g, true => f.cnf true ++ g.cnf true
  | and f g, false => Cnf.distribute (f.cnf false) (g.cnf false)
  | or f g, true => Cnf.distribute (f.cnf true) (g.cnf true)
  | or f g, false => f.cnf false ++ g.cnf false
  | imp f g, true => Cnf.distribute (f.cnf false) (g.cnf true)
  | imp f g, false => f.cnf true ++ g.cnf false
  | iff f g, true =>
    Cnf.distribute (f.cnf false) (g.cnf true) ++ Cnf.distribute (f.cnf true) (g.cnf false)
  | iff f g, false =>
    Cnf.distribute (f.cnf true) (g.cnf true) ++ Cnf.distribute (f.cnf false) (g.cnf false)

/-- The conversion keeps the truth value: the clause set of a formula is true exactly when the
formula is, and the clause set of its negation exactly when the formula is false. -/
theorem eval_cnf (σ : Assignment) (f : Formula) (positive : Bool) :
    Cnf.eval σ (f.cnf positive) = (f.eval σ == positive) := by
  induction f generalizing positive with
  | atom i => simp [cnf, eval, Literal.eval]
  | tt => cases positive <;> simp [cnf, eval]
  | ff => cases positive <;> simp [cnf, eval]
  | not f ih =>
    simp only [cnf, eval, ih]
    cases f.eval σ <;> cases positive <;> rfl
  | and f g ihf ihg | or f g ihf ihg | imp f g ihf ihg | iff f g ihf ihg =>
    cases positive <;>
      simp only [cnf, eval, Cnf.eval_append, Cnf.eval_distribute, ihf, ihg] <;>
      cases f.eval σ <;> cases g.eval σ <;> rfl

/-- The clause set to refute in order to prove a formula: the clauses of its negation,
simplified. -/
def refutation (f : Formula) : Cnf := Cnf.simplify (f.cnf false)

theorem eval_refutation (σ : Assignment) (f : Formula) :
    Cnf.eval σ f.refutation = !f.eval σ := by
  rw [refutation, Cnf.eval_simplify, eval_cnf]
  cases f.eval σ <;> rfl

/-- A formula is valid when the clause set of its negation is unsatisfiable. -/
theorem valid_of_refutation_unsatisfiable {f : Formula} (h : f.refutation.Unsatisfiable) :
    f.Valid := by
  intro σ
  have := h σ
  rw [eval_refutation] at this
  simpa using this

/-- The converse: the clause set of the negation of a valid formula is unsatisfiable. -/
theorem refutation_unsatisfiable_of_valid {f : Formula} (h : f.Valid) :
    f.refutation.Unsatisfiable := by
  intro σ
  rw [eval_refutation, h σ]
  rfl

end Formula

end Maithic.Oracle.Logic
