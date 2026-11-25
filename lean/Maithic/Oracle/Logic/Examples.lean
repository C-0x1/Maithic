/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Reflect
import Maithic.Oracle.Logic.LinearIntTactic

/-!
# Worked examples of the logic checkers

Each theorem here is closed by a checker of this directory, so that the file doubles as a
compiled tour: the truth table, the search with its checked proof, proofs given as text, a
resolution refutation given as a term, a falsifying assignment, and certificates of linear
integer arithmetic. All of them depend on the standard axioms only.
-/

namespace Maithic.Oracle.Logic.Examples

variable {p q r s : Prop}

/-! ## By truth table -/

/-- Peirce's law. -/
theorem peirce : ((p → q) → p) → p := by maithic_taut

/-- An implication is equivalent to its contrapositive. -/
theorem contraposition : (p → q) ↔ (¬q → ¬p) := by maithic_taut

/-- De Morgan's law for conjunction. -/
theorem not_and_iff : ¬(p ∧ q) ↔ ¬p ∨ ¬q := by maithic_taut

/-- De Morgan's law for disjunction. -/
theorem not_or_iff : ¬(p ∨ q) ↔ ¬p ∧ ¬q := by maithic_taut

/-- Disjunction distributes over conjunction. -/
theorem or_and_distrib : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) := by maithic_taut

/-! ## By search, with the proof checked by `Lrat.check` -/

/-- A chain of implications. -/
theorem chain (hpq : p → q) (hqr : q → r) (hrs : r → s) : p → s := by maithic_sat

/-- The resolution rule itself. -/
theorem resolve (h₁ : p ∨ q) (h₂ : ¬p ∨ r) : q ∨ r := by maithic_sat

/-- Atoms may be any propositions: the three comparisons are unrelated atoms here, and the
hypothesis that one of them holds is what connects them. -/
theorem eq_of_not_lt_of_not_gt (x : Nat) (h : x < 3 ∨ x = 3 ∨ x > 3) (h₁ : ¬x < 3) (h₂ : ¬x > 3) :
    x = 3 := by maithic_sat

/-- Three pigeons do not fit into two holes. `a₁` says that the first pigeon sits in the first
hole, and so on. -/
theorem pigeonhole {a₁ a₂ b₁ b₂ c₁ c₂ : Prop} (ha : a₁ ∨ a₂) (hb : b₁ ∨ b₂) (hc : c₁ ∨ c₂)
    (h₁ : ¬(a₁ ∧ b₁)) (h₂ : ¬(a₁ ∧ c₁)) (h₃ : ¬(b₁ ∧ c₁))
    (h₄ : ¬(a₂ ∧ b₂)) (h₅ : ¬(a₂ ∧ c₂)) (h₆ : ¬(b₂ ∧ c₂)) : False := by maithic_sat

/-! ## From a clausal proof given as text -/

/-- Modus ponens, from a proof of one step: the clauses `p`, `¬p ∨ q` and `¬q` conflict by unit
propagation. `maithic_cnf` prints the clauses and their numbers. -/
theorem modus_ponens (hp : p) (hpq : p → q) : q := by maithic_lrat "4 0 1 2 3 0"

/-- The pigeonhole statement again, from the two steps a solver would write: the first pigeon
cannot sit in the first hole, and then nothing is left. -/
theorem pigeonhole' {a₁ a₂ b₁ b₂ c₁ c₂ : Prop} (ha : a₁ ∨ a₂) (hb : b₁ ∨ b₂) (hc : c₁ ∨ c₂)
    (h₁ : ¬(a₁ ∧ b₁)) (h₂ : ¬(a₁ ∧ c₁)) (h₃ : ¬(b₁ ∧ c₁))
    (h₄ : ¬(a₂ ∧ b₂)) (h₅ : ¬(a₂ ∧ c₂)) (h₆ : ¬(b₂ ∧ c₂)) : False := by
  maithic_lrat "10 -1 0 4 2 5 3 7 9 0\n11 0 10 1 7 2 6 3 8 0"

/-- Four pigeons do not fit into three holes. The proof text was written by the solver CaDiCaL
for the problem that `maithic_cnf` prints for this goal; the solver is not trusted, and need not
be: the kernel runs the checker on its fifteen steps. -/
theorem pigeonhole_four_three {a₁ a₂ a₃ b₁ b₂ b₃ c₁ c₂ c₃ d₁ d₂ d₃ : Prop}
    (ha : a₁ ∨ a₂ ∨ a₃) (hb : b₁ ∨ b₂ ∨ b₃) (hc : c₁ ∨ c₂ ∨ c₃) (hd : d₁ ∨ d₂ ∨ d₃)
    (h₁ : ¬(a₁ ∧ b₁) ∧ ¬(a₁ ∧ c₁) ∧ ¬(a₁ ∧ d₁) ∧ ¬(b₁ ∧ c₁) ∧ ¬(b₁ ∧ d₁) ∧ ¬(c₁ ∧ d₁))
    (h₂ : ¬(a₂ ∧ b₂) ∧ ¬(a₂ ∧ c₂) ∧ ¬(a₂ ∧ d₂) ∧ ¬(b₂ ∧ c₂) ∧ ¬(b₂ ∧ d₂) ∧ ¬(c₂ ∧ d₂))
    (h₃ : ¬(a₃ ∧ b₃) ∧ ¬(a₃ ∧ c₃) ∧ ¬(a₃ ∧ d₃) ∧ ¬(b₃ ∧ c₃) ∧ ¬(b₃ ∧ d₃) ∧ ¬(c₃ ∧ d₃)) : False := by
  maithic_lrat "23 -11 -12 0 22 19 13 16 1 3 6 0
    24 -7 -12 0 21 19 6 8 1 2 11 0
    25 -12 0 19 21 22 24 3 12 14 1 2 5 0
    26 -8 0 25 12 14 16 4 7 9 1 2 17 0
    27 -7 0 25 6 8 10 4 13 15 1 2 17 0
    28 9 0 27 26 3 0
    29 -3 0 28 18 0
    30 -6 0 28 20 0
    31 -1 0 30 25 5 7 2 4 15 0
    32 2 0 31 29 1 0
    33 -5 0 32 11 0
    34 -11 0 32 13 0
    35 4 0 33 30 2 0
    36 10 0 34 25 4 0
    37 0 35 36 9 0"

/-! ## On formulas and clause sets directly -/

/-- The four clauses over two atoms, which exclude every assignment. -/
def twoAtoms : Cnf :=
  [[⟨0, true⟩, ⟨1, true⟩], [⟨0, false⟩, ⟨1, true⟩], [⟨0, true⟩, ⟨1, false⟩], [⟨0, false⟩, ⟨1, false⟩]]

/-- A resolution refutation given as a term: two resolutions on the first atom leave the second
atom and its negation, and one more gives the empty clause. -/
theorem twoAtoms_unsatisfiable : twoAtoms.Unsatisfiable :=
  Resolution.unsatisfiable_of_refutes
    (proof := [⟨0, 1, ⟨0, true⟩⟩, ⟨2, 3, ⟨0, true⟩⟩, ⟨4, 5, ⟨1, true⟩⟩]) (by decide)

/-- The same clause set, from a clausal proof. -/
theorem twoAtoms_unsatisfiable' : twoAtoms.Unsatisfiable :=
  Lrat.unsatisfiable_of_refutes
    (proof := [.add 5 [⟨1, true⟩] [1, 2], .add 6 [] [5, 3, 4]]) (by decide)

/-- Validity of a formula is decidable. -/
theorem excluded_middle_valid : (Formula.or (.atom 0) (.not (.atom 0))).Valid := by decide

/-- A formula that is not valid, with the assignment that shows it: the first atom true and the
second false. -/
theorem imp_not_valid : ¬(Formula.imp (.atom 0) (.atom 1)).Valid :=
  Formula.not_valid_of_falsifiedBy (trueAtoms := [0]) (by decide)

/-- The same assignment refutes the statement about all propositions. -/
theorem not_forall_imp : ¬∀ a b : Prop, a → b := fun h =>
  Formula.not_forall_denote_of_falsifiedBy (f := .imp (.atom 0) (.atom 1)) (trueAtoms := [0])
    (by decide) fun ρ => h (ρ 0) (ρ 1)

/-! ## Linear integer arithmetic from certificates -/

/-- No integer point lies between the two lines. Over the rationals there are such points; the
first row is the hypothesis strengthened for the integers, and adding it to the second gives
`0 ≤ -1`. -/
theorem no_integer_point (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h₁, h₂]

/-- A goal that is not `False`: negate it, and treat the two ways an equation can fail. -/
theorem eq_of_le_of_le (a b : Int) (h₁ : a ≤ b) (h₂ : b ≤ a) : a = b := by
  apply Classical.byContradiction
  intro negated
  rcases Int.lt_or_gt_of_ne negated with negated | negated
  · maithic_linear_int [a, b] [(1, ⟨[1, -1], .lt, 0⟩), (1, ⟨[-1, 1], .le, 0⟩)] using [negated, h₂]
  · maithic_linear_int [a, b] [(1, ⟨[-1, 1], .lt, 0⟩), (1, ⟨[1, -1], .le, 0⟩)] using [negated, h₁]

/-- An equation may be used with a multiplier of either sign. -/
theorem equation_and_bound (x y : Int) (h₁ : x + y = 10) (h₂ : x ≤ 3) (h₃ : y ≤ 6) : False := by
  maithic_linear_int [x, y] [(-1, ⟨[1, 1], .eq, 10⟩), (1, ⟨[1], .le, 3⟩), (1, ⟨[0, 1], .le, 6⟩)]
    using [h₁, h₂, h₃]

end Maithic.Oracle.Logic.Examples
