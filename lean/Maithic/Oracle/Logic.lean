/-
Copyright (c) 2026 The Maithic Authors. All rights reserved.
Released under the Apache 2.0 license as described in the file LICENSE.
-/
import Maithic.Oracle.Logic.Formula
import Maithic.Oracle.Logic.Table
import Maithic.Oracle.Logic.Clause
import Maithic.Oracle.Logic.Resolution
import Maithic.Oracle.Logic.Lrat
import Maithic.Oracle.Logic.Search
import Maithic.Oracle.Logic.Soundness
import Maithic.Oracle.Logic.Reflect
import Maithic.Oracle.Logic.LinearInt
import Maithic.Oracle.Logic.LinearIntTactic
import Maithic.Oracle.Logic.Examples

/-!
# Logic checkers

Small programs that check a certificate, each with a proof that what it accepts is true. A
solver, a search or a person finds the certificate; the kernel runs the checker; the soundness
theorem turns "the checker returned `true`" into a proof of the goal. Nothing that searches is
trusted.

| File | What it has |
|---|---|
| `Formula` | propositional formulas, evaluation, denotation, validity |
| `Table` | the truth table, sound and complete, so validity is decidable |
| `Clause` | literals, clauses, the conversion to clause normal form with its correctness, DIMACS |
| `Resolution` | a checker of resolution proofs, with soundness |
| `Lrat` | a checker of clausal proofs by unit propagation, with soundness, and the text form |
| `Search` | a search that writes such proofs; unverified, its output is checked |
| `Soundness` | the theorems from an accepted proof to the goal, and from a model to a refutation |
| `Reflect` | `maithic_taut`, `maithic_sat`, `maithic_lrat`, `maithic_cnf`, `#maithic_reflect` |
| `LinearInt` | a checker of certificates of linear integer arithmetic, with soundness |
| `LinearIntTactic` | `maithic_linear_int` |
| `Examples` | worked examples of all of the above |
-/
