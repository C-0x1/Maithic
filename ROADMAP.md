# Roadmap

Maithic is nineteen pillars, and the release holds at least twenty capabilities of each: 481 in all. The register
in [`registry/`](registry) lists every one with its status and its tests; [`registry/STATUS.md`](registry/STATUS.md)
is the current count. This page says what is built and what comes after it.

## Built and tested

All nineteen pillars are complete: every capability of the release has a test that names it.

- **Proving.** The proving loop on whole declarations, the Lean bridge with proof states, a step-wise proof
  search with a record and a picture of every search, the gateway for three providers, and the trust policy
  with the fresh re-check and the ledger.
- **Stating.** `maithic formalize`: from a problem in words to candidate statements in Lean, a bounded loop
  that repairs what does not elaborate, and the checks that a statement says what was meant. Lean rejects the
  vacuous and the refuted candidates; back-translation and a round-trip score flag the rest.
- **Working in roles.** `maithic council`: a researcher, a prover, a critic and a verifier on one blackboard,
  with budgets and a transcript. Only evidence made from an answer of Lean changes the status of a claim.
- **Computing without trusting.** Certificate-producing oracles for four families of goals, with their registry
  and the hand-off of certificates to Lean; verified checkers for propositional proofs and for linear integer
  certificates; and `maithic nnv verify`, in which a verifier in Julia decides a property of a neural network
  and Lean's kernel checks the answer with a checker that is proved sound.
- **Refuting.** Tests of statements by sampling and for vacuity in Lean, and `maithic refute`: counterexamples
  from tests and from solvers, each confirmed by a proof that Lean's kernel accepts.
- **Reading code.** Extraction of what Lean knows about a file, the code graph with its questions and the
  health report, graph measures in Julia, tables and figures in R; and `maithic search`, which finds
  declarations by words with an inverted index, BM25 and Lean's own search, merged by rank.
- **Planning.** `maithic blueprint`: the plan of a formalization as a graph of definitions, lemmas and
  theorems, in files that parse and print; a planner that takes an argument in words apart; skeletons that
  compile; progress read from Lean; and the list of what is ready to prove next.
- **Datasets and guidance.** Proofs traced into theorem, step, premise and pair records, shards named by
  content with a manifest that can be verified, preparation and leakage checks in Julia; and a small tactic
  model trained on such a dataset, measured on a split by theorem, that orders the candidates of the step-wise
  search.
- **Review and repair.** Lint rules that run in Lean, policies and suppression with reasons, simpler proofs
  that are suggested only after Lean accepted them; `maithic mend`, which classifies what a change broke,
  repairs it by rule, with a model and with the prover, and shortens proofs without adding an axiom.
- **Verified algorithms and their laboratory.** Four sorts with the theorem that they are the same function,
  linear and binary search against one specification, minimum and maximum, removal of repeats, Fibonacci
  numbers by table, prefix sums, Euclid's algorithm, exponentiation by squaring, pattern search and paths in
  graphs; proved bounds on the comparisons of four of them; `maithic algo synth`, which drafts a function with
  a model and judges it by Lean and by examples; differential tests of the compiled code against reference
  implementations in Julia; and growth models fitted to measured costs in R.
- **Measuring.** The benchmark with its suites and the gate that holds a run against a baseline, the statistics
  and figure packages, the schemas of every document that one language writes for another, and the
  repository's own gates.

## Next

The release is complete. Every capability has a test that names it, every part has a guide, every Julia and R
package has a page with an example that runs, and every figure and picture of the documentation is drawn from
the commands, from recorded results or by hand, and can be drawn again. What comes after it is below.

## Later

Beyond this release: verified checkers for linear arithmetic over the rationals and for polynomial identities,
so that the oracles prove goals the built-in automation does not; checkers that need Mathlib, for primality and
for real inequalities; a model that proposes one tactic at a time inside the step-wise search; a local service
with an HTTP interface, and a workbench in the browser. Dates are not promised; the register always shows the
actual state.
