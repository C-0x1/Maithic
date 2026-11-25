<p align="center">
  <img src="assets/brand/maithic-wordmark.svg" alt="Maithic. AI proposes. The kernel decides." width="560">
</p>

**Maithic** is a toolkit for AI-assisted mathematics and formal verification on [Lean 4](https://lean-lang.org).
A model, or plain automation, proposes a proof. Lean's kernel checks it. A trust policy and a re-check in a fresh
process decide whether the result may be called *verified*. Nothing else in the system can produce that word.

It works with the model provider you choose, **Anthropic, OpenAI or xAI**, with your own API key, and it works
with **no key at all**: the built-in automation proves what it can, and every test and demonstration in this
repository runs from stored answers with no network.

Nineteen parts make up the toolkit. Each has a command, and all of them answer to the same judge:

<p align="center">
  <img src="assets/readme/overview-map.svg" alt="Nineteen cards in a grid, one for each part of Maithic, with the command that belongs to it. Platform: one command with typed errors and stable exit codes. Prove: proves theorems, automation first, then a model that reads Lean's errors. Formalize: turns a problem in words into a tested Lean statement. Review: reviews Lean files and checks every suggestion with Lean. Search: finds declarations by words. AlgoLab: algorithms drafted, tested, proved and measured. Council: several roles on one theorem with Lean as the judge. Intel: a codebase as a graph. NNV: properties of neural networks checked by the kernel. Bridge: the Lean process behind everything. Gateway: one request for three providers. Corpus: datasets traced from proofs. Guide: a small model that orders tactics. Oracle: solvers that answer with certificates. Refute: counterexamples before proofs. Bench: a benchmark with a regression gate. Blueprint: a formalization planned and tracked. Mend: repair of proofs that stopped compiling. Ledger: the audit and policy behind the word verified." width="900">
</p>

> **Status: in development, version 0.1.0.** Maithic is nineteen parts that work together, from the proving loop
> to the trust ledger. Each part is a set of capabilities, and a capability counts as done only when a test names
> it. All 481 capabilities of this release are done; [`registry/STATUS.md`](registry/STATUS.md) lists every one
> with the tests that name it, and [`ROADMAP.md`](ROADMAP.md) says what comes next.

## Contents

- [Try it](#try-it)
- [How a result becomes verified](#how-a-result-becomes-verified)
- [Searching one tactic at a time](#searching-one-tactic-at-a-time)
- [Testing a statement before proving it](#testing-a-statement-before-proving-it)
- [Refuting a statement, with Lean as the judge](#refuting-a-statement-with-lean-as-the-judge)
- [From a problem in words to a statement in Lean](#from-a-problem-in-words-to-a-statement-in-lean)
- [A council: several roles, one judge](#a-council-several-roles-one-judge)
- [Reading a codebase as a graph](#reading-a-codebase-as-a-graph)
- [Datasets from proofs](#datasets-from-proofs)
- [Guiding the search with a small model](#guiding-the-search-with-a-small-model)
- [Review that Lean checks](#review-that-lean-checks)
- [Repairing proofs that stopped compiling](#repairing-proofs-that-stopped-compiling)
- [Planning a formalization and tracking it](#planning-a-formalization-and-tracking-it)
- [Finding what is already there](#finding-what-is-already-there)
- [Oracles and certificates](#oracles-and-certificates)
- [Checkers with proofs](#checkers-with-proofs)
- [A neural network, with the kernel as judge](#a-neural-network-with-the-kernel-as-judge)
- [Algorithms: drafted, tested, proved, measured](#algorithms-drafted-tested-proved-measured)
- [Model providers and keys](#model-providers-and-keys)
- [Commands](#commands)
- [The Bridge: one Lean process behind every command](#the-bridge-one-lean-process-behind-every-command)
- [What is in the repository](#what-is-in-the-repository)
- [Verified libraries](#verified-libraries)
- [Benchmark](#benchmark)
- [What the kernel says about this repository](#what-the-kernel-says-about-this-repository)
- [Development](#development)
- [Documentation](#documentation)
- [License](#license)

## Try it

Four steps take you from a clone to a proof that Lean's kernel has checked, and none of them needs a key:

<p align="center">
  <img src="assets/readme/quickstart-steps.svg" alt="Four steps in a row. Build: make build-ocaml build-lean builds the command and the Bridge. Check the installation: maithic doctor lists each component and runs the self-test of the Bridge. Prove: maithic prove examples/even.lean proves that the sum of two even numbers is even, by omega, with no key. Keep it: the option write puts the proof into the file, and maithic check re-checks the file from its imports in a new process." width="900">
</p>

The command and the Lean libraries need OCaml with dune, and Lean 4 through
[elan](https://lean-lang.org/install/). Julia and R are needed only for the parts that compute and measure:

<p align="center">
  <img src="assets/readme/install-profiles.svg" alt="A table. With OCaml and Lean alone: prove, check, refute, formalize, council, review, mend, blueprint, search, intel, corpus and bench. With Julia as well: the oracles, the tactic model, the verification of networks and the differential test of algorithms. With R as well: the statistics and the figures." width="900">
</p>

This release is built and tested with OCaml 5.5, Lean 4.36, Julia 1.13 and R 4.5, the versions that the workflow
pins. [`docs/install.md`](docs/install.md) has the details.

```sh
make build-ocaml build-lean
alias maithic="$PWD/ocaml/_build/default/bin/maithic/main.exe"
maithic doctor
```

`doctor` checks each toolchain, starts the Bridge and runs its self-test, and says what to do about anything that
is missing:

<p align="center">
  <img src="assets/readme/term-doctor.svg" alt="A terminal session: maithic version prints the version of the command, of the Bridge protocol, of Lean and of OCaml. maithic doctor lists Lean, Lake, the Bridge with the result of its self-test, curl, Julia and R, each with its version and the status ok, and then the gateway mode, the provider and model, and that candidate screening is active and that Lean workers never see API keys." width="643">
</p>

**Prove a theorem with no key.** [`examples/even.lean`](examples/even.lean) states that the sum of two even
numbers is even and leaves the proof as `sorry`:

```sh
maithic prove examples/even.lean
```

<p align="center">
  <img src="assets/readme/term-prove.svg" alt="A terminal session: maithic prove closes a theorem about even numbers; the proof that the automation found is checked by Lean's kernel and the result is verified, with the axioms it rests on." width="484">
</p>

Three candidates failed, the fourth was accepted by the kernel, the axioms it rests on were audited, and a second
Lean process re-checked the finished text before the word *verified* was printed. Add `--write` to put the proof
into the file.

**See the model-assisted path, still with no key.** [`examples/triangle.lean`](examples/triangle.lean) needs
induction, which the automation portfolio does not find. In replay mode the model's answers come from the
cassettes in [`fixtures/cassettes/`](fixtures/cassettes), and `--verbose` lists every candidate that was tried:

```sh
maithic prove examples/triangle.lean --mode replay --provider anthropic --verbose
```

<p align="center">
  <img src="assets/readme/term-prove-rounds.svg" alt="A terminal session: maithic prove with the option verbose lists every candidate proof of a theorem about triangular numbers. The eighteen candidates of the automation fail. In round 1 a model proposes two proofs and Lean rejects both, the first as a failed tactic and the second with unsolved goals. In round 2 the model's proof by induction is accepted, and the result is verified after 21 attempts." width="663">
</p>

The eighteen candidates of the automation failed, and so did the model's first two. Lean's error for each went
back to the model, and the proof of the second round was accepted. This is the whole run, with who said what to
whom:

<p align="center">
  <img src="assets/readme/prove-run.svg" alt="A sequence diagram with four lifelines: the maithic command, a model, Lean in the session, and Lean in a new process. The command sends the file up to the theorem to Lean and receives an environment. It sends the 18 candidates of the portfolio, and all fail. It asks the model, which answers with two proofs; Lean rejects both, one because simp made no progress and one with unsolved goals. The command asks again, with the two proofs and Lean's error for each, and the model answers with a proof by induction, which Lean accepts with the axioms it rests on. The whole file with the proof is then elaborated in a new process, which reaches the same statement, and the theorem is verified." width="900">
</p>

The same command works with `--provider openai` and `--provider xai`. The cassettes are test fixtures written by
hand, not recordings of a model; they exist so that this path can be run and tested anywhere.

**Use your own key.** Export the key of the provider you want and drop `--mode replay`:

```sh
export ANTHROPIC_API_KEY=...        # or OPENAI_API_KEY, or XAI_API_KEY
maithic prove examples/triangle.lean --provider anthropic
```

Nothing else changes between the two ways of running: the same commands, the same checks by Lean, and a different
source for the model's words.

<p align="center">
  <img src="assets/readme/replay-vs-live.svg" alt="Two cards side by side. Without a key, in replay mode, the answers of the model are read from cassettes in the repository: every model-assisted command runs on any machine with the same result, and the answers were written by hand for the demonstrations. With your key, in live mode, the same commands ask the model you chose, and record mode keeps the exchanges as cassettes." width="900">
</p>

**State it, argue about it, repair it.** Proving is one of the things the command does. These three also run
from the cassettes, with no key:

```sh
export MAITHIC_GATEWAY_MODE=replay
maithic formalize "Show that the square of a negative integer is positive." --candidates 4
maithic council examples/council.lean
maithic mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml
```

The first turns a sentence into a Lean statement and lets Lean throw out the candidates that are vacuous or
false. The second puts four roles to work on a theorem, with Lean as the only judge. The third repairs a file
that an upgrade broke, and keeps no edit that Lean does not accept. Each has a section below.

## How a result becomes verified

<p align="center">
  <img src="assets/readme/proving-loop.svg" alt="The proving loop: a theorem goes to the automation portfolio and, if needed, to a model; every proposal passes the screen, the Lean kernel, the trust policy and a fresh re-check; Lean's errors go back to the model for the next round." width="900">
</p>

Every proposal takes the same path, whether it came from `omega` or from a model. Proposals have four sources,
and they are tried in the order of what they cost:

<p align="center">
  <img src="assets/readme/prove-sources.svg" alt="Four stages from top to bottom, each with a note at its right. First the automation portfolio, a fixed list of scripts built from rfl, trivial, simp, omega, decide, simp_all and grind, which needs no model and always runs first. Second, with the option oracle, a solver that returns a certificate for an arithmetic goal. Third, with the option search, a search that runs one tactic at a time on proof states. Fourth, unless AI is switched off, a model that proposes whole proofs in rounds and sees Lean's errors from the round before. The first candidate that the kernel accepts ends the work." width="900">
</p>

Five layers stand between a proposal and the status `verified`:

<p align="center">
  <img src="assets/readme/trust-layers.svg" alt="Five layers: the screen, the kernel, the axiom audit, the trust policy and the fresh re-check, each with an example of what it stops or reveals." width="900">
</p>

**The screen.** A candidate from a model is text from outside, and it is read before Lean sees it. A proof has no
need to declare an axiom, set an option or run a program, so a text that tries is refused without being
elaborated:

<p align="center">
  <img src="assets/readme/screen-rules.svg" alt="Six cards, one for each kind of text that the screen refuses in a candidate from a model. Commands that run something, such as #eval, #print, run_cmd and initialize. New declarations, such as def, theorem, axiom, instance and inductive. Meta-programs and syntax: elab, macro, syntax, notation, run_tac. Options and scope: set_option, import, open, namespace, attribute. The outside world: names under IO, System, Lean.Elab, Lean.Meta and Lean.Compiler. Ways around the kernel: sorry, admit, native_decide, unsafe, implemented_by, Lean.ofReduceBool." width="900">
</p>

**The axiom audit.** When the kernel has accepted a proof, Lean is asked which axioms the proof rests on, through
everything it uses. The answer decides more than any reading of the proof could:

<p align="center">
  <img src="assets/readme/axiom-audit.svg" alt="A table of six rows. propext, Quot.sound and Classical.choice are the axioms of ordinary mathematics in Lean, and the default policy allows them. sorryAx appears when a proof, or anything beneath it, contains sorry, and is never allowed. An axiom with native in its name appears for a proof by compiled evaluation and is rejected unless the policy sets allow_native. An axiom declared in a file appears under its own name and is rejected unless the policy lists it." width="900">
</p>

**The fresh re-check.** A session that has tried a hundred candidates is not the place to judge a result. The
finished text is elaborated once more, from its imports, in a process that has seen nothing else:

<p align="center">
  <img src="assets/readme/recheck.svg" alt="Two cards side by side with an arrow from the first to the second. In the session, a candidate is elaborated on an environment that the Bridge has kept, among many other candidates; this is fast and good enough to find a proof. In a fresh process, the finished text, the file up to the theorem followed by the theorem with its proof, is elaborated from its imports in a new Bridge; it must reach the same statement, and its facts, not the session's, are what the policy judges." width="900">
</p>

The policy is a file. The default, [`config/policies/default.toml`](config/policies/default.toml), allows the
three standard axioms, rejects proofs that trust compiled evaluation and requires the re-check. Maithic uses a
fixed vocabulary of eight status words; `maithic help status` prints it, and only `verified` is ever presented as
an established result.

<p align="center">
  <img src="assets/readme/status-words.svg" alt="Eight tiles, one for each status word Maithic uses. Verified: the kernel accepted the proof of exactly the requested statement and the trust policy passed. Checked: the kernel accepted a proof, but a policy requirement such as the re-check is pending. Refuted: a counterexample was confirmed by the kernel. Conjectured: stated and not yet proved or refuted. Unverified: produced by a model or engine and not checked. Unknown: a verification method ended without a decision. Failed: the attempt ended without a proof, with a recorded reason. Abstained: the system declined to choose. Only verified is presented as an established result." width="900">
</p>

`maithic check` applies the same layers to a file that is already written. It reports every theorem with its
status and the axioms it rests on, and a proof that is not finished is reported as failed, whatever else is true
of it:

<p align="center">
  <img src="assets/readme/term-check.svg" alt="A terminal session: maithic check elaborates two files, each in a new process. In the first, ten theorems about Euclid's algorithm are verified, each with the axioms it rests on. In the second, six conjectures are reported as failed because each proof depends on sorry, and the command ends with status 7." width="590">
</p>

What the word *verified* does not cover is said as plainly as what it does:

<p align="center">
  <img src="assets/readme/limits.svg" alt="Four cards. The statement is yours: Maithic checks that the proof proves the statement in your file, not that the statement says what you meant. The trusted base: Lean's kernel, the Lean toolchain that elaborates the file, and the Maithic code that reads Lean's answers and applies the policy. The screen is a filter: it reduces what untrusted text can do and is not an operating-system sandbox. Native evaluation: a proof that passes with allow_native trusts the compiler in addition to the kernel." width="900">
</p>

## Searching one tactic at a time

Proposing whole proofs is one way to search. The other is to take a proof apart: open the goal, try a tactic,
look at what is left, and try again from there. The Bridge holds every proof state it has reached, so a search
can branch from any of them without redoing the steps that led there.

<p align="center">
  <img src="assets/readme/proof-states.svg" alt="A chain of proof states for the statement p and q. From the first state, exact hp fails with a type mismatch and constructor succeeds, giving two goals. From there exact hq fails and exact hp succeeds, leaving one goal, which exact hq closes. The script found is then written out as a whole declaration and checked by the kernel like any other candidate." width="900">
</p>

```json
{"method": "startProof", "params": {"env": 1, "statement": "theorem pair (p q : Prop) (hp : p) (hq : q) : p ∧ q"}}
{"method": "tryTactics", "params": {"state": 2, "tactics": ["exact hp", "exact hq", "constructor", "rfl"]}}
```

`maithic prove --search` runs such a search when the automation portfolio has failed and before any model is
asked. Every goal is searched on its own: a tactic that leaves two goals makes two problems, a goal met twice is
searched once, and a tactic that leads back to a goal above it is blocked. With no model, the tactics come from
the shape of the goal. The theorem of [`examples/double.lean`](examples/double.lean) needs an induction that no
single call of automation finds:

```console
$ maithic prove examples/double.lean --no-ai
double_eq  failed  (-, 18 attempt(s), 1170 ms)
  statement: theorem double_eq (n : Nat) : double n = 2 * n
  reason:    no proof was found in 18 attempt(s)
  note:      the model was not asked (prove.use_ai is false)
```

With `--search` the same command finds the induction, and the proof it builds goes through the same five layers:

<p align="center">
  <img src="assets/readme/term-prove-search.svg" alt="A terminal session: with models switched off, maithic prove finds a proof by step-wise search, by induction with rfl for the first case and simp and omega for the second, and the result is verified after 19 attempts." width="390">
</p>

`--search-trees DIR` writes the record of each search with a drawing of it. The picture below is drawn by the
command, from what happened:

<p align="center">
  <img src="assets/readme/search-tree.svg" alt="The search tree for the theorem double_eq, drawn by the command. On the first goal, double n = 2 * n, rfl, assumption, omega and simp fail, and induction n leaves two goals. The goal for zero is closed by rfl; simp and simp_all lead from it to a goal that was never expanded. On the goal for n + 1 the closing tactics fail and simp with the definition of double leaves one goal, double n + 2 = 2 * (n + 1), which omega closes. The tactics of the proof are drawn in the colour of proved goals, and the proof is written out below the tree: induction n, then rfl, then simp [double] and omega." width="777">
</p>

`maithic tree` reads a record back, with neither Lean nor the file the search was about: every goal that was
expanded, what each tactic did on it, and how long it took.

<p align="center">
  <img src="assets/readme/term-tree.svg" alt="A terminal session: maithic tree prints a recorded proof search step by step: which goal was expanded, which tactics were tried, and what each led to." width="423">
</p>

A state with no goals left is a script that works, not a theorem. The script goes back through the five layers
above as a whole declaration; [`docs/adr/0006`](docs/adr/0006-proof-states-are-not-evidence.md) explains why
nothing less counts. [`docs/guides/search.md`](docs/guides/search.md) describes the strategies, the budgets and
the limits of the search, and
[`docs/reference/bridge-protocol.md`](docs/reference/bridge-protocol.md#proof-states) the requests.

## Testing a statement before proving it

A proof attempt on a false statement spends its whole budget and proves nothing. A proof of a vacuous statement
proves nothing worth having. Three commands of the Lean library examine a statement before anyone tries to
prove it.

<p align="center">
  <img src="assets/readme/statement-checks.svg" alt="Three commands examine a statement. maithic_test generates values and reports that x times y at least zero is falsified at x equal to 1 and y equal to minus 1, after shrinking. maithic_vacuous reports that the hypotheses x less than 3 and x greater than 5 are contradictory, found by omega. maithic_necessity reports that the hypothesis a at most b is needed, with a counterexample, and that b less than 1000 is not. Below, the findings are sorted by what they are worth: vacuity and hypotheses that are not needed rest on proofs the kernel accepted, falsifications come with a counterexample, and passing cases or finding nothing is evidence only." width="900">
</p>

```lean
import Maithic.Refute.Testing
import Maithic.Refute.Vacuity

#maithic_test ∀ l : List Nat, l.reverse.reverse = l
-- passed 100 cases

#maithic_test ∀ x y : Int, x * y ≥ 0
-- error: falsified at case 12, shrunk 2 times
--   x := 1
--   y := -1

#maithic_vacuous ∀ (x : Nat) (h₁ : x < 3) (h₂ : x > 5), x = 4
-- warning: vacuous: the hypotheses are contradictory (found by omega, from h₁ and h₂)
```

A test run is determined by its options: the random stream is SplitMix64 from a seed, the same stream as in the
Julia packages, so a failure is found again by running the same line. A finding of vacuity rests on a derivation
that the kernel accepted, and it names the hypotheses the derivation used. Passing a hundred cases is reported
as that and as nothing more. [`docs/guides/testing-statements.md`](docs/guides/testing-statements.md) has the
options, the tactic and the types that can be generated.

## Refuting a statement, with Lean as the judge

`maithic refute` brings those checks to the command line and adds a solver. It examines the unproved theorems
of a file by three methods, and it reports a counterexample only when Lean has accepted a proof that the
statement is false there. The proof is printed, and anyone can check it again.

<p align="center">
  <img src="assets/readme/refute-pipeline.svg" alt="The statement square_monotone is examined by three methods: tests on sampled values in Lean, a solver in Julia, and the vacuity checks in Lean. A candidate counterexample from the first two is put into the statement, and Lean's kernel is asked to accept a proof that the statement is false at those values. Four verdicts follow: refuted with exit status 8, vacuous and open with exit status 7, and standing with exit status 0." width="900">
</p>

Three statements, and the three answers:

<p align="center">
  <img src="assets/readme/term-refute.svg" alt="A terminal session: maithic refute examines three unproved statements. One is refuted by a counterexample, with the Lean proof that confirms it; one is found vacuous because its hypotheses contradict each other; one stands after three rounds of tests." width="783">
</p>

A test that fails and a solver that names a point are both claims. The values are put into the statement,
hypotheses and all, and a decision procedure of Lean settles what is left; a point that the hypotheses exclude
can never confirm anything. A statement that is false by a slip of the key is caught the same way:

```lean
theorem pred_lt (n : Nat) (h : 0 < n) : n - 1 < m := by sorry   -- `m` was meant to be `n`
```

Lean takes a name it does not know for a new variable, so this elaborates and says something else. `refute`
answers with `m := 0, n := 1`, and the variable nobody wrote is the clue. Given a file and no theorem, the command
examines every statement whose proof is unfinished, and this one is among the four it refutes:

<p align="center">
  <img src="assets/readme/term-refute-file.svg" alt="A terminal session: maithic refute examines the six unproved statements of a file. Four are refuted, each with a counterexample and the Lean proof that confirms it, among them one that is false by a slip of the key; one is found vacuous; one stands." width="836">
</p>

For arithmetic whose failing points are not small, the solver names an exact one:

<p align="center">
  <img src="assets/readme/term-refute-oracle.svg" alt="A terminal session: a solver finds the exact point x = 3, y = 0 where a statement about rationals fails, and Lean accepts the refutation built from it." width="776">
</p>

A run comes back with one of three things, and the exit status says which:

<p align="center">
  <img src="assets/readme/refute-outcomes.svg" alt="Three cards. Refuted, exit status 8: a counterexample was found and Lean's kernel confirmed that the statement fails there. Vacuous or not confirmed, exit status 7: the hypotheses can never hold together, so the statement says nothing, or a candidate counterexample was not confirmed. Standing, exit status 0: no counterexample turned up, which is not a proof." width="900">
</p>

Nothing found is reported as that and as nothing more. [`docs/guides/refuting-statements.md`](docs/guides/refuting-statements.md)
has the methods, the budgets and the way a counterexample is confirmed.

## From a problem in words to a statement in Lean

Before a theorem can be proved it has to be written down, and the step from a sentence to a formal statement is
where meaning is lost without anyone noticing. A statement can compile and say something else: about the wrong
kind of number, with a bound that is off by one, or with hypotheses that cannot hold together, which makes it
true and empty. `maithic formalize` has a model draft several statements and then puts each one to Lean, which
rejects the vacuous and the false ones and never needs to be told what the sentence meant.

<p align="center">
  <img src="assets/readme/term-formalize.svg" alt="A terminal session: maithic formalize turns the sentence 'the square of a negative integer is positive' into four candidate Lean statements. Lean rejects one as vacuous and one with the counterexample n = 0, a third is repaired and found to be a duplicate, and the remaining statement passes every check and is chosen." width="663">
</p>

Three of the four candidates are out before any model is asked to read anything: one because no natural number
is below zero, one because zero is not negative, and one because it is the first statement in other notation.
A statement that does not compile goes back to the model with Lean's error and the signatures of the names Lean
does know. Only then come the softer checks: whether every number of the problem is in the statement, whether
an operation means what its symbol suggests, and whether a model that sees the statement alone describes the
problem it came from. Those flag a statement; they never pass one that Lean rejected.

<p align="center">
  <img src="assets/readme/formalize-funnel.svg" alt="Four candidate statements for the sentence 'the square of a negative integer is positive', one row each, with how far each got through five stages: elaborates, not a duplicate, not vacuous, no counterexample, and six softer checks. The statement for an integer n below zero passes all five and is chosen. The one for a natural number is rejected as vacuous, because its hypothesis cannot hold. The one that allows zero is rejected with the counterexample n = 0. The one written with a symbol Lean does not know is repaired once and dropped as a duplicate of the first. Below: a rejection rests on a proof, a flag is evidence for a reader, and nothing here proves the chosen statement." width="900">
</p>

Sometimes the problem itself is off, and the report says how:

<p align="center">
  <img src="assets/readme/term-formalize-flagged.svg" alt="A terminal session: for the claim that (a - b) + b = a over the natural numbers, Lean refutes the literal statement at a = 0, b = 1. The statement that is chosen adds an assumption, and the report flags it: its subtraction is cut off at zero, and its reading differs from the problem." width="836">
</p>

Subtraction of natural numbers stops at zero, so the sentence as written is false, and Lean says where. The
statement that survives adds `b ≤ a`, the run ends with exit status 7, and a person decides whether that is the
statement they meant. `--write` puts the chosen statement into a Lean file with `sorry` for a proof, and there
the prover takes over:

<p align="center">
  <img src="assets/readme/term-formalize-prove.svg" alt="A terminal session: maithic prove takes the Lean file that maithic formalize wrote and proves its theorem. After 21 attempts a proof from the model is verified, and the statement, the proof and the one axiom it rests on are printed." width="484">
</p>

Three problems were put through the command, and this is what became of every statement that was drafted:

<p align="center">
  <img src="assets/readme/formalize-candidates.svg" alt="What became of the candidate statements of three problems. For the square of a negative integer: one chosen, one duplicate, two rejected by Lean. For (a - b) + b = a: one chosen and flagged, one kept behind it, one rejected by Lean. For n squared is at least n: one chosen, one kept, one duplicate, one that never elaborated." width="780">
</p>

The three runs in the figure are recorded under [`fixtures/formalize`](fixtures/formalize) and replay without a
key. Of their eleven drafted statements, eight elaborate as drafted and ten after repair; three are rejected by
Lean on a proof. With `--statement` the candidates are yours, and with `--no-ai` as well no model is asked at
all. [`docs/guides/formalizing.md`](docs/guides/formalizing.md) has the seven steps, the checks and the three
Lean commands behind them.

## A council: several roles, one judge

A model that is asked to prove a theorem, check its own proof and report the result will report success.
`maithic council` splits that work among roles that cannot vouch for each other. A researcher names lemmas, and
every name is looked up in Lean. A prover proposes proofs. A critic explains failures from Lean's own errors.
A verifier gives every proof to Lean, and asks no model.

<p align="center">
  <img src="assets/readme/term-council.svg" alt="A terminal session: a council works on the theorem that the first n odd numbers add up to n squared. The verifier examines the statement; the researcher names lemmas, four of five of which exist; the prover's first two proofs are rejected by Lean; the critic explains why; the prover's next proof is accepted by Lean's kernel and the claim is verified." width="830">
</p>

The members do not hand work to each other. They post it on a shared board, where an item is never changed and
a claim has a status: `conjectured` when it is posted, whatever its text says, `formalized` when Lean has
elaborated its statement, `verified` when the kernel has accepted a proof within the trust policy, `refuted`
when Lean has accepted a proof that it is false at given values. The functions that change a status take
evidence, and evidence is made from an answer of Lean by tools that only the verifier is handed. The prover can
post "this is proved" as often as it likes; it is a note.

<p align="center">
  <img src="assets/readme/council-board.svg" alt="The blackboard of a council session, with its eleven items: the statement and the claim that it holds, four names the researcher found in the library, the researcher's idea, two proofs of the prover that Lean rejected, the critic's note on why they failed, and a third proof that Lean accepted. Beside it, the one path by which a claim changes status: the prover posts a proof, the verifier gives it to Lean, and Lean's answer becomes the evidence that moves the claim from conjectured to formalized to verified. The answer is built from items 2, 11, 3, 4 and 5; one definition, two proofs and two notes are set aside." width="900">
</p>

The same session, read as a sequence of turns:

<p align="center">
  <img src="assets/readme/council-session.svg" alt="A council session turn by turn: one lane for each role and one mark for each turn, joined by a line, with what each turn did written in a row under the lanes. Turn 1, the verifier examines the statement. In round 1 the researcher finds four of five names, the prover proposes two proofs, the verifier reports that Lean rejected both, and the critic gives a diagnosis. In round 2 the prover proposes one proof and the verifier reports that Lean accepted it." width="780">
</p>

In this session one of the five lemmas the researcher named does not exist; Lean said so and it never reached
the prover. The critic's remark, that `omega` cannot read a product of two variables, is what the second
attempt acts on. The session took four questions to models and twenty-four calls to Lean, and the answer at the
end is built from what Lean accepted and from nothing else that was said. A session has budgets for tokens,
cost, time and calls, and it begins with an examination that costs no tokens and often settles the matter:

<p align="center">
  <img src="assets/readme/term-council-examine.svg" alt="A terminal session: two sessions that end before any model is asked. A false statement is refuted in the examination with the counterexample a = 1, b = 0, and a statement that automation can prove is verified in the examination." width="830">
</p>

[`docs/guides/council.md`](docs/guides/council.md) has the roles, the board, the budgets and the transcript;
[`fixtures/council/sum-odd.md`](fixtures/council/sum-odd.md) is the transcript of the session above.

## Reading a codebase as a graph

Lean knows what every declaration refers to. `maithic intel scan` asks once and keeps the answer as a graph;
the questions that follow need no Lean and take milliseconds.

<p align="center">
  <img src="assets/readme/intel-pipeline.svg" alt="Four stages, each in the language that suits it. Lean elaborates every file and reports its declarations, their types and what each proof refers to. OCaml builds the graph and answers questions about it. Julia computes PageRank, betweenness and connected components. R reads all of it and makes tables and figures. Below, a piece of the real graph of the insertion sort in this repository, and four answers from the same scan." width="900">
</p>

One scan, and the questions it answers:

<p align="center">
  <img src="assets/readme/intel-questions.svg" alt="Six cards. Scan asks Lean once about every file and keeps the answer as a graph. Show tells what a declaration is, what it uses and what uses it. Deps lists everything it depends on, with the distance. Dependents lists everything that depends on it, which is what a change would reach. Health lists every unfinished proof with its open goal, and the axioms in use. Stats gives the size and shape of the graph and the declarations most others use." width="900">
</p>

Here is the scan of the algorithm library of this repository, and the shape of its graph:

<p align="center">
  <img src="assets/readme/term-intel-scan.svg" alt="A terminal session: maithic intel scan elaborates the 23 files of the algorithm library and counts 261 declarations, none resting on sorry. maithic intel stats then gives the size of the graph, and the declarations that most others use: Sorted, TotalPreorder and insertionSort among those of the scanned files, and List, Nat and Eq from anywhere." width="604">
</p>

`show` and `dependents` answer what a change to one definition would reach:

<p align="center">
  <img src="assets/readme/term-intel-show.svg" alt="A terminal session: maithic intel show prints what a definition uses and what uses it; intel dependents lists every declaration a change would reach, with its distance." width="790">
</p>

`deps` looks the other way, at everything a theorem rests on, with the distance of each declaration:

<p align="center">
  <img src="assets/readme/term-intel-deps.svg" alt="A terminal session: maithic intel deps lists what the theorem sorted_insertionSort rests on, with the distance of each declaration: at distance 1 the definitions Sorted and insertionSort, the class TotalPreorder and two theorems, and at distance 2 what those use in turn, each with its kind and the file and line where it is." width="677">
</p>

The graph is read from the environment the kernel checked, not from the text: a dependency is there because
the proof term refers to the constant. That is why the report on the state of a project can say more than a
search for the word `sorry`. It shows the goal that is open at each one, and it lists the theorems that contain
none and still rest on one:

<p align="center">
  <img src="assets/readme/term-intel-health.svg" alt="A terminal session: maithic intel health counts declarations by kind, lists every unfinished proof with its open goal, and names the axioms the code relies on." width="484">
</p>

The graph also says what a library rests on. These are the declarations that most others use:

<p align="center">
  <img src="assets/readme/intel-top-depended.svg" alt="Bar chart of the twelve declarations of the Maithic Lean library that most others depend on, with the number and share of the 980 declarations that use each, coloured by kind. Literal leads with 67, then Clause with 56, Interval with 55, Assignment with 47 and Formula with 45; Sorted, with 27, is the first from the algorithm library." width="780">
</p>

The figure is the Lean library of this repository, 980 declarations in 66 files, drawn in R from its graph.
[`docs/guides/code-intelligence.md`](docs/guides/code-intelligence.md) has the commands, the graph and the
computations on it.

## Datasets from proofs

`maithic corpus harvest` traces proofs while Lean elaborates them and writes what Lean saw: every tactic with
the goals before and after it, and every constant each proof term refers to.

<p align="center">
  <img src="assets/readme/corpus-records.svg" alt="The theorem double_eq, proved by induction with the tactics rfl, simp and omega, becomes four kinds of record: one theorem record with its statement, steps, premises and the mark complete; four step records, one for each tactic as written; 82 premise records, one for each constant the proof term refers to; and three pair records, each a proof state with the tactic that was run on it. Pairs come only from complete proofs, shards are named by their content, and the manifest holds a SHA-256 for every shard." width="900">
</p>

Harvesting a library and checking the result are one command each:

<p align="center">
  <img src="assets/readme/term-corpus.svg" alt="A terminal session: maithic corpus harvest traces the Lean files of a library into theorem, step, premise and pair records, and corpus verify confirms the dataset against its manifest." width="836">
</p>

Three decisions shape what is written, and each is made so that a record says only what Lean knows:

<p align="center">
  <img src="assets/readme/corpus-decisions.svg" alt="Three cards. Steps are tactics, and only tactics: only syntax that is a tactic becomes a step, so intro a b is one step. Pairs come from complete proofs only: a pair says that on this state this tactic is a step of a proof, and that is known only of a proof the kernel accepted whole. Premises are read from the proof term: the proof of double_eq names one lemma, and its proof term refers to 82 constants." width="900">
</p>

A state-and-tactic pair is written only for a proof the kernel accepted whole; a proof that rests on `sorry`
gives none. Shards are named by the hash of their content, so the same sources give the same files on any
machine, and `maithic corpus verify` checks a dataset against its manifest:

<p align="center">
  <img src="assets/readme/corpus-integrity.svg" alt="Three steps in a row. Records are written into shards, files of one record per line, and a shard is named by the SHA-256 of its content. The manifest lists every shard with its hash and its number of records, and names the sources and their revision. maithic corpus verify checks that every shard is there with that hash and count, that every record follows its schema, and that the folder holds nothing the manifest does not list." width="900">
</p>

Julia splits a dataset by theorem and checks the split for leakage, because a split by step would measure a
model on proofs it has all but seen:

<p align="center">
  <img src="assets/readme/split-by-theorem.svg" alt="Two cards side by side. Split by step: the steps of one proof land on both sides, the model is tested on states it has all but seen, and the measured accuracy flatters it. Split by theorem, which is what is done: every step of a proof is on the same side, the model is measured on proofs it has seen no part of, and the split depends on the set of theorems and the seed and on nothing else." width="900">
</p>

R shows what is in a dataset. These two figures are the proofs of the Lean library of this repository, 307
theorems and 1,278 pairs of a state and a tactic:

<p align="center">
  <img src="assets/readme/corpus-tactics.svg" alt="Bar chart of the fifteen most used tactics in the proofs of the Maithic Lean library, each bar divided into the steps that closed a goal and the steps that left goals open. simp leads with 296 uses, of which 56 percent closed a goal; exact follows with 174, cases with 97, have with 91 and rw with 86; exact, rfl and omega closed a goal every time." width="780">
</p>

And how long the proofs are:

<p align="center">
  <img src="assets/readme/corpus-proof-lengths.svg" alt="Bar chart of the tactic proofs of the Maithic Lean library by their number of smallest steps. Most proofs take one to five steps; the median is three, and its bar is drawn in gold; the longest proof runs to 83 steps." width="780">
</p>

[`docs/guides/datasets.md`](docs/guides/datasets.md) has the records, the checks and the preparation of data.

## Guiding the search with a small model

The step-wise search tries tactics on a goal in a fixed order of preference, so a proof that needs the eighth
tactic on the list waits for seven others at every goal. A *guide* is a small model, trained on a dataset of
recorded proofs, that says which tactics are worth trying first on the goal at hand. It reorders the candidates
and does nothing else: every step is still run by Lean, and a proof still counts only when the kernel accepts
it.

<p align="center">
  <img src="assets/readme/guide-cycle.svg" alt="Four steps in a row. Proofs in Lean files are traced into a dataset of pairs: a proof state and the tactic that was run on it. A small model is trained on the pairs, with the split by theorem, and its accuracy on proofs it never saw is recorded beside a baseline. In the step-wise search the model orders which goal to turn to next. Lean runs every step, and the kernel accepts the finished proof or does not." width="900">
</p>

The effect shows on a single theorem, proved once without a guide and once with one:

<p align="center">
  <img src="assets/readme/term-guide.svg" alt="A terminal session: the same theorem is proved twice by step-wise search, first without and then with a trained model that orders the tactics. Both proofs are verified by Lean; the guided one is a step shorter, and the last line says that the guide answered six of six questions." width="609">
</p>

On [`examples/guided.lean`](examples/guided.lean) the unguided search expands 28 goals and visits 71 states
before it finds a proof. With the model this repository keeps it expands 6 and visits 21. The model is trained
in Julia with no dependency outside the standard library, `maithic guide train` writes it with the record of
its training, and the same seed gives the same two files byte for byte. From proofs to a model that ranks tactics
takes three commands and a few seconds:

<p align="center">
  <img src="assets/readme/term-guide-train.svg" alt="A terminal session: from proofs to a tactic model in three commands. maithic corpus harvest traces the algorithm library into pairs of a goal and the tactic used on it. maithic guide train trains a small model on them and reports its accuracy on proofs it has not seen, beside a baseline that always answers with the most frequent tactics. maithic guide rank asks the new model about the goal double n = 2 * n, and it puts simp [double] first." width="796">
</p>

What the guide changes is the order in which goals are taken up. Each circle below is a goal that a search expanded:

<p align="center">
  <img src="assets/readme/guide-expansions.svg" alt="The goals that two searches for the same theorem expanded, in order, as rows of numbered circles. Without a guide the search expands 28 goals, four of them on the proof it finds; the others are goals left by constructor, intros, induction and simp on side branches. With the guide it expands 6, four of them on the proof. Below: the guide reads a goal as text and answers with probabilities for tactic templates; it changes the order in which open goals are taken up; it cannot change which tactics succeed or whether a proof counts; and a search never fails because of its guide." width="900">
</p>

`maithic guide info` says what a model is made of, and `guide rank` shows how it would order the tactics on one goal:

<p align="center">
  <img src="assets/readme/term-guide-rank.svg" alt="A terminal session: maithic guide info describes a trained tactic model, with its most frequent tactic templates and how often each closed a goal, and maithic guide rank lists the tactics the model would try first on the goal of a theorem, with the model's probability, the preference of the search and the two combined." width="769">
</p>

How much a guide knows is measured on proofs it never saw any part of: the data are split by theorem, not by
step. Twenty training runs are kept under [`fixtures/guide/runs`](fixtures/guide/runs), four shapes of model
with five seeds each, on 570 pairs of a state and a tactic from 104 theorems of the Lean library.

<p align="center">
  <img src="assets/readme/guide-accuracy.svg" alt="How often a tactic model names the next tactic, in two panels: as its first answer, and among its first five. One row for each of four shapes of model and one for the baseline that always answers with the most frequent templates; a dot at the share of held-out proof steps, a line for its 95 percent Wilson interval, and the share at the end of the row. First answer: mean-32 33 percent, attention-mean-32 31, attention-first-32 24, first-32 9, baseline 10. Among the first five: 68, 70, 64, 49 and 49 percent." width="780">
</p>

Pooled over the seeds, the plain model with mean pooling names the tactic the proof used in 32.7% of held-out
steps (95% interval 29.3% to 36.4%), where a baseline that always answers with the most frequent templates
does so in 10.3%; within the first five ranks it is 68.0% against 48.8%. A model that reads only the first
token of a goal is no better than the baseline.

<p align="center">
  <img src="assets/readme/guide-learning-curves.svg" alt="How the training of the model with mean pooling went, in five panels, one for each seed: training loss and validation loss by epoch, with a mark at the epoch whose model was kept, which is epoch 7, 19, 7, 15 and 30. The training loss falls throughout. In four panels the validation loss turns up after the mark; in the fifth it is still falling at the last epoch." width="780">
</p>

These are measurements on one small library. Ordering tactics well makes a search that would succeed succeed
sooner; it does not make a search succeed that would fail, and a search never fails because of its guide: if
the service stops answering, the search goes on in its own order and the record says so.
[`docs/guides/guided-search.md`](docs/guides/guided-search.md) has the training, the options and the numbers.

## Review that Lean checks

`maithic review` reads Lean files for unfinished work, axioms, restated theorems, missing documentation and
names out of convention, and every one of these is read from what Lean elaborated. With `--suggest` it also
tries to make proofs simpler, and it shows an edit only after Lean has compiled it.

<p align="center">
  <img src="assets/readme/review-rules.svg" alt="A table of eight rules. sorry: a proof that contains sorry or rests on one, an error, read from Lean. axiom: a declaration that rests on an axiom beyond Lean's logic, a warning, from Lean. duplicate: a theorem that states what another already states, a warning, from Lean. docstring and naming: notes, from Lean. automation: a proof that one call of automation replaces, and unneeded-step: a step that can be deleted, both suggestions that Lean accepted. suppression: a suppression comment without a reason, a warning, from the policy." width="900">
</p>

With `--suggest`, on a file whose proofs can be made simpler:

<p align="center">
  <img src="assets/readme/term-review.svg" alt="A terminal session: maithic review reports a proof that one call of omega replaces and a step that is not needed, each edit accepted by Lean, and a suppression comment without a reason." width="703">
</p>

A policy sets which rules run and how serious each is; a comment above a declaration suppresses a rule there,
and has to say why. A model may add notes with `--ai`, and cannot remove or lower anything Lean found. The exit
status is 7 when a finding is an error, so a review can gate a merge. This is the review of a file with
unfinished proofs, each finding with its place, its rule and the line it is about:

<p align="center">
  <img src="assets/readme/term-review-errors.svg" alt="A terminal session: maithic review of a file with unfinished proofs lists each finding with its place, severity, rule and the line of source, and ends with a summary." width="703">
</p>

A report can also be read as counts, by rule and by how serious each finding is:

<p align="center">
  <img src="assets/readme/review-findings.svg" alt="Bar chart of the findings of a review by rule, split by severity: seven notes about missing documentation, three errors for unfinished proofs, three warnings for theorems that restate another, two for axioms, and one finding each for naming, a faulty suppression and the two kinds of suggested edit." width="780">
</p>

[`docs/guides/review.md`](docs/guides/review.md) has the rules, the policy and the reports.

## Repairing proofs that stopped compiling

A library is upgraded and a hundred proofs break. Most of the breakages are not about mathematics: a lemma was
renamed, a name was deprecated, a tactic closes less than it used to. `maithic mend` finds what is broken, sorts
it by cause, and repairs it with the cheapest remedy that works. An edit is kept only when Lean accepts it, and
a model may change a proof and never a statement.

<p align="center">
  <img src="assets/readme/term-mend.svg" alt="A terminal session: maithic mend scan lists five breakages of a Lean file after an upgrade, each with its class; maithic mend repair then repairs three by rule and two with a model, every repair compiled by Lean, and reports that Lean accepts the whole file." width="816">
</p>

Rules come first and need no model: a deprecated name is replaced by the successor Lean names, a name that is
gone by its entry in a table of renames, and a call such as `exact?` by what Lean suggested. A model is asked
only about what is left, for the smallest change to the proof that compiles, and the statement Lean prints for
the repaired theorem must be the statement it printed before. What the model cannot repair, the prover tries to
prove again from nothing.

<p align="center">
  <img src="assets/readme/mend-remedies.svg" alt="A table of five breakages of one Lean file after an upgrade, each taken by the cheapest remedy that works, with one column for each of three stages: rules, which need no model; a model, which is asked for the smallest change to a proof; and the prover, which proves a statement again from nothing. The breakage of sum_comm, an unknown constant Nat.add_commutes, is repaired by the rule rename from a table of renames. The breakage of twice_add_zero, a deprecated name, is repaired by the rule rename from Lean's own message. The breakage of le_succ_self, a call that printed a suggestion, is repaired by the rule suggestion. For the breakages of twice_add, where simp made no progress, and of add_zero_eq, a type mismatch, no rule applies, and a model repairs each. The prover is not needed. Below the table, the four conditions under which an edit is kept: Lean compiles it where the theorem stands, the statement Lean prints is the one it printed before, no new breakage appears, and at the end Lean reads the whole file once more." width="900">
</p>

What each stage is worth shows when the same file is repaired three times, with one more stage each time:

<p align="center">
  <img src="assets/readme/mend-repair-stages.svg" alt="Three repair runs of the same file with five breakages. With the rules alone three are repaired and two are left. With the rules and the prover four are repaired and one is left. With the rules, a model and the prover all five are repaired and the file compiles." width="780">
</p>

`maithic mend minimize` goes the other way and takes out what a proof does not need, one step and one lemma at a
time. A removal is kept when Lean accepts the proof, the theorem states what it stated, and it rests on no
axiom it did not rest on before:

<p align="center">
  <img src="assets/readme/term-mend-minimize.svg" alt="A terminal session: maithic mend minimize shortens the proofs of a file. Four of six proofs get shorter, each shorter proof compiled by Lean with the same statement and no new axiom, and one is left alone because the shorter proof would need axioms the original does without." width="823">
</p>

The third condition is why one proof in the picture is left alone: `by omega` would prove it at the price of
three axioms that its three-line proof does without.
[`docs/guides/repairing-proofs.md`](docs/guides/repairing-proofs.md) has the classes of breakage, the stages and
the options.

## Planning a formalization and tracking it

A large proof is taken apart before it is written: definitions, lemmas, and what rests on what. A *blueprint* is
that plan as a Markdown file. `maithic blueprint plan` drafts one from an argument in words, `skeleton` turns it
into Lean in which every proof is still `sorry`, and from then on `status` reports where the work stands by asking
Lean.

<p align="center">
  <img src="assets/readme/blueprint-pipeline.svg" alt="Four steps in a row. Plan: a model takes an argument in words apart into nodes, and the answer is put in order before it is written. Skeleton: every statement is written as Lean with sorry for a proof, and Lean compiles the file. Status: Lean reports each node as planned, stated, proved or done. Next: the nodes that can be proved now are listed, the most useful first." width="900">
</p>

This is a blueprint of eight nodes, part of the way through:

<p align="center">
  <img src="assets/readme/blueprint-status.svg" alt="The dependency graph of a blueprint of eight nodes, coloured by status. Two definitions and one lemma are done. Two lemmas are stated and ready to prove. One theorem is proved but rests on a lemma that is only stated. One lemma is stated and waits for another, and one corollary is planned. Dashed arrows mark dependencies of proofs only." width="648">
</p>

The picture is drawn by the command from what Lean reported. A node is *stated* when its declaration exists with
`sorry` in it, *proved* when its own proof is complete, and *done* only when everything it rests on is complete
too. The theorem in dark blue was proved first, on a lemma that is still open: it looks finished in the file, and
Lean knows it is not.

<p align="center">
  <img src="assets/readme/blueprint-states.svg" alt="Four states in a row with arrows between them. Planned: no statement in Lean yet. Writing the statement in Lean makes a node stated: the declaration exists and its proof contains sorry. Finishing its own proof makes it proved: its proof is complete, and something beneath it is not. Finishing everything it rests on makes it done." width="900">
</p>

On the command line the same report is a table:

<p align="center">
  <img src="assets/readme/term-blueprint.svg" alt="A terminal session: maithic blueprint status lists the eight nodes of a blueprint with what Lean reports of each: three are done, one is proved on lemmas that are not, three are stated and one is planned; two of the stated ones are ready to prove. maithic blueprint next then names those two, the one that most rests on first." width="696">
</p>

`maithic blueprint next` answers the question a contributor has: what can be worked on now, and which piece
unblocks the most. Snapshots of the status over time become the figure below; `--strict` makes a finished
blueprint a condition of continuous integration.

<p align="center">
  <img src="assets/readme/blueprint-progress.svg" alt="Four horizontal bars, one for each snapshot of a blueprint of eight nodes, divided by status. At the skeleton two nodes are done and six stated. Half done: three done, one proved, three stated, one planned. With one proof left: five done, two proved, one stated. Finished: all eight done." width="780">
</p>

The plan itself can come from a model. `blueprint plan` takes an argument in words apart, and what the model
answers is put in order before it is written: here an identifier was made plain and a reference to a node that
does not exist was dropped, and both are reported.

<p align="center">
  <img src="assets/readme/term-blueprint-plan.svg" alt="A terminal session: maithic blueprint plan takes an argument in words apart into two definitions, four lemmas, a theorem and a corollary, with what each rests on, and notes the two things it put in order in the answer. maithic blueprint skeleton then writes the eight declarations as Lean, each with sorry for a proof, and Lean compiles them." width="809">
</p>

[`docs/guides/blueprints.md`](docs/guides/blueprints.md) has the file format, the planner and what each status
means.

## Finding what is already there

Half of proving is knowing which lemma exists. `maithic search` finds declarations by the words of their names,
statements and documentation, with `add comm` matching `Nat.add_comm` and a statement that contains
`a + b = b + a`; with `--lean` it also asks Lean about everything in scope.

<p align="center">
  <img src="assets/readme/term-find.svg" alt="A terminal session: maithic search answers the words sorting keeps the elements with theorems about insertion sort, the first of them the theorem that insertion sort keeps the elements, each with its statement, its documentation and the words that matched. A second search, for add comm, is also put to Lean and returns the commutativity of addition for integers, natural numbers and rationals from Lean's own library." width="703">
</p>

A name, a formula and a sentence are brought to one vocabulary before anything is compared, which is why a query
in words finds a statement written in symbols:

<p align="center">
  <img src="assets/readme/search-words.svg" alt="A table of six rows. The name Nat.add_comm becomes the words nat, add, comm. The statement a + b = b + a becomes add, eq, add: notation is called what Lean's lemma names call it, and single letters, which are variables, are dropped. The name insertionSort becomes insertion, sort. x ∈ xs ++ ys becomes mem, xs, append, ys. The sentence addition is commutative becomes add, comm. The query sorting keeps the elements becomes sort, keeps, mem." width="900">
</p>

Every hit says which retriever found it and why. Up to three retrievers answer, and their rankings are merged by
rank, so that no score of one has to be compared with a score of another:

<p align="center">
  <img src="assets/readme/search-fusion.svg" alt="Three cards at the top, one for each retriever. Lexical: BM25 over the words of names, statements and documentation, with a word in a name weighing most. Name: the query read as a name or a part of one. Lean: with the option lean, Lean's own environment is searched for names that hold every word. Arrows lead from each to one card below: the rankings are merged by reciprocal rank fusion, where a declaration scores one over sixty plus its rank in every ranking that holds it, and an exact name weighs double." width="900">
</p>

The search is measured like any retrieval system. On the sixteen queries the repository keeps, the first hit is a
relevant declaration for fifteen, and the share of the relevant declarations that are found rises from 78 percent
at the first result to 97 percent at ten:

<p align="center">
  <img src="assets/readme/search-recall.svg" alt="Line chart of recall at 1, 3, 5 and 10 results for two retrievers on the sixteen queries the repository keeps, with bands for 95 percent bootstrap intervals. The lexical retriever rises from 78 percent at the first result to 97 percent at ten; the retriever that takes the query for a name stays at 6 percent." width="780">
</p>

[`docs/guides/finding-declarations.md`](docs/guides/finding-declarations.md) explains the ranking and the
evaluation.

## Oracles and certificates

Some goals are better computed than searched for. A solver can decide a system of inequalities at once, but a
solver is a large program and its answer is only a claim. The oracles of Maithic therefore return more than an
answer: a **certificate**, a small piece of data from which the fact can be checked by arithmetic alone.

<p align="center">
  <img src="assets/readme/oracle-certificates.svg" alt="A goal goes to an oracle in Julia, which searches and returns a certificate with exact numbers. The certificate is checked exactly before it is written and its claim is checked again by Lean's kernel. Four families are shown with an example goal, the certificate and the check: linear arithmetic with multipliers, polynomial equations with cofactors, real inequalities with boxes and enclosures, and discrete facts with Pratt proofs." width="900">
</p>

```julia
julia> using MaithicOracle

julia> outcome = prove_polynomial(["x + y = 3", "x * y = 2"], "x^2 + y^2 = 5");

julia> claim(outcome.certificate)
"∀ x y : ℚ, x + y = 3 → x * y = 2 → x ^ 2 + y ^ 2 = 5"

julia> print(lean_check(outcome.certificate; name = "sum_of_squares"))
theorem sum_of_squares (x y : Rat) : (x ^ 2 + y ^ 2 - 5 : Rat) = (x + y + 3) * (x + y - 3) + -2 * (x * y - 2) := by grind

julia> prove_polynomial(String[], "(x + y)^2 = x^2 + y^2")        # a false goal gets a counterexample, not a shrug
Refuted(x = 1, y = 1)
```

An oracle answers *proved* with a certificate, *refuted* with an exact counterexample, or *unknown* with the
reason; unknown is never a verdict. Every number in a certificate is exact and written as text. The statements
that the Julia package writes for Lean are in [`fixtures/oracle/lean/Checks.lean`](fixtures/oracle/lean/Checks.lean),
and the kernel accepts all of them:

```console
$ maithic check fixtures/oracle/lean/Checks.lean
theorem                        status    axioms or reason
-----------------------------  --------  -------------------------------------
discrete_bezout                verified  no axioms
discrete_factorization_fermat  verified  no axioms
linear_integer_parity          verified  propext, Quot.sound
polynomial_symmetric           verified  propext, Classical.choice, Quot.sound
...
19 of 19 theorem(s) verified, 0 error(s), 0 warning(s), 19 declaration(s) in 467 ms
```

From the command line, `maithic oracle list` names the oracles with what each takes, and `maithic oracle solve`
sends one a goal:

<p align="center">
  <img src="assets/readme/term-oracle.svg" alt="A terminal session: maithic oracle list prints the three oracles with the goals and number types each takes, and maithic oracle solve proves a goal of linear integer arithmetic and prints the claim, the name of its certificate and the Lean script that replays it." width="823">
</p>

`solve` also takes a theorem of a Lean file. The goal of the theorem is exported with Lean's own names, and the
certificate comes back as a tactic call for the verified checker:

<p align="center">
  <img src="assets/readme/oracle-handoff.svg" alt="The command maithic oracle solve reads the theorem no_point from a Lean file. Its goal is exported with the integer variables x and y and the hypotheses h1 and h2. The linear oracle in Julia, which is not trusted, answers with multipliers: one times x plus 2 y at most 3, plus one times minus x minus 2 y at most minus 4, gives 0 at most minus 1. The certificate becomes a call of the tactic maithic_linear_int, which Lean's kernel checks where sorry stood. Below, the three answers of an oracle: proved with a certificate, refuted with an exact point, as for the theorem wrong at x equal to 0 and y equal to 0, and unknown with the reason." width="900">
</p>

```console
$ maithic oracle solve examples/gap.lean --theorem no_point --project lean
linear  proved  (linear over int, 472 ms)
  claim:        ∀ x y : ℤ, 2 * x + 4 * y < 7 → x + 2 * y ≥ 4 → False
  certificate:  linear.farkas, 30ee6df090d61d8e
  in Lean:      maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h₁, h₂]

$ maithic oracle solve examples/gap.lean --theorem wrong --project lean
linear  refuted  (linear over int, 240 ms)
  fails at:     x = 0, y = 0
```

The other answers are as exact about what they are: a point where the goal fails, a reason why nothing was
decided, and a certificate that Lean cannot check yet, which is said in so many words:

<p align="center">
  <img src="assets/readme/term-oracle-answers.svg" alt="A terminal session: three goals sent to the oracles. A linear goal over the rationals is refuted with the exact point x = 3, y = 0. A linear goal over the integers is answered with unknown, and the reason: the hypotheses and the negated goal have rational solutions, and no integer one was found nearby. A polynomial goal is proved with a certificate, and the command says that Lean has no checker for this kind of certificate yet." width="783">
</p>

Put where `sorry` stood, the script is a proof like any other, and `maithic check` reports the theorem verified.
`maithic prove --oracle` asks the oracles by itself when the portfolio fails on an arithmetic goal. Today only
certificates for linear integer goals come with a script, and those are goals that `omega` decides as well; the
hand-off earns its place with the checkers for the other certificates, which are on the
[roadmap](ROADMAP.md).

Certificates of primality and of real inequalities have no checker in core Lean yet; they are checked exactly by
the oracle and are not reported as verified. [`docs/concepts/oracles.md`](docs/concepts/oracles.md) has the whole
picture and [`docs/reference/oracle-format.md`](docs/reference/oracle-format.md) the formats.

## Checkers with proofs

A certificate is only as good as what checks it. For propositional logic and for linear arithmetic over the
integers the checker is a small Lean function, and its soundness is a theorem of the Lean library: the kernel
runs the checker, and the theorem turns "it returned `true`" into a proof of the goal. Whatever found the
certificate, a solver, a model or a person, is never trusted.

<p align="center">
  <img src="assets/readme/verified-checkers.svg" alt="A goal with hypotheses p and p implies q is read as a formula. A search that is not trusted writes a proof, here the single line 4 0 1 2 3 0. The kernel runs the checker on the proof and gets true, and the soundness theorem, proved once, concludes the goal. Three checkers are shown with what each checks and what is proved about it: the truth table behind maithic_taut, clausal proofs behind maithic_sat and maithic_lrat, and linear integer certificates behind maithic_linear_int." width="900">
</p>

```lean
import Maithic.Oracle.Logic

-- Search, then check: the kernel runs the verified checker on the proof that the search wrote.
example (p q r s : Prop) (hpq : p → q) (hqr : q → r) (hrs : r → s) : p → s := by maithic_sat

-- A proof given as text, in the form solvers write. `maithic_cnf` prints the problem it must refute.
example (p q : Prop) (hp : p) (hpq : p → q) : q := by maithic_lrat "4 0 1 2 3 0"

-- A certificate of linear integer arithmetic: rows, multipliers, and the hypothesis behind each row.
example (x y : Int) (h₁ : 2 * x + 4 * y < 7) (h₂ : x + 2 * y ≥ 4) : False := by
  maithic_linear_int [x, y] [(1, ⟨[1, 2], .le, 3⟩), (1, ⟨[-1, -2], .le, -4⟩)] using [h₁, h₂]
```

A goal that does not follow is answered with truth values under which it fails, and a proof with a flaw is
refused at its first bad step:

```text
maithic_sat: the goal does not follow by propositional reasoning; it fails when
  p := True
  q := False

maithic_lrat: step 0: clause 4: unit propagation along the hints ends without a conflict
```

The library's examples include four pigeons and three holes, closed from the fifteen steps that the solver
CaDiCaL wrote for the problem. The linear oracle writes the `maithic_linear_int` calls itself, and
[`fixtures/oracle/lean/Replay.lean`](fixtures/oracle/lean/Replay.lean) goes through `maithic check` on every run
of `make e2e`. [`docs/guides/logic-checkers.md`](docs/guides/logic-checkers.md) says what is proved and what is
only checked.

## A neural network, with the kernel as judge

A verifier for neural networks computes in floating point, and its verdict is a claim. `maithic nnv verify` has
the verifier make the claim and Lean's kernel check it against the exact network.

<p align="center">
  <img src="assets/readme/nnv-flow.svg" alt="Four steps in a row. A network and a property are given as JSON with exact numbers. The verifier, written in Julia, propagates intervals through the network with outward rounding and makes a claim with evidence: a box for every layer, or a counterexample. The claim is written as a Lean theorem with the exact rational weights of the network. Lean's kernel runs a checker that is proved sound, or evaluates the exact network at the counterexample, and only then is the status verified or refuted." width="900">
</p>

Two properties, one that holds and one that fails:

<p align="center">
  <img src="assets/readme/term-nnv.svg" alt="A terminal session: maithic nnv verify decides two properties of small neural networks. For the first the verifier claims that the outputs stay within the claimed box, and Lean's kernel accepts the theorem that says so. For the second the verifier claims a violation, the input 3 mapped to 3, outside the box, and Lean's kernel accepts the theorem that the property fails." width="830">
</p>

When the verifier says the property holds, it hands over a box for every layer, and the kernel runs a checker on
them that is proved sound in Lean. When it says the property fails, it hands over an input, and the kernel
evaluates the exact network there. Either way the status is *verified* or *refuted* only after the kernel accepted
a theorem; a claim that Lean rejects is reported as failed and settles nothing.

The method is sound and not complete, and one small network shows both. It computes the absolute value as
`relu x + relu (-x)`. On inputs from -2 to 3 it never outputs more than 3, and bounds that follow each neuron on
its own can only prove 5:

<p align="center">
  <img src="assets/readme/nnv-intervals.svg" alt="The network that computes the absolute value, as relu of x plus relu of minus x, on inputs from minus 2 to 3. After the first layer, relu of x lies between 0 and 3 and relu of minus x between 0 and 2. Their sum is bounded by 0 and 5, although the network never outputs more than 3, because the two are never large together. Below, on a number line from 0 to 6: the true outputs cover 0 to 3, the proved box covers 0 to 5. The claim 0 to 6 is verified, because the proved box lies inside it. The claim 0 to 2 is refuted, because the input 3 gives 3. The claim 0 to 4 is true and stays unknown, because 5 is more than 4." width="900">
</p>

So the same network gets three answers, one for each claim. The third is *unknown*: nothing is claimed, Lean is
not asked, and the exit status says that nothing was verified.

<p align="center">
  <img src="assets/readme/term-nnv-unknown.svg" alt="A terminal session: maithic nnv verify decides two more properties of the absolute-value network on the inputs from -2 to 3. That the output stays within 0 and 6 is verified, and Lean's kernel accepts the theorem. That it stays within 0 and 4 is answered with unknown: the bounds are too loose to show the property and no tested point leaves the box, so Lean is not asked and nothing is claimed." width="802">
</p>

The four facts of the picture are theorems of the Lean library, in
[`Maithic.NN.Examples.AbsoluteValue`](lean/Maithic/NN/Examples/AbsoluteValue.lean): the outputs stay within
[0, 6]; they do not stay within [0, 2]; the checker does not accept [0, 4]; and the outputs do stay within [0, 3],
which is proved there by cases on the sign of the input, where no certificate of this kind can reach. The method
is exact for the network and the boxes that are given and scales to small networks;
[`docs/guides/verifying-networks.md`](docs/guides/verifying-networks.md) says what it shows and what it does not.

## Algorithms: drafted, tested, proved, measured

An algorithm earns trust in steps, and the laboratory is exact about which step a function has reached.

<p align="center">
  <img src="assets/readme/algo-ladder.svg" alt="Five blocks that rise to the right. Compiles: Lean elaborates the definition, so it is total and well typed. Passes its examples: Lean runs it on your examples. Agrees with a reference: the compiled program and an independent implementation give the same answers on generated inputs. Meets its specification: a theorem checked by the kernel says what it returns for every input. Stays within a bound: a theorem bounds how much work it does." width="900">
</p>

The first two steps are one command:

<p align="center">
  <img src="assets/readme/term-algo-synth.svg" alt="A terminal session: maithic algo synth asks a model for a run-length encoding function. The first draft does not compile because Lean cannot see that it terminates. The correction compiles and gets three of five examples right, and the two wrong answers are shown. The next correction gets all five right and is accepted." width="856">
</p>

**Drafted and tested.** `maithic algo synth` takes a task, a description with examples, and asks a model for a
Lean function. Lean compiles each draft and runs the examples; an error or a wrong answer goes back to the model
word for word. The function that comes out has passed its examples, and the command says exactly that.

<p align="center">
  <img src="assets/readme/synth-loop.svg" alt="Four steps in a row with a path that leads back from the last three to the first. Draft: a model writes one Lean definition for the task. Screen: the draft has to be one definition of the function that was asked for, with nothing that could act; a refused draft never reaches Lean. Compile: Lean elaborates the definition. Run: Lean evaluates every example and compares the value with what was expected. Whatever goes wrong goes back to the model with the draft, for a correction, a bounded number of times." width="900">
</p>

**Proved.** [`Maithic.Algo.Lab.RunLengths`](lean/Maithic/Algo/Lab/RunLengths.lean) takes the function from the
picture as the model wrote it and proves that decoding its result gives back the list, that no run is empty and
that neighbouring runs differ, and that these three properties have exactly one solution. The model's draft is
that solution.

**Tested from outside.** A proof does not say that the compiled program is the definition. `maithic algo
differential` sends generated inputs to the compiled Lean code of nineteen verified algorithms and to independent
implementations in Julia, and reports every answer that differs. There are none.

<p align="center">
  <img src="assets/readme/differential.svg" alt="On the left, inputs are generated from a seed: lists, sorted lists, arrays, graphs and strings. The same requests go to two sides: the program maithic-algo, compiled by Lake from the verified Lean library, and reference implementations in Julia that share no code with it. On the right a judge accepts or rejects each answer: it compares, and where several answers are right, as for a path in a graph, it checks the answer instead." width="900">
</p>

A run with ten generated requests for each algorithm:

<p align="center">
  <img src="assets/readme/term-algo-differential.svg" alt="A terminal session: maithic algo differential sends ten generated requests for each of nineteen verified algorithms to the compiled Lean program and to reference implementations in Julia. No answer differs: 190 cases, and every answer is accepted by its reference." width="609">
</p>

**Measured.** For four algorithms Lean also counts comparisons and proves a bound on the count. The figure lays
the counts of the compiled code beside the proved bounds.

<p align="center">
  <img src="assets/readme/algo-growth.svg" alt="Four panels on logarithmic axes, one for each algorithm, with input sizes from 8 to 512. In each, points mark the comparisons the compiled Lean code made and a line marks the proved bound. For linear search and insertion sort the points lie on the line. For binary search and merge sort they lie just below it. The measured growth is linear, logarithmic, quadratic and linearithmic." width="780">
</p>

No count exceeds its bound, and the growth that R fits to the counts is the growth of each bound. A fit describes
the inputs that were measured; it is the proof that covers the rest. For insertion sort both ends of the range are
theorems too, in [`Maithic.Algo.Cost.Extremes`](lean/Maithic/Algo/Cost/Extremes.lean): a list in falling order
costs exactly `n (n - 1) / 2` comparisons, a sorted list exactly `n - 1`, and no list costs less.
[`docs/guides/algorithm-lab.md`](docs/guides/algorithm-lab.md) walks through all four steps.

## Model providers and keys

Every part that asks a model builds one request that names no provider, and the gateway turns it into the call
that the chosen provider expects:

<p align="center">
  <img src="assets/readme/providers.svg" alt="Four cards. Anthropic: the Messages API at api.anthropic.com, with the schema of the answer in output_config.format. OpenAI: the Responses API at api.openai.com, with a strict schema in text.format and store false. xAI: the Responses API at api.x.ai by default, chat completions when the registry selects them. Local: chat completions on the local machine, with no key. Every caller builds one provider-independent request and receives one provider-independent response or a typed error." width="900">
</p>

| Provider | Setting | Key variable | API |
|---|---|---|---|
| Anthropic | `--provider anthropic` | `ANTHROPIC_API_KEY` | Messages |
| OpenAI | `--provider openai` | `OPENAI_API_KEY` | Responses |
| xAI | `--provider xai` | `XAI_API_KEY` | Responses, or chat completions |
| A server on your machine | `--provider local` | none | chat completions |

The provider, the quality tier (`fast`, `balanced`, `best`) and the model can be set in `maithic.toml`, by
`MAITHIC_*` variables or by flags. `maithic models` lists what each tier means; the list is the file
[`config/models.toml`](config/models.toml), which you can replace, and `maithic auth status` shows which keys are
configured without printing one:

<p align="center">
  <img src="assets/readme/term-models.svg" alt="A terminal session: maithic models lists four providers, anthropic, openai, xai and local, with the API each speaks, the model of the fast, balanced and best tier, and the endpoint. maithic auth status shows for each provider whether a key is set and the environment variable it is read from, and prints no key." width="870">
</p>

A setting can come from six places, and a later one overrides an earlier one. `maithic config show` prints every
setting with its value and the place it came from:

<p align="center">
  <img src="assets/readme/config-layers.svg" alt="Six bands from the lowest to the highest priority: built-in defaults; the system file /etc/maithic/maithic.toml; the user file config.toml in the Maithic home directory; the project file maithic.toml, found at or above the current directory; MAITHIC environment variables; and command-line flags. A later layer overrides an earlier one." width="900">
</p>

The settings of this repository as the command reports them, with the gateway mode taken from the environment:

<p align="center">
  <img src="assets/readme/term-config.svg" alt="A terminal session: maithic config show prints every setting with its effective value and the layer it came from. Most values come from the defaults; the gateway mode comes from the environment variable MAITHIC_GATEWAY_MODE." width="550">
</p>

**Keys.** A key is read at the moment of a call. It is never written to a configuration file, a log or a
cassette, and Lean worker processes are started without it. `maithic auth login` stores one in a file only you
can read.

<p align="center">
  <img src="assets/readme/key-handling.svg" alt="Three steps in a row. A key is looked for in three places, in order: passed explicitly, the environment variable of the provider, and the file credentials.toml in the Maithic home directory. It is handed to curl on its standard input and never on a command line. Errors, logs and cassettes pass through a scrubber that removes the key in use and anything shaped like a key." width="900">
</p>

**Modes.** The gateway answers a model call in one of five ways. Tests and continuous integration use `replay`,
which never opens a connection:

<p align="center">
  <img src="assets/readme/gateway-modes.svg" alt="Five cards, one for each way the gateway can answer a model call. Live calls the provider. Record calls the provider and saves the exchange as a cassette. Replay answers from cassettes only, and a request with no cassette is an error. Mock returns a canned answer chosen by the purpose of the request. Off makes every call fail, and commands fall back to work that needs no model. Live and record need a key; replay, mock and off need neither a key nor a network." width="900">
</p>

**Cassettes.** A recorded exchange is found again by what was asked. The same request gives the same file name
on every machine, and a request that was never recorded is an error in replay mode, never a silent call:

<p align="center">
  <img src="assets/readme/cassette-key.svg" alt="Three steps. First the request is made canonical: provider, model, purpose, system prompt, messages, output limit, temperature and schema. Second its SHA-256 digest is taken, and the digest is the name of the file. Third the cassette, a JSON file, holds the request, the response and an origin, recorded or authored; it never holds a key or a header." width="900">
</p>

[`docs/reference/gateway.md`](docs/reference/gateway.md) has the providers, the modes, the cassettes and the
prompts.

## Commands

One command, `maithic`, with forty-nine subcommands in seven families:

<p align="center">
  <img src="assets/readme/command-families.svg" alt="Seven bands of commands. Prove and check: prove, tree, check. State and test: formalize, refute, council. Plan, find and read: blueprint with six subcommands, search, and intel with seven. Repair and review: mend scan, repair and minimize, and review. Compute with certificates: oracle list and solve, nnv verify, algo synth and differential. Data and measurement: corpus harvest and verify, guide info, rank and train, bench run and gate. Setup: doctor, config, auth, models, bridge selftest, help and version." width="900">
</p>

| Command | What it does |
|---|---|
| `maithic prove FILE` | Proves the theorems of a file that contain `sorry`, or the ones named with `--theorem`. |
| `maithic check FILE` | Checks a file from its imports and reports the status of every theorem. |
| `maithic tree FILE` | Shows the record of a step-wise search, step by step, and draws it. |
| `maithic refute FILE` | Looks for counterexamples to unproved statements and has Lean confirm each one; finds vacuous statements. |
| `maithic formalize [PROBLEM]` | States a problem written in words as a Lean theorem; Lean rejects the vacuous and the false candidates. |
| `maithic council FILE` | Puts a researcher, a prover, a critic and a verifier to work on a theorem, with Lean as the only judge. |
| `maithic review PATH` | Reviews Lean files, and suggests only the edits that Lean accepted. |
| `maithic mend scan` · `repair` · `minimize` | Finds what stopped compiling, repairs it by rule, with a model and with the prover, and shortens proofs. |
| `maithic intel scan` · `show` · `deps` · `dependents` · `health` · `stats` | Reads a project into a graph of its declarations and answers questions about it. |
| `maithic corpus harvest` · `verify` | Writes the proofs of a project as a dataset of states, tactics and premises, and checks a dataset. |
| `maithic guide train` · `info` · `rank` | Trains a tactic model on a dataset, describes it, and shows how it orders tactics on a goal. |
| `maithic blueprint check` · `plan` · `skeleton` · `status` · `next` · `graph` | Plans a formalization as nodes that rest on each other, writes its Lean skeleton, and reports from Lean what is stated, proved and done. |
| `maithic search QUERY` | Finds declarations by the words of their names, statements and documentation, in your files and in Lean's scope. |
| `maithic oracle list` · `solve` | Lists the oracles; sends a goal, or the goal of a theorem, to one. |
| `maithic nnv verify MODEL PROPERTY` | Decides a property of a neural network and has Lean's kernel check the answer. |
| `maithic algo synth` · `differential` | Drafts a Lean function for a task and judges it by Lean and by examples; tests the compiled verified algorithms against independent references. |
| `maithic bench run` · `gate` | Runs the prover on a benchmark suite; holds a run against a stored baseline and fails on a regression. |
| `maithic doctor` | Checks toolchains, the Bridge, the provider and keys, and prints remedies. |
| `maithic config show` | Shows the effective configuration and where each value came from. |
| `maithic auth status` · `login` · `logout` · `check` | Manages and tests provider keys. |
| `maithic models` | Lists providers and the model of each tier. |
| `maithic bridge selftest` | Starts the Lean bridge and runs its self-test. |
| `maithic help exit-codes` · `help status` | Prints the reference tables. |

Every command accepts `--json` and then prints one document that names its schema, so that a script or another
language can read a result without parsing text:

<p align="center">
  <img src="assets/readme/term-json.svg" alt="A terminal session: maithic check with the option json prints its report as a JSON document that names its schema, maithic.check.report/1, with the file, the counts of errors, warnings and declarations, the theorem even_add_even with its statement, the status failed, the axiom sorryAx and the reason, and the diagnostic of Lean with its line and column." width="543">
</p>

The schema that a document names is a file of the repository, and a gate holds every recorded document to it:

<p align="center">
  <img src="assets/readme/json-contracts.svg" alt="Three steps in a row. A command prints a document with the option json; the document names a schema, such as maithic.prove.run/1. The schema is a file under schemas, at a path that follows from the name. The contracts gate validates every fixture, every recorded Bridge answer and every result the command writes against the schema it names, and the readers in R and Julia check a file before they compute a number from it." width="900">
</p>

Exit codes are stable and documented in [`docs/reference/exit-codes.md`](docs/reference/exit-codes.md): `0` means
every result is verified, `7` means something was not, `8` means a statement was refuted, and `13` means the
regression gate failed. [`docs/guides/scripting.md`](docs/guides/scripting.md) shows a shell script, a Julia
program and an R program that work from the reports and these codes.

<p align="center">
  <img src="assets/readme/exit-codes.svg" alt="Fourteen tiles, one for each exit code of the maithic command: 0 success, 1 general failure, 2 usage error, 3 configuration error, 4 toolchain or runtime missing, 5 provider or authentication error, 6 budget exceeded, 7 abstained or not verified, 8 statement refuted, 9 trust or review policy violation, 10 timeout, 11 cancelled, 12 feature unavailable in this installation, 13 regression gate failed. A command whose outcome is a status exits with 0 only when every result is verified." width="900">
</p>

The command prints both reference tables itself, the status words and the exit codes:

<p align="center">
  <img src="assets/readme/term-help.svg" alt="A terminal session: maithic help status prints the eight status words with the meaning of each, and maithic help exit-codes prints the fourteen exit codes from 0, success, to 13, regression gate failed." width="743">
</p>

An error has a code of the form `MTH-AREA-NNNN`, says what happened in one line, and ends the command with the
exit status of its kind. [`docs/reference/errors.md`](docs/reference/errors.md) lists every code with its remedy.

<p align="center">
  <img src="assets/readme/term-errors.svg" alt="A terminal session: three commands that end with an error, each with a code and its own exit status. A file that does not exist: MTH-CLI-0002, status 2. A setting with a value of the wrong type: MTH-CONFIG-0002, status 3. A model call in replay mode with no cassette for the request: MTH-FORMALIZE-0004, status 5, with the digest of the request." width="836">
</p>

## The Bridge: one Lean process behind every command

Nothing in Maithic reads Lean's mind from its text. The command talks to `maithic-bridge`, a Lean program of this
repository, in JSON-RPC over standard input and output, and every fact about a proof is an answer of that
process:

<p align="center">
  <img src="assets/readme/bridge-session.svg" alt="A sequence diagram between the maithic command, written in OCaml, and maithic-bridge, the Lean process. The command sends handshake and learns the protocol and the Lean version. It sends loadHeader with the imports and receives environment 1. It sends runCommand with the file up to the theorem and receives a report and environment 2. It sends tryCandidates with several candidate proofs and receives, for each, whether it was admitted and a report with the declarations and their axioms. Environments are immutable, so every candidate is judged against the same state. It then releases the environment and shuts the Bridge down." width="900">
</p>

An environment is never changed by what is run on it, so many candidates can be tried on the same one, and a
failed candidate leaves nothing behind. The protocol has sixteen methods in five groups:

<p align="center">
  <img src="assets/readme/bridge-methods.svg" alt="Five bands. Environments: loadHeader, runCommand, checkFile, tryCandidates and releaseEnv run text on an environment and return a report. Proof states: startProof, runTactic, tryTactics, getState and releaseState run one tactic at a time on a goal. Description: extract describes a whole file. Safety: guard says whether untrusted text would be admitted. Housekeeping: handshake, stats, selfTest and shutdown." width="900">
</p>

`maithic bridge selftest` starts the Bridge and has it show that it tells a true statement from a false one,
that it sees an axiom that was slipped in, and that its guard refuses commands that would run something:

<p align="center">
  <img src="assets/readme/term-bridge.svg" alt="A terminal session: maithic bridge selftest starts the Bridge and runs five checks, all ok: the kernel accepts a true statement, the kernel rejects a false statement, the axiom audit sees injected axioms, the guard blocks evaluation commands, and tactics step a proof state to its end." width="510">
</p>

[`docs/reference/bridge-protocol.md`](docs/reference/bridge-protocol.md) specifies every request and answer, and
[`docs/adr/0002`](docs/adr/0002-bridge-with-environment-handles.md) says why the Bridge is built this way.

## What is in the repository

<p align="center">
  <img src="assets/readme/language-roles.svg" alt="Four languages and their roles: OCaml orchestrates, Lean proves, Julia computes, R measures." width="900">
</p>

Each language does the work it is best at, and none of them is decoration: the register of capabilities counts
230 in OCaml, 117 in Lean, 72 in Julia and 62 in R.

<p align="center">
  <img src="assets/readme/register-by-pillar.svg" alt="Bar chart of the 481 capabilities of the register by pillar, each bar divided by the language the capabilities are written in. Fourteen pillars hold 20 capabilities; Ledger holds 22, Prove 33, Gateway 46, Bridge 47 and Oracle 53. OCaml accounts for 230 capabilities, Lean for 117, Julia for 72 and R for 62." width="780">
</p>

The folders at the top of the repository, and what each holds:

<p align="center">
  <img src="assets/readme/repo-map.svg" alt="A list of the top-level folders with what each holds. ocaml: the maithic command, the AI gateway, the Bridge client, the prover and every other orchestrator, and the repository tools. lean: the Bridge server, the tactic suite, the verified libraries and the benchmark suite. julia: eight packages for certificates, the tactic model, network bounds, reference algorithms, graph measures, datasets and embeddings. r: two packages for statistics and figures. schemas: the JSON Schema of every document that crosses a language boundary. config: the model registry, the prompts and the default trust policy. registry: the capability register. fixtures: recorded sessions, cassettes, certificates and results. examples: the small files of the demonstrations. bench: the benchmark manifest, baselines and committed results. docs: guides, concepts, reference pages and design decisions." width="900">
</p>

```text
ocaml/      the maithic command, the AI gateway, the Bridge client, the prover with its step-wise search
            and the pictures of it, the refuter, formalization, the council, repair and review, blueprints,
            the search for declarations, the algorithm laboratory, the network verification job, the
            code graph, the dataset writer, the oracle registry, the benchmark gate, the trust policy,
            the schema validator and the repository tools
lean/       the Bridge server with proof states, the tactic suite, the axiom audit, verified algorithms
            with counted costs and the program that runs them, verified checkers (network bounds and
            properties, propositional proofs, linear integer certificates), tests of statements
            (sampling, vacuity, necessity of hypotheses), the commands that inspect, find and compare
            statements, the tracer of proofs, the benchmark suite
julia/      MaithicOracle (certificates for linear, polynomial, real and discrete goals), MaithicGuide
            (the tactic model, its training and its service), MaithicNNV (bounds and verdicts for neural
            networks), MaithicAlgoRef (reference algorithms and differential tests), MaithicGraphs
            (measures on the code graph), MaithicData (splits and checks of datasets), MaithicEmbed
            (embeddings and search), MaithicCore (what they share)
r/          maithicstats (intervals, comparisons, and readers for every kind of result file),
            maithicviz (figures in one visual style)
schemas/    the JSON Schemas of every document that crosses a language boundary
config/     the model registry, the prompts and the default trust policy
registry/   the capability register: every capability, its status and its tests
fixtures/   Bridge sessions and transcripts, provider exchanges, cassettes, oracle goals and certificates,
            clausal proofs with expected verdicts, trained models with their training records, recorded
            runs, searches, repairs, formalizations, council sessions, blueprint snapshots, network
            jobs, syntheses and measured costs
examples/   the small Lean files of the demonstrations, a blueprint, networks with properties and a task
bench/      the benchmark manifest and committed results
docs/       guides, concepts, reference pages and design decisions
```

The languages meet at documents, and every such document names a schema under [`schemas/`](schemas). A check
that is part of `make hygiene` validates each fixture, each recorded Bridge answer and each result the command
writes against the schema it names; [`docs/reference/schemas.md`](docs/reference/schemas.md) lists them.

What the code depends on is short enough to draw. The OCaml core uses the compiler's own libraries, the Lean
package uses core Lean, and the Julia packages use the standard library and each other:

<p align="center">
  <img src="assets/readme/dependencies.svg" alt="Four tiles. OCaml: the standard library and unix, and nothing else. Lean: core Lean only, with no Mathlib and no other package. Julia: the standard library; the eight packages use each other by path. R: jsonlite to read, ggplot2 and svglite to draw, testthat to test." width="900">
</p>

Six decisions shaped the design, and each is recorded with its reasons under [`docs/adr/`](docs/adr):

<p align="center">
  <img src="assets/readme/design-decisions.svg" alt="Six cards, one for each recorded design decision. One: the OCaml core uses only the compiler's own libraries. Two: the Bridge exposes immutable environments and checks whole declarations. Three: demonstration cassettes are authored, and say so. Four: the four languages each have a floor in the share of code. Five: the Julia and R packages build on the standard distribution. Six: a closed proof state is not a verified theorem." width="900">
</p>

Every capability has an identifier, and a capability counts as done only when a test carries that identifier.
A gate compares the register with the tests on every run:

<p align="center">
  <img src="assets/readme/capability-register.svg" alt="Three steps in a row. The register lists 481 capabilities in 19 pillars, each with an identifier such as P10-07-05, a specification and a status. A test that covers a capability carries its identifier in its name, in each of the four languages. The gate fails when a capability is marked done without such a test, or when a test names an identifier that the register does not have." width="900">
</p>

A file that one language writes for another is a fixture, and fixtures are regenerated, never edited. Each kind
has a target that makes it again:

<p align="center">
  <img src="assets/readme/fixtures-map.svg" alt="A list of ten make targets with what each one regenerates. transcripts: the Bridge transcripts that the OCaml unit tests replay. cassettes: the answers the demonstrations replay. julia-fixtures: the oracle and network certificates that Julia writes for the other languages. lean-fixtures: what Lean says about the demonstration sources. guide-fixtures: the tactic model the repository keeps. run-fixtures: the demonstrations as result files. figures: the benchmark results and the figures drawn from data. shots: the pictures of terminal sessions. docs: the generated reference pages. cap-sync: the capability register and its status page." width="900">
</p>

## Verified libraries

The Lean package contains proofs, not only tools: 980 declarations in 66 files, 307 of them theorems.

<p align="center">
  <img src="assets/readme/library-areas.svg" alt="Bar chart of the 980 declarations of the Maithic Lean library by area, each bar divided into theorems, definitions and other kinds. The logic checkers are the largest area with 233 declarations in 11 files; then the testing of statements with 99, the sorting algorithms with 86, the network certificates with 75 and formalization with 72. In all there are 307 theorems and 455 definitions." width="780">
</p>

Everything below is checked by the kernel on every build, with no `sorry` and no added axiom:

| Module | What is proved |
|---|---|
| [`Maithic.Algo.Sort.Insertion`](lean/Maithic/Algo/Sort/Insertion.lean) | Insertion sort returns a sorted permutation of its input, and is idempotent. |
| [`Maithic.Algo.Sort.MergeSort`](lean/Maithic/Algo/Sort/MergeSort.lean) | Merge sort returns a sorted permutation of its input. A sorted permutation of a list is unique, so merge sort and insertion sort are the same function. |
| [`Maithic.Algo.Sort.Linear`](lean/Maithic/Algo/Sort/Linear.lean) | Linear search returns the first position of the target, or proves its absence. |
| [`Maithic.Algo.Sort.Binary`](lean/Maithic/Algo/Sort/Binary.lean) | Binary search on a sorted array terminates, returns a position that holds the target, and returns none only when the target is absent. |
| [`Maithic.Algo.Sort.MinMax`](lean/Maithic/Algo/Sort/MinMax.lean), [`Dedup`](lean/Maithic/Algo/Sort/Dedup.lean) | The minimum and the maximum of a list are members and bound every member. Removing repeats from a sorted list keeps its members and its order, and leaves no repeat. |
| [`Maithic.Algo.Graph.Fib`](lean/Maithic/Algo/Graph/Fib.lean), [`PrefixSum`](lean/Maithic/Algo/Graph/PrefixSum.lean) | The table and the pair-based computations of the Fibonacci numbers agree with the definition. A sum over a range is the difference of two prefix sums. |
| [`Maithic.Algo.Spec.Perm`](lean/Maithic/Algo/Spec/Perm.lean), [`Search`](lean/Maithic/Algo/Spec/Search.lean) | Two lists are permutations of each other exactly when every value occurs equally often in both, which a function decides. What it means for a search to be right, and that the first position found is unique. |
| [`Maithic.Algo.Sort.Selection`](lean/Maithic/Algo/Sort/Selection.lean) | Selection sort and quicksort return sorted permutations of their input, and so all four sorts of the library are the same function. |
| [`Maithic.Algo.Sort.Match`](lean/Maithic/Algo/Sort/Match.lean) | The search for a pattern in a text returns the first position where the pattern occurs, and none only when it occurs nowhere. |
| [`Maithic.Algo.Graph.Path`](lean/Maithic/Algo/Graph/Path.lean) | A path that the search returns starts and ends where it was asked to and follows edges of the graph. |
| [`Maithic.Algo.Arith.Gcd`](lean/Maithic/Algo/Arith/Gcd.lean), [`Pow`](lean/Maithic/Algo/Arith/Pow.lean) | Euclid's algorithm computes the greatest common divisor. Exponentiation by squaring computes the power, and its modular form the remainder of the power. |
| [`Maithic.Algo.Cost.Comparisons`](lean/Maithic/Algo/Cost/Comparisons.lean) | Bounds on work: linear search compares at most `n` times, binary search looks at no more than `log₂ n + 1` elements, insertion sort compares at most `n (n - 1) / 2` times and merge sort at most `n (log₂ n + 1)` times. |
| [`Maithic.Algo.Cost.Extremes`](lean/Maithic/Algo/Cost/Extremes.lean) | The bound of insertion sort is reached: a list in falling order costs exactly `n (n - 1) / 2` comparisons. A sorted list costs exactly `n - 1`, and no list of `n` elements costs less. |
| [`Maithic.Algo.Lab.RunLengths`](lean/Maithic/Algo/Lab/RunLengths.lean) | A run-length encoder that a model drafted decodes to its input, has no empty run and no equal neighbours, and is the only function with these properties. |
| [`Maithic.NN.Cert.Interval`](lean/Maithic/NN/Cert/Interval.lean) | Rational interval arithmetic is sound, operation by operation. |
| [`Maithic.NN.Cert.Bounds`](lean/Maithic/NN/Cert/Bounds.lean) | If the bound-certificate checker accepts, the bounds hold for the exact semantics of the ReLU network. |
| [`Maithic.NN.Cert.Property`](lean/Maithic/NN/Cert/Property.lean) | If the property checker accepts, every input of the box is mapped into the claimed box; and an input whose exact output lies outside refutes the property. |
| [`Maithic.NN.Examples.AbsoluteValue`](lean/Maithic/NN/Examples/AbsoluteValue.lean) | A network whose outputs stay within [0, 3], a fact that the certificate checker cannot reach: it accepts the claim [0, 6], rejects [0, 4], and the claim [0, 2] is false. |
| [`Maithic.Oracle.Logic.Clause`](lean/Maithic/Oracle/Logic/Clause.lean) | The conversion to clause normal form keeps the truth value of a formula under every assignment. |
| [`Maithic.Oracle.Logic.Table`](lean/Maithic/Oracle/Logic/Table.lean) | The truth table is sound and complete, so validity of a formula is decidable. |
| [`Maithic.Oracle.Logic.Lrat`](lean/Maithic/Oracle/Logic/Lrat.lean) | A clause set with an accepted clausal proof is unsatisfiable. |
| [`Maithic.Oracle.Logic.Resolution`](lean/Maithic/Oracle/Logic/Resolution.lean) | A clause set with an accepted resolution proof is unsatisfiable. |
| [`Maithic.Oracle.Logic.LinearInt`](lean/Maithic/Oracle/Logic/LinearInt.lean) | The rows of an accepted certificate of linear integer arithmetic have no common solution. |
| [`Maithic.Tactic.Auto`](lean/Maithic/Tactic/Auto.lean) | Not a theorem but a tactic: `maithic_auto`, the same portfolio the prover uses, inside the editor. |

The certificate checker is the reason [`MaithicNNV`](julia/MaithicNNV) exists: it computes bounds quickly in
floating point with outward rounding and writes them as exact rational certificates, which the Lean checker then
accepts or rejects. The certificates in [`fixtures/nnv`](fixtures/nnv) are written by the Julia tests and run
through the checker by `make e2e`.

## Benchmark

The *basics* suite is 24 statements of logic, arithmetic and list facts, defined in
[`lean/MaithicBench/Basics.lean`](lean/MaithicBench/Basics.lean) with kernel-checked reference solutions. With no
model, the automation portfolio verifies all 24; the run is committed as
[`bench/results/basics-portfolio.json`](bench/results/basics-portfolio.json) and the integration tests repeat it.

```sh
maithic bench run --no-ai
```

<p align="center">
  <img src="assets/readme/term-bench.svg" alt="A terminal session: maithic bench run with AI switched off proves the 24 problems of the basics suite with the automation portfolio. Every row is a problem with the status verified, the proof that was found and the time it took, and the last line gives 24 of 24." width="597">
</p>

The figures below are drawn from that committed run by the R packages; `make figures` repeats the run and redraws
them. The intervals are long because the groups are small, which is the honest way to show seven problems.

<p align="center">
  <img src="assets/readme/bench-basics-by-tag.svg" alt="Five rows, one for each tag of the basics problems, with a dot at the share verified and a line for its 95 percent Wilson interval. Every dot is at 100 percent: decide 2 of 2, int 3 of 3, list 4 of 4, logic 7 of 7 and nat 8 of 8. The lines are long because the groups are small." width="780">
</p>

How many candidates were tried before one was accepted:

<p align="center">
  <img src="assets/readme/bench-basics-attempts.svg" alt="Bar chart: number of candidate proofs tried before one was accepted, for the verified basics problems." width="780">
</p>

And how long each problem took:

<p align="center">
  <img src="assets/readme/bench-basics-time.svg" alt="Bar chart: seconds per basics problem from request to verified result, including the fresh-process re-check." width="780">
</p>

The suite is deliberately easy. Its purpose is to exercise the whole pipeline with no provider and to catch any
change that makes the prover worse on simple problems. It says nothing about hard mathematics.

Catching such a change is the job of `maithic bench gate`: it holds a run against the baseline stored under
[`bench/baselines`](bench/baselines) and exits with status 13 when a problem that used to be verified no longer
is.

<p align="center">
  <img src="assets/readme/gate-rule.svg" alt="Three steps in a row. A baseline, stored for each suite, records for every problem whether it was verified, how many candidates were tried and how long it took; a run records the same. The two are compared problem by problem: a problem is lost when the baseline verified it and the run did not. The verdict is passed, or failed with the problems that were lost, and the command exits with status 13." width="900">
</p>

The second run in the picture below is a copy of the committed one with a result changed by hand, to show a
failure.

<p align="center">
  <img src="assets/readme/term-gate.svg" alt="A terminal session: maithic bench gate holds a benchmark run against the stored baseline. The committed run passes, with 24 of 24 problems verified as in the baseline. A copy of the run in which one result was changed fails: problem basics-007, which the baseline verified, is not verified, and the command ends with status 13." width="663">
</p>

[`docs/guides/regression-gate.md`](docs/guides/regression-gate.md) has the thresholds and how baselines are stored.

## What the kernel says about this repository

The same packages summarise what `maithic check` and `maithic prove` report. The figures here are drawn from the
result files under [`fixtures/runs`](fixtures/runs): the insertion-sort library, the examples of the logic
checkers, the statements written from oracle certificates, the certificates replayed through the verified
checker, and the demonstration proofs. Every figure takes the same way from a file to a picture:

<p align="center">
  <img src="assets/readme/data-to-figure.svg" alt="Four steps in a row. A result file is written by the maithic command with the option json. It is read and checked in R against its schema, its statuses and its totals before any number is computed. It is summarised with uncertainty: success rates with 95 percent Wilson intervals, bootstrap intervals for other measures, and paired comparisons. It is drawn in one visual style, and make figures redraws every figure from the files in the repository." width="900">
</p>

The axioms that the recorded results rest on:

<p align="center">
  <img src="assets/readme/ledger-axioms.svg" alt="Bar chart of the axioms the recorded results rest on. Propext, Quot.sound and Classical.choice, the three axioms of Lean's logic, account for most; a group of results uses no axiom at all; one unfinished proof depends on sorryAx." width="780">
</p>

And the status of each result:

<p align="center">
  <img src="assets/readme/ledger-trust.svg" alt="Two rows, one for each status among the 60 recorded results, with a dot at its share and a line for its 95 percent Wilson interval: 59 results are verified, and one, an example file whose proof is still to be found, is failed." width="780">
</p>

The one result that is not verified is [`examples/triangle.lean`](examples/triangle.lean) as it ships, with a
`sorry` where its proof belongs. It is the example the prover is shown on: the portfolio fails on it, a first
round of candidates fails, and a repair round that carries Lean's error messages succeeds.

<p align="center">
  <img src="assets/readme/prover-rejections.svg" alt="Bar chart of the candidate proofs that Lean rejected in the recorded prove runs, by the class of Lean's error. Most are tactic failures." width="780">
</p>

Where the time of a proof goes:

<p align="center">
  <img src="assets/readme/prover-time.svg" alt="Stacked bars of seconds per theorem in the recorded prove runs, each divided into the part spent in Lean on candidates and the re-check, and everything else: two theorems proved with no model, one by the portfolio and one by the step-wise search, and one theorem proved with replayed model answers for each of the three providers. Lean takes most of the time in every run." width="780">
</p>

And how many theorems were proved, by provider:

<p align="center">
  <img src="assets/readme/prover-success.svg" alt="Four rows, one for each provider in the recorded prove runs, with a dot at the share of theorems proved and a line for its 95 percent Wilson interval. Every dot is at 100 percent, and every line spans most of the scale, because each group holds one or two theorems." width="780">
</p>

These runs are in replay mode: the model answers come from cassettes written by hand for the demonstration, and
each caption says so. The charts show how the tools report, with intervals as long as five theorems deserve. They
are not a measurement of any model. [`docs/guides/statistics.md`](docs/guides/statistics.md) describes the
packages.

## Development

```sh
make ci          # everything the required workflow runs, in the same order
make test        # unit tests in all four languages
make integration # the real Bridge and the built command
make hygiene     # language share, capability register, schemas and contracts, scans, README images
```

The required workflow has six jobs, and `make ci` runs what they run:

<p align="center">
  <img src="assets/readme/ci-jobs.svg" alt="Six cards. The OCaml job builds the command and the developer tool, runs the unit tests and the repository gates. The Lean job builds the libraries, the Bridge and the tools, failing on warnings, and runs the Lean tests. The Julia job tests the eight packages. The R job installs the two packages and runs their tests. The integration job brings OCaml, Lean and Julia together: the real Bridge, the built command, the oracle engines, and the sessions a user would run. The last card is the gate, which succeeds only when all five did. No job needs a key." width="900">
</p>

The tests are in layers, from one language on its own to the whole command on a fresh clone:

<p align="center">
  <img src="assets/readme/test-layers.svg" alt="Five bands. Unit tests run in each language on its own; the OCaml tests need neither Lean nor a network, because the Bridge is played by a stub that replays recorded transcripts. Scripted Bridge sessions are run three ways: inside the Lean test program, against the built server, and against the stub. Integration tests run the built command against the real Bridge and the Julia engines, and check every JSON answer against its schema. End-to-end recipes run the sessions of the documentation, and the minimal profile with OCaml and Lean only. Repository gates check contracts, mirrored files, images, scans, the capability register and the language shares." width="900">
</p>

`make hygiene` is six gates that look at the repository and not at the code alone:

<p align="center">
  <img src="assets/readme/hygiene-gates.svg" alt="Six tiles, one for each gate of make hygiene. Contracts: every recorded document validates against the schema it names. Sync-check: files that mirror built-in data are up to date. Readme: every image of the README is a file of the repository and has alternative text. Hygiene: no unfinished-work marker, no sorry or axiom in the Lean libraries, no key-shaped string, one version everywhere, no dead link, a copyright header on every source file. Capcheck: every capability marked done has a test that names it. Langshare: at least 85 percent of the code is OCaml, Lean, Julia and R, with a floor for each." width="900">
</p>

The versions are pinned where a build can read them, and they are the versions this release is tested with:

<p align="center">
  <img src="assets/readme/versions-pinned.svg" alt="Five tiles. Lean 4.36.0, pinned in the file lean-toolchain and installed by elan. OCaml 5.5, with dune 3.25, set in the workflow. Julia 1.13, set in the workflow; the packages need 1.11 or later. R 4.5, set in the workflow; the packages need 4.4 or later. Ubuntu 24.04 is the image every job runs on." width="900">
</p>

- Unit tests need neither Lean nor a network: the Bridge is played by a stub that replays recorded transcripts,
  and model calls are served by scripted providers and cassettes.
- Every capability has an identifier such as `P10-07-05`. A test that covers a capability carries its identifier,
  and `make hygiene` fails if the register and the tests disagree. `make cap-sync` updates the register.
- The repository must stay at least 85 percent OCaml, Lean, Julia and R by bytes of code, with a floor for
  each: OCaml 30, Lean 15, Julia 12, R 8. `maithic-dev langshare` measures it against
  [`PROJECT.toml`](PROJECT.toml).
- Fixtures that one language writes for another are regenerated, never edited: `make julia-fixtures`,
  `make lean-fixtures`, `make run-fixtures`, `make transcripts`, `make cassettes`. The tests fail when a
  fixture is stale.
- The figures of this page are drawn from recorded results by `make figures`, and the pictures of terminal
  sessions from what the built command prints, by `make shots`. The diagrams that explain how a part works are
  SVG files written by hand, under [`assets/readme/`](assets/readme).

[`CONTRIBUTING.md`](CONTRIBUTING.md) describes the workflow and the conventions.

## Documentation

- [`docs/install.md`](docs/install.md): toolchains and building.
- [`docs/quickstart.md`](docs/quickstart.md): the first proof, step by step.
- [`docs/concepts/trust.md`](docs/concepts/trust.md): what *verified* means here and what it does not.
- [`docs/concepts/oracles.md`](docs/concepts/oracles.md): computing without trusting, by certificates.
- [`docs/guides/search.md`](docs/guides/search.md): the step-wise proof search, its record and its picture.
- [`docs/guides/testing-statements.md`](docs/guides/testing-statements.md): testing a statement by sampling,
  for vacuity and for the hypotheses it needs.
- [`docs/guides/refuting-statements.md`](docs/guides/refuting-statements.md): counterexamples from the command
  line, each confirmed by Lean.
- [`docs/guides/formalizing.md`](docs/guides/formalizing.md): from a problem in words to a statement that Lean
  has checked for vacuity and for counterexamples.
- [`docs/guides/council.md`](docs/guides/council.md): several roles at work on one theorem, their shared board
  and their budgets.
- [`docs/guides/code-intelligence.md`](docs/guides/code-intelligence.md): a project as a graph of its
  declarations, and the questions it answers.
- [`docs/guides/datasets.md`](docs/guides/datasets.md): proofs as datasets of states, tactics and premises.
- [`docs/guides/guided-search.md`](docs/guides/guided-search.md): training a tactic model and searching with it.
- [`docs/guides/review.md`](docs/guides/review.md): review of Lean code, with edits that Lean accepted.
- [`docs/guides/repairing-proofs.md`](docs/guides/repairing-proofs.md): repairing proofs after an upgrade, and
  shortening them.
- [`docs/guides/blueprints.md`](docs/guides/blueprints.md): a formalization as a plan of nodes, and its status
  according to Lean.
- [`docs/guides/finding-declarations.md`](docs/guides/finding-declarations.md): searching declarations by words,
  and measuring the search.
- [`docs/guides/logic-checkers.md`](docs/guides/logic-checkers.md): the verified checkers and their tactics.
- [`docs/guides/verifying-networks.md`](docs/guides/verifying-networks.md): properties of neural networks, decided
  by a verifier and checked by the kernel.
- [`docs/guides/algorithm-lab.md`](docs/guides/algorithm-lab.md): functions drafted by a model, tested, proved
  and measured.
- [`docs/guides/regression-gate.md`](docs/guides/regression-gate.md): baselines and the gate that guards them.
- [`docs/guides/statistics.md`](docs/guides/statistics.md): summaries and figures from result files.
- [`docs/guides/scripting.md`](docs/guides/scripting.md): the command from a script, from Julia, from R and
  from any other language, by its JSON reports and exit codes.
- [`docs/reference/commands.md`](docs/reference/commands.md): the commands and their options.
- [`docs/reference/configuration.md`](docs/reference/configuration.md): every setting.
- [`docs/reference/gateway.md`](docs/reference/gateway.md): providers, modes, cassettes and prompts.
- [`docs/reference/bridge-protocol.md`](docs/reference/bridge-protocol.md): the protocol of the Lean bridge.
- [`docs/reference/oracle-format.md`](docs/reference/oracle-format.md): goals, certificates and outcomes.
- [`docs/reference/schemas.md`](docs/reference/schemas.md): the schemas and the check that enforces them.
- [`docs/reference/errors.md`](docs/reference/errors.md): error codes and remedies.
- [`docs/adr/`](docs/adr): design decisions and their reasons.
- [`CHANGELOG.md`](CHANGELOG.md): what this release contains, part by part, and [`ROADMAP.md`](ROADMAP.md): what
  comes after it.
- A page for each package, with an example and what it prints:
  [`MaithicCore`](julia/MaithicCore/README.md), [`MaithicOracle`](julia/MaithicOracle/README.md),
  [`MaithicNNV`](julia/MaithicNNV/README.md), [`MaithicGuide`](julia/MaithicGuide/README.md),
  [`MaithicData`](julia/MaithicData/README.md), [`MaithicGraphs`](julia/MaithicGraphs/README.md),
  [`MaithicEmbed`](julia/MaithicEmbed/README.md), [`MaithicAlgoRef`](julia/MaithicAlgoRef/README.md),
  [`maithicstats`](r/maithicstats/README.md) and [`maithicviz`](r/maithicviz/README.md).

## License

Code is released under the [Apache License 2.0](LICENSE); documentation under CC BY 4.0. See [`NOTICE`](NOTICE)
and [`CITATION.cff`](CITATION.cff). Security reports: [`SECURITY.md`](SECURITY.md).
