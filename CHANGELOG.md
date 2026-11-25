# Change log

All notable changes are recorded here. Versions follow semantic versioning.

## 0.1.0

The foundation.

### Added

- The `maithic` command: `prove`, `check`, `bench run`, `doctor`, `config`, `auth`, `models`, `bridge selftest`
  and the reference tables, each with `--json` and stable exit codes.
- The Lean bridge `maithic-bridge`: JSON-RPC 2.0 over standard input and output, environments as handles,
  guarded candidate elaboration, axiom reporting, per-request limits and a self-test.
- The proving loop: the automation portfolio, model rounds with Lean's errors fed back, the candidate screen, the
  trust policy and the fresh-process re-check.
- The AI gateway for Anthropic, OpenAI and xAI, with a model registry, bring-your-own-key handling, five modes,
  cassettes, scripted and mock providers and file-based prompts.
- Lean libraries: the `maithic_auto` tactic and its combinators, the axiom audit with `#maithic_audit`, verified
  insertion sort and linear search, sound rational interval arithmetic, a verified checker for bound certificates
  of ReLU networks, and the basics benchmark suite with reference solutions.
- Julia packages: `MaithicNNV` (interval bound propagation with outward rounding, rational certificates, property
  decisions) and `MaithicEmbed` (hashed n-gram embeddings, vector serialization, an exact index).
- The R packages `maithicstats` (Wilson and bootstrap intervals, exact paired comparison, comparison tables and
  the standard analysis of a benchmark run) and `maithicviz` (the brand theme and the benchmark figures).
- The capability register, which lists every capability of the release with its tests, and the developer tool
  that checks it against the test suites, measures the language share and scans the repository.
- Proof states in the Bridge: `startProof` opens a state at every `sorry` in tactic position, `runTactic` and
  `tryTactics` step states without changing them, `getState` and `releaseState` inspect and free them. Goals are
  reported with stable identifiers, case tags and hypotheses as Lean shows them. The OCaml client has typed
  functions for each, and the self-test of the Bridge steps a proof.
- The Julia package `MaithicOracle`: certificates for linear arithmetic over the rationals and the integers
  (exact simplex, Farkas multipliers), for polynomial equations (Buchberger's algorithm with cofactors), for real
  inequalities on a box (interval arithmetic with directed rounding, subdivision, a second check in higher
  precision) and for discrete facts (Pratt proofs of primality, factorizations, modular inverses, orders,
  logarithms and square roots, Bézout coefficients, witnesses of existential goals). Every oracle answers
  proved, refuted with an exact counterexample, or unknown with the reason, and writes Lean statements that the
  kernel checks.
- The Julia package `MaithicCore`: a strict JSON reader and a deterministic writer, the exact text form of
  numbers, the expression language of the oracle exchange format, and a random generator with a fixed stream.
- JSON Schemas for every document that crosses a language boundary, a JSON Schema validator in the OCaml core,
  and `maithic-dev contracts`, which validates every fixture, every recorded Bridge answer and the schemas
  themselves as part of `make hygiene`.
- In `maithicstats`: summaries of prove runs (success, time, tokens and cost, steps, failures), of the ledger
  (trust levels, axiom census, re-check agreement and time, policy decisions), of gateway usage (cost from a
  price table, tokens, latency, error rates, daily spend) and of retrieval (recall, mean reciprocal rank, nDCG,
  engine comparison), each with a Markdown report table. In `maithicviz`: the figures for all of them.
- Usage records in every result file: one per model call, with provider, model, who served it, the kind of
  failure, tokens, start time and duration, and never a prompt, an answer or a key.
- Verified checkers in the Lean library, under `Maithic.Oracle.Logic`: propositional formulas with a conversion
  to clause normal form that is proved to keep truth values; a truth-table procedure proved sound and complete,
  which makes validity decidable; checkers for resolution proofs and for clausal proofs in the LRAT text form,
  each with its soundness theorem and each reporting the first step that fails; and a proof-writing search whose
  output is checked like any other. The tactics `maithic_taut`, `maithic_sat`, `maithic_lrat` and `maithic_cnf`
  and the command `#maithic_reflect` bring them to goals. A goal that does not follow is answered with the truth
  values under which it fails.
- A verified checker for certificates of linear integer arithmetic, with the tactic `maithic_linear_int`: the
  kernel runs the checker on rows and multipliers, and each row is derived from the single hypothesis named for
  it. `MaithicOracle.lean_replay` writes these calls from the certificates of the linear oracle, and `make e2e`
  checks the replay of every integer fixture.
- Proof fixtures under `fixtures/oracle/logic`, with the verdict expected of the checker for each: hand-written
  proofs that are accepted, proofs with a flaw that must be rejected at a given step, and proofs written by the
  solver CaDiCaL.
- Testing of statements in the Lean library, under `Maithic.Refute.Testing`: the command `#maithic_test` and the
  tactic `maithic_test` try a statement on generated values, with premises as filters, shrinking of failing
  cases, a seeded random stream that is the same as in the Julia packages, and generators for naturals,
  integers, rationals, truth values, characters, strings, `Fin`, lists, arrays, options and pairs.
- Vacuity analysis, under `Maithic.Refute.Vacuity`: `#maithic_vacuous` finds contradictory hypotheses, variables
  that range over empty types and conclusions that hold without the hypotheses; `#maithic_necessity` says for
  each hypothesis whether the statement survives without it, by a proof or by a counterexample;
  `#maithic_unused` lists the hypotheses a proof does not mention. Every derivation behind a finding is accepted
  by the kernel before it is reported.
- A step-wise proof search in the prover: an AND/OR tree over goals, best-first, breadth-first and depth-first
  strategies, budgets on expansions, time, calls to Lean and tokens, blocking of tactics that make no progress
  or lead in a circle, and one search for a goal that is met several times. `maithic prove --search` runs it
  after the portfolio and before any model, with tactics chosen from the shape of the goal; its script goes
  through the same checks as any proof.
- The record of a search and its picture: `maithic prove --search-trees DIR` writes every search as one JSON
  document (`maithic.prove.search/1`) with a drawing of the tree, and `maithic tree FILE` lists the steps of a
  record and draws it again.
- The oracle core in the `maithic` command: a registry of oracles, the export of a Lean goal to the exchange
  format with Lean's own names, dispatch to the Julia engines with a time limit, and result records.
  `maithic oracle list` and `maithic oracle solve` use it from the command line, on a goal document or on a
  theorem of a Lean file, and `maithic prove --oracle` tries the scripts of a certificate as proofs. The Julia
  package is set up on first use, with no use of the network.
- The examples `examples/double.lean`, proved by the search, and `examples/gap.lean`, with one theorem proved by
  a certificate and one refuted by a counterexample.
- `maithic refute`: a search for counterexamples to the unproved theorems of a file, by tests on sampled values
  in Lean, by the oracles, and by the vacuity checks, within a budget of cases, rounds and time. A
  counterexample counts only when Lean accepts a proof that the statement is false at its values; the proof is
  printed, with the axioms it rests on. The report has a schema, `maithic.refute.report/1`, and the exit status
  tells refuted, vacuous and standing statements apart. `examples/conjectures.lean` shows each.
- `#maithic_test` takes the name of a theorem as well as a statement, and with `json` it prints its report for
  a program to read.
- Extraction of what Lean knows about a file, in the Lean library and as the `extract` request of the Bridge:
  every declaration with its place, type, documentation, dependencies and axioms; on request the tactic steps
  of every proof with the goals before and after, the open goal at every `sorry`, the theorems elsewhere that
  state the same thing, and the findings of lint rules that run in Lean.
- `maithic intel`: `scan` reads Lean files into a graph of declarations, modules, namespaces and files and a
  report on the state of the code; `show`, `deps`, `dependents`, `health` and `stats` answer from the graph
  without Lean; `extract` writes the raw material. The Julia package `MaithicGraphs` computes degrees,
  PageRank, betweenness and components on the graph, and `maithicstats` reads a scan and ranks the declarations
  that most others depend on.
- `maithic corpus`: `harvest` traces the proofs of Lean files into theorem, step, premise and pair records,
  each kind with a schema, in shards that are named by their content, with a manifest of hashes and a record of
  the harvest; `verify` checks a dataset against its manifest and its schemas. Pairs of a state and a tactic
  are written for complete proofs only. The Julia package `MaithicData` loads a dataset, removes duplicates,
  splits it by theorem and checks a split for leakage; `maithicstats` summarizes proof lengths and the use of
  tactics.
- `maithic review`: unfinished proofs and what rests on them, axioms beyond Lean's logic, restated theorems,
  missing documentation and names out of convention, each read from what Lean elaborated; with `--suggest`,
  simpler proofs that are shown only after Lean accepted them; with `--ai`, notes from a model, which cannot
  remove or lower a finding. Policies by profile and by file, suppression comments that need a reason, and
  reports for the terminal, as Markdown and as JSON. `maithicstats` reads reports and counts findings by rule,
  severity and repository.
- Figures for the code graph, for datasets and for reviews in `maithicviz`, and `render_insights`, which
  redraws them.
- `maithic guide`: `train` fits a small tactic model in the Julia package `MaithicGuide` to a dataset of
  proofs, with a split by theorem, and writes the model and the record of its training, both determined by the
  seed; `info` describes a model; `rank` shows how it orders tactics on a goal. `maithic prove --guide MODEL`
  starts the model as a service and lets it order the candidates of the step-wise search, which goes on in
  its own order when the service fails. `maithicstats` reads training records and compares models with a
  baseline; `maithicviz` draws accuracy and learning curves.
- `maithic mend`: `scan` collects what Lean reports about files by declaration and class; `repair` treats the
  breakages by rule (deprecated names, a table of renames, Lean's own suggestions), then with a model, then
  with the prover, keeps an edit only when Lean accepts it and the statement is unchanged, and gives the whole
  file to Lean at the end; `minimize` removes steps and lemmas a proof does not need, and keeps a shorter
  proof only when it rests on no new axiom. Reports have schemas, and `maithicstats` reads them.
- `maithic formalize`: a problem in words, Markdown or LaTeX is normalized and read for its formulas, numbers
  and variables; a model drafts several statements; Lean elaborates each, with a bounded repair loop that
  carries Lean's error, its class and the signatures of the names Lean knows; duplicates are dropped; and the
  statements are checked for vacuity and for counterexamples, which reject, and for triviality, missing
  numbers, unused variables, operations that mislead and agreement of a back-translation with the problem,
  which flag. `--statement` checks statements of your own, `--no-ai` asks no model, and `--write` produces a
  Lean file that `maithic prove` takes.
- In the Lean library, under `Maithic.Formalize`: `#maithic_stmt`, which shows the binders and the conclusion
  of a statement and warns of operations that do not mean what their symbols suggest; `#maithic_explicit`,
  which writes out implicit arguments, instances and coercions; and `#maithic_compare`, which says whether two
  statements are identical, the same by definition, equivalent, or one stronger than the other, the last two
  by proofs the kernel accepted.
- `maithic council`: an examination of the statement that asks no model, then rounds in which a researcher
  names lemmas that Lean looks up, a prover proposes proofs, a verifier gives each to Lean and a critic reads
  Lean's errors. Members post to a blackboard whose claims change status only on evidence made from an answer
  of Lean. A session has limits on tokens, cost, time, calls to Lean and calls to models, a transcript as
  Markdown, a schema, and an answer that is built from what Lean accepted.
- Verified algorithms in the Lean library: binary search on sorted arrays with its specification, merging and
  merge sort with the theorem that a sorted permutation is unique, so that merge sort and insertion sort are
  the same function; minimum and maximum; removal of repeats from sorted lists; Fibonacci numbers by table
  and by pairs; prefix sums with range queries; permutations as equal counts, with a function that decides
  them; a class of total orders with instances for the naturals and the integers; and a specification of
  searching that linear and binary search both meet.
- `#maithic_find` in the Lean library, which searches the declarations in scope by parts of their names and
  by text in their statements.
- The examples `examples/guided.lean` and `examples/council.lean`, and recorded runs of the four new commands
  under `fixtures/guide`, `fixtures/mend`, `fixtures/formalize` and `fixtures/council`.
- `maithic blueprint`: `check`, `plan`, `skeleton`, `status`, `next` and `graph`. A blueprint is a Markdown
  file of nodes with statements in words and in Lean and the nodes each rests on. The planner asks a model to
  take an argument apart and puts the answer in order; the skeleton is compiled by Lean; and the status of
  every node, planned, stated, proved or done, is read from what Lean reports, with the frontier of what can
  be proved next and a picture of the graph. Snapshots are read by `maithicstats::read_blueprint_snapshots`
  and drawn by `maithicviz::plot_blueprint_progress`.
- `maithic search`: declarations found by the words of their names, statements and documentation, with an
  inverted index and BM25, a retriever for names, and with `--lean` Lean's own search, merged by reciprocal
  rank fusion. A query set can be answered for evaluation with the retrieval measures of `maithicstats`.
- `maithic bench gate`: a benchmark run held against a stored baseline, with thresholds for lost problems, the
  share verified, time and candidates, and exit status 13 when a suite fails.
- `maithic nnv verify`: a property of a ReLU network decided by `MaithicNNV` and checked by Lean's kernel. The
  module `Maithic.NN.Cert.Property` proves the property checker sound and that an input whose exact output
  lies outside the claimed box refutes the property; the command writes the theorem with exact weights and
  has it checked in a fresh process within the trust policy.
- `maithic algo synth`: a Lean function drafted by a model for a task with examples, screened, compiled by Lean
  and run on the examples, with errors and wrong answers fed back for a bounded number of corrections, and a
  record of every draft. `maithic algo differential` tests the compiled program `maithic-algo` against the
  reference implementations of the new Julia package `MaithicAlgoRef` on generated inputs.
- In the Lean library: selection sort and quicksort with the theorem that all four sorts agree, the search for
  a pattern in a text, paths in graphs, Euclid's algorithm and exponentiation by squaring;
  `Maithic.Algo.Cost.Comparisons`, which counts the comparisons of linear search, binary search, insertion
  sort and merge sort and proves a bound for each; and `Maithic.Algo.Lab.RunLengths`, which proves that the
  run-length encoder accepted by `maithic algo synth` meets a specification that has one solution.
- `maithicstats::fit_growth` and `growth_table`, which fit constant, logarithmic, linear, linearithmic and
  quadratic models to measured costs and lay them beside the proved bounds, and `maithicviz::plot_growth`;
  `maithicstats::read_nnv_jobs` and `verification_summary`.
- The prompts `blueprint.decompose`, `algolab.draft` and `algolab.repair`, the settings `nnv.project`,
  `nnv.timeout_s` and `algolab.project`, and schemas for every new document.
- Guides for blueprints, the search for declarations, the verification of networks, the regression gate and
  the algorithm laboratory, with pictures drawn from the commands.
- `maithic-dev shots`, which draws the pictures of terminal sessions in the documentation from what the built
  command prints, so that a picture cannot drift from the program.
- A scan in `make hygiene` for paths below the home directory of a user, which come into a repository with
  recorded output, and the same scans on files of JSON lines.
- `Maithic.Algo.Cost.Extremes`: the bound of insertion sort is reached by every list in falling order, a sorted
  list costs exactly `n - 1` comparisons, and no list costs less. `Maithic.NN.Examples.AbsoluteValue`: the three
  verdicts of `maithic nnv verify` on the absolute-value network as theorems, with the fact that the claim
  [0, 4] is true and out of the reach of interval bounds. Four corollaries in `Maithic.Algo.Lab.RunLengths`:
  never more runs than elements, only the empty list has none, one value repeated is one run, and the encoding
  loses nothing.
- `registry/status.json` counts the capabilities of each pillar by language. `maithicstats::read_register_status`,
  `register_by_language` and `declarations_by_area`; `maithicviz::plot_register`, `plot_library_areas` and the
  renderers `render_register`, `render_library` and `render_search`, with three new figures: the register by
  pillar and language, the Lean library by area, and the recall of the search.
- A page for each of the eight Julia packages and the two R packages, each with an example and the output that
  the example prints, and a guide to the command from a script, from Julia, from R and from any other language
  ([`docs/guides/scripting.md`](docs/guides/scripting.md)).
- Diagrams of every part of the system in the README and the guides, and six more pictures of terminal
  sessions: the rounds of a model-assisted proof, the third answer of the network verifier, the answers of the
  oracles, a JSON report, the dependencies of a theorem, and a tactic model trained in three commands.

### Fixed

- A theorem with a documentation comment, or with attributes on lines of their own, was cut off from them: the
  text before the theorem ended in a comment with nothing to attach to, and `prove` reported errors before the
  theorem. The comment and the attributes now belong to the declaration, and they are kept when a proof is
  written back.
- The statements given to `#maithic_test`, `#maithic_vacuous` and `#maithic_necessity` no longer draw warnings
  about hypotheses that nothing refers to.
- A command that ends in `in` on the lines before a theorem, such as `set_option autoImplicit false in`, was
  cut off from it, and what stood before the theorem ended in the middle of a command. Such lines now belong
  to the declaration.
- A proof by tactics was written back with `by` on a line of its own below `:=`. It is written as `:= by`
  with the tactics below it.
- A table that was wider than the terminal ran past its edge. A table is now fitted to the width, which is the
  value of `COLUMNS` or 118 characters, by continuing its last column on further lines; and a coloured cell is
  padded by what it shows, so that its column lines up.
- `maithic check` listed declarations that Lean generates, such as equation lemmas, among the declarations of a
  file. It lists the ones that were written.
- The pictures of terminal sessions lost their indentation and their columns in browsers, which do not keep
  runs of spaces. Every stretch of text is now placed at its column and states its length, so that the layout
  is the same whatever the reader's monospaced font is; and a line that is longer than the picture goes on in
  the next line instead of being cut.
- The figures named a font of the machine that drew them, and a reader without it saw a serif face. Their text
  now names fonts that every system has and carries its measured length, as does the text of every diagram: a
  wider font, or one hinted to whole pixels, no longer moves a label out of its box. Figures in which one mark
  covered another were redrawn: a rate is a dot on the line of its interval, bars that carry numbers have no
  grid, the learning curves have a panel for each seed, and the notes of a council session stand in a row of
  their own.
- The picture of a blueprint cut long titles and ended two arrows in one point. Titles take two lines, and
  every arrow has an end of its own.
- The help of `maithic tree` and of `maithic intel extract` had a run of spaces in the middle of a sentence.
- A mistake in the arguments, such as an unknown option or an unknown command, was reported as text even when
  the arguments asked for JSON. With `--json` it is an error document on standard output, like every other
  error.
- Two recorded Bridge transcripts were older than the protocol, and one held a path of the machine it was
  recorded on. They were recorded again, and the recorder leaves search paths out.

### Changed

- The Julia packages need Julia 1.11 or later, for dependencies by path. `MaithicNNV` writes its certificates
  through `MaithicCore`; their text is unchanged.
- The self-test of the Bridge has five checks instead of four.
- `make test-julia` and the Julia job of the workflow cover eight packages, and `make e2e` runs the new
  commands end to end.
- The language-share gate has all four floors at their final values: OCaml 30, Lean 15, Julia 12, R 8.
- The recorded runs under `fixtures/runs` include the examples of the logic checkers and the replayed
  certificates, and the figures of the README are drawn from them.
- The integration job of the workflow installs Julia, because its tests and the end-to-end session call the
  oracle engines.
- The Bridge puts the compiled modules of its own package on Lean's search path, after those of the project it
  runs in, so that a file of any project can import the statement tests.
- Colour can be asked for where the output is not a terminal, with `CLICOLOR_FORCE=1`; `NO_COLOR` and
  `--no-color` still switch it off.
- The end-to-end session of `make e2e` runs the new commands on the examples and the fixtures.
- Refutations are confirmed by one function, shared by `maithic refute`, `maithic formalize` and the council.
- The demonstration fixtures of the code graph and of the dataset are made from a named list of sources, so
  that they do not change when the library grows.
- The workflow names its versions in one place, pins every action to a commit, sets a time limit for each job,
  installs the system libraries that the R packages need, and can be started by hand.
- `maithic doctor` shortens a long version line between two words, as do the list of candidates of
  `maithic prove --verbose` and the statements in the results of `maithic search`; `maithic guide train` prints
  the baseline on a line of its own.
- The wordmark is drawn as outlines, so that it is the same on every system.
