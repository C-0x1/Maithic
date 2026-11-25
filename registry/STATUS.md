# Capability status

This page is written by `make cap-sync` from the capability register and the test suites. A capability is counted
as done only when a test that names it exists; `make hygiene` fails if the register and the tests disagree.

**481 of 481 capabilities are done** (0 to do).

| Pillar | Name | What it is | Capabilities | Done | To do |
|---|---|---|---:|---:|---:|
| P00 | Platform | Platform foundation | 20 | 20 | 0 |
| P01 | Prove | Self-correcting theorem-proving agent | 33 | 33 | 0 |
| P02 | Formalize | Natural language to formally verified Lean | 20 | 20 | 0 |
| P03 | Review | AI-powered Lean code reviewer | 20 | 20 | 0 |
| P04 | Search | Retrieval system for mathematical theorem discovery | 20 | 20 | 0 |
| P05 | AlgoLab | AI-generated algorithm verification laboratory | 20 | 20 | 0 |
| P06 | Council | Multi-agent mathematical research assistant | 20 | 20 | 0 |
| P07 | Intel | Lean codebase intelligence platform | 20 | 20 | 0 |
| P08 | NNV | AI-assisted neural network verification | 20 | 20 | 0 |
| P09 | Bridge | Lean interaction engine and verification sandbox | 47 | 47 | 0 |
| P10 | Gateway | Multi-provider AI model gateway | 46 | 46 | 0 |
| P11 | Corpus | Proof-trace extraction and dataset foundry | 20 | 20 | 0 |
| P12 | Guide | Neural guidance studio | 20 | 20 | 0 |
| P13 | Oracle | Certified computation bridge | 53 | 53 | 0 |
| P14 | Refute | Refutation and conjecture laboratory | 20 | 20 | 0 |
| P15 | Bench | Benchmark observatory | 20 | 20 | 0 |
| P16 | Blueprint | Formalization project planner | 20 | 20 | 0 |
| P17 | Mend | Proof repair and refactoring engine | 20 | 20 | 0 |
| P18 | Ledger | Trust ledger and proof certificates | 22 | 22 | 0 |

## Capabilities by group

### P00-01 Command-line interface

The `maithic` command: one consistent interface to everything.

- [x] `P00-01-01` Global options
- [x] `P00-01-02` Version command
- [x] `P00-01-03` Doctor command
- [x] `P00-01-04` Exit code table
- [x] `P00-01-05` JSON output mode
- [x] `P00-01-06` Error presentation

### P00-02 Configuration and workspace

One layered, validated configuration for the whole product.

- [x] `P00-02-01` Configuration file
- [x] `P00-02-02` Layered precedence
- [x] `P00-02-03` Schema validation
- [x] `P00-02-04` Environment variables
- [x] `P00-02-05` Typed access
- [x] `P00-02-06` Default configuration
- [x] `P00-02-07` Configuration reference generation
- [x] `P00-02-08` Configuration tests
- [x] `P00-02-09` Workspace discovery
- [x] `P00-02-10` Secrets separation
- [x] `P00-02-11` Effective configuration display

### P00-03 Packaging and distribution

Gets Maithic onto users' machines reproducibly.

- [x] `P00-03-01` Version stamping
- [x] `P00-03-02` Language share tool
- [x] `P00-03-03` Capability register tool

### P01-01 Search kernel

Data structures and algorithms that explore the space of tactic states.

- [x] `P01-01-01` Search tree store
- [x] `P01-01-02` Best-first search
- [x] `P01-01-03` Breadth-first search
- [x] `P01-01-04` Depth-first search
- [x] `P01-01-05` Node budgets
- [x] `P01-01-06` Goal-level AND/OR model
- [x] `P01-01-07` Solved propagation
- [x] `P01-01-08` Dead-end propagation
- [x] `P01-01-09` Transposition table
- [x] `P01-01-10` Search invariants checker

### P01-02 Tactic proposal and prompting

Turns a proof state into ranked candidate tactics from AI and non-AI proposers.

- [x] `P01-02-01` Portfolio proposer

### P01-03 Self-correction and reflection

Reads Lean errors, decides how to repair, and learns within and across attempts.

- [x] `P01-03-01` Error taxonomy
- [x] `P01-03-02` Repair prompt
- [x] `P01-03-03` Failed-tactic memory
- [x] `P01-03-04` Loop detection
- [x] `P01-03-05` No-progress detection
- [x] `P01-03-06` Error-to-hint rules

### P01-04 Lean tactic suite

Tactics and commands that expose Maithic automation inside Lean itself.

- [x] `P01-04-01` maithic_auto tactic
- [x] `P01-04-02` maithic_close combinator
- [x] `P01-04-03` First-success combinator
- [x] `P01-04-04` Progress check

### P01-05 Run analytics and calibration

Statistical analysis of prover runs.

- [x] `P01-05-01` Run import
- [x] `P01-05-02` Success summary
- [x] `P01-05-03` Time-to-proof summary
- [x] `P01-05-04` Token and cost summary
- [x] `P01-05-05` Step statistics
- [x] `P01-05-06` Failure breakdown
- [x] `P01-05-07` Standard plots
- [x] `P01-05-08` Report table

### P01-06 Visualization and session experience

Renders searches, states and final proofs for terminal, Studio and documents.

- [x] `P01-06-01` Search-tree SVG
- [x] `P01-06-02` Attempt timeline
- [x] `P01-06-03` Session JSON export
- [x] `P01-06-04` Rendering golden tests

### P02-01 Input ingestion and math parsing

Reads informal mathematics from text, LaTeX and Markdown into a typed intake record.

- [x] `P02-01-01` Plain-text intake
- [x] `P02-01-02` Unicode math normalization
- [x] `P02-01-03` Intake record

### P02-02 Candidate statement generation

Drafts several distinct Lean statements for each problem.

- [x] `P02-02-01` Statement drafting prompt
- [x] `P02-02-02` Multi-candidate generation
- [x] `P02-02-03` Candidate record

### P02-03 Elaboration repair loop

Compiles candidates and repairs the ones that fail to elaborate.

- [x] `P02-03-01` Candidate compilation
- [x] `P02-03-02` Statement error classes
- [x] `P02-03-03` Statement repair prompt
- [x] `P02-03-04` Repair loop bound
- [x] `P02-03-05` Elaboration result record

### P02-04 Faithfulness validation

Tests whether a compiling statement actually says what the problem says.

- [x] `P02-04-01` Back-translation
- [x] `P02-04-02` Round-trip comparison
- [x] `P02-04-03` Vacuity check
- [x] `P02-04-04` Triviality check
- [x] `P02-04-05` Counterexample check

### P02-05 Lean statement utilities

Commands that inspect, probe and compare theorem statements inside Lean.

- [x] `P02-05-01` Statement inspection command
- [x] `P02-05-02` Elaborated-form printer

### P02-06 Formalization quality analytics

Measures how often formalization is right and why it goes wrong.

- [x] `P02-06-01` Formalization run import
- [x] `P02-06-02` Compile-rate summary

### P03-01 Lean lint engine

Lean-native checks that run over the environment and syntax.

- [x] `P03-01-01` Lint framework
- [x] `P03-01-02` Sorry detector
- [x] `P03-01-03` Axiom detector
- [x] `P03-01-04` Missing-docstring lint
- [x] `P03-01-05` Naming-convention lint

### P03-02 Semantic checks

Checks that need the dependency graph, search or a second build.

- [x] `P03-02-01` Duplicate-lemma check
- [x] `P03-02-02` Finding record

### P03-03 AI review reasoning

Model-driven explanation, intent checking and comment drafting under verification rules.

- [x] `P03-03-01` Structured review output
- [x] `P03-03-02` Untrusted-content isolation

### P03-04 Verified simplification suggestions

Proposes shorter or sturdier proofs and compiles each proposal first.

- [x] `P03-04-01` Automation replacement trial
- [x] `P03-04-02` Suggestion verification
- [x] `P03-04-03` Step deletion trial

### P03-05 Policy and configuration

Decides which findings matter for a given repository.

- [x] `P03-05-01` Rule switches
- [x] `P03-05-02` Severity mapping
- [x] `P03-05-03` Inline suppression

### P03-06 Review analytics

Measures whether reviews are useful and which rules are noisy.

- [x] `P03-06-01` Review run import
- [x] `P03-06-02` Finding counts

### P03-07 Report renderers

Renders findings for terminals, forges, editors and CI systems.

- [x] `P03-07-01` Markdown report
- [x] `P03-07-02` Terminal report
- [x] `P03-07-03` JSON report

### P04-01 Indexing pipeline

Builds and maintains all search indexes from declaration exports.

- [x] `P04-01-01` Declaration document model

### P04-02 Embedding engine

Turns statements and queries into vectors without depending on any provider.

- [x] `P04-02-01` Embedding interface
- [x] `P04-02-02` Hashed n-gram embedder
- [x] `P04-02-03` Deterministic embeddings
- [x] `P04-02-04` Query and document modes
- [x] `P04-02-05` Vector serialization

### P04-03 Vector index

Nearest-neighbor search structures with persistence and filters.

- [x] `P04-03-01` Flat index
- [x] `P04-03-02` Top-k query
- [x] `P04-03-03` Identifier mapping

### P04-04 Lexical and type-pattern retrieval

Keyword, name and unification-based retrieval that needs no embeddings.

- [x] `P04-04-01` Inverted index
- [x] `P04-04-02` BM25 scoring

### P04-05 Hybrid fusion and ranking

Combines retrievers into one ranked, deduplicated answer.

- [x] `P04-05-01` Search entry point

### P04-06 Retrieval evaluation

Measures retrieval quality with standard metrics and significance tests.

- [x] `P04-06-01` Query-set format
- [x] `P04-06-02` Recall at k
- [x] `P04-06-03` Mean reciprocal rank
- [x] `P04-06-04` nDCG
- [x] `P04-06-05` Engine comparison table
- [x] `P04-06-06` Query latency summary
- [x] `P04-06-07` Retrieval plots
- [x] `P04-06-08` Retrieval report table

### P05-01 Specification library

Reusable vocabulary for stating what an algorithm must do.

- [x] `P05-01-01` Sortedness predicate
- [x] `P05-01-02` Permutation predicate
- [x] `P05-01-03` Search specification

### P05-02 Verified searching and sorting

Implementations with machine-checked correctness theorems.

- [x] `P05-02-01` Linear search
- [x] `P05-02-02` Binary search
- [x] `P05-02-03` Insertion sort
- [x] `P05-02-04` Merge function
- [x] `P05-02-05` Minimum and maximum
- [x] `P05-02-06` Sorted deduplication

### P05-03 Verified graphs and dynamic programming

Graph and dynamic-programming algorithms with correctness theorems.

- [x] `P05-03-01` Memoized Fibonacci
- [x] `P05-03-02` Prefix sums

### P05-04 Algorithm synthesis agent

Drafts Lean implementations from a task description and iterates until they compile and pass tests.

- [x] `P05-04-01` Algorithm task intake
- [x] `P05-04-02` Implementation drafting prompt
- [x] `P05-04-03` Implementation compile loop
- [x] `P05-04-04` Example-based check
- [x] `P05-04-05` Synthesis record

### P05-05 Reference implementations and differential testing

Independent implementations and generators that test the Lean code from outside.

- [x] `P05-05-01` Reference algorithm set
- [x] `P05-05-02` Input generators
- [x] `P05-05-03` Differential runner

### P05-06 Complexity and performance analytics

Empirical growth analysis and comparison with proven bounds.

- [x] `P05-06-01` Growth model fitting

### P06-01 Coordinator and task decomposition

Plans the work, dispatches it in parallel and synthesizes the result.

- [x] `P06-01-01` Problem intake
- [x] `P06-01-02` Role assignment
- [x] `P06-01-03` Result collection
- [x] `P06-01-04` Synthesis step

### P06-02 Role library

The specialists: prompts, tools and limits for each role.

- [x] `P06-02-01` Researcher role
- [x] `P06-02-02` Prover role
- [x] `P06-02-03` Critic role
- [x] `P06-02-04` Verifier role
- [x] `P06-02-05` Role registry

### P06-03 Blackboard and shared memory

The shared theorem and proof-state workspace that all agents read and write.

- [x] `P06-03-01` Blackboard store
- [x] `P06-03-02` Item kinds
- [x] `P06-03-03` Claim status field

### P06-04 Communication protocol and logs

Typed messages between agents and a complete, replayable record of them.

- [x] `P06-04-01` Message schema
- [x] `P06-04-02` Transcript log
- [x] `P06-04-03` Transcript export

### P06-05 Tool belt

The typed tools through which agents act on the world.

- [x] `P06-05-01` Lean check tool
- [x] `P06-05-02` Prove tool

### P06-06 Budgets, scheduling and consensus

Keeps a council within budget and resolves disagreements.

- [x] `P06-06-01` Council session budget
- [x] `P06-06-02` Round scheduler

### P06-07 Collaboration analytics

Measures what each role contributes and where councils fail.

- [x] `P06-07-01` Council session import

### P07-01 Environment and syntax extractor

Exports everything Lean knows about a project in a stable, documented schema.

- [x] `P07-01-01` Declaration export
- [x] `P07-01-02` Dependency export
- [x] `P07-01-03` Docstring export
- [x] `P07-01-04` Import graph export
- [x] `P07-01-05` Sorry flagging

### P07-02 Code knowledge graph

Stores declarations, modules and their relations as a queryable graph.

- [x] `P07-02-01` Graph schema
- [x] `P07-02-02` Graph builder
- [x] `P07-02-03` Node lookup
- [x] `P07-02-04` Neighbor queries
- [x] `P07-02-05` Graph statistics

### P07-03 Query language and analyses

A small query language and the standard analyses built on it.

- [x] `P07-03-01` Dependencies query
- [x] `P07-03-02` Dependents query
- [x] `P07-03-03` Unfinished-proofs query

### P07-04 Graph analytics and layout

Numerical graph algorithms and layouts for large dependency graphs.

- [x] `P07-04-01` Graph loader
- [x] `P07-04-02` Degree and centrality
- [x] `P07-04-03` Component analysis

### P07-05 Unfinished work and health tracking

Tracks unfinished proofs and the general condition of a codebase.

- [x] `P07-05-01` Sorry inventory
- [x] `P07-05-02` Axiom census

### P07-06 Codebase metrics and trends

Time-series analysis of how a codebase grows and where its risks are.

- [x] `P07-06-01` Snapshot import
- [x] `P07-06-02` Top-depended table

### P08-01 Layer and network semantics

A precise meaning for each layer and for whole networks, executable and provable.

- [x] `P08-01-01` Dense layer
- [x] `P08-01-02` ReLU
- [x] `P08-01-03` Exact network evaluation

### P08-02 Certificate checkers

Proven-sound programs that accept or reject evidence produced outside Lean.

- [x] `P08-02-01` Interval type
- [x] `P08-02-02` Interval soundness
- [x] `P08-02-03` Interval-bound checker
- [x] `P08-02-04` Bound certificate format
- [x] `P08-02-05` Checker soundness theorem
- [x] `P08-02-06` Counterexample checker

### P08-03 Bound propagation engines

Fast incomplete verifiers that produce certificates.

- [x] `P08-03-01` Network structs
- [x] `P08-03-02` Interval bound propagation
- [x] `P08-03-03` Bound certificate emission
- [x] `P08-03-04` Property decision from bounds
- [x] `P08-03-05` Reference forward pass
- [x] `P08-03-06` Rational enclosure
- [x] `P08-03-07` Deterministic bounds

### P08-04 AI-assisted verification workflow

Orchestrates engines, checkers and models into one verification job.

- [x] `P08-04-01` Verification orchestration
- [x] `P08-04-02` Checker invocation
- [x] `P08-04-03` Verify command

### P08-05 Verification analytics and reporting

Standard robustness-verification statistics and figures.

- [x] `P08-05-01` Verification results import

### P09-01 Bridge server

The Lean-side process that executes requests.

- [x] `P09-01-01` Server executable
- [x] `P09-01-02` Request loop
- [x] `P09-01-03` Load-header request
- [x] `P09-01-04` Run-command request
- [x] `P09-01-05` Start-proof request
- [x] `P09-01-06` Run-tactic request
- [x] `P09-01-07` Get-state request
- [x] `P09-01-08` Version handshake
- [x] `P09-01-09` State store
- [x] `P09-01-10` Whole-file check request
- [x] `P09-01-11` Goal selection
- [x] `P09-01-12` Tactic batch request
- [x] `P09-01-13` Release-state request
- [x] `P09-01-14` Typed error returns
- [x] `P09-01-15` Server shutdown
- [x] `P09-01-16` Server self-test
- [x] `P09-01-17` Server test suite

### P09-02 Proof-state serialization

Turns Lean's internal state into stable, structured data.

- [x] `P09-02-01` Goal serialization
- [x] `P09-02-02` Goal identifiers
- [x] `P09-02-03` Message serialization
- [x] `P09-02-04` Protocol JSON instances
- [x] `P09-02-05` Serialization schema tests
- [x] `P09-02-06` Goal tags
- [x] `P09-02-07` Safe string escaping
- [x] `P09-02-08` Serialization round-trip tests

### P09-03 Environment management and snapshots

Keeps environments and proof states cheap to create, copy and restore.

- [x] `P09-03-01` Module import resolution
- [x] `P09-03-02` Base environment cache
- [x] `P09-03-03` Derived environments
- [x] `P09-03-04` State snapshots
- [x] `P09-03-05` Environment disposal
- [x] `P09-03-06` Environment tests
- [x] `P09-03-07` Header deduplication
- [x] `P09-03-08` Environment diff

### P09-04 Protocol and client library

The typed OCaml interface used by every other component.

- [x] `P09-04-01` Protocol types
- [x] `P09-04-02` Protocol codecs
- [x] `P09-04-03` Process spawn
- [x] `P09-04-04` Request correlation
- [x] `P09-04-05` Typed client functions
- [x] `P09-04-06` Client timeouts
- [x] `P09-04-07` Bridge error type
- [x] `P09-04-08` Stub-server tests

### P09-05 Sandbox and resource control

Contains Lean code written by models or taken from untrusted repositories.

- [x] `P09-05-01` Wall-clock limit
- [x] `P09-05-02` Environment scrubbing
- [x] `P09-05-03` Isolation capability report
- [x] `P09-05-04` Elaborator limits
- [x] `P09-05-05` Construct screening

### P09-06 Project and toolchain management

Finds, builds and prepares the Lean projects the bridge works in.

- [x] `P09-06-01` Project detection

### P10-01 Anthropic adapter

Speaks the Claude Messages API.

- [x] `P10-01-01` Messages request
- [x] `P10-01-02` Anthropic authentication headers
- [x] `P10-01-03` Messages response parsing
- [x] `P10-01-04` Anthropic structured output
- [x] `P10-01-05` Anthropic error mapping

### P10-02 OpenAI adapter

Speaks the OpenAI Responses API.

- [x] `P10-02-01` Responses request
- [x] `P10-02-02` OpenAI bearer authentication
- [x] `P10-02-03` Responses parsing
- [x] `P10-02-04` OpenAI structured output
- [x] `P10-02-05` OpenAI error mapping

### P10-03 xAI adapter

Speaks the xAI API, which follows the OpenAI request shapes.

- [x] `P10-03-01` xAI base URL
- [x] `P10-03-02` xAI bearer authentication
- [x] `P10-03-03` xAI chat request
- [x] `P10-03-04` xAI responses request
- [x] `P10-03-05` xAI response parsing
- [x] `P10-03-06` xAI structured output

### P10-04 Unified model interface

One typed interface that all pillars program against.

- [x] `P10-04-01` Provider signature
- [x] `P10-04-02` Provider selection
- [x] `P10-04-03` Unified errors

### P10-05 Key management and authentication

Bring-your-own-key handling that never leaks.

- [x] `P10-05-01` Key environment variables
- [x] `P10-05-02` Provider setting
- [x] `P10-05-03` Login command
- [x] `P10-05-04` Key validation
- [x] `P10-05-05` Key redaction
- [x] `P10-05-06` Auth status command
- [x] `P10-05-07` Key precedence

### P10-06 Routing, fallback and resilience

Keeps model calls working through rate limits and outages.

- [x] `P10-06-01` Default route
- [x] `P10-06-02` Call timeouts

### P10-07 Caching, record and replay

Makes model behavior reproducible in tests.

- [x] `P10-07-01` Gateway modes
- [x] `P10-07-02` Canonical request hash
- [x] `P10-07-03` Cassette format
- [x] `P10-07-04` Replay provider
- [x] `P10-07-05` Missing-cassette failure
- [x] `P10-07-06` Record-time redaction
- [x] `P10-07-07` Scripted provider
- [x] `P10-07-08` Mock provider

### P10-08 Prompt registry and evaluation

Keeps every prompt versioned, typed and tested.

- [x] `P10-08-01` Prompt rendering
- [x] `P10-08-02` System prompt policy

### P10-09 Usage analytics and provider comparison

Analyzes cost, latency and quality by provider and model.

- [x] `P10-09-01` Usage import
- [x] `P10-09-02` Cost by provider
- [x] `P10-09-03` Token breakdown
- [x] `P10-09-04` Call latency distributions
- [x] `P10-09-05` Provider error rates
- [x] `P10-09-06` Daily spend
- [x] `P10-09-07` Gateway plots
- [x] `P10-09-08` Gateway report table

### P11-01 Tactic trace extraction

Records what each tactic did to the proof state.

- [x] `P11-01-01` Tactic step record
- [x] `P11-01-02` Information-tree traversal
- [x] `P11-01-03` Theorem record
- [x] `P11-01-04` Trace JSON lines output
- [x] `P11-01-05` Top-level step filter

### P11-02 Premise and dependency annotation

Records which lemmas and definitions each step relied on.

- [x] `P11-02-01` Per-proof premises

### P11-03 Repository harvesting

Fetches, builds and traces repositories at scale.

- [x] `P11-03-01` Tracer invocation
- [x] `P11-03-02` Harvest record
- [x] `P11-03-03` Harvest command

### P11-04 Dataset schema and storage

Canonical record formats and a versioned store with lineage.

- [x] `P11-04-01` Record schemas
- [x] `P11-04-02` Dataset manifest
- [x] `P11-04-03` Content addressing
- [x] `P11-04-04` Schema validation on write

### P11-05 Dataset processing

Cleans, deduplicates and splits data without leaking test information.

- [x] `P11-05-01` Record loader
- [x] `P11-05-02` Exact deduplication
- [x] `P11-05-03` Random split
- [x] `P11-05-04` Leakage check

### P11-06 Corpus statistics and exploration

Describes what a dataset contains.

- [x] `P11-06-01` Corpus import
- [x] `P11-06-02` Proof length distribution
- [x] `P11-06-03` Tactic frequency

### P12-01 Tokenization and encoding

Converts Lean text into model inputs.

- [x] `P12-01-01` Lean lexer
- [x] `P12-01-02` Vocabulary builder
- [x] `P12-01-03` Encode and decode

### P12-02 Model zoo

The network architectures used for guidance.

- [x] `P12-02-01` Embedding layer
- [x] `P12-02-02` Pooling head
- [x] `P12-02-03` Tactic classifier head

### P12-03 Tactic ranking and generation

Trains models that propose and rank next tactics.

- [x] `P12-03-01` Tactic template vocabulary
- [x] `P12-03-02` Tactic dataset loader
- [x] `P12-03-03` Tactic classification loss
- [x] `P12-03-04` Tactic training loop
- [x] `P12-03-05` Top-k accuracy
- [x] `P12-03-06` Tactic prior output

### P12-04 Training infrastructure

Shared machinery for reproducible training.

- [x] `P12-04-01` Optimizer setup
- [x] `P12-04-02` Seed control

### P12-05 OCaml guidance client

Connects the search kernel to the guidance service.

- [x] `P12-05-01` Guidance client
- [x] `P12-05-02` Prior integration
- [x] `P12-05-03` Uniform fallback

### P12-06 Model evaluation and experiment tracking

Compares models and training runs with proper statistics.

- [x] `P12-06-01` Run manifest import
- [x] `P12-06-02` Learning curves
- [x] `P12-06-03` Accuracy plots

### P13-01 Certificate protocol and registry

The contract between untrusted solvers and trusted checkers.

- [x] `P13-01-01` Oracle interface
- [x] `P13-01-02` Certificate schema
- [x] `P13-01-03` Oracle registry
- [x] `P13-01-04` Goal export
- [x] `P13-01-05` Solve dispatch
- [x] `P13-01-06` Certificate hand-off
- [x] `P13-01-07` Oracle result record
- [x] `P13-01-08` Protocol fixtures

### P13-02 Linear arithmetic certificates

Finds exact multipliers that prove linear goals.

- [x] `P13-02-01` Linear goal parser
- [x] `P13-02-02` Exact rational simplex
- [x] `P13-02-03` Farkas certificate
- [x] `P13-02-04` Linear combination certificate
- [x] `P13-02-05` Certificate self-check
- [x] `P13-02-06` Linear certificate writer
- [x] `P13-02-07` Integer goal handling
- [x] `P13-02-08` Linear oracle tests
- [x] `P13-02-09` Linear round-trip fixtures

### P13-03 Polynomial certificates

Finds witnesses for polynomial identities and inequalities.

- [x] `P13-03-01` Polynomial parser
- [x] `P13-03-02` Polynomial normal form
- [x] `P13-03-03` Identity check
- [x] `P13-03-04` Ideal-membership certificate
- [x] `P13-03-05` Groebner basis computation
- [x] `P13-03-06` Polynomial certificate writer
- [x] `P13-03-07` Polynomial self-check
- [x] `P13-03-08` Polynomial oracle tests

### P13-04 Lean logic certificate checkers

Checkers for propositional and decision-procedure certificates.

- [x] `P13-04-01` Propositional reflection
- [x] `P13-04-02` Clause normal form
- [x] `P13-04-03` Resolution proof checker
- [x] `P13-04-04` Clausal certificate tactic
- [x] `P13-04-05` Logic checker soundness
- [x] `P13-04-06` Model checker
- [x] `P13-04-07` Logic checker examples
- [x] `P13-04-08` Logic checker tests
- [x] `P13-04-09` Linear integer certificates
- [x] `P13-04-10` Checker error reporting
- [x] `P13-04-11` Checker fuzz fixtures
- [x] `P13-04-12` Axiom audit of logic checkers

### P13-05 Validated numerics

Rigorous numerical computation whose results come with guaranteed enclosures.

- [x] `P13-05-01` Interval evaluation
- [x] `P13-05-02` Expression parser for numerics
- [x] `P13-05-03` Inequality proof by enclosure
- [x] `P13-05-04` Domain subdivision
- [x] `P13-05-05` Interval certificate
- [x] `P13-05-06` Rational endpoints
- [x] `P13-05-07` Numerics self-check
- [x] `P13-05-08` Validated numerics tests

### P13-06 Discrete and algebraic computation

Witnesses for number theory, combinatorics and algebra.

- [x] `P13-06-01` Primality certificates
- [x] `P13-06-02` Factorization witnesses
- [x] `P13-06-03` Modular arithmetic witnesses
- [x] `P13-06-04` Bezout coefficients
- [x] `P13-06-05` Existence witnesses
- [x] `P13-06-06` Discrete certificate writer
- [x] `P13-06-07` Witness self-check
- [x] `P13-06-08` Discrete oracle tests

### P14-01 Property-based testing in Lean

Random and exhaustive testing of statements inside Lean.

- [x] `P14-01-01` Testable class
- [x] `P14-01-02` Generator class
- [x] `P14-01-03` Shrinking class
- [x] `P14-01-04` Test runner command
- [x] `P14-01-05` Base generators
- [x] `P14-01-06` Seeded testing
- [x] `P14-01-07` Counterexample output
- [x] `P14-01-08` Precondition handling
- [x] `P14-01-09` Testing tactic
- [x] `P14-01-10` Test budget options

### P14-02 Counterexample search orchestration

Runs every refutation method and confirms results in Lean.

- [x] `P14-02-01` Witness verification
- [x] `P14-02-02` Refute command

### P14-03 Vacuity and consistency checks

Detects statements that are true for the wrong reasons.

- [x] `P14-03-01` Contradictory-hypothesis check
- [x] `P14-03-02` Vacuity command
- [x] `P14-03-03` Trivial-conclusion check
- [x] `P14-03-04` Unused-hypothesis report
- [x] `P14-03-05` Hypothesis-drop variants
- [x] `P14-03-06` Empty-domain check
- [x] `P14-03-07` Vacuity result output
- [x] `P14-03-08` Vacuity tests

### P15-01 Benchmark suite definitions

Problems stated in Lean, with metadata, that compile cleanly.

- [x] `P15-01-01` Problem-as-proposition convention
- [x] `P15-01-02` Problem attribute
- [x] `P15-01-03` Basics suite
- [x] `P15-01-04` Suite manifest export
- [x] `P15-01-05` Reference solutions
- [x] `P15-01-06` Suite tests

### P15-02 Statistical analysis

Turns result tables into conclusions with stated uncertainty.

- [x] `P15-02-01` Binomial intervals
- [x] `P15-02-02` Bootstrap intervals
- [x] `P15-02-03` Paired comparison
- [x] `P15-02-04` Effect size
- [x] `P15-02-05` Comparison table
- [x] `P15-02-06` Statistical helpers tests
- [x] `P15-02-07` Analysis entry point

### P15-03 Figure factory

One package that draws every chart in reports, documentation and the README.

- [x] `P15-03-01` Brand theme
- [x] `P15-03-02` Success bar chart
- [x] `P15-03-03` SVG and PNG export
- [x] `P15-03-04` Render-all function

### P15-04 Regression gates

Fails CI when quality or performance drops.

- [x] `P15-04-01` Baseline store
- [x] `P15-04-02` Gate rule
- [x] `P15-04-03` Gate command

### P16-01 Blueprint document model

The data structure that pairs informal mathematics with formal declarations.

- [x] `P16-01-01` Node record
- [x] `P16-01-02` Node kinds
- [x] `P16-01-03` Dependency edges
- [x] `P16-01-04` Blueprint file format
- [x] `P16-01-05` Parser and printer
- [x] `P16-01-06` Blueprint validation
- [x] `P16-01-07` Blueprint graph export

### P16-02 AI decomposition planner

Breaks an informal argument into a graph of lemmas.

- [x] `P16-02-01` Blueprint decomposition prompt
- [x] `P16-02-02` Plan to blueprint

### P16-03 Skeleton generation

Writes Lean files that state everything and prove nothing yet.

- [x] `P16-03-01` Skeleton emission
- [x] `P16-03-02` Skeleton compilation
- [x] `P16-03-03` Dependency-respecting order

### P16-04 Progress tracking

Knows what is done, what is blocked and what can be worked on now.

- [x] `P16-04-01` Status computation
- [x] `P16-04-02` Status summary
- [x] `P16-04-03` Ready frontier
- [x] `P16-04-04` Blocked nodes
- [x] `P16-04-05` Status command
- [x] `P16-04-06` Next command

### P16-05 Project analytics

Shows how a formalization project is going.

- [x] `P16-05-01` Status series import
- [x] `P16-05-02` Status composition

### P17-01 Breakage detection and classification

Finds what broke and puts each failure in a class with a known treatment.

- [x] `P17-01-01` Failure collection
- [x] `P17-01-02` Breakage record
- [x] `P17-01-03` Class: unknown identifier
- [x] `P17-01-04` Class: unsolved goals

### P17-02 Deterministic repair rules

Fixes the predictable breakages with no model call.

- [x] `P17-02-01` Rule interface
- [x] `P17-02-02` Rename rule
- [x] `P17-02-03` Rule application loop
- [x] `P17-02-04` Suggestion rule
- [x] `P17-02-05` Rule registry

### P17-03 AI repair agent

Repairs what the rules cannot, with the smallest verified edit.

- [x] `P17-03-01` Breakage repair prompt
- [x] `P17-03-02` Verify-and-retry loop
- [x] `P17-03-03` Statement preservation check
- [x] `P17-03-04` Re-prove fallback

### P17-04 Proof minimization and golfing

Makes proofs shorter without making them worse.

- [x] `P17-04-01` Step deletion
- [x] `P17-04-02` Verification of every candidate
- [x] `P17-04-03` Minimized proof output
- [x] `P17-04-04` Minimize command
- [x] `P17-04-05` Statement and axiom preservation

### P17-05 Repair analytics

Shows how well maintenance automation works.

- [x] `P17-05-01` Repair run import
- [x] `P17-05-02` Repair success summary

### P18-01 Axiom and trust audit

Inspects a declaration for everything that could weaken what "proved" means.

- [x] `P18-01-01` Axiom collection
- [x] `P18-01-02` Sorry detection
- [x] `P18-01-03` Standard-axiom classification
- [x] `P18-01-04` Audit command
- [x] `P18-01-05` Native-evaluation detection
- [x] `P18-01-06` Audit tests

### P18-02 Independent re-checking

Checks a result again, separately from the process that produced it.

- [x] `P18-02-01` Fresh-process re-check
- [x] `P18-02-02` Re-check isolation

### P18-03 Trust policy and compliance

Decides what counts as acceptable evidence in a given project.

- [x] `P18-03-01` Allowed-axiom policy
- [x] `P18-03-02` Required re-check policy
- [x] `P18-03-03` Policy evaluation
- [x] `P18-03-04` Policy decision record
- [x] `P18-03-05` Default policy
- [x] `P18-03-06` Trust policy tests

### P18-04 Trust analytics and reporting

Summarizes what the evidence says across a project.

- [x] `P18-04-01` Certificate import
- [x] `P18-04-02` Trust level distribution
- [x] `P18-04-03` Certificate axiom census
- [x] `P18-04-04` Re-check agreement
- [x] `P18-04-05` Re-check time summary
- [x] `P18-04-06` Policy decision summary
- [x] `P18-04-07` Ledger plots
- [x] `P18-04-08` Ledger report table
