# Commands

`maithic --help` lists the commands; `maithic COMMAND --help` lists the options of one.

## Global options

| Option | Meaning |
|---|---|
| `--config FILE` | Read this configuration file instead of the discovered ones. |
| `--provider NAME` | Model provider: `anthropic`, `openai`, `xai`, `local` or `mock`. |
| `--model ID` | Model identifier; overrides the tier. |
| `--tier TIER` | Quality tier: `fast`, `balanced` or `best`. |
| `--mode MODE` | Gateway mode: `live`, `record`, `replay`, `mock` or `off`. |
| `--json` | Print machine-readable JSON instead of text. |
| `--quiet`, `--verbose` | Print less, or print progress details. |
| `--no-color` | Do not colour the output. Colour is also off when `NO_COLOR` is set or the output is not a terminal; `CLICOLOR_FORCE=1` asks for it where the output is not a terminal. |

## `maithic prove FILE`

Proves theorems of a Lean file. Without `--theorem`, every theorem whose proof contains `sorry` is attempted.

| Option | Meaning |
|---|---|
| `--theorem NAME` | The theorem to prove; may be repeated. |
| `--no-ai` | Use only the automation portfolio. |
| `--search` | Search one tactic at a time when the portfolio fails, before any model is asked. See [the guide](../guides/search.md). |
| `--search-expansions N` | Goals the search may expand for one theorem. |
| `--search-trees DIR` | Write the record of every step-wise search, `DIR/NAME.json`, and a picture of it, `DIR/NAME.svg`. |
| `--oracle` | Ask the oracles for a certificate when the portfolio fails on an arithmetic goal. |
| `--rounds N` | Model rounds per theorem. Each round sees the errors of the previous one. |
| `--candidates N` | Proofs requested from the model in each round. |
| `--project DIR` | A Lake project whose modules the file may import. |
| `--policy FILE` | A trust policy file. |
| `--heartbeats N` | Step budget of one elaboration, in thousands of heartbeats. |
| `--write` | Write each verified proof back into the file. |

Exit status: 0 when every result is `verified`, 8 when a statement was refuted, 7 otherwise.

## `maithic check FILE`

Elaborates a file from its imports in a new Bridge process and reports each theorem with its status and axioms.
Options: `--project`, `--policy`, `--heartbeats`. Exit status 0 when there is no error and every theorem is
verified, 7 otherwise.

## `maithic tree FILE`

Shows the record of a step-wise search that `prove --search-trees` wrote: the statement, how the search ended,
what it cost and the proof. The command needs neither Lean nor the file the search was about.

| Option | Meaning |
|---|---|
| `--steps` | Print the search step by step: every goal that was expanded, how long it took, and what each tactic did on it. A star marks the tactic that proved the goal. |
| `--svg OUT` | Write a picture of the search tree to OUT. |
| `--max-nodes N` | The most goals the picture shows; sixty unless stated. |

With `--json` the record itself is printed, in the schema `maithic.prove.search/1`. A file that is not such a
record is a usage error.

## `maithic refute FILE`

Looks for a counterexample to the theorems of a file whose proofs are unfinished, or to the one named with
`--theorem`, and for hypotheses that make a statement empty. A counterexample counts only when Lean accepts a
proof that the statement is false at its values; that proof is printed. See
[the guide](../guides/refuting-statements.md).

| Option | Meaning |
|---|---|
| `--theorem NAME` | Examine this theorem, whatever its proof. |
| `--method LIST` | The methods to try, separated by commas: `test`, `oracle`, `vacuity`, or `all` (the default). They are tried in that order. |
| `--cases N` | Cases that must pass in one round of testing (100). |
| `--size N` | The size of the largest case (30). |
| `--seed N` | The seed of the first round (0). Each round takes the next. |
| `--rounds N` | Rounds of testing (3). |
| `--seconds S` | No method is started after this many seconds (60). |
| `--project DIR` | A Lake project whose modules the file may import. |
| `--out FILE` | Write the report as JSON to FILE as well. |

With `--json` the report is printed, in the schema `maithic.refute.report/1`. Exit status: 8 when a statement is
refuted, 7 when one is vacuous or has a counterexample that Lean did not confirm, 0 when nothing was found.

## `maithic intel scan | extract | show | deps | dependents | health | stats`

Reads Lean files into a graph of their declarations and answers questions about it. See
[the guide](../guides/code-intelligence.md).

| Command | What it does |
|---|---|
| `intel scan PATH...` | Elaborates every Lean file among the paths and writes `graph.json` and `health.json`. |
| `intel extract PATH...` | Writes what Lean reports about each file as one JSON document, and all declarations as JSON lines. `--trace` adds the tactic steps of every proof, `--lint` the findings of the lint rules, `--duplicates` the theorems that state the same; `--out DIR` says where (`.maithic/extract`). |
| `intel show NAME` | A declaration: its kind, place, documentation, axioms, what it uses and what uses it. |
| `intel deps NAME` | What it depends on, directly and through others. `--depth N` limits the distance; `--all` follows into declarations from outside the scanned files. |
| `intel dependents NAME` | What depends on it. `--depth N` limits the distance. |
| `intel health` | Declarations by kind, every unfinished proof with its open goal, and the axioms in use. |
| `intel stats` | The size of the graph and the declarations most others use. |

`scan` and `extract` take `--project DIR`. Every command but `extract` takes `--dir DIR`, where the graph is
kept: `.maithic/intel` unless given. The commands that read the graph need no Lean.

## `maithic corpus harvest | verify`

Writes the proofs of Lean files as a dataset, and checks a dataset. See [the guide](../guides/datasets.md).

| Command | What it does |
|---|---|
| `corpus harvest PATH...` | Traces every Lean file among the paths and writes theorem, step, premise and pair records as shards of JSON lines, with a manifest of hashes and a record of the harvest. Options: `--project DIR`, `--out DIR` (`dataset`), `--name NAME`, `--dataset-version V`, `--shard-size N` (5000), `--revision TEXT`. |
| `corpus verify DIR` | Checks every shard against the hash and the count in the manifest, every record against its schema, and that the directory holds nothing else. Exit status 7 when the dataset is not sound. |

## `maithic review PATH...`

Reviews Lean files: unfinished work, axioms, restated theorems, documentation, naming, and simpler proofs. See
[the guide](../guides/review.md).

| Option | Meaning |
|---|---|
| `--project DIR` | A Lake project whose modules the files may import. |
| `--profile NAME` | The policy to start from: `default`, `strict` or `minimal`. |
| `--review-config FILE` | A policy file with `[review]` tables; `maithic-review.toml` is read when it is there. |
| `--suggest` | Try simpler proofs and report the ones Lean accepts. |
| `--ai` | Ask the model for findings as well. It can add notes and cannot remove or lower a finding. |
| `--format FORMAT` | `terminal` (the default), `markdown` or `json`. |
| `--out FILE` | Write the report to FILE instead of printing it. |

Exit status: 7 when a finding is an error, 0 otherwise.

## `maithic oracle list | solve`

`list` prints the oracles with the kinds of goal and the number types each takes.

`solve FILE` sends a goal to an oracle and prints its answer: proved with a certificate, refuted with an exact
counterexample, or unknown with the reason. FILE is a goal document, or a Lean file with `--theorem NAME`, whose
goal is then exported: the integer or rational variables, the hypotheses that are comparisons, and the target.
Hypotheses that say something else are left out and listed. Options: `--theorem NAME`, `--oracle NAME`,
`--project DIR`. With `--json` the answer is a record in the schema `maithic.oracle.result/1`.

An answer is a claim of the oracle. `maithic prove --oracle` has Lean check it. Exit status: 0 for proved, 8 for
refuted, 7 for unknown. The oracle engines need Julia 1.11 or later.

## `maithic guide train | info | rank`

A small model that orders the tactics of the step-wise search. See [the guide](../guides/guided-search.md).

`guide train DATASET OUT` trains a model on a dataset made by `maithic corpus harvest` and writes
`OUT/model.json` and `OUT/run.json`. Options: `--seed N`, `--epochs N`, `--batch N`, `--dim N`,
`--max-length N`, `--pooling mean|first`, `--mixer none|attention`, `--rate X`, `--weight-decay X`, `--k N`,
`--name NAME`. `guide info MODEL [--top N]` describes a model. `guide rank MODEL [TACTIC...]` shows how the
model orders tactics on a goal given with `--goal TEXT`, `--goal-file FILE`, or `--file FILE --theorem NAME`.
`maithic prove --guide MODEL` uses a model in the search. The commands need Julia.

## `maithic mend scan | repair | minimize`

Finds what is broken in Lean files, repairs it and shortens proofs. See
[the guide](../guides/repairing-proofs.md).

| Command | What it does | Exit status |
|---|---|---|
| `mend scan PATH...` | Lists the breakages of files and folders by declaration and class. `--out FILE`. | 7 when a file has an error |
| `mend repair FILE` | Repairs by rule, then with a model, then with the prover; every edit is compiled by Lean. `--renames FILE`, `--no-ai`, `--no-prove`, `--rounds N`, `--candidates N`, `--write`, `--out FILE`, `--policy FILE`. | 7 when something is left |
| `mend minimize FILE` | Removes what a proof does not need, keeping the statement and the axioms. `--theorem NAME`, `--automation`, `--max-checks N`, `--write`, `--out FILE`. | 0 |

## `maithic formalize [PROBLEM]`

States a problem written in words as a Lean theorem and checks the statement. The problem is the argument,
the content of `--file FILE`, or standard input. See [the guide](../guides/formalizing.md).

| Option | Meaning |
|---|---|
| `--file FILE` | Read the problem from a file; `-` is standard input. |
| `--statement TEXT` | A statement to check instead of drafting; may be repeated. |
| `--candidates N` | Statements asked of the model (3). |
| `--rounds N` | Repair rounds for a statement that does not compile (2). |
| `--name NAME` | The name the theorem is given (`problem`). |
| `--import MODULE` | A module the statements may use, besides the core library; may be repeated. |
| `--no-ai` | Ask no model: check the statements given with `--statement`, with Lean alone. |
| `--write FILE` | Write the chosen statement as a Lean file, with `sorry` for a proof. |
| `--out FILE` | Write the run as JSON. |

Exit status: 0 for a statement that no check flagged; 7 when the chosen statement is flagged or no candidate
is left.

## `maithic council FILE`

Puts four roles to work on a theorem: a researcher, a prover, a critic and a verifier. See
[the guide](../guides/council.md).

| Option | Meaning |
|---|---|
| `--theorem NAME` | The theorem to work on; by default the one theorem with an unfinished proof. |
| `--goal TEXT` | What you are after, in words. |
| `--rounds N`, `--candidates N` | Rounds after the examination (3); proofs the prover asks for in a turn (3). |
| `--no-ai` | Only the examination: the statement is checked, a counterexample is looked for, automation tries. |
| `--budget-tokens N`, `--budget-cost USD`, `--budget-seconds S`, `--budget-lean-calls N`, `--budget-model-calls N` | The limits of the session; 0 is no limit. |
| `--price-input USD`, `--price-output USD` | The price of a million tokens read and written, for `--budget-cost`. |
| `--transcript FILE` | Write the transcript as Markdown. |
| `--out FILE` | Write the session as JSON. |
| `--write` | Write a verified proof into the file. |
| `--policy FILE` | A trust policy file. |

Exit status: 0 when the claim is verified, 8 when it is refuted, 7 otherwise.

## `maithic blueprint check | plan | skeleton | status | next | graph`

A formalization as a plan of nodes, and where it stands according to Lean. See
[the guide](../guides/blueprints.md).

| Command | What it does | Exit status |
|---|---|---|
| `blueprint check FILE` | Reads a blueprint and reports what is wrong with it. | 7 when there is a problem |
| `blueprint plan ARGUMENT` | Has a model take an argument in words apart into nodes, puts the answer in order and writes a blueprint. `--out FILE`, `--title TEXT`, `--namespace NAME`, `--import MODULE`. | 0 |
| `blueprint skeleton FILE` | Writes the statements as Lean, each with `sorry` for a proof, and has Lean compile them. `--out FILE`, `--no-check`, `--force`, `--project DIR`. | 7 when Lean rejects the skeleton |
| `blueprint status FILE` | Gives every node a status from what Lean reports: planned, stated, proved or done. `--lean FILE`, `--label TEXT`, `--out FILE`, `--svg FILE`, `--strict`. | 7 with `--strict` unless every node is done |
| `blueprint next FILE` | Lists the nodes that can be worked on now, the one most others wait for first. `--lean FILE`, `--limit N`, `--blocked`. | 0 |
| `blueprint graph FILE` | Writes the dependency graph. `--format json\|dot\|svg`, `--out FILE`. | 0 |

## `maithic search QUERY...`

Finds declarations by the words of their names, statements and documentation. See
[the guide](../guides/finding-declarations.md).

| Option | Meaning |
|---|---|
| `--from DIR` | Declarations that `maithic intel extract --out DIR` wrote. |
| `--in PATH` | A Lean file or a folder of them to read now; may be repeated. `--project DIR` names their Lake project. |
| `--lean`, `--import MODULE` | Ask Lean about everything in scope as well; modules to have in scope. |
| `--limit N`, `--kind KIND`, `--module PREFIX` | The number of hits (10); only declarations of a kind, or of modules with a prefix. |
| `--only lexical\|name` | Ask one retriever. |
| `--queries FILE`, `--rankings FILE` | Answer a query set and write the rankings for evaluation. |

Exit status: 0 when something is found, 7 when nothing is.

## `maithic nnv verify MODEL PROPERTY`

Decides a property of a ReLU network with a verifier in Julia and has Lean's kernel check the answer. See
[the guide](../guides/verifying-networks.md). Options: `--max-corners N` (1024), `--write FILE` for the Lean
theorem, `--out FILE` for the job as JSON, `--policy FILE`. Exit status: 0 for verified, 8 for refuted, 7
when the verifier cannot tell or Lean does not accept its claim. The command needs Julia.

## `maithic algo synth | differential`

A function drafted by a model and judged by Lean, and the verified algorithms tested against independent
references. See [the guide](../guides/algorithm-lab.md).

| Command | What it does | Exit status |
|---|---|---|
| `algo synth TASK` | Drafts a Lean function for a task, compiles it, runs the task's examples, and feeds what went wrong back to the model. `--rounds N` (3), `--given FILE`, `--no-ai`, `--import MODULE`, `--write FILE`, `--out FILE`, `--project DIR`. | 7 when nothing was accepted |
| `algo differential` | Sends generated requests to the compiled program `maithic-algo` and to reference implementations in Julia, and reports every answer that differs. `--exe FILE`, `--seed N` (1), `--count N` (190), `--size N` (12), `--algorithm NAME`, `--out FILE`. Needs Julia. | 7 when an answer is not accepted |

## `maithic bench run | gate`

`bench run` runs the prover on every problem of a suite manifest. Options: `--manifest FILE`,
`--project DIR`, `--suite NAME`, `--out FILE`, `--no-ai`, `--rounds N`, `--policy FILE`. By default the
manifest is `bench/suites/manifest.json` and the project is `lean/`, both found at or above the current
directory.

`bench gate RUN` holds a run against the stored baseline of each suite and fails when a problem that the
baseline verified is no longer verified. See [the guide](../guides/regression-gate.md). Options:
`--baselines DIR` (`bench/baselines`), `--suite NAME`, `--max-regressions N` (0), `--min-rate X`,
`--max-slowdown X`, `--max-attempts X`, `--allow-missing`, `--update` to store the run as the baseline,
`--note TEXT`, `--out FILE`. Exit status: 13 when a suite fails.

## `maithic doctor`

Checks `lean`, `lake`, the Bridge (with its self-test), `curl`, `julia` and `Rscript`, then the gateway mode,
provider, model and key. Exit status 4 when a required component is missing.

## `maithic config show | validate | reference`

`show` prints every setting with its effective value and the layer it came from. `validate` loads the
configuration and reports the first problem. `reference` prints the settings table as Markdown.

## `maithic auth status | login | logout | check`

`status` lists the providers and whether each has a key, showing only the last four characters. `login` reads a
key without echoing it and stores it in `credentials.toml` in the Maithic home directory, readable by its owner
only. `logout` removes it. `check` makes one small call through the gateway.

## `maithic models`, `maithic bridge selftest`, `maithic version`

`models` prints the model registry. `bridge selftest` starts the Bridge and runs its five health checks.
`version` prints the version of the command, the Bridge protocol and the pinned Lean toolchain.

## `maithic help exit-codes | status`

Print the exit-code table and the status vocabulary.
