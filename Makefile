# Maithic: one entry point for building, testing and checking the repository.
#
# `make ci` runs what the required workflow runs, in the same order, so a
# change that passes here passes there. Every target works with no API key
# and no network: model calls are served from cassettes (replay mode).

SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := help

export MAITHIC_GATEWAY_MODE ?= replay

# The Julia packages depend on the standard library and on each other, by path.
# Nothing is fetched from a registry, so the package manager is told not to try.
export JULIA_PKG_OFFLINE ?= true

JULIA ?= julia
RSCRIPT ?= Rscript
# Whether a warning raised while the R tests run fails them. TRUE here; the workflow passes FALSE,
# because it installs the plotting packages as they are on the day and a newer one may announce a change.
R_WARNINGS_FAIL ?= TRUE
R ?= R
LAKE ?= lake
DUNE ?= dune

DEV := ocaml/_build/default/tools/maithic_dev.exe
MAITHIC := ocaml/_build/default/bin/maithic/main.exe
R_LIB := $(CURDIR)/r/.library
JULIA_PACKAGES := MaithicCore MaithicNNV MaithicEmbed MaithicOracle MaithicGraphs MaithicData MaithicGuide MaithicAlgoRef
R_PACKAGES := maithicstats maithicviz

# The sources the recorded demonstrations of the code graph and of the dataset are made from: a
# fixed set of files, so that the records change only when these files do.
DEMO_SOURCES = fixtures/intel/lean lean/Maithic/Algo/Spec/Sorted.lean lean/Maithic/Algo/Sort/Insertion.lean lean/Maithic/Algo/Sort/Linear.lean

.PHONY: help setup build lean-fixtures shots build-ocaml build-lean build-julia build-r test test-ocaml test-lean test-julia test-r \
        integration e2e e2e-minimal hygiene cap-sync sync cassettes transcripts manifest julia-fixtures guide-fixtures run-fixtures docs \
        figures ci clean

help: ## Show the targets
	@grep -E '^[a-z0-9-]+:.*## ' $(MAKEFILE_LIST) | sed -E 's/:.*## /\t/' | awk -F'\t' '{printf "  %-14s %s\n", $$1, $$2}'

setup: ## Check that the four toolchains are installed
	@for tool in ocaml $(DUNE) lean $(LAKE) $(JULIA) $(RSCRIPT); do \
	  if command -v $$tool >/dev/null 2>&1; then printf '  %-8s %s\n' $$tool "$$(command -v $$tool)"; else printf '  %-8s MISSING\n' $$tool; missing=1; fi; \
	done; \
	if [ -n "$${missing:-}" ]; then echo "Install the missing tools; see docs/install.md."; exit 1; fi

build: build-ocaml build-lean build-julia build-r ## Build everything

build-ocaml: ## Build the OCaml libraries, the maithic command and the developer tool
	cd ocaml && $(DUNE) build

build-lean: ## Build the Lean libraries, the Bridge and the benchmark tool, failing on warnings
	cd lean && $(LAKE) build --wfail

build-julia: ## Resolve and load every Julia package once
	@for p in $(JULIA_PACKAGES); do $(JULIA) --startup-file=no --project=julia/$$p -e "using Pkg; Pkg.resolve(io = devnull); Pkg.instantiate(io = devnull); using $$p"; done

build-r: ## Install the R packages into r/.library
	@mkdir -p $(R_LIB)
	@for p in $(R_PACKAGES); do $(R) CMD INSTALL --no-docs --library=$(R_LIB) r/$$p >/dev/null; echo "installed $$p"; done

test: test-ocaml test-lean test-julia test-r ## Run every unit test suite

test-ocaml: ## OCaml unit tests (no Lean and no network needed)
	cd ocaml && $(DUNE) build @runtest --force

test-lean: ## Lean tests: compile-time checks and the scripted Bridge sessions
	cd lean && $(LAKE) test

test-julia: ## Julia package tests
	@for p in $(JULIA_PACKAGES); do $(JULIA) --startup-file=no --project=julia/$$p -e 'using Pkg; Pkg.test()'; done

test-r: build-r ## R package tests
	@for p in $(R_PACKAGES); do \
	  (cd r/$$p/tests && R_LIBS=$(R_LIB) $(RSCRIPT) -e "library(testthat); library($$p); test_dir('testthat', reporter = 'summary', stop_on_failure = TRUE, stop_on_warning = $(R_WARNINGS_FAIL))"); \
	done

integration: build-ocaml build-lean ## Cross-language tests: the real Bridge and the built command
	cd ocaml && $(DUNE) build @integration --force

e2e: build-ocaml build-lean ## End-to-end sessions in replay mode, as a user would run them (the oracle and guide steps need Julia)
	$(MAITHIC) doctor
	$(MAITHIC) prove examples/even.lean
	$(MAITHIC) prove examples/double.lean --no-ai --search
	$(MAITHIC) tree fixtures/trees/found/double_eq.json --steps
	$(MAITHIC) prove examples/triangle.lean --provider anthropic
	$(MAITHIC) prove examples/triangle.lean --provider openai
	$(MAITHIC) prove examples/triangle.lean --provider xai
	$(MAITHIC) check lean/Maithic/Algo/Sort/Insertion.lean --project lean
	$(MAITHIC) check lean/Maithic/Oracle/Logic/Examples.lean --project lean
	$(MAITHIC) check fixtures/oracle/lean/Checks.lean
	$(MAITHIC) check fixtures/oracle/lean/Replay.lean --project lean
	$(MAITHIC) check fixtures/nnv/lean/Certificates.lean --project lean
	$(MAITHIC) oracle list
	$(MAITHIC) oracle solve fixtures/oracle/goals/linear-integer-gap.json
	$(MAITHIC) oracle solve examples/gap.lean --theorem no_point --project lean
	status=0; $(MAITHIC) refute examples/conjectures.lean || status=$$?; test $$status -eq 8
	$(MAITHIC) refute examples/even.lean --method test,vacuity
	$(MAITHIC) intel scan $(DEMO_SOURCES) --project lean --dir .maithic/e2e/intel
	$(MAITHIC) intel show insertionSort --dir .maithic/e2e/intel
	$(MAITHIC) intel dependents Maithic.Algo.insertSorted --dir .maithic/e2e/intel
	$(MAITHIC) intel health --dir .maithic/e2e/intel
	$(MAITHIC) corpus harvest $(DEMO_SOURCES) --project lean --out .maithic/e2e/corpus --name e2e
	$(MAITHIC) corpus verify .maithic/e2e/corpus
	$(MAITHIC) guide info fixtures/guide/library/model.json
	$(MAITHIC) guide train .maithic/e2e/corpus .maithic/e2e/guide --seed 1 --epochs 5 --dim 8
	$(MAITHIC) guide rank .maithic/e2e/guide/model.json --file examples/double.lean --theorem double_eq
	$(MAITHIC) prove examples/guided.lean --no-ai --guide fixtures/guide/library/model.json
	$(MAITHIC) review fixtures/review/lean --suggest
	status=0; $(MAITHIC) review fixtures/intel/lean || status=$$?; test $$status -eq 7
	status=0; $(MAITHIC) mend scan fixtures/mend/lean/Upgrade.lean || status=$$?; test $$status -eq 7
	$(MAITHIC) mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml
	status=0; $(MAITHIC) mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml --no-ai || status=$$?; test $$status -eq 7
	$(MAITHIC) mend minimize fixtures/mend/lean/Verbose.lean --automation
	$(MAITHIC) formalize --file fixtures/formalize/problems/negative-square.txt --candidates 4 --write .maithic/e2e/NegativeSquare.lean
	status=0; $(MAITHIC) formalize --file fixtures/formalize/problems/subtraction.md || status=$$?; test $$status -eq 7
	$(MAITHIC) formalize "Show that the square of a negative integer is positive." --no-ai --statement "(n : Int) (h : n < 0) : 0 < n ^ 2"
	$(MAITHIC) prove .maithic/e2e/NegativeSquare.lean
	$(MAITHIC) council examples/council.lean --transcript .maithic/e2e/council.md
	$(MAITHIC) council examples/even.lean --no-ai
	status=0; $(MAITHIC) council examples/conjectures.lean --theorem sum_le_product || status=$$?; test $$status -eq 8
	status=0; $(MAITHIC) council examples/council.lean --budget-model-calls 2 || status=$$?; test $$status -eq 7
	$(MAITHIC) blueprint check examples/blueprint/triangles.md
	$(MAITHIC) blueprint plan examples/blueprint/argument.md --namespace Triangles --out .maithic/e2e/plan.blueprint.md
	$(MAITHIC) blueprint skeleton examples/blueprint/triangles.md --out .maithic/e2e/Triangles.lean --force
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean examples/blueprint/triangles.lean --svg .maithic/e2e/blueprint.svg
	$(MAITHIC) blueprint next examples/blueprint/triangles.md --lean examples/blueprint/triangles.lean
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean fixtures/blueprint/lean/Stage4.lean --strict
	$(MAITHIC) blueprint graph examples/blueprint/triangles.md --format dot --out .maithic/e2e/blueprint.dot
	$(MAITHIC) search sorting keeps the elements --from fixtures/intel/extractions --limit 5
	$(MAITHIC) search add comm --in lean/Maithic/Algo/Arith --project lean --lean --limit 4
	$(MAITHIC) nnv verify examples/nnv/small-classifier.json examples/nnv/unit-square.json --write .maithic/e2e/UnitSquare.lean
	$(MAITHIC) check .maithic/e2e/UnitSquare.lean --project lean
	status=0; $(MAITHIC) nnv verify examples/nnv/absolute-value.json examples/nnv/too-tight.json || status=$$?; test $$status -eq 8
	$(MAITHIC) algo synth examples/algo/run-lengths.toml --write .maithic/e2e/RunLengths.lean
	$(MAITHIC) check .maithic/e2e/RunLengths.lean
	status=0; $(MAITHIC) algo synth examples/algo/run-lengths.toml --given fixtures/algolab/given/loses-a-run.lean --no-ai || status=$$?; test $$status -eq 7
	$(MAITHIC) check lean/Maithic/Algo/Lab/RunLengths.lean --project lean
	$(MAITHIC) check lean/Maithic/Algo/Cost/Comparisons.lean --project lean
	$(MAITHIC) algo differential
	$(MAITHIC) bench run --no-ai
	$(MAITHIC) bench gate bench/results/basics-portfolio.json

e2e-minimal: build-ocaml build-lean ## The minimal profile: OCaml and Lean only, AI switched off
	MAITHIC_GATEWAY_MODE=off $(MAITHIC) prove examples/even.lean
	MAITHIC_GATEWAY_MODE=off $(MAITHIC) bridge selftest
	MAITHIC_GATEWAY_MODE=off $(MAITHIC) blueprint status examples/blueprint/triangles.md --lean examples/blueprint/triangles.lean
	MAITHIC_GATEWAY_MODE=off $(MAITHIC) search insertion sort is sorted --from fixtures/intel/extractions --limit 3
	MAITHIC_GATEWAY_MODE=off $(MAITHIC) algo synth examples/algo/run-lengths.toml --given fixtures/algolab/given/keeps-every-run.lean --no-ai
	MAITHIC_GATEWAY_MODE=off $(MAITHIC) bench gate bench/results/basics-portfolio.json

hygiene: build-ocaml ## Language share, capability register, scans, README images, mirrored files
	$(DEV) all

cap-sync: build-ocaml ## Update the capability register and its status page from the tests
	$(DEV) cap-sync

sync: build-ocaml ## Rewrite the files that mirror built-in data (model registry, prompts, tables)
	$(DEV) sync

cassettes: build-ocaml build-lean ## Author the replay cassettes of the demonstrations
	$(DEV) cassettes

transcripts: build-ocaml build-lean ## Record the Bridge transcripts that the unit tests replay
	$(DEV) bridge-record

manifest: build-lean ## Export the benchmark manifest from the Lean package
	lean/.lake/build/bin/maithic-bench manifest > bench/suites/manifest.json

julia-fixtures: build-julia ## Recompute the fixtures that Julia writes for the other languages: oracle and network certificates
	$(JULIA) --startup-file=no --project=julia/MaithicOracle julia/MaithicOracle/test/fixtures.jl
	$(JULIA) --startup-file=no --project=julia/MaithicNNV julia/MaithicNNV/test/fixtures.jl

lean-fixtures: build-ocaml build-lean build-julia ## Record what Lean says about the demonstration sources: extractions, the code graph, the dataset, the review and the refutations
	$(MAITHIC) intel extract $(DEMO_SOURCES) --project lean --trace --out fixtures/intel/extractions
	$(MAITHIC) intel scan $(DEMO_SOURCES) --project lean --dir fixtures/intel
	$(JULIA) --startup-file=no --project=julia/MaithicGraphs julia/MaithicGraphs/bin/metrics.jl fixtures/intel/graph.json > fixtures/intel/metrics.json
	$(MAITHIC) corpus harvest $(DEMO_SOURCES) --project lean --out fixtures/corpus/demo --name demo --revision fixture
	$(MAITHIC) intel extract fixtures/intel/lean fixtures/review/lean --lint --duplicates --out fixtures/review/extractions
	-$(MAITHIC) review fixtures/intel/lean fixtures/review/lean --suggest --format json --out fixtures/review/report.json
	-$(MAITHIC) refute examples/conjectures.lean --quiet --out fixtures/refute/conjectures.json
	-$(MAITHIC) mend scan fixtures/mend/lean/Upgrade.lean --quiet --out fixtures/mend/scan.json
	-$(MAITHIC) mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml --no-ai --no-prove --quiet --out fixtures/mend/repair-rules.json
	-$(MAITHIC) mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml --no-ai --quiet --out fixtures/mend/repair-no-ai.json
	MAITHIC_GATEWAY_MODE=replay $(MAITHIC) mend repair fixtures/mend/lean/Upgrade.lean --renames fixtures/mend/renames.toml --quiet --out fixtures/mend/repair.json
	$(MAITHIC) mend minimize fixtures/mend/lean/Verbose.lean --automation --quiet --out fixtures/mend/minimization.json
	MAITHIC_GATEWAY_MODE=replay $(MAITHIC) formalize --file fixtures/formalize/problems/negative-square.txt --candidates 4 --quiet --out fixtures/formalize/negative-square.json --write fixtures/formalize/lean/NegativeSquare.lean
	-MAITHIC_GATEWAY_MODE=replay $(MAITHIC) formalize --file fixtures/formalize/problems/subtraction.md --quiet --out fixtures/formalize/subtraction.json
	MAITHIC_GATEWAY_MODE=replay $(MAITHIC) formalize --file fixtures/formalize/problems/square-bound.txt --candidates 4 --quiet --out fixtures/formalize/square-bound.json
	-$(MAITHIC) formalize --file fixtures/formalize/problems/subtraction.md --no-ai --statement "(a b : Nat) : a - b + b = a" --statement "(a b : Nat) (h : b ≤ a) : a - b + b = a" --quiet --out fixtures/formalize/subtraction-no-ai.json
	MAITHIC_GATEWAY_MODE=replay $(MAITHIC) council examples/council.lean --quiet --transcript fixtures/council/sum-odd.md --out fixtures/council/sum-odd.json
	MAITHIC_GATEWAY_MODE=replay $(MAITHIC) blueprint plan examples/blueprint/argument.md --namespace Triangles --quiet --out fixtures/blueprint/plan.blueprint.md
	$(MAITHIC) blueprint graph examples/blueprint/triangles.md --format json --quiet --out fixtures/blueprint/graph.json
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean fixtures/blueprint/lean/Stage1.lean --label "skeleton" --quiet --out fixtures/blueprint/snapshots/1-skeleton.json
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean examples/blueprint/triangles.lean --label "half done" --quiet --out fixtures/blueprint/snapshots/2-half-done.json
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean fixtures/blueprint/lean/Stage3.lean --label "one proof left" --quiet --out fixtures/blueprint/snapshots/3-one-proof-left.json
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean fixtures/blueprint/lean/Stage4.lean --label "finished" --quiet --out fixtures/blueprint/snapshots/4-finished.json
	$(MAITHIC) search --from fixtures/intel/extractions --queries fixtures/search/queries.json --rankings fixtures/search/rankings-merged.json --quiet
	$(MAITHIC) search --from fixtures/intel/extractions --queries fixtures/search/queries.json --only lexical --rankings fixtures/search/rankings-lexical.json --quiet
	$(MAITHIC) search --from fixtures/intel/extractions --queries fixtures/search/queries.json --only name --rankings fixtures/search/rankings-name.json --quiet
	$(MAITHIC) nnv verify examples/nnv/absolute-value.json examples/nnv/bounded.json --quiet --out fixtures/nnv/jobs/bounded.json
	-$(MAITHIC) nnv verify examples/nnv/absolute-value.json examples/nnv/too-tight.json --quiet --out fixtures/nnv/jobs/too-tight.json
	-$(MAITHIC) nnv verify examples/nnv/absolute-value.json examples/nnv/beyond-the-bounds.json --quiet --out fixtures/nnv/jobs/beyond-the-bounds.json
	$(MAITHIC) nnv verify examples/nnv/small-classifier.json examples/nnv/unit-square.json --quiet --out fixtures/nnv/jobs/unit-square.json --write fixtures/nnv/lean/UnitSquare.lean
	MAITHIC_GATEWAY_MODE=replay $(MAITHIC) algo synth examples/algo/run-lengths.toml --quiet --out fixtures/algolab/run-lengths.json --write fixtures/algolab/lean/RunLengths.lean
	-$(MAITHIC) algo synth examples/algo/run-lengths.toml --given fixtures/algolab/given/loses-a-run.lean --no-ai --quiet --out fixtures/algolab/loses-a-run.json
	$(MAITHIC) algo differential --quiet --out fixtures/algolab/differential.json
	$(JULIA) --startup-file=no --project=julia/MaithicAlgoRef julia/MaithicAlgoRef/bin/costs.jl lean/.lake/build/bin/maithic-algo > fixtures/algolab/costs.csv

guide-fixtures: build-ocaml build-lean build-julia ## Train the tactic model the repository keeps on the proofs of the library, and record the runs that compare the shapes of model (minutes)
	$(MAITHIC) corpus harvest lean/Maithic --project lean --out .maithic/guide/corpus --name maithic-library --revision working-tree
	$(MAITHIC) guide train .maithic/guide/corpus fixtures/guide/library --seed 1 --name library
	$(JULIA) --startup-file=no --project=julia/MaithicGuide julia/MaithicGuide/bin/compare.jl .maithic/guide/corpus fixtures/guide/runs --seeds 1,2,3,4,5
	@mkdir -p fixtures/guide/rankings
	$(MAITHIC) guide rank fixtures/guide/library/model.json --file examples/double.lean --theorem double_eq --json > fixtures/guide/rankings/double.json

docs: sync ## Regenerate the generated reference pages
	$(MAITHIC) config reference > docs/reference/configuration-table.md

run-fixtures: build-ocaml build-lean ## Record the demonstrations as result files for the statistics tests and figures
	@mkdir -p fixtures/runs
	$(MAITHIC) prove examples/even.lean --json > fixtures/runs/prove-even.json
	$(MAITHIC) prove examples/double.lean --no-ai --search --search-trees fixtures/trees/found --json > fixtures/runs/prove-double.json
	-$(MAITHIC) prove examples/double.lean --no-ai --search --search-expansions 2 --search-trees fixtures/trees/stopped > /dev/null
	cp fixtures/trees/found/double_eq.svg assets/readme/search-tree.svg
	@for p in anthropic openai xai; do $(MAITHIC) prove examples/triangle.lean --provider $$p --json > fixtures/runs/prove-triangle-$$p.json; done
	$(MAITHIC) check lean/Maithic/Algo/Sort/Insertion.lean --project lean --json > fixtures/runs/check-insertion.json
	$(MAITHIC) check lean/Maithic/Oracle/Logic/Examples.lean --project lean --json > fixtures/runs/check-logic.json
	$(MAITHIC) check fixtures/oracle/lean/Checks.lean --json > fixtures/runs/check-oracle.json
	$(MAITHIC) check fixtures/oracle/lean/Replay.lean --project lean --json > fixtures/runs/check-replay.json
	-$(MAITHIC) check examples/triangle.lean --json > fixtures/runs/check-triangle.json

figures: build-ocaml build-lean build-r ## Re-run the benchmark and redraw the figures made from it and from the recorded runs
	$(MAITHIC) bench run --no-ai --out bench/results/basics-portfolio.json --quiet
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_all('bench/results/basics-portfolio.json', 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_reports('fixtures/runs', 'assets/readme'))"
	$(MAITHIC) intel scan lean/Maithic --project lean --dir .maithic/figures/intel
	$(MAITHIC) corpus harvest lean/Maithic --project lean --out .maithic/figures/corpus --name maithic-library --revision working-tree
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_insights('.maithic/figures/intel', '.maithic/figures/corpus', 'fixtures/review/report.json', 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_library('.maithic/figures/intel', 'assets/readme', label = 'lean/Maithic'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_register('registry/status.json', 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_search('fixtures/search/queries.json', c('fixtures/search/rankings-lexical.json', 'fixtures/search/rankings-name.json'), 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_guide('fixtures/guide/runs', 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_mend(c('rules' = 'fixtures/mend/repair-rules.json', 'rules and the prover' = 'fixtures/mend/repair-no-ai.json', 'rules, a model and the prover' = 'fixtures/mend/repair.json'), 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_council('fixtures/council/sum-odd.json', 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_formalize(c('the square of a negative integer' = 'fixtures/formalize/negative-square.json', '(a - b) + b = a' = 'fixtures/formalize/subtraction.json', 'n squared is at least n' = 'fixtures/formalize/square-bound.json'), 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_blueprint(file.path('fixtures/blueprint/snapshots', c('1-skeleton.json', '2-half-done.json', '3-one-proof-left.json', '4-finished.json')), 'assets/readme'))"
	R_LIBS=$(R_LIB) $(RSCRIPT) -e "invisible(maithicviz::render_growth('fixtures/algolab/costs.csv', 'assets/readme'))"
	$(MAITHIC) blueprint status examples/blueprint/triangles.md --lean examples/blueprint/triangles.lean --svg assets/readme/blueprint-status.svg --quiet

shots: build-ocaml build-lean ## Redraw the pictures of terminal sessions from what the built command prints (the oracle pictures need Julia)
	$(DEV) shots

ci: build test integration e2e e2e-minimal hygiene ## Everything the required workflow runs

clean: ## Remove build products
	rm -rf ocaml/_build lean/.lake r/.library
