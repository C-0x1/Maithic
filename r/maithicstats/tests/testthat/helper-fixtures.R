# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Shared by the tests: where the repository's fixtures are, and builders for
# small result files whose every number can be checked by hand.

fixture_root <- function() {
  dir <- normalizePath(getwd())
  for (step in 1:12) {
    if (file.exists(file.path(dir, "PROJECT.toml"))) return(file.path(dir, "fixtures"))
    dir <- dirname(dir)
  }
  NULL
}

run_fixture <- function(name) file.path(fixture_root(), "runs", name)

write_document <- function(document) {
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(document, path, auto_unbox = TRUE, null = "null")
  path
}

attempt <- function(accepted, ms, class = NULL, round = 0, proposer = "portfolio") {
  out <- list(round = round, proposer = proposer, proof = "by omega", accepted = accepted, elapsed_ms = ms)
  if (!is.null(class)) out$error_class <- class
  out
}

prove_result <- function(theorem, status = "verified", proposer = "portfolio", attempts = list(), rounds = 0, ms = 100,
                         proof = "by omega", axioms = character(), recheck_ms = NULL, recheck_axioms = axioms,
                         violations = character()) {
  out <- list(schema = "maithic.prove.result/1", theorem = theorem, status = status, proposer = proposer, proof = proof,
              axioms = as.list(axioms), decision = list(status = status, reasons = list(), violations = as.list(violations)),
              attempts = attempts, rounds_used = rounds, elapsed_ms = ms)
  if (!is.null(recheck_ms)) out$recheck <- list(passed = TRUE, axioms = as.list(recheck_axioms), elapsed_ms = recheck_ms)
  out
}

usage_record <- function(purpose, provider, model, ok = TRUE, input = 0, output = 0, ms = 10, at = "2026-11-20T10:00:00Z",
                         class = NULL, served_by = "live", cached = NULL, reasoning = NULL) {
  out <- list(purpose = purpose, provider = provider, model = model, served_by = served_by, ok = ok, input_tokens = input,
              output_tokens = output, started_at = at, elapsed_ms = ms)
  if (!is.null(class)) out$error_class <- class
  if (!is.null(cached)) out$cached_tokens <- cached
  if (!is.null(reasoning)) out$reasoning_tokens <- reasoning
  out
}

write_prove_run <- function(results, records = list(), mode = "live", file = "demo.lean", total = length(results), verified = NULL) {
  if (is.null(verified)) verified <- sum(vapply(results, function(r) identical(r$status, "verified"), logical(1)))
  write_document(list(
    schema = "maithic.prove.run/1", file = file, policy = list(name = "default"), results = results,
    summary = list(total = total, verified = verified),
    gateway = list(mode = mode, calls = length(records), failed_calls = sum(!vapply(records, function(r) r$ok, logical(1))), records = records)
  ))
}

tri_proof <- "by\n  induction n with\n  | zero => rfl\n  | succ k ih => simp only [tri]; grind"

demo_records <- list(
  usage_record("prove.propose", "anthropic", "m1", input = 1000, output = 200, ms = 800, at = "2026-11-20T10:00:00Z"),
  usage_record("prove.repair", "anthropic", "m1", input = 1500, output = 300, ms = 1200, at = "2026-11-20T23:59:59Z"),
  usage_record("prove.repair", "anthropic", "m1", ok = FALSE, ms = 30, class = "rate_limited", at = "2026-11-22T00:00:00Z")
)

demo_run_paths <- function() {
  first <- write_prove_run(list(
    prove_result("t1", attempts = list(attempt(FALSE, 5, "tactic-failed"), attempt(TRUE, 7)), ms = 100, axioms = "propext", recheck_ms = 40),
    prove_result("t2", proposer = "model", rounds = 1, ms = 900, proof = tri_proof, axioms = c("propext", "Classical.choice"),
                 recheck_ms = 60, recheck_axioms = "propext",
                 attempts = list(attempt(FALSE, 5, "tactic-failed"), attempt(FALSE, 6, "type-mismatch"),
                                 attempt(FALSE, 4, "tactic-failed", round = 1, proposer = "model"), attempt(TRUE, 10, round = 1, proposer = "model"))),
    prove_result("t3", status = "failed", proposer = "", rounds = 2, ms = 2000, proof = "",
                 attempts = list(attempt(FALSE, 500, "timeout"), attempt(FALSE, 3)))
  ), records = demo_records)
  second <- write_prove_run(list(prove_result("u1", attempts = list(attempt(TRUE, 3)), ms = 50, recheck_ms = 20)), file = "other.lean", mode = "off")
  c(first, second)
}

demo_prices <- data.frame(provider = c("anthropic", "openai"), model = c("m1", "g1"), input_per_mtok = c(3, 2),
                          output_per_mtok = c(15, 8), cached_per_mtok = c(3, 0.5), stringsAsFactors = FALSE)

# A folder as `maithic intel scan` leaves it, from nodes and edges given by name.
graph_declaration <- function(name, internal = TRUE, kind = "theorem", line = 1, sorry = FALSE, documented = FALSE) {
  list(id = paste0("d:", name), kind = "declaration", name = name, internal = internal, declKind = kind, file = "Demo.lean", line = line,
       endLine = line, usesSorry = sorry, containsSorry = sorry, axioms = list(), documented = documented)
}

graph_use <- function(user, used, via = "value") list(source = paste0("d:", user), target = paste0("d:", used), kind = "uses", via = via)

demo_graph <- function() {
  list(schema = "maithic.intel.graph/1",
       nodes = list(graph_declaration("a", kind = "definition", documented = TRUE), graph_declaration("b", line = 5, documented = TRUE),
                    graph_declaration("c", line = 9, sorry = TRUE), graph_declaration("d", line = 12), graph_declaration("Nat", internal = FALSE, kind = "inductive"),
                    list(id = "m:Demo", kind = "module", name = "Demo", internal = TRUE, file = "Demo.lean")),
       edges = list(graph_use("a", "Nat", "type"), graph_use("b", "a", "both"), graph_use("c", "a"), graph_use("c", "a"), graph_use("c", "b"),
                    graph_use("d", "a", "type"), graph_use("d", "d"), graph_use("b", "Nat", "type"),
                    list(source = "m:Demo", target = "d:a", kind = "contains")))
}

demo_health <- function() {
  list(schema = "maithic.intel.health/1", files = 1, filesWithErrors = 0, declarations = 4, byKind = list(theorem = 3, definition = 1),
       documented = 2, restingOnSorry = 1,
       unfinished = list(list(name = "c", file = "Demo.lean", line = 9, column = 2, goals = list("x : Nat\n|- x = x"))),
       axioms = list(list(axiom = "propext", standard = TRUE, usedBy = list(list(name = "a", file = "Demo.lean", line = 1))),
                     list(axiom = "shortcut", standard = FALSE, usedBy = list(list(name = "b", file = "Demo.lean", line = 5), list(name = "c", file = "Demo.lean", line = 9)))))
}

write_snapshot <- function(graph = demo_graph(), health = demo_health(), metrics = NULL) {
  dir <- tempfile("snapshot")
  dir.create(dir)
  jsonlite::write_json(graph, file.path(dir, "graph.json"), auto_unbox = TRUE, null = "null")
  jsonlite::write_json(health, file.path(dir, "health.json"), auto_unbox = TRUE, null = "null")
  if (!is.null(metrics)) jsonlite::write_json(metrics, file.path(dir, "metrics.json"), auto_unbox = TRUE, null = "null")
  dir
}

# A dataset as `maithic corpus harvest` writes it, from records given as lists.
corpus_theorem <- function(name, steps, complete = TRUE, module = "M") {
  list(schema = "maithic.corpus.theorem/1", id = paste0("t-", name), name = name, module = module, file = "M.lean", line = 1,
       statement = "p", proof = if (steps == 0) "trivial" else "by simp", steps = steps, premises = 0, complete = complete)
}

corpus_pair <- function(theorem, tactic, before = 1, after = 0, module = "M") {
  list(schema = "maithic.corpus.pair/1", id = paste0("p-", theorem, "-", tactic), theorem = theorem, module = module, state = "|- p", tactic = tactic,
       goals_before = before, goals_after = after)
}

corpus_step <- function(theorem, index, tactic, inner = 0, depth = 0, module = "M") {
  list(schema = "maithic.corpus.step/1", id = paste0("s-", theorem, "-", index), theorem = theorem, module = module, index = index, tactic = tactic,
       goals_before = list("|- p"), goals_after = list(), depth = depth, inner = inner, line = 2, column = 2)
}

corpus_premise <- function(theorem, premise, module = "M") {
  list(schema = "maithic.corpus.premise/1", id = paste0("r-", theorem, "-", premise), theorem = theorem, module = module, premise = premise)
}

can_hash <- function() exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)

demo_corpus_records <- function() {
  list(
    theorem = list(corpus_theorem("t1", 3), corpus_theorem("t2", 1), corpus_theorem("t3", 0), corpus_theorem("t4", 2, complete = FALSE), corpus_theorem("t5", 1)),
    step = list(corpus_step("t1", 0, "intro h"), corpus_step("t1", 1, "simp [foo] at h"), corpus_step("t1", 2, "exact h")),
    premise = list(corpus_premise("t1", "foo"), corpus_premise("t3", "trivial")),
    pair = list(corpus_pair("t1", "intro h", 1, 1), corpus_pair("t1", "simp [foo] at h", 1, 1), corpus_pair("t1", "exact h", 1, 0), corpus_pair("t2", "simp", 1, 0),
                corpus_pair("t4", "constructor", 1, 2), corpus_pair("t4", "rw[add_comm]", 2, 2), corpus_pair("t5", "simp only [bar]", 1, 0))
  )
}

# Writes the records as one shard for each kind; `stated` overrides what the
# manifest says about the counts, and `change` edits the manifest before it is written.
write_corpus <- function(records = demo_corpus_records(), change = identity) {
  dir <- tempfile("corpus")
  dir.create(file.path(dir, "shards"), recursive = TRUE)
  shards <- lapply(names(records), function(kind) {
    file <- sprintf("shards/%s-%016d.jsonl", kind, 0)
    lines <- vapply(records[[kind]], function(record) as.character(jsonlite::toJSON(record, auto_unbox = TRUE)), character(1))
    con <- file(file.path(dir, file), open = "wb")
    writeLines(lines, con, sep = "\n", useBytes = TRUE)
    close(con)
    hash <- if (can_hash()) unname(get("sha256sum", envir = asNamespace("tools"))(file.path(dir, file))) else strrep("0", 64)
    list(kind = kind, file = file, records = length(lines), sha256 = hash)
  })
  manifest <- list(schema = "maithic.corpus.manifest/1", name = "tiny", version = "1",
                   sources = list(list(repository = "demo", revision = "abc123", toolchain = "4.36.0", files = 1)),
                   records = lapply(records, length), shards = shards)
  jsonlite::write_json(change(manifest), file.path(dir, "manifest.json"), auto_unbox = TRUE, null = "null")
  dir
}

# A review report as `maithic review --format json` writes it.
review_finding <- function(rule, severity, file, line = 1, declaration = "d", origin = "lint", edit = NULL) {
  out <- list(rule = rule, origin = origin, severity = severity, file = file, line = line, declaration = declaration, message = sprintf("about %s", rule))
  if (!is.null(edit)) out$edit <- edit
  out
}

review_file <- function(file, declarations, findings, suppressed = 0) list(file = file, declarations = declarations, suppressed = suppressed, findings = findings)

write_review_report <- function(files, profile = "default", change = identity) {
  severities <- unlist(lapply(files, function(file) vapply(file$findings, function(finding) finding$severity, character(1))))
  summary <- list(files = length(files), findings = length(severities), error = sum(severities == "error"), warning = sum(severities == "warning"),
                  info = sum(severities == "info"), suppressed = sum(vapply(files, function(file) file$suppressed, numeric(1))))
  write_document(change(list(schema = "maithic.review.report/1", profile = profile, summary = summary, files = files)))
}

demo_review_paths <- function() {
  alpha <- write_review_report(list(
    review_file("a.lean", 3, list(review_finding("sorry", "error", "a.lean", 4, "gap"), review_finding("docstring", "info", "a.lean", 1),
                                  review_finding("docstring", "info", "a.lean", 9)), suppressed = 1),
    review_file("b.lean", 2, list(review_finding("naming", "info", "b.lean", 2, "Bad_Name"),
                                  review_finding("automation", "info", "b.lean", 5, "easy", origin = "suggestion", edit = list(before = "by\n  simp\n  omega", after = "by omega"))))
  ))
  beta <- write_review_report(list(
    review_file("c.lean", 4, list(review_finding("docstring", "info", "c.lean", 3), review_finding("axiom", "warning", "c.lean", 8, "leans")))
  ), profile = "strict")
  c(alpha = alpha, beta = beta)
}

# The record of a training run of a tactic model. `losses` holds one pair for
# each epoch: the training loss and the validation loss.
guide_run <- function(name, seed, test = 20, top1 = 5, topk = 10, base1 = 2, basek = 8,
                      losses = list(c(2.0, 2.1), c(1.5, 1.8), c(1.2, 1.9)), best = 2, train = 60, validation = 20, k = 5,
                      dim = 8, pooling = "mean", mixer = "none", parameters = 1000) {
  epochs <- lapply(seq_along(losses), function(i) {
    list(epoch = i, train_loss = losses[[i]][1], validation_loss = if (validation == 0) NULL else losses[[i]][2],
         validation_top1 = if (validation == 0) NULL else 0.1 * i, validation_topk = if (validation == 0) NULL else 0.2 * i)
  })
  list(schema = "maithic.guide.run/1", name = name, seed = seed,
       config = list(dim = dim, max_length = 128, pooling = pooling, mixer = mixer, epochs = length(losses), batch = 16, rate = 0.01,
                     weight_decay = 1e-4, decoupled = TRUE, min_token_count = 2, min_template_count = 2, k = k),
       dataset = list(name = "toy", version = "1", pairs = train + validation + test, theorems = 30, train = train, validation = validation,
                      test = test),
       vocabulary = 50, classes = 9, parameters = parameters, epochs = epochs, best_epoch = best,
       test = list(examples = test, loss = 1.9, top1 = top1 / test, topk = topk / test, k = k),
       baseline = list(examples = test, loss = 2.2, top1 = base1 / test, topk = basek / test, k = k))
}

# Two models on two seeds each, and a third on one.
demo_guide_paths <- function() {
  c(write_document(guide_run("mean-8", 1, top1 = 5, topk = 10, base1 = 2, basek = 8)),
    write_document(guide_run("mean-8", 2, top1 = 9, topk = 16, base1 = 2, basek = 10)),
    write_document(guide_run("attention-mean-8", 1, top1 = 4, topk = 12, base1 = 2, basek = 8, mixer = "attention", parameters = 1200,
                             losses = list(c(2.2, 2.0), c(1.0, 2.4)), best = 1)),
    write_document(guide_run("attention-mean-8", 2, top1 = 6, topk = 12, base1 = 2, basek = 10, mixer = "attention", parameters = 1200,
                             losses = list(c(2.2, 2.0), c(1.0, 2.4)), best = 1)),
    write_document(guide_run("first-8", 2, top1 = 2, topk = 10, base1 = 2, basek = 10, pooling = "first")))
}

# One repair of a repair report.
mend_repair <- function(declaration, class, stage, tried = 1, changed = 2, severity = "error", line = 3) {
  out <- list(declaration = declaration, line = line, class = class, severity = severity, error = sprintf("about %s", declaration),
              outcome = if (stage == "none") "unrepaired" else "repaired", stage = stage, tried = tried, changed_lines = changed,
              before = "theorem t : True := by\n  broken")
  if (stage == "rule") out$rule <- if (class == "suggestion") "suggestion" else "rename"
  if (stage == "model") out$round <- 1
  if (stage == "prove") out$proposer <- "portfolio"
  if (stage == "none") out$reason <- "no repair was found" else {
    out$description <- "what was done"
    out$after <- "theorem t : True := by\n  trivial"
  }
  out
}

write_mend_report <- function(file, repairs, calls = NULL) {
  stage <- vapply(repairs, function(r) r$stage, character(1))
  report <- list(schema = "maithic.mend.report/1", file = file,
                 summary = list(breakages = length(repairs), repaired = sum(stage != "none"), unrepaired = sum(stage == "none"),
                                by_rule = sum(stage == "rule"), by_model = sum(stage == "model"), by_prover = sum(stage == "prove"),
                                remaining = sum(stage == "none"), clean = all(stage != "none"), checks = length(repairs) + 2),
                 repairs = repairs, remaining = list())
  if (!is.null(calls)) report$gateway <- list(mode = "replay", calls = calls, failed_calls = 0, records = list())
  write_document(report)
}

# Two repair runs: one with the rules and the prover, one with a model as well.
demo_mend_paths <- function() {
  c(rules = write_mend_report("A.lean", list(mend_repair("a", "unknown-identifier", "rule"), mend_repair("b", "deprecated", "rule", severity = "warning"),
                                             mend_repair("c", "unsolved-goals", "none", tried = 0, changed = 0),
                                             mend_repair("d", "type-mismatch", "prove"), mend_repair("e", "unknown-identifier", "none", tried = 1, changed = 0)),
                              calls = 0),
    model = write_mend_report("B.lean", list(mend_repair("f", "unsolved-goals", "model", tried = 3), mend_repair("g", "suggestion", "rule", severity = "info"),
                                             mend_repair("h", "unsolved-goals", "model", tried = 2, changed = 4), mend_repair("i", "unsolved-goals", "none", tried = 6, changed = 0)),
                              calls = 5))
}

# One candidate of a formalization run. `repairs` is how many repaired
# statements were compiled; `flagged` names the checks that flagged it.
formalize_candidate <- function(id, verdict, repairs = 0, flagged = character(), untested = FALSE, agreement = NULL, score = NULL, rank = NULL,
                                rejected_by = "vacuity", origin = "draft") {
  elaborated <- verdict != "failed"
  failures <- if (elaborated) repairs else repairs + 1
  history <- lapply(seq_len(failures), function(i) {
    list(round = i - 1, statement = "(n : N) : n = n", elaborated = FALSE, error_class = "unknown-constant", error = "Unknown identifier `N`")
  })
  if (elaborated) history <- c(history, list(list(round = repairs, statement = "(n : Nat) : n = n", elaborated = TRUE)))
  out <- list(id = id, name = sprintf("problem_%d", id), drafted = if (repairs > 0 || !elaborated) "(n : N) : n = n" else "(n : Nat) : n = n",
              statement = "(n : Nat) : n = n", reading = "A reading.", provenance = list(origin = origin, round = 0),
              elaboration = list(status = if (elaborated) "elaborated" else "failed", first_try = elaborated && repairs == 0, repairs = repairs, history = history))
  names <- c("vacuity", "counterexample", "triviality", "numbers", "variables", "traps", "back-translation", "round-trip")
  if (verdict %in% c("chosen", "kept")) {
    out$checks <- lapply(names, function(name) {
      outcome <- if (name %in% flagged) "flagged" else if (untested && name == "counterexample") "skipped" else "passed"
      if (outcome == "passed") list(name = name, outcome = outcome) else list(name = name, outcome = outcome, note = sprintf("about %s", name))
    })
    out$score <- score
    if (!is.null(agreement)) out$agreement <- agreement
  } else if (verdict == "rejected") {
    at <- match(rejected_by, names)
    out$checks <- lapply(seq_along(names), function(i) {
      if (i < at) list(name = names[i], outcome = "passed")
      else if (i == at) list(name = names[i], outcome = "failed", note = "what Lean found")
      else list(name = names[i], outcome = "skipped", note = "the statement was rejected")
    })
    out$score <- 0
  }
  out$verdict <- verdict
  if (verdict == "duplicate") out$duplicate_of <- list(id = 1, relation = "definitional")
  if (verdict %in% c("duplicate", "rejected", "failed")) out$reason <- sprintf("why candidate %d is out", id)
  if (!is.null(rank)) out$rank <- rank
  out
}

write_formalize_run <- function(candidates, status, calls = NULL, origin = "argument") {
  verdict <- vapply(candidates, function(c) c$verdict, character(1))
  first <- vapply(candidates, function(c) isTRUE(c$elaboration$first_try), logical(1))
  elaborated <- vapply(candidates, function(c) c$elaboration$status == "elaborated", logical(1))
  document <- list(schema = "maithic.formalize.run/1",
                   problem = list(origin = origin, source = "A problem.", text = "A problem.", task = "prove", elements = list()),
                   settings = list(candidates = length(candidates), rounds = 2, name = "problem", imports = list()), status = status)
  chosen <- which(verdict == "chosen")
  if (length(chosen) == 1) document$chosen <- candidates[[chosen]]$id
  document$candidates <- candidates
  document$summary <- list(drafted = length(candidates), elaborated_first_try = sum(first), repaired = sum(elaborated & !first), elaborated = sum(elaborated),
                           failed = sum(verdict == "failed"), duplicates = sum(verdict == "duplicate"), rejected = sum(verdict == "rejected"),
                           kept = sum(verdict %in% c("chosen", "kept")))
  if (!is.null(calls)) document$gateway <- list(mode = "replay", calls = calls, failed_calls = 0, records = list())
  write_document(document)
}

# Two formalization runs: one that ends with a statement, one whose statement is flagged.
demo_formalize_paths <- function() {
  c(squares = write_formalize_run(list(formalize_candidate(1, "chosen", agreement = 10, score = 1, rank = 1),
                                       formalize_candidate(2, "rejected"),
                                       formalize_candidate(3, "rejected", rejected_by = "counterexample"),
                                       formalize_candidate(4, "duplicate", repairs = 1)),
                                  "formalized", calls = 4, origin = "problems/squares.txt"),
    bound = write_formalize_run(list(formalize_candidate(1, "chosen", flagged = c("traps", "round-trip"), agreement = 7, score = 0.65, rank = 1),
                                     formalize_candidate(2, "kept", flagged = c("triviality", "round-trip"), agreement = 6, score = 0.3, rank = 3),
                                     formalize_candidate(3, "failed", repairs = 2),
                                     formalize_candidate(4, "kept", repairs = 2, untested = TRUE, agreement = 9, score = 0.9, rank = 2)),
                                "flagged", calls = 9))
}

# A council session with the turns and the board given, for the tests of the import.
write_council_session <- function(theorem, status, turns, items, messages, ending = "settled", rounds = 3, model = TRUE, spent = NULL) {
  if (is.null(spent)) {
    spent <- list(input_tokens = 0, output_tokens = 0, cost = 0, seconds = 1.5, lean_calls = sum(vapply(turns, function(t) t$lean_calls, numeric(1))),
                  model_calls = sum(vapply(turns, function(t) t$model_calls, numeric(1))))
  }
  write_document(list(
    schema = "maithic.council.session/1", problem = list(file = "T.lean", theorem = theorem, statement = sprintf("theorem %s : True", theorem), line = 1),
    settings = list(rounds = rounds, candidates = 3, policy = "default", model = model, roles = list(), schedule = "examine: verifier"),
    status = status, ending = list(reason = ending), answer = list(status = status, statement = "True", text = "What is established.", components = list(2), set_aside = c(note = 1)),
    turns = turns, board = items, messages = messages,
    budget = list(limits = list(tokens = 0, cost = 0, seconds = 0, lean_calls = 0, model_calls = 0), spent = spent),
    tools = list(lean_check = 0, lookup = 0, elaborate = 1, check_proof = 0, prove = 0, refute = 0)))
}

council_turn <- function(number, round, role, task, posted = integer(), model_calls = 0, lean_calls = 0, status = "done") {
  list(number = number, round = round, task = task, role = role, status = status, summary = sprintf("%s did %s", role, task), posted = as.list(posted),
       model_calls = model_calls, lean_calls = lean_calls, seconds = 0.25)
}

council_item <- function(id, kind, author, round = 0, checked = FALSE, status = NULL, examined = NULL) {
  out <- list(id = id, kind = kind, author = author, round = round, at = 0, title = sprintf("%s %d", kind, id), body = "text")
  if (checked) out$checked <- TRUE
  if (!is.null(status)) out$status <- status
  if (!is.null(examined)) out$examined <- examined
  out
}

council_message <- function(seq, sender, performative, round = 0, recipients = "coordinator", refers = integer()) {
  list(seq = seq, at = 0, round = round, sender = sender, recipients = as.list(recipients), performative = performative, subject = sprintf("message %d", seq),
       content = "said", refers = as.list(refers))
}

# Two sessions: one that ends with a proof in the second round, one that ends unproved when its rounds are over.
demo_council_paths <- function() {
  c(proved = write_council_session(
      "t", "verified",
      turns = list(council_turn(1, 0, "verifier", "examine", lean_calls = 20), council_turn(2, 1, "researcher", "research", posted = 3:4, model_calls = 1, lean_calls = 1),
                   council_turn(3, 1, "prover", "prove", posted = 5, model_calls = 1), council_turn(4, 1, "verifier", "verify", lean_calls = 1),
                   council_turn(5, 1, "critic", "critique", posted = 6, model_calls = 1), council_turn(6, 2, "prover", "prove", posted = 7, model_calls = 1),
                   council_turn(7, 2, "verifier", "verify", lean_calls = 1)),
      items = list(council_item(1, "statement", "coordinator"), council_item(2, "claim", "coordinator", status = "verified"),
                   council_item(3, "definition", "researcher", 1, checked = TRUE), council_item(4, "note", "researcher", 1),
                   council_item(5, "proof", "prover", 1, examined = "rejected"), council_item(6, "note", "critic", 1),
                   council_item(7, "proof", "prover", 2, examined = "accepted")),
      messages = list(council_message(1, "coordinator", "inform", recipients = c("researcher", "prover")), council_message(2, "verifier", "verdict", refers = 2),
                      council_message(3, "researcher", "inform", 1, recipients = c("prover", "coordinator"), refers = 3:4),
                      council_message(4, "prover", "propose", 1, recipients = "verifier", refers = 5), council_message(5, "verifier", "verdict", 1, refers = 5),
                      council_message(6, "critic", "critique", 1, recipients = "prover", refers = 5:6),
                      council_message(7, "prover", "propose", 2, recipients = "verifier", refers = 7), council_message(8, "verifier", "verdict", 2, refers = c(7, 2)))),
    open = write_council_session(
      "u", "formalized", ending = "rounds", rounds = 1,
      turns = list(council_turn(1, 0, "verifier", "examine", lean_calls = 19), council_turn(2, 1, "researcher", "research", model_calls = 1, status = "idle"),
                   council_turn(3, 1, "prover", "prove", posted = 3, model_calls = 1), council_turn(4, 1, "verifier", "verify", lean_calls = 1)),
      items = list(council_item(1, "statement", "coordinator"), council_item(2, "claim", "coordinator", status = "formalized"),
                   council_item(3, "proof", "prover", 1, examined = "rejected")),
      messages = list(council_message(1, "coordinator", "inform"), council_message(2, "prover", "propose", 1, recipients = "verifier", refers = 3),
                      council_message(3, "verifier", "verdict", 1, refers = 3))))
}

# ---- blueprint status snapshots ----

blueprint_node <- function(id, kind, status, ready = FALSE, unblocks = 0, blockers = character(), notes = character()) {
  list(id = id, kind = kind, title = sprintf("About %s", id), status = status, uses = list(), proof_uses = list(), blockers = as.list(blockers), ready = ready,
       unblocks = unblocks, notes = as.list(notes))
}

write_blueprint_status <- function(nodes, label = NULL, title = "A plan", by_status = NULL) {
  statuses <- vapply(nodes, function(node) node$status, character(1))
  if (is.null(by_status)) by_status <- lapply(stats::setNames(nm = c("planned", "stated", "proved", "done")), function(status) sum(statuses == status))
  document <- list(schema = "maithic.blueprint.status/1", title = title,
                   summary = list(nodes = length(nodes), by_status = by_status, by_kind = stats::setNames(list(), character()), complete = all(statuses == "done")),
                   nodes = nodes, frontier = list(), blocked = list())
  if (!is.null(label)) document$label <- label
  write_document(document)
}

# Three snapshots of one plan of four nodes: stated, half proved, and done but for one node that came later.
demo_blueprint_paths <- function() {
  c(write_blueprint_status(label = "start", list(
      blueprint_node("d", "definition", "done"), blueprint_node("a", "lemma", "stated", ready = TRUE, unblocks = 2),
      blueprint_node("b", "lemma", "stated", blockers = "a"), blueprint_node("t", "theorem", "planned", blockers = c("a", "b")))),
    write_blueprint_status(label = "middle", list(
      blueprint_node("d", "definition", "done"), blueprint_node("a", "lemma", "done"),
      blueprint_node("b", "lemma", "stated", ready = TRUE, unblocks = 1), blueprint_node("t", "theorem", "proved"))),
    write_blueprint_status(label = "end", list(
      blueprint_node("d", "definition", "done"), blueprint_node("a", "lemma", "done"), blueprint_node("b", "lemma", "done"),
      blueprint_node("t", "theorem", "done", notes = "a note"), blueprint_node("c", "corollary", "planned", ready = TRUE))))
}

# ---- verification jobs ----

write_nnv_job <- function(status, claim, accepted = NULL, model = "a network", property = "a property", axioms = c("propext", "Quot.sound"), verify_ms = 2000,
                          check_ms = 300, reason = NULL) {
  neuron <- function(weights) list(weights = as.list(weights), bias = "0")
  verdict <- list(schema = "maithic.nnv.verdict/1", verdict = claim,
                  network = list(list(activation = "relu", neurons = list(neuron(c("1", "0")), neuron(c("0", "1")))), list(activation = "identity", neurons = list(neuron(c("1", "1"))))),
                  input = list(list("0", "1"), list("0", "1")), output = list(list("0", "2")))
  document <- list(schema = "maithic.nnv.job/1", model = model, property = property, status = status, claim = claim, verify_ms = verify_ms, verdict = verdict)
  if (!is.null(accepted)) {
    document$check <- list(theorem = "Maithic.NNV.Job.property_holds", statement = "S", accepted = accepted, axioms = as.list(axioms), policy = list(status = status),
                           elapsed_ms = check_ms)
  }
  if (!is.null(reason)) document$reason <- reason
  write_document(document)
}

demo_nnv_paths <- function() {
  c(holds = write_nnv_job("verified", "verified", accepted = TRUE), fails = write_nnv_job("refuted", "violated", accepted = TRUE, verify_ms = 1000, check_ms = 200),
    open = write_nnv_job("unknown", "unknown", reason = "the bounds are too loose", verify_ms = 3000),
    wrong = write_nnv_job("failed", "verified", accepted = FALSE, axioms = character(), reason = "Lean did not accept it", check_ms = 100),
    again = write_nnv_job("verified", "verified", accepted = TRUE, verify_ms = 4000, check_ms = 500))
}
