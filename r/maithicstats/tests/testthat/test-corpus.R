# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P11-06-01 a dataset is loaded and checked against its manifest", {
  corpus <- read_corpus(write_corpus())
  expect_s3_class(corpus, "maithic_corpus")
  expect_equal(corpus$name, "tiny")
  expect_equal(corpus$version, "1")
  expect_equal(corpus$records, c(theorem = 5L, step = 3L, premise = 2L, pair = 7L))
  expect_equal(corpus$sources$revision, "abc123")
  expect_equal(corpus$shards$kind, c("theorem", "step", "premise", "pair"))
  expect_equal(corpus$theorems$name, c("t1", "t2", "t3", "t4", "t5"))
  expect_equal(corpus$theorems$steps, c(3, 1, 0, 2, 1))
  expect_equal(corpus$theorems$complete, c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(corpus$steps$tactic, c("intro h", "simp [foo] at h", "exact h"))
  expect_equal(corpus$steps$goals_before[[1]], "|- p")
  expect_equal(lengths(corpus$steps$goals_after), c(0, 0, 0))
  expect_equal(corpus$premises$premise, c("foo", "trivial"))
  expect_equal(corpus$pairs$goals_after, c(1, 1, 0, 0, 2, 2, 0))

  # What does not agree with the manifest is refused.
  expect_error(read_corpus(file.path(tempdir(), "no-such-dataset")), "folder not found")
  gone <- write_corpus()
  file.rename(file.path(gone, "shards", sprintf("pair-%016d.jsonl", 0)), file.path(gone, "elsewhere.jsonl"))
  expect_error(read_corpus(gone), "the shard shards/pair-0000000000000000.jsonl is missing")
  expect_error(read_corpus(write_corpus(change = function(m) { m$shards[[1]]$records <- 9; m })), "holds 5 record\\(s\\), and the manifest states 9")
  expect_error(read_corpus(write_corpus(change = function(m) { m$records$theorem <- 6; m })),
               "the manifest counts 6 theorem record\\(s\\), and its shards hold 5")
  expect_error(read_corpus(write_corpus(change = function(m) { m$schema <- "maithic.corpus.harvest/1"; m })), "is not the manifest of a dataset")
  expect_error(read_corpus(write_corpus(change = function(m) { m$shards[[1]]$kind <- "poem"; m })), "unknown kind 'poem'")
  orphan <- demo_corpus_records()
  orphan$pair <- c(orphan$pair, list(corpus_pair("ghost", "simp")))
  expect_error(read_corpus(write_corpus(orphan)), "belongs to ghost, which is not a theorem of the dataset")
  mixed <- demo_corpus_records()
  mixed$pair[[1]]$schema <- "maithic.corpus.step/1"
  expect_error(read_corpus(write_corpus(mixed)), "is not a pair record")
})

test_that("P11-06-01 a shard that was changed after the manifest was written is refused", {
  skip_if_not(can_hash(), "this R cannot compute SHA-256")
  changed <- write_corpus(change = function(m) { m$shards[[2]]$sha256 <- strrep("0", 64); m })
  expect_error(read_corpus(changed), "the shard shards/step-0000000000000000.jsonl does not have the hash the manifest states")
  expect_equal(read_corpus(changed, verify = FALSE)$records[["step"]], 3)
})

test_that("P11-06-01 the dataset of the repository's fixture is read whole", {
  corpus <- read_corpus(file.path(fixture_root(), "corpus", "demo"))
  expect_equal(corpus$name, "demo")
  expect_equal(unname(corpus$records), c(nrow(corpus$theorems), nrow(corpus$steps), nrow(corpus$premises), nrow(corpus$pairs)))
  expect_equal(nrow(corpus$theorems), 28)
  # The smallest steps of the step records are the steps the theorem records count,
  # and those of the complete proofs are the pairs: a proof that rests on sorry gives none.
  smallest <- corpus$steps[corpus$steps$inner == 0, ]
  expect_equal(nrow(smallest), sum(corpus$theorems$steps))
  expect_equal(nrow(corpus$pairs), sum(corpus$theorems$steps[corpus$theorems$complete]))
  complete <- paste(corpus$theorems$module, corpus$theorems$name)[corpus$theorems$complete]
  expect_true(all(paste(corpus$pairs$module, corpus$pairs$theorem) %in% complete))
  expect_false(any(tactic_head(corpus$pairs$tactic) %in% c("sorry", "admit")))
  expect_true(all(nzchar(corpus$pairs$state)))
  expect_true(all(lengths(corpus$steps$goals_before) >= 1))
})

test_that("P11-06-02 the lengths of proofs are counted in their smallest steps", {
  corpus <- read_corpus(write_corpus())
  distribution <- proof_length_distribution(corpus)
  expect_equal(distribution$steps, c(1, 2, 3))
  expect_equal(distribution$proofs, c(2, 1, 1))
  expect_equal(distribution$share, c(0.5, 0.25, 0.25))
  expect_equal(distribution$cumulative, c(0.5, 0.75, 1))
  # Without the proof that rests on sorry.
  finished <- proof_length_distribution(corpus, complete_only = TRUE)
  expect_equal(finished$steps, c(1, 3))
  expect_equal(finished$share, c(2 / 3, 1 / 3))

  summary <- proof_length_summary(corpus)
  expect_equal(summary$theorems, 5)
  expect_equal(summary$term_proofs, 1)
  expect_equal(summary$tactic_proofs, 4)
  expect_equal(summary$median, 1.5)
  expect_equal(summary$p90, 2.7)
  expect_equal(summary$mean, 1.75)
  expect_equal(summary$max, 3)
  expect_equal(summary$one_step, 0.5)
  expect_equal(proof_length_summary(corpus, complete_only = TRUE)$theorems, 4)

  # A dataset of term proofs only has no distribution, and says so in its summary.
  terms <- demo_corpus_records()
  terms$theorem <- list(corpus_theorem("t3", 0))
  terms$step <- list()
  terms$pair <- list()
  terms$premise <- list()
  bare <- read_corpus(write_corpus(terms))
  expect_equal(nrow(proof_length_distribution(bare)), 0)
  expect_equal(proof_length_summary(bare)$tactic_proofs, 0)
  expect_true(is.na(proof_length_summary(bare)$median))
  expect_error(proof_length_distribution(list()), "read_corpus")

  fixture <- read_corpus(file.path(fixture_root(), "corpus", "demo"))
  whole <- proof_length_summary(fixture)
  expect_equal(c(whole$theorems, whole$term_proofs, whole$tactic_proofs), c(28, 15, 13))
  expect_equal(sum(proof_length_distribution(fixture)$proofs), 13)
  expect_equal(max(proof_length_distribution(fixture)$steps), whole$max)
})

test_that("P11-06-03 tactics are counted by the tactic each step applies", {
  expect_equal(tactic_head(c("simp [double]", "exact h.trans (le_of_lt hk)", "rw[add_comm]", "  omega", "", NA, "exact?", "simp_all",
                             "refine ⟨_, _⟩", "cases h with\n| inl a => skip", "apply(foo)", "ext{x}")),
               c("simp", "exact", "rw", "omega", "", "", "exact?", "simp_all", "refine", "cases", "apply", "ext"))

  corpus <- read_corpus(write_corpus())
  table <- tactic_frequency(corpus)
  expect_equal(table$tactic, c("simp", "constructor", "exact", "intro", "rw"))
  expect_equal(table$uses, c(3, 1, 1, 1, 1))
  expect_equal(table$share, c(3, 1, 1, 1, 1) / 7)
  expect_equal(table$proofs, c(3, 1, 1, 1, 1))
  # simp closed its goal twice in three uses; constructor opened a second goal; exact closed its goal.
  expect_equal(table$closes, c(2 / 3, 0, 1, 0, 0))
  expect_equal(tactic_frequency(corpus, 2)$tactic, c("simp", "constructor"))
  expect_error(tactic_frequency(corpus, NA), "must be a count")

  none <- demo_corpus_records()
  none$pair <- list()
  expect_equal(nrow(tactic_frequency(read_corpus(write_corpus(none)))), 0)

  fixture <- read_corpus(file.path(fixture_root(), "corpus", "demo"))
  used <- tactic_frequency(fixture)
  expect_equal(used$tactic[1:2], c("simp", "exact"))
  expect_equal(sum(used$uses), nrow(fixture$pairs))
  expect_equal(sum(used$share), 1)
  expect_true(all(used$closes >= 0 & used$closes <= 1))

  report <- corpus_report_table(corpus, n = 1)
  expect_equal(report[1], "Dataset tiny, version 1: 5 theorem record(s), 3 step record(s), 2 premise record(s), 7 pair record(s).")
  expect_true("| demo | abc123 | 4.36.0 | 1 |" %in% report)
  expect_true("| 5 | 1 | 4 | 1.5 | 2.7 | 1.8 | 3 | 50.0% |" %in% report)
  expect_true("| simp | 3 | 42.9% | 3 | 66.7% |" %in% report)
})
