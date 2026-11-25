# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P03-06-01 review reports are loaded with their findings, comments and outcomes", {
  runs <- read_review_runs(demo_review_paths())
  expect_s3_class(runs, "maithic_review_runs")
  expect_equal(runs$runs$repository, c("alpha", "beta"))
  expect_equal(runs$runs$profile, c("default", "strict"))
  expect_equal(runs$runs$files, c(2, 1))
  expect_equal(runs$runs$declarations, c(5, 4))
  expect_equal(runs$runs$findings, c(5, 2))
  expect_equal(runs$runs$errors, c(1, 0))
  expect_equal(runs$runs$warnings, c(0, 1))
  expect_equal(runs$runs$notes, c(4, 1))
  expect_equal(runs$runs$suppressed, c(1, 0))
  expect_equal(runs$runs$suggestions, c(1, 0))
  # A review fails when one of its findings is an error.
  expect_equal(runs$runs$outcome, c("failed", "passed"))

  expect_equal(runs$files$file, c("a.lean", "b.lean", "c.lean"))
  expect_equal(runs$files$findings, c(3, 2, 2))
  expect_equal(runs$files$run, c(1, 1, 2))

  expect_equal(nrow(runs$findings), 7)
  expect_equal(runs$findings$rule, c("sorry", "docstring", "docstring", "naming", "automation", "docstring", "axiom"))
  expect_equal(runs$findings$repository, c(rep("alpha", 5), rep("beta", 2)))
  expect_equal(runs$findings$declaration[c(1, 4)], c("gap", "Bad_Name"))
  expect_equal(runs$findings$line[1:2], c(4, 1))
  expect_equal(runs$findings$message[1], "about sorry")
  expect_equal(runs$findings$origin[5], "suggestion")
  expect_equal(runs$findings$suggestion, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(runs$findings$after[5], "by omega")
  expect_equal(runs$findings$before[5], "by\n  simp\n  omega")
  expect_true(is.na(runs$findings$after[1]))

  # Repositories are named by the names of the paths, by the file names, or outright.
  paths <- unname(demo_review_paths())
  expect_equal(read_review_runs(paths)$runs$repository, sub("\\.json$", "", basename(paths)))
  expect_equal(read_review_runs(paths, repository = c("one", "two"))$findings$repository[c(1, 7)], c("one", "two"))
  expect_error(read_review_runs(paths, repository = "one"), "one repository for each report")

  # A review without findings is a review that passed.
  clean <- read_review_runs(c(clean = write_review_report(list(review_file("ok.lean", 6, list())))))
  expect_equal(clean$runs$outcome, "passed")
  expect_equal(nrow(clean$findings), 0)
  expect_equal(names(clean$findings), names(runs$findings))

  expect_error(read_review_runs(character()), "no review reports")
  expect_error(read_review_runs(file.path(tempdir(), "absent.json")), "file not found")
  expect_error(read_review_runs(write_document(list(schema = "maithic.prove.run/1"))), "is not a review report")
  one <- list(review_file("a.lean", 1, list(review_finding("sorry", "error", "a.lean"))))
  expect_error(read_review_runs(write_review_report(one, change = function(r) { r$summary$error <- 0; r$summary$info <- 1; r })),
               "the summary states 1 finding\\(s\\) \\(0 error, 0 warning, 1 info\\), and the report holds 1 \\(1 error, 0 warning, 0 info\\)")
  expect_error(read_review_runs(write_review_report(one, change = function(r) { r$summary$files <- 3; r })), "states 3 file\\(s\\), and the report holds 1")
  expect_error(read_review_runs(write_review_report(list(review_file("a.lean", 1, list(review_finding("sorry", "fatal", "a.lean")))))), "unknown severity 'fatal'")
})

test_that("P03-06-01 the review of the repository's fixture is read whole", {
  runs <- read_review_runs(c(demo = file.path(fixture_root(), "review", "report.json")))
  expect_equal(runs$runs$files, 2)
  expect_equal(runs$runs$findings, nrow(runs$findings))
  expect_equal(runs$runs$outcome, "failed")
  expect_equal(runs$runs$errors, sum(runs$findings$severity == "error"))
  expect_equal(runs$runs$suggestions, 2)
  expect_true(all(runs$findings$origin %in% c("lint", "semantic", "suggestion", "model")))
  expect_equal(sort(runs$findings$rule[runs$findings$suggestion]), c("automation", "unneeded-step"))
  expect_equal(runs$findings$after[runs$findings$rule == "automation"], "by omega")
})

test_that("P03-06-02 findings are counted by rule, severity and repository", {
  runs <- read_review_runs(demo_review_paths())
  by_rule <- finding_counts(runs)
  expect_equal(names(by_rule), c("rule", "severity", "findings", "share"))
  expect_equal(by_rule$rule, c("docstring", "automation", "axiom", "naming", "sorry"))
  expect_equal(by_rule$severity, c("info", "info", "warning", "info", "error"))
  expect_equal(by_rule$findings, c(3, 1, 1, 1, 1))
  expect_equal(by_rule$share, c(3, 1, 1, 1, 1) / 7)

  # Severities are ordered from the one that fails a review down, not by their names.
  by_severity <- finding_counts(runs, "severity")
  expect_equal(by_severity$severity, c("info", "error", "warning"))
  expect_equal(by_severity$findings, c(5, 1, 1))

  by_repository <- finding_counts(runs, c("repository", "severity"))
  expect_equal(by_repository$repository, c("alpha", "alpha", "beta", "beta"))
  expect_equal(by_repository$severity, c("info", "error", "warning", "info"))
  expect_equal(by_repository$findings, c(4, 1, 1, 1))
  expect_equal(finding_counts(runs, "repository")$findings, c(5, 2))
  expect_equal(finding_counts(runs, "origin")$origin, c("lint", "suggestion"))
  expect_equal(finding_counts(runs, "file")$file, c("a.lean", "b.lean", "c.lean"))
  expect_equal(finding_counts(runs, c("rule", "severity", "repository"))$findings, c(2, 1, 1, 1, 1, 1))
  expect_equal(sum(finding_counts(runs, c("rule", "severity", "repository"))$share), 1)

  expect_error(finding_counts(runs, "colour"), "`by` must name one or more of: rule, severity, repository, origin, file, run")
  expect_error(finding_counts(runs, c("rule", "rule")), "`by` must name")
  expect_error(finding_counts(list()), "read_review_runs")
  clean <- read_review_runs(c(clean = write_review_report(list(review_file("ok.lean", 6, list())))))
  expect_equal(nrow(finding_counts(clean)), 0)
  expect_equal(names(finding_counts(clean)), c("rule", "severity", "findings", "share"))

  fixture <- read_review_runs(c(demo = file.path(fixture_root(), "review", "report.json")))
  counted <- finding_counts(fixture)
  expect_equal(c(counted$rule[1], counted$severity[1]), c("docstring", "info"))
  expect_equal(sum(counted$findings), fixture$runs$findings)
  expect_equal(finding_counts(fixture, "severity")$findings[finding_counts(fixture, "severity")$severity == "error"], fixture$runs$errors)

  report <- review_report_table(runs)
  expect_equal(report[1], "2 review(s): 1 passed, 1 failed.")
  expect_true("| alpha | default | 2 | 5 | 1 | 0 | 4 | 1 | 1 | failed |" %in% report)
  expect_true("| docstring | lint | 0 | 0 | 3 | 3 |" %in% report)
  expect_true("| automation | suggestion | 0 | 0 | 1 | 1 |" %in% report)
})
