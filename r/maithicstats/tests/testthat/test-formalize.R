# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P02-06-01 formalization runs are loaded with their candidates, checks and outcomes", {
  runs <- read_formalize_runs(demo_formalize_paths())
  expect_s3_class(runs, "maithic_formalize_runs")
  expect_equal(runs$runs$label, c("squares", "bound"))
  expect_equal(runs$runs$origin, c("problems/squares.txt", "argument"))
  expect_equal(runs$runs$task, c("prove", "prove"))
  expect_equal(runs$runs$status, c("formalized", "flagged"))
  expect_equal(runs$runs$chosen, c(1, 1))
  expect_equal(runs$runs$drafted, c(4, 4))
  expect_equal(runs$runs$elaborated_first_try, c(3, 2))
  expect_equal(runs$runs$repaired, c(1, 1))
  expect_equal(runs$runs$elaborated, c(4, 3))
  expect_equal(runs$runs$failed, c(0, 1))
  expect_equal(runs$runs$duplicates, c(1, 0))
  expect_equal(runs$runs$rejected, c(2, 0))
  expect_equal(runs$runs$kept, c(1, 3))
  expect_equal(runs$runs$model_calls, c(4, 9))

  expect_equal(nrow(runs$candidates), 8)
  expect_equal(runs$candidates$run, rep(1:2, each = 4))
  expect_equal(runs$candidates$id, rep(1:4, 2))
  expect_equal(runs$candidates$verdict, c("chosen", "rejected", "rejected", "duplicate", "chosen", "kept", "failed", "kept"))
  expect_equal(runs$candidates$elaborated, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(runs$candidates$first_try, c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(runs$candidates$repairs, c(0, 0, 0, 1, 0, 0, 2, 2))
  expect_equal(runs$candidates$rank, c(1, NA, NA, NA, 1, 3, NA, 2))
  expect_equal(runs$candidates$score, c(1, 0, 0, NA, 0.65, 0.3, NA, 0.9))
  expect_equal(runs$candidates$agreement, c(10, NA, NA, NA, 7, 6, NA, 9))
  expect_equal(runs$candidates$origin, rep("draft", 8))
  expect_equal(runs$candidates$reason[c(2, 4, 7)], c("why candidate 2 is out", "why candidate 4 is out", "why candidate 3 is out"))
  expect_true(is.na(runs$candidates$reason[1]))

  # Every check that was made, with what it said: eight for each statement that was checked.
  expect_equal(nrow(runs$checks), 6 * 8)
  expect_equal(runs$checks$check[1:8], c("vacuity", "counterexample", "triviality", "numbers", "variables", "traps", "back-translation", "round-trip"))
  expect_equal(runs$checks$outcome[runs$checks$run == 1 & runs$checks$id == 2], c("failed", rep("skipped", 7)))
  expect_equal(runs$checks$outcome[runs$checks$run == 1 & runs$checks$id == 3], c("passed", "failed", rep("skipped", 6)))
  expect_equal(runs$checks$check[runs$checks$outcome == "flagged"], c("traps", "round-trip", "triviality", "round-trip"))
  expect_equal(runs$checks$note[runs$checks$outcome == "flagged"][1], "about traps")
  expect_equal(sum(runs$checks$run == 1 & runs$checks$id == 4), 0)

  # Every statement that was compiled: the drafts, and each repair.
  expect_equal(nrow(runs$attempts), 4 + 1 + 4 + 2 + 2)
  third <- runs$attempts[runs$attempts$run == 2 & runs$attempts$id == 3, ]
  expect_equal(third$round, 0:2)
  expect_equal(third$elaborated, c(FALSE, FALSE, FALSE))
  expect_equal(third$error_class, rep("unknown-constant", 3))
  fourth <- runs$attempts[runs$attempts$run == 2 & runs$attempts$id == 4, ]
  expect_equal(fourth$elaborated, c(FALSE, FALSE, TRUE))
  expect_true(is.na(fourth$error_class[3]))

  # Runs are labelled by the names of the paths, by the file names, or outright.
  paths <- unname(demo_formalize_paths())
  expect_equal(read_formalize_runs(paths)$runs$label, sub("\\.json$", "", basename(paths)))
  expect_equal(read_formalize_runs(paths, label = c("first", "second"))$candidates$label[c(1, 8)], c("first", "second"))
  expect_error(read_formalize_runs(paths, label = "one"), "one label for each run")
  expect_error(read_formalize_runs(character()), "no formalization runs given")
  expect_error(read_formalize_runs(tempfile(fileext = ".json")), "file not found")
  expect_error(read_formalize_runs(write_document(list(schema = "maithic.mend.report/1"))), "is not a formalization run")

  # A run that contradicts itself is refused.
  broken <- function(change) {
    document <- jsonlite::fromJSON(demo_formalize_paths()[["squares"]], simplifyVector = FALSE)
    write_document(change(document))
  }
  expect_error(read_formalize_runs(broken(function(d) { d$summary$rejected <- 3; d })), "the summary states 3 for 'rejected', and the run holds 2")
  expect_error(read_formalize_runs(broken(function(d) { d$candidates[[2]]$verdict <- "lost"; d })), "a candidate with the unknown verdict 'lost'")
  expect_error(read_formalize_runs(broken(function(d) { d$candidates[[1]]$checks[[1]]$name <- "luck"; d })), "a check with the unknown name 'luck'")
  expect_error(read_formalize_runs(broken(function(d) { d$candidates[[1]]$checks[[1]]$outcome <- "fine"; d })), "a check with the unknown outcome 'fine'")
  expect_error(read_formalize_runs(broken(function(d) { d$candidates[[4]]$elaboration$status <- "failed"; d })), "a candidate that failed is said to elaborate")
  expect_error(read_formalize_runs(broken(function(d) { d$status <- "none"; d })), "the status is 'none', and 1 candidate\\(s\\) are chosen")
  expect_error(read_formalize_runs(broken(function(d) { d$status <- "done"; d })), "a run with the unknown status 'done'")
  expect_error(compile_rate(list()), "must come from read_formalize_runs")
})

test_that("P02-06-01 the runs the repository keeps are read whole", {
  skip_if(is.null(fixture_root()), "the repository fixtures are not available")
  folder <- file.path(fixture_root(), "formalize")
  runs <- read_formalize_runs(c(`negative square` = file.path(folder, "negative-square.json"), subtraction = file.path(folder, "subtraction.json"),
                                `square bound` = file.path(folder, "square-bound.json"), `Lean alone` = file.path(folder, "subtraction-no-ai.json")))
  expect_equal(runs$runs$status, c("formalized", "flagged", "formalized", "flagged"))
  expect_equal(runs$runs$drafted, c(4, 3, 4, 2))
  expect_equal(runs$runs$elaborated_first_try, c(3, 3, 2, 2))
  expect_equal(runs$runs$repaired, c(1, 0, 1, 0))
  expect_equal(runs$runs$failed, c(0, 0, 1, 0))
  expect_equal(runs$runs$duplicates, c(1, 0, 1, 0))
  expect_equal(runs$runs$rejected, c(2, 1, 0, 1))
  expect_equal(runs$runs$kept, c(1, 2, 2, 1))
  expect_equal(runs$runs$chosen, c(1, 2, 1, 2))
  expect_equal(runs$runs$model_calls, c(4, 5, 9, NA))
  expect_equal(runs$runs$origin[1], "fixtures/formalize/problems/negative-square.txt")
  expect_equal(runs$candidates$verdict[runs$candidates$run == 1], c("chosen", "rejected", "rejected", "duplicate"))
  expect_equal(unique(runs$candidates$origin[runs$candidates$run == 4]), "given")
  expect_equal(runs$candidates$agreement[runs$candidates$run == 2], c(NA, 7, 6))
  # The statement that needed two repairs, and the one that was given up after two.
  bound <- runs$attempts[runs$attempts$run == 3, ]
  expect_equal(bound$error_class[bound$id == 2], c("syntax", "unknown-constant", NA))
  expect_equal(bound$elaborated[bound$id == 4], c(FALSE, FALSE, FALSE))
})

test_that("P02-06-02 the share of candidates that elaborate is given before and after repair, with intervals", {
  runs <- read_formalize_runs(demo_formalize_paths())
  all <- compile_rate(runs)
  expect_equal(nrow(all), 1)
  expect_equal(all$candidates, 8)
  expect_equal(all$first_try, 5)
  expect_equal(all$after_repair, 7)
  expect_equal(all$repaired, 2)
  expect_equal(all$rate_before, 5 / 8)
  expect_equal(all$rate_after, 7 / 8)
  before <- wilson_interval(5, 8)
  after <- wilson_interval(7, 8)
  expect_equal(c(all$lower_before, all$upper_before), c(before$lower, before$upper))
  expect_equal(c(all$lower_after, all$upper_after), c(after$lower, after$upper))
  expect_true(all$lower_before < all$rate_before && all$rate_before < all$upper_before)
  # A wider interval for more confidence.
  wide <- compile_rate(runs, conf = 0.99)
  expect_true(wide$lower_after < all$lower_after && wide$upper_after > all$upper_after)

  by_run <- compile_rate(runs, by = "label")
  expect_equal(by_run$label, c("bound", "squares"))
  expect_equal(by_run$candidates, c(4, 4))
  expect_equal(by_run$first_try, c(2, 3))
  expect_equal(by_run$after_repair, c(3, 4))
  expect_equal(by_run$rate_after, c(0.75, 1))
  expect_equal(compile_rate(runs, by = "origin")$origin, "draft")
  expect_error(compile_rate(runs, by = "verdict"), "`by` must name some of: label, run, origin")

  # What the checks found, in the order they are made.
  checks <- check_summary(runs)
  expect_equal(checks$check, c("vacuity", "counterexample", "triviality", "numbers", "variables", "traps", "back-translation", "round-trip"))
  expect_equal(checks$made, rep(6, 8))
  expect_equal(checks$passed, c(5, 3, 3, 4, 4, 3, 4, 2))
  expect_equal(checks$failed, c(1, 1, 0, 0, 0, 0, 0, 0))
  expect_equal(checks$flagged, c(0, 0, 1, 0, 0, 1, 0, 2))
  expect_equal(checks$skipped, c(0, 2, 2, 2, 2, 2, 2, 2))
  expect_equal(checks$passed + checks$failed + checks$flagged + checks$skipped, checks$made)

  table <- formalize_report_table(runs)
  expect_equal(table[1], "2 formalization run(s): 8 candidate statement(s), 1 run(s) ended with a statement that no check flagged.")
  expect_true(any(grepl("^\\| squares +\\| +4 \\| +3 \\| +1 \\| +0 \\| +1 \\| +2 \\| +1 \\| a statement +\\|$", table)))
  expect_true(any(grepl("^\\| bound +\\| +4 \\| +2 \\| +1 \\| +1 \\| +0 \\| +0 \\| +3 \\| a statement, flagged \\|$", table)))
  expect_true(any(grepl("^\\| as drafted +\\| +5 \\| +8 \\| +62.5% \\|", table)))
  expect_true(any(grepl("^\\| after repair +\\| +7 \\| +8 \\| +87.5% \\|", table)))
  expect_true(any(grepl("^\\| round-trip +\\| +6 \\| +2 \\| +0 \\| +2 \\| +2 \\|$", table)))

  # Runs without candidates, as when a model returned nothing, are counted and not divided by.
  empty <- read_formalize_runs(write_formalize_run(list(), "none"))
  expect_equal(nrow(compile_rate(empty)), 0)
  expect_equal(names(compile_rate(empty, by = "label")), c("label", "candidates", "first_try", "after_repair", "repaired", "rate_before", "lower_before",
                                                           "upper_before", "rate_after", "lower_after", "upper_after"))
  expect_equal(nrow(check_summary(empty)), 0)
  expect_equal(formalize_report_table(empty)[1], "1 formalization run(s): 0 candidate statement(s), 0 run(s) ended with a statement that no check flagged.")
})

test_that("P02-06-02 on the runs the repository keeps, repair raises the share that elaborates", {
  skip_if(is.null(fixture_root()), "the repository fixtures are not available")
  folder <- file.path(fixture_root(), "formalize")
  runs <- read_formalize_runs(file.path(folder, c("negative-square.json", "subtraction.json", "square-bound.json")))
  rate <- compile_rate(runs)
  expect_equal(rate$candidates, 11)
  expect_equal(rate$first_try, 8)
  expect_equal(rate$after_repair, 10)
  expect_equal(round(100 * c(rate$rate_before, rate$rate_after), 1), c(72.7, 90.9))
  checks <- check_summary(runs)
  # Eight statements were checked: of the eleven, two are duplicates and one never elaborated.
  expect_equal(checks$made, rep(8, 8))
  expect_equal(checks$failed[1:2], c(1, 2))
})
