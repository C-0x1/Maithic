# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

check_theorem <- function(name, status = "verified", axioms = character(), reasons = character()) {
  list(name = name, statement = "True", status = status, axioms = as.list(axioms), reasons = as.list(reasons))
}

write_check_report <- function(theorems, file = "Demo.lean") {
  write_document(list(schema = "maithic.check.report/1", file = file, ok = TRUE, errors = 0, warnings = 0,
                      declarations = length(theorems), theorems = theorems, diagnostics = list()))
}

demo_records_table <- function() {
  checks <- read_check_reports(write_check_report(list(
    check_theorem("a"), check_theorem("b", axioms = c("propext", "Quot.sound")),
    check_theorem("c", status = "failed", axioms = "sorryAx", reasons = "the proof is unfinished: it depends on sorry"),
    check_theorem("d", status = "checked", axioms = c("d._native.native_decide.ax_1", "propext"))
  )))
  ledger_records(checks, read_prove_runs(demo_run_paths()))
}

test_that("P18-04-01 check reports and prove runs are imported as ledger records", {
  records <- demo_records_table()
  expect_equal(records$source, rep(c("check", "prove"), each = 4))
  expect_equal(records$name, c("a", "b", "c", "d", "t1", "t2", "t3", "u1"))
  expect_equal(records$status, c("verified", "verified", "failed", "checked", "verified", "verified", "failed", "verified"))
  expect_equal(records$recheck_passed, c(NA, NA, NA, NA, TRUE, TRUE, NA, TRUE))
  expect_equal(records$axioms[[2]], c("propext", "Quot.sound"))
  expect_equal(records$problems[[3]], "the proof is unfinished: it depends on sorry")
  expect_equal(nrow(ledger_records(runs = read_prove_runs(demo_run_paths()))), 4)
  checks <- read_check_reports(write_check_report(list(check_theorem("a"))))
  expect_equal(checks$reports[c("file", "ok", "theorems")], data.frame(file = "Demo.lean", ok = TRUE, theorems = 1L, stringsAsFactors = FALSE))
  expect_equal(nrow(read_check_reports(write_check_report(list()))$theorems), 0)
  expect_error(read_check_reports(character()), "no report files")
  expect_error(read_check_reports(write_document(list(schema = "maithic.prove.run/1"))), "not a check report")
  expect_error(read_check_reports(write_check_report(list(check_theorem("a", status = "fine")))), "unknown status 'fine'")
  expect_error(read_check_reports(write_check_report(list(check_theorem("")))), "has no name")
  expect_error(ledger_records(), "give check reports, prove runs or both")
  expect_error(ledger_records(checks = list()), "read_check_reports")
})

test_that("P18-04-01 the reports recorded in the repository are read as they are", {
  skip_if(is.null(fixture_root()), "the repository fixtures are not available")
  checks <- read_check_reports(run_fixture(c("check-insertion.json", "check-logic.json", "check-oracle.json",
                                            "check-replay.json", "check-triangle.json")))
  expect_equal(checks$reports$theorems, c(9, 20, 19, 6, 1))
  expect_equal(checks$reports$ok, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  records <- ledger_records(checks, read_prove_runs(run_fixture(c("prove-even.json", "prove-triangle-anthropic.json"))))
  expect_equal(nrow(records), 57)
  distribution <- trust_distribution(records)
  expect_equal(distribution$status, c("verified", "failed"))
  expect_equal(distribution$count, c(56, 1))
  census <- axiom_census(records)
  expect_equal(census$class[census$axiom == "sorryAx"], "incomplete")
  expect_true(all(census$class %in% c("standard", "incomplete", "none")))
  expect_equal(recheck_agreement(records)[c("rechecked", "agreed")], data.frame(rechecked = 2L, agreed = 2L))
})

test_that("P18-04-02 trust levels are reported in their fixed order with intervals", {
  distribution <- trust_distribution(demo_records_table())
  expect_equal(distribution$status, c("verified", "checked", "failed"))
  expect_equal(distribution$count, c(5, 1, 2))
  expect_equal(distribution$share, c(5, 1, 2) / 8)
  expect_equal(distribution$lower, wilson_interval(c(5, 1, 2), 8)$lower)
  expect_error(trust_distribution(data.frame(status = "verified")), "ledger_records")
  expect_error(trust_distribution(demo_records_table()[0, ]), "no results")
})

test_that("P18-04-03 the axiom census counts results per axiom and names the class of each", {
  census <- axiom_census(demo_records_table())
  expect_equal(census$axiom, c("propext", "(none)", "Classical.choice", "Quot.sound", "d._native.native_decide.ax_1", "sorryAx"))
  expect_equal(census$results, c(4, 3, 1, 1, 1, 1))
  expect_equal(census$class, c("standard", "none", "standard", "standard", "native", "incomplete"))
  expect_equal(census$share[1:2], c(0.5, 0.375))
  expect_equal(axiom_class(c("propext", "Quot.sound", "Classical.choice", "sorryAx", "Lean.ofReduceBool", "Lean.trustCompiler",
                             "fast._native.native_decide.ax_1_2", "My.axiom", "native_helper")),
               c("standard", "standard", "standard", "incomplete", "native", "native", "native", "other", "other"))
})

test_that("P18-04-04 the re-check agrees only when it passes and sees the same axioms", {
  agreement <- recheck_agreement(demo_records_table())
  expect_equal(agreement[c("rechecked", "passed", "same_axioms", "agreed")], data.frame(rechecked = 3L, passed = 3L, same_axioms = 2L, agreed = 2L))
  expect_equal(agreement$rate, 2 / 3)
  expect_equal(c(agreement$lower, agreement$upper), unlist(wilson_interval(2, 3)[c("lower", "upper")], use.names = FALSE))
  checks_only <- ledger_records(read_check_reports(write_check_report(list(check_theorem("a")))))
  none <- recheck_agreement(checks_only)
  expect_equal(none$rechecked, 0)
  expect_true(is.na(none$rate))
  failed <- demo_records_table()
  failed$recheck_passed[5] <- FALSE
  expect_equal(recheck_agreement(failed)$agreed, 1)
})

test_that("P18-04-05 the time of the re-check is summarised", {
  time <- recheck_time(demo_records_table())
  expect_equal(time[c("n", "median", "max")], data.frame(n = 3L, median = 40, max = 60))
  expect_equal(recheck_time(demo_records_table(), probs = 0.5)$p50, 40)
})

test_that("P18-04-06 policy decisions are counted as passed, failed, waived or not decided", {
  decisions <- policy_decisions(demo_records_table())
  expect_equal(decisions$decision, c("passed", "failed", "waived", "not decided"))
  expect_equal(decisions$count, c(5, 2, 0, 1))
  expect_equal(sum(decisions$share), 1)
  waived <- demo_records_table()
  waived$waived <- waived$name == "d"
  expect_equal(policy_decisions(waived)$count, c(5, 2, 1, 0))
  violated <- demo_records_table()
  violated$problems[[4]] <- "axiom outside the policy"
  expect_equal(policy_decisions(violated)$count, c(5, 3, 0, 0))
})

test_that("P18-04-08 the report table covers trust levels, axioms and the re-check", {
  lines <- ledger_report_table(demo_records_table())
  expect_equal(lines[1], "8 result(s): 4 from check, 4 from prove.")
  expect_true("| verified | 5 | 62.5% | 30.6% | 86.3% |" %in% lines)
  expect_true("| propext | standard | 4 | 50.0% |" %in% lines)
  expect_true("| 3 | 3 | 2 | 2 | 66.7% | 20.8% | 93.9% |" %in% lines)
  checks_only <- ledger_records(read_check_reports(write_check_report(list(check_theorem("a")))))
  expect_false(any(grepl("Re-check", ledger_report_table(checks_only))))
})
