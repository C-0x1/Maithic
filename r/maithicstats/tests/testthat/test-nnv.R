# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P08-05-01 verification jobs are loaded, each checked against itself", {
  loaded <- read_nnv_jobs(demo_nnv_paths())
  expect_s3_class(loaded, "maithic_nnv_jobs")
  jobs <- loaded$jobs
  expect_equal(jobs$label, c("holds", "fails", "open", "wrong", "again"))
  expect_equal(jobs$status, c("verified", "refuted", "unknown", "failed", "verified"))
  expect_equal(jobs$claim, c("verified", "violated", "unknown", "verified", "verified"))
  expect_equal(jobs$accepted, c(TRUE, TRUE, NA, FALSE, TRUE))
  expect_equal(jobs$axioms, c(2, 2, NA, 0, 2))
  expect_equal(jobs$layers, rep(2, 5))
  expect_equal(jobs$neurons, rep(3, 5))
  expect_equal(jobs$inputs, rep(2, 5))
  expect_equal(jobs$outputs, rep(1, 5))
  expect_equal(jobs$verify_ms, c(2000, 1000, 3000, 2000, 4000))
  expect_equal(jobs$check_ms, c(300, 200, NA, 100, 500))
  expect_equal(jobs$reason[3:4], c("the bounds are too loose", "Lean did not accept it"))

  summary <- verification_summary(loaded)
  expect_equal(summary$status, c("verified", "refuted", "unknown", "failed"))
  expect_equal(summary$jobs, c(2, 1, 1, 1))
  expect_equal(summary$share, c(0.4, 0.2, 0.2, 0.2))
  expect_true(all(summary$lower < summary$share & summary$share < summary$upper))
  expect_equal(summary$median_verify_ms, c(3000, 1000, 3000, 2000))
  expect_equal(summary$median_check_ms, c(400, 200, NA, 100))

  expect_error(read_nnv_jobs(character()), "no job was given")
  expect_error(read_nnv_jobs(demo_nnv_paths(), label = "one"), "one entry for each path")
  expect_error(read_nnv_jobs(write_nnv_job("proved", "verified", accepted = TRUE)), "unknown status 'proved'")
  expect_error(read_nnv_jobs(write_nnv_job("verified", "certain", accepted = TRUE)), "unknown claim 'certain'")
  # A status that the claim and the check do not make.
  expect_error(read_nnv_jobs(write_nnv_job("verified", "verified", accepted = FALSE)), "the claim and the check make it 'failed'")
  expect_error(read_nnv_jobs(write_nnv_job("verified", "violated", accepted = TRUE)), "make it 'refuted'")
  expect_error(read_nnv_jobs(write_nnv_job("verified", "verified")), "Lean was not asked")
  expect_error(read_nnv_jobs(write_nnv_job("unknown", "unknown", accepted = TRUE)), "nothing was claimed, and something was checked")
  expect_error(read_nnv_jobs(write_document(list(schema = "maithic.bench.run/1"))), "verification job")
  expect_error(verification_summary(list()), "must come from read_nnv_jobs")
})

test_that("the jobs the repository keeps are read, and say what the commands said", {
  root <- fixture_root()
  skip_if(is.null(root), "the repository fixtures are not at hand")
  paths <- file.path(root, "nnv", "jobs", c("bounded.json", "too-tight.json", "beyond-the-bounds.json", "unit-square.json"))
  jobs <- read_nnv_jobs(paths)$jobs
  expect_equal(jobs$label, c("bounded", "too-tight", "beyond-the-bounds", "unit-square"))
  expect_equal(jobs$status, c("verified", "refuted", "unknown", "verified"))
  expect_equal(jobs$model, c("absolute value", "absolute value", "absolute value", "small classifier"))
  expect_equal(jobs$neurons, c(3, 3, 3, 6))
  expect_equal(jobs$inputs, c(1, 1, 1, 2))
  expect_equal(jobs$axioms, c(3, 3, NA, 3))
})
