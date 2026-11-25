# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The repository root: the nearest directory above the tests with PROJECT.toml.
repo_root <- function() {
  dir <- normalizePath(getwd())
  for (step in 1:12) {
    if (file.exists(file.path(dir, "PROJECT.toml"))) return(dir)
    dir <- dirname(dir)
  }
  NULL
}

write_run <- function(problems, total = length(problems), verified = NULL) {
  path <- tempfile(fileext = ".json")
  run <- list(schema = "maithic.bench.run/1", manifest = "test.json", mode = "replay", use_ai = FALSE, total = total, problems = problems)
  if (!is.null(verified)) run$verified <- verified
  jsonlite::write_json(run, path, auto_unbox = TRUE)
  path
}

problem <- function(id, status, tags, ms, proposer = "portfolio", difficulty = 1) {
  list(id = id, suite = "demo", tags = as.list(tags), difficulty = difficulty, status = status, proposer = if (status == "verified") proposer else "", proof = "by omega", attempts = 3, rounds = 0, elapsed_ms = ms)
}

demo <- list(
  problem("demo-001", "verified", "nat", 100), problem("demo-002", "verified", c("nat", "logic"), 200),
  problem("demo-003", "failed", "logic", 900, difficulty = 3), problem("demo-004", "verified", "list", 300, proposer = "model", difficulty = 2)
)

test_that("result import validates the file before anything is computed", {
  run <- read_bench_run(write_run(demo, verified = 3))
  expect_equal(nrow(run$problems), 4)
  expect_equal(run$problems$solved, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(run$problems$tags[[2]], c("nat", "logic"))
  expect_equal(run$meta$mode, "replay")
  expect_error(read_bench_run(file.path(tempdir(), "missing.json")), "not found")
  wrong_schema <- tempfile(fileext = ".json")
  writeLines('{"schema": "something/else", "problems": []}', wrong_schema)
  expect_error(read_bench_run(wrong_schema), "not a benchmark run")
  expect_error(read_bench_run(write_run(demo, total = 5)), "stated total")
  expect_error(read_bench_run(write_run(demo, verified = 4)), "verified problems")
  expect_error(read_bench_run(write_run(list(problem("x-001", "proved", "nat", 1)))), "unknown status")
  expect_error(read_bench_run(write_run(list(demo[[1]], demo[[1]]))), "unique")
  expect_error(read_bench_run(write_run(list())), "no problems")
})

test_that("success by group counts a problem under each of its tags", {
  run <- read_bench_run(write_run(demo))
  by_tag <- success_by(run$problems, "tag")
  expect_equal(by_tag$group, c("list", "logic", "nat"))
  expect_equal(by_tag$n, c(1, 2, 2))
  expect_equal(by_tag$solved, c(1, 1, 2))
  by_difficulty <- success_by(run$problems, "difficulty")
  expect_equal(by_difficulty$group, c("1", "2", "3"))
  expect_equal(by_difficulty$rate, c(1, 1, 0))
  expect_error(success_by(run$problems, "colour"))
})

test_that("P15-02-07 one call produces the standard analysis of a run", {
  analysis <- analyze_run(write_run(demo, verified = 3))
  expect_named(analysis, c("meta", "overall", "by_tag", "by_difficulty", "by_proposer", "time"))
  expect_equal(analysis$overall$estimate, 0.75)
  expect_equal(analysis$overall$lower, wilson_interval(3, 4)$lower)
  expect_equal(analysis$by_proposer$group, c("model", "portfolio"))
  expect_equal(analysis$by_proposer$n, c(1, 2))
  expect_equal(analysis$time$median_ms, 250)
  expect_equal(analysis$time$total_ms, 1500)
  expect_lte(analysis$time$median_lower, 250)
  expect_gte(analysis$time$median_upper, 250)
  expect_identical(analyze_run(read_bench_run(write_run(demo)))$overall, analysis$overall)
})

test_that("P15-02-07 the analysis of the committed basics run reports what the run contains", {
  root <- repo_root()
  skip_if(is.null(root), "the repository root was not found")
  path <- file.path(root, "bench", "results", "basics-portfolio.json")
  skip_if_not(file.exists(path), "the committed run is not present")
  analysis <- analyze_run(path)
  manifest <- jsonlite::fromJSON(file.path(root, "bench", "suites", "manifest.json"), simplifyVector = FALSE)
  expect_equal(analysis$overall$n, manifest$count)
  expect_equal(analysis$overall$successes, analysis$meta$verified)
  expect_false(analysis$meta$use_ai)
  expect_true(all(analysis$by_proposer$group == "portfolio"))
  expect_equal(sum(analysis$by_difficulty$n), manifest$count)
  expect_true(analysis$overall$lower > 0 && analysis$overall$upper <= 1)
})
