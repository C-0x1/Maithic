# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

write_json_file <- function(document) {
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(document, path, auto_unbox = TRUE, null = "null")
  path
}

attempt <- function(accepted, ms, class = NULL) {
  out <- list(round = 0, proposer = "portfolio", proof = "by omega", accepted = accepted, elapsed_ms = ms)
  if (!is.null(class)) out$error_class <- class
  out
}

result <- function(theorem, status, ms, attempts, axioms = character(), recheck_ms = NULL) {
  out <- list(theorem = theorem, status = status, proposer = if (status == "verified") "portfolio" else "", proof = "by omega",
              axioms = as.list(axioms), decision = list(status = status, violations = list()), attempts = attempts, rounds_used = 0, elapsed_ms = ms)
  if (!is.null(recheck_ms)) out$recheck <- list(passed = TRUE, axioms = as.list(axioms), elapsed_ms = recheck_ms)
  out
}

record <- function(model, ok, input, output, ms) {
  list(purpose = "prove.propose", provider = "p", model = model, served_by = "live", ok = ok, input_tokens = input, output_tokens = output,
       started_at = "2026-11-20T10:00:00Z", elapsed_ms = ms)
}

demo_runs <- function() {
  records <- list(record("m1", TRUE, 1000, 200, 800), record("m1", TRUE, 500, 100, 1200), record("m2", TRUE, 300, 900, 400), record("m2", FALSE, 0, 0, 50))
  maithicstats::read_prove_runs(write_json_file(list(
    schema = "maithic.prove.run/1", file = "demo.lean", policy = list(name = "default"),
    results = list(
      result("a", "verified", 1000, list(attempt(FALSE, 100, "tactic-failed"), attempt(TRUE, 200)), axioms = "propext", recheck_ms = 300),
      result("b", "verified", 400, list(attempt(TRUE, 100)), recheck_ms = 100),
      result("c", "failed", 3000, list(attempt(FALSE, 50, "tactic-failed"), attempt(FALSE, 60, "timeout")), axioms = "sorryAx")
    ),
    summary = list(total = 3, verified = 2), gateway = list(mode = "live", calls = 4, records = records)
  )))
}

layer <- function(plot, i) ggplot2::ggplot_build(plot)$data[[i]]

test_that("P01-05-07 the prover's figures draw what its summaries say", {
  runs <- demo_runs()
  success <- plot_prover_success(runs)
  expected <- maithicstats::prover_success(runs, by = "provider")
  # One row: a dot at the rate on the line of its interval, with the counts in the label of the row.
  expect_equal(layer(success, 2)$x, expected$rate)
  expect_equal(layer(success, 1)$xmin, expected$lower)
  expect_equal(layer(success, 1)$xmax, expected$upper)
  expect_equal(levels(success$data$row), sprintf("%s (2 of 3)", expected$provider))
  expect_match(success$labels$caption, "gateway mode live")
  expect_match(success$labels$caption, "Lines show 95% Wilson intervals")
  by_file <- plot_prover_success(runs, by = "file")
  expect_equal(by_file$labels$title, "Theorems proved by file")

  # Each bar is the whole attempt in two parts that do not cover each other: the part in Lean
  # stands on the axis, and everything else on top of it.
  time <- layer(plot_prover_time(runs), 1)
  colors <- maithic_colors()
  lean <- time[time$fill == colors[["teal"]], ]
  rest <- time[time$fill == colors[["gold"]], ]
  expect_equal(lean$ymin, c(0, 0, 0))
  expect_equal(lean$ymax, c(0.6, 0.2, 0.11))
  expect_equal(rest$ymin, lean$ymax)
  expect_equal(rest$ymax, c(1, 0.4, 3))

  failures <- layer(plot_prover_failures(runs), 1)
  expect_equal(sort(failures$y), c(1, 2))
  expect_equal(unique(failures$fill), colors[["coral"]])
  clean <- runs
  clean$attempts <- clean$attempts[clean$attempts$accepted, ]
  expect_error(plot_prover_failures(clean), "no candidate proof was rejected")
})

test_that("P18-04-07 the ledger's figures draw trust levels and the axiom census", {
  records <- maithicstats::ledger_records(runs = demo_runs())
  trust <- plot_trust_distribution(records)
  expected <- maithicstats::trust_distribution(records)
  # One row for each status: a dot at its share on the line of its interval, in the colour of the status.
  dots <- layer(trust, 2)
  expect_equal(dots$x[order(-dots$y)], expected$share)
  expect_equal(dots$colour[order(-dots$y)], unname(status_colors()[expected$status]))
  expect_equal(layer(trust, 1)$xmax[order(-layer(trust, 1)$y)], expected$upper)
  expect_equal(rev(levels(trust$data$row)), sprintf("%s (%d result%s)", expected$status, expected$count, ifelse(expected$count == 1, "", "s")))
  expect_match(trust$labels$caption, "3 result\\(s\\), 3 from prove")
  expect_equal(names(status_colors()), c("verified", "checked", "refuted", "conjectured", "unverified", "unknown", "failed", "abstained"))

  census <- maithicstats::axiom_census(records)
  bars <- layer(plot_axiom_census(records), 1)
  expect_equal(sort(bars$y), sort(census$results))
  colors <- maithic_colors()
  expect_setequal(bars$fill, unname(c(colors["indigo"], colors["teal"], colors["coral"])))
})

test_that("P10-09-07 the gateway's figures draw tokens and latency", {
  usage <- demo_runs()$usage
  tokens <- layer(plot_usage_tokens(usage), 1)
  expect_equal(sum(tokens$ymax - tokens$ymin), 3000)
  expect_equal(max(tokens$ymax[tokens$x == 1]), 1800)
  latency <- plot_call_latency(usage)
  points <- layer(latency, 1)
  expect_equal(sort(points$y), c(0.4, 0.8, 1.2))
  expect_equal(layer(latency, 2)$y, c(1.0, 0.4))
  expect_match(latency$labels$caption, "4 call\\(s\\), served by live \\(4\\)")
  expect_equal(layer(plot_call_latency(usage), 1)$x, points$x)                               # the jitter is seeded
  expect_error(plot_call_latency(usage[!usage$ok, ]), "no successful call")
})

test_that("P04-06-07 retrieval figures draw recall curves and query latency", {
  queryset <- maithicstats::read_query_set(write_json_file(list(
    schema = "maithic.search.queryset/1", name = "demo",
    queries = list(list(id = "q1", text = "one", relevant = list("A", "B")), list(id = "q2", text = "two", relevant = list("C")))
  )))
  rankings <- function(engine, first, second, ms) {
    maithicstats::read_rankings(write_json_file(list(
      schema = "maithic.search.rankings/1", engine = engine, queryset = "demo",
      results = list(list(query = "q1", ranked = as.list(first), elapsed_ms = ms[1]), list(query = "q2", ranked = as.list(second), elapsed_ms = ms[2]))
    )))
  }
  good <- rankings("good", c("A", "B"), "C", c(1, 3))
  poor <- rankings("poor", c("Z", "A"), c("Y", "X"), c(2, 2))
  curve <- plot_recall_curve(queryset, list(good, poor), k = c(1, 2))
  lines <- layer(curve, 2)
  expect_equal(lines$y, c(0.75, 1, 0, 0.25))
  expect_equal(length(unique(lines$colour)), 2)
  expect_match(curve$labels$caption, "query set demo, 2 queries")
  expect_equal(layer(plot_recall_curve(queryset, good, k = 2), 3)$y, 1)
  latency <- plot_query_latency(list(good, poor))
  expect_equal(sort(layer(latency, 1)$y), c(1, 2, 2, 3))
  expect_equal(layer(latency, 2)$y, c(2, 2))
})

test_that("P15-03-04 the figures of the recorded runs are redrawn from the fixtures", {
  root <- repo_root()
  skip_if(is.null(root), "the repository fixtures are not available")
  out <- file.path(tempdir(), "report-figures")
  paths <- render_reports(file.path(root, "fixtures", "runs"), out)
  expect_equal(basename(paths), c("prover-success.svg", "prover-time.svg", "prover-rejections.svg", "ledger-trust.svg", "ledger-axioms.svg"))
  expect_true(all(file.exists(paths)) && all(file.size(paths) > 2000))
  expect_true(any(grepl("gateway mode replay", readLines(paths[1], warn = FALSE))))
  expect_error(render_reports(tempdir(), out), "holds no prove runs")
})
