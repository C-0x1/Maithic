# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

demo_run <- function() {
  path <- tempfile(fileext = ".json")
  problem <- function(id, status, tags, ms, proposer, attempts) {
    list(id = id, suite = "demo", tags = as.list(tags), difficulty = 1, status = status, proposer = proposer, proof = "by omega", attempts = attempts, rounds = 0, elapsed_ms = ms)
  }
  problems <- list(
    problem("demo-001", "verified", "nat", 400, "portfolio", 4), problem("demo-002", "verified", c("nat", "logic"), 500, "portfolio", 4),
    problem("demo-003", "failed", "logic", 1500, "", 18), problem("demo-004", "verified", "list", 900, "model", 21)
  )
  jsonlite::write_json(list(schema = "maithic.bench.run/1", manifest = "demo.json", mode = "replay", use_ai = TRUE, total = 4, verified = 3, problems = problems), path, auto_unbox = TRUE)
  maithicstats::read_bench_run(path)
}

test_that("P15-03-01 the theme uses the brand colours", {
  colors <- maithic_colors()
  expect_equal(colors[["teal"]], "#0FA89B")
  expect_equal(colors[["gold"]], "#F0A92E")
  expect_true(all(grepl("^#[0-9A-F]{6}$", colors)))
  light <- theme_maithic()
  dark <- theme_maithic(dark = TRUE)
  expect_s3_class(light, "theme")
  expect_equal(light$plot.background$fill, colors[["paper"]])
  expect_equal(dark$plot.background$fill, colors[["ink"]])
  expect_equal(light$plot.title$colour, colors[["ink"]])
  expect_equal(dark$plot.title$colour, colors[["paper"]])
})

test_that("P15-03-02 the success chart draws one row per group: a dot at the rate on the line of its interval", {
  run <- demo_run()
  plot <- plot_success_by_group(run, "tag")
  expect_s3_class(plot, "ggplot")
  built <- ggplot2::ggplot_build(plot)
  intervals <- built$data[[1]]
  dots <- built$data[[2]]
  expected <- maithicstats::success_by(run$problems, "tag")
  # The first group is the top row; the label of a row gives its counts.
  expect_equal(nrow(dots), 3)
  expect_equal(dots$x[order(-dots$y)], expected$rate)
  expect_equal(intervals$xmin[order(-intervals$y)], expected$lower)
  expect_equal(intervals$xmax[order(-intervals$y)], expected$upper)
  expect_equal(rev(levels(plot$data$row)), sprintf("%s (%d of %d)", expected$group, expected$solved, expected$n))
  # Two layers and no more: nothing is written inside the panel, and no bar lies under the marks.
  expect_equal(length(built$data), 2)
  expect_match(plot$labels$caption, "4 problems")
  expect_match(plot$labels$caption, "Lines show 95% Wilson intervals")
})

test_that("the time chart keeps the order of the suite and colours by proposer", {
  run <- demo_run()
  built <- ggplot2::ggplot_build(plot_time_per_problem(run))
  bars <- built$data[[1]]
  expect_equal(bars$y, c(0.4, 0.5, 1.5, 0.9))
  colors <- maithic_colors()
  expect_equal(bars$fill, unname(c(colors["teal"], colors["teal"], colors["coral"], colors["gold"])))
  attempts <- ggplot2::ggplot_build(plot_attempts(run))$data[[1]]
  expect_equal(attempts$y, c(2, 1))
})

test_that("P15-03-03 figures are saved as SVG at a fixed size, identically each time", {
  run <- demo_run()
  first <- save_figure(plot_success_by_group(run), tempfile(fileext = ".svg"))
  second <- save_figure(plot_success_by_group(run), tempfile(fileext = ".svg"))
  text <- readLines(first, warn = FALSE)
  expect_match(text[1], "<\\?xml")
  expect_true(any(grepl("width='648.00pt' height='345.60pt'", text, fixed = TRUE)))
  expect_true(any(grepl("Verified problems by tag", text, fixed = TRUE)))
  expect_identical(text, readLines(second, warn = FALSE))
  png_path <- save_figure(plot_success_by_group(run), tempfile(fileext = ".png"), width = 6, height = 4)
  header <- readBin(png_path, "raw", n = 24)
  expect_identical(header[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  size <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  expect_equal(c(size(header[17:20]), size(header[21:24])), c(900, 600))
  expect_error(save_figure(plot_success_by_group(run), tempfile(fileext = ".bmp")), "unsupported")
})

test_that("P15-03-04 render_all regenerates every figure from a result file", {
  root <- repo_root()
  skip_if(is.null(root), "the repository root was not found")
  run_path <- file.path(root, "bench", "results", "basics-portfolio.json")
  skip_if_not(file.exists(run_path), "the committed run is not present")
  out <- file.path(tempdir(), "maithic-figures")
  paths <- render_all(run_path, out)
  expect_equal(basename(paths), c("bench-basics-by-tag.svg", "bench-basics-time.svg", "bench-basics-attempts.svg"))
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 2000))
})
