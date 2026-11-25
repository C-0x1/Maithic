# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()

test_that("the figure of a blueprint's progress has one bar for each snapshot, divided by status", {
  paths <- file.path(root, "fixtures", "blueprint", "snapshots", c("1-skeleton.json", "2-half-done.json", "3-one-proof-left.json", "4-finished.json"))
  series <- maithicstats::read_blueprint_snapshots(paths)
  plot <- plot_blueprint_progress(series)
  built <- ggplot2::ggplot_build(plot)
  bars <- built$data[[1]]
  # Four snapshots of four statuses each; every bar is eight nodes long.
  expect_equal(nrow(bars), 16)
  expect_equal(as.vector(tapply(bars$xmax - bars$xmin, bars$y, sum)), c(8, 8, 8, 8))
  # The earliest snapshot is at the top.
  expect_equal(levels(plot$data$label), c("finished", "one proof left", "half done", "skeleton"))
  # A status without a node has no number on it.
  labels <- built$data[[2]]$label
  expect_equal(sum(labels == ""), 6)
  expect_true("8" %in% labels)
  expect_match(plot$labels$caption, "4 snapshot\\(s\\) of the blueprint \"Odd numbers, squares and triangles\"; at the last one 8 of 8 node\\(s\\) are done")
  expect_s3_class(plot_blueprint_progress(series, dark = TRUE), "ggplot")
  expect_error(plot_blueprint_progress(list()), "must come from read_blueprint_snapshots")

  out <- tempfile("figures")
  path <- render_blueprint(paths, out)
  expect_equal(basename(path), "blueprint-progress.svg")
  expect_true(file.exists(path) && file.size(path) > 2000)
})
