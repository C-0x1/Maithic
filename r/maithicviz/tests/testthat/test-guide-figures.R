# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()
layer <- function(plot, i) ggplot2::ggplot_build(plot)$data[[i]]

test_that("P12-06-02 the figure of learning curves draws both losses of every seed and marks the model that was kept", {
  runs <- maithicstats::read_guide_runs(file.path(root, "fixtures", "guide", "runs"))
  curves <- maithicstats::learning_curves(runs)
  best <- maithicstats::model_accuracy(runs)$name[1]
  own <- curves[curves$name == best, ]
  plot <- plot_learning_curves(runs)
  lines <- layer(plot, 1)
  # Five seeds, each in a panel of its own with its two lines, one point for every epoch.
  expect_equal(length(unique(lines$PANEL)), 5)
  expect_equal(nrow(unique(lines[c("PANEL", "group")])), 10)
  expect_equal(levels(plot$data$panel)[1], "seed 1: epoch 7 kept")
  expect_equal(nrow(lines), nrow(own))
  expect_equal(sort(lines$y), sort(own$value))
  # One mark for each run, at the validation loss of the epoch that was kept.
  marks <- layer(plot, 2)
  kept <- own[own$best & own$part == "validation", ]
  expect_equal(nrow(marks), 5)
  expect_equal(sort(as.integer(marks$PANEL)), 1:5)
  expect_equal(sort(marks$y), sort(kept$value))
  expect_equal(sort(marks$x), sort(kept$epoch))
  expect_match(plot$labels$title, best, fixed = TRUE)
  expect_match(plot$labels$caption, sprintf("5 run\\(s\\) of %s on maithic-library", best))
  # Another model can be asked for, and one that does not exist is refused.
  other <- plot_learning_curves(runs, model = "attention-mean-32", dark = TRUE)
  expect_equal(nrow(layer(other, 1)), nrow(curves[curves$name == "attention-mean-32", ]))
  expect_error(plot_learning_curves(runs, model = "median-32"), "there is no run of a model called 'median-32'")
})

test_that("P12-06-03 the figure of accuracy draws each model with its interval beside the baseline", {
  runs <- maithicstats::read_guide_runs(file.path(root, "fixtures", "guide", "runs"))
  accuracy <- maithicstats::model_accuracy(runs)
  plot <- plot_model_accuracy(runs)
  # Two panels: the first answer, and the first five. In each, one line for every model, from the
  # lower to the upper end of its interval; the most accurate model is the top row.
  intervals <- layer(plot, 1)
  expect_equal(nrow(intervals), 2 * nrow(accuracy))
  first <- intervals[intervals$PANEL == 1, ]
  expect_equal(first$xmin[order(-first$y)], accuracy$top1_lower)
  expect_equal(first$xmax[order(-first$y)], accuracy$top1_upper)
  # A dot for every model at its share, and in the last row a cross for the baseline.
  dots <- layer(plot, 2)
  top1 <- dots[dots$PANEL == 1, ]
  topk <- dots[dots$PANEL == 2, ]
  expect_equal(top1$x[order(-top1$y)], c(accuracy$top1, unique(accuracy$baseline_top1)))
  expect_equal(topk$x[order(-topk$y)], c(accuracy$topk, unique(accuracy$baseline_topk)))
  expect_equal(top1$shape[order(-top1$y)], c(rep(16, nrow(accuracy)), 4))
  expect_equal(levels(plot$data$name), rev(c(accuracy$name, "baseline")))
  # The shares stand in a column to the right of the last grid line, clear of every mark.
  shares <- layer(plot, 3)
  expect_true(all(shares$x > 1))
  expect_true(all(shares$x > max(intervals$xmax, dots$x)))
  expect_equal(shares$label[shares$PANEL == 1][order(-shares$y[shares$PANEL == 1])],
               sprintf("%.0f%%", 100 * c(accuracy$top1, unique(accuracy$baseline_top1))))
  expect_match(plot$labels$caption, "20 training run\\(s\\) on maithic-library; each model was tested on 678 proof step\\(s\\)")
  expect_s3_class(plot_model_accuracy(runs, dark = TRUE), "ggplot")
})

test_that("P12-06-03 the figures of the tactic models are written as files", {
  out <- tempfile("figures")
  paths <- render_guide(file.path(root, "fixtures", "guide", "runs"), out)
  expect_equal(basename(paths), c("guide-accuracy.svg", "guide-learning-curves.svg"))
  expect_true(all(file.exists(paths)))
  expect_true(all(file.size(paths) > 2000))
  text <- paste(readLines(paths[1], warn = FALSE), collapse = "\n")
  expect_match(text, "How often a tactic model names the next tactic", fixed = TRUE)
  expect_false(grepl(normalizePath(root), text, fixed = TRUE))
})
