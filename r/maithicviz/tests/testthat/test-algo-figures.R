# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()

test_that("the figure of measured growth has a panel for each algorithm, with its points on or under the proved bound", {
  path <- file.path(root, "fixtures", "algolab", "costs.csv")
  costs <- maithicstats::read_algo_costs(path)
  plot <- plot_growth(costs)
  built <- ggplot2::ggplot_build(plot)
  lines <- built$data[[1]]
  points <- built$data[[2]]
  # Four algorithms at seven sizes: a line and a row of points in each panel.
  expect_equal(length(unique(lines$PANEL)), 4)
  expect_equal(nrow(points), 28)
  expect_equal(nrow(lines), 28)
  # On the logarithmic axis no point lies above the line of its panel.
  bound <- lines$y[order(lines$PANEL, lines$x)]
  measured <- points$y[order(points$PANEL, points$x)]
  expect_true(all(measured <= bound + 1e-9))
  expect_equal(levels(plot$data$panel),
               c("linear search\nmeasured: linear", "binary search\nmeasured: logarithmic", "insertion sort\nmeasured: quadratic", "merge sort\nmeasured: linearithmic"))
  expect_match(plot$labels$caption, "28 measurement\\(s\\) of 4 algorithm\\(s\\); the largest input has 512 elements")
  expect_match(plot$labels$caption, "No measurement exceeds its bound")
  expect_s3_class(plot_growth(costs, dark = TRUE), "ggplot")
  # A count above its bound is said, not hidden.
  wrong <- costs
  wrong$comparisons[1] <- wrong$bound[1] + 1
  expect_match(plot_growth(wrong)$labels$caption, "1 measurement\\(s\\) exceed their bound")

  out <- tempfile("figures")
  written <- render_growth(path, out)
  expect_equal(basename(written), "algo-growth.svg")
  expect_true(file.exists(written) && file.size(written) > 2000)
})
