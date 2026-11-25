# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()
formalize_fixtures <- function() {
  folder <- file.path(root, "fixtures", "formalize")
  c(`the square of a negative integer` = file.path(folder, "negative-square.json"), `(a - b) + b = a` = file.path(folder, "subtraction.json"),
    `n squared is at least n` = file.path(folder, "square-bound.json"))
}

test_that("the figure of formalization runs draws each problem by what became of its candidates", {
  runs <- maithicstats::read_formalize_runs(formalize_fixtures())
  plot <- plot_candidate_fates(runs)
  bars <- ggplot2::ggplot_build(plot)$data[[1]]
  # One piece of a bar for each outcome a run had: chosen, duplicate and rejected; chosen, kept and rejected; chosen, kept, duplicate and failed.
  expect_equal(nrow(bars), 10)
  # The bars add up to the candidates of each problem; the first problem is the top bar.
  totals <- tapply(bars$ymax - bars$ymin, bars$x, sum)
  expect_equal(unname(as.numeric(totals)), c(4, 3, 4))
  expect_equal(levels(plot$data$label), rev(names(formalize_fixtures())))
  notes <- ggplot2::ggplot_build(plot)$data[[2]]
  expect_equal(notes$label[order(-notes$x)], c("a statement", "a statement, flagged", "a statement"))
  expect_match(plot$labels$caption, "3 formalization run\\(s\\), 11 candidate\\(s\\); 8 elaborate as drafted, 10 after repair")
  expect_s3_class(plot_candidate_fates(runs, dark = TRUE), "ggplot")
  expect_error(plot_candidate_fates(list()), "must come from maithicstats::read_formalize_runs")

  out <- tempfile("figures")
  path <- render_formalize(formalize_fixtures(), out)
  expect_equal(basename(path), "formalize-candidates.svg")
  expect_true(file.exists(path) && file.size(path) > 2000)
})
