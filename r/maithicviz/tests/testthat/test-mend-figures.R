# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()
mend_fixtures <- function() {
  folder <- file.path(root, "fixtures", "mend")
  c(`rules` = file.path(folder, "repair-rules.json"), `rules and the prover` = file.path(folder, "repair-no-ai.json"),
    `rules, a model and the prover` = file.path(folder, "repair.json"))
}

test_that("P17-05-02 the figure of repair runs draws each run by the remedy that repaired its breakages", {
  runs <- maithicstats::read_mend_runs(mend_fixtures())
  plot <- plot_repair_stages(runs)
  bars <- ggplot2::ggplot_build(plot)$data[[1]]
  # One piece of a bar for each stage a run used: 3 and 2 left; 3, 1 and 1 left; 3 and 2.
  expect_equal(nrow(bars), 7)
  # The bars of the three runs all add up to the five breakages of the file.
  totals <- tapply(bars$ymax - bars$ymin, bars$x, sum)
  expect_equal(unname(as.numeric(totals)), c(5, 5, 5))
  # The first run is the top bar, and its label says what was left.
  expect_equal(levels(plot$data$label), rev(names(mend_fixtures())))
  notes <- ggplot2::ggplot_build(plot)$data[[2]]
  expect_equal(notes$label[order(-notes$x)], c("2 left", "1 left", "the file compiles"))
  expect_match(plot$labels$caption, "3 repair run\\(s\\) of fixtures/mend/lean/Upgrade.lean; 5 breakage\\(s\\) in each")
  expect_s3_class(plot_repair_stages(runs, dark = TRUE), "ggplot")
  expect_error(plot_repair_stages(list()), "must come from maithicstats::read_mend_runs")

  out <- tempfile("figures")
  path <- render_mend(mend_fixtures(), out)
  expect_equal(basename(path), "mend-repair-stages.svg")
  expect_true(file.exists(path) && file.size(path) > 2000)
})
