# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()
built <- function(plot, i) ggplot2::ggplot_build(plot)$data[[i]]
# The values of a layer of horizontal bars, from the top bar down.
downward <- function(plot, i, column = "y") {
  data <- built(plot, i)
  data[[column]][order(-data$x)]
}

test_that("P07-06-02 the figure of the most depended-upon declarations draws the table", {
  snapshot <- maithicstats::read_intel_snapshot(file.path(root, "fixtures", "intel"))
  table <- maithicstats::top_depended(snapshot, 12)
  table <- table[table$used_by > 0, ]
  plot <- plot_top_depended(snapshot)
  # The most used declaration is the top bar.
  expect_equal(downward(plot, 1), table$used_by)
  expect_equal(rev(levels(plot$data$label))[1], "Sorted")
  expect_equal(downward(plot, 2, "label")[1], "12  (29%)")
  expect_match(plot$labels$caption, "41 declaration\\(s\\) in 4 file\\(s\\); 3 rest on sorry")
  expect_equal(nrow(built(plot_top_depended(snapshot, n = 3), 1)), 3)
  # Bars that carry their numbers have no grid in either direction: no line runs through a number.
  expect_true(inherits(plot$theme$panel.grid.major.x, "element_blank") && inherits(plot$theme$panel.grid.major.y, "element_blank"))
  expect_s3_class(plot_top_depended(snapshot, dark = TRUE), "ggplot")
  # Names are shortened to their last part, and told apart where two end alike.
  expect_equal(maithicviz:::short_names(c("A.B.sort", "A.C.sort", "A.B.merge", "top")), c("B.sort", "C.sort", "merge", "top"))
  expect_equal(maithicviz:::short_names(c("x", "x")), c("x", "x"))
})

test_that("P11-06-02 the figure of proof lengths draws the distribution and marks the median", {
  corpus <- maithicstats::read_corpus(file.path(root, "fixtures", "corpus", "demo"))
  distribution <- maithicstats::proof_length_distribution(corpus)
  plot <- plot_proof_lengths(corpus)
  expect_equal(built(plot, 1)$x, distribution$steps)
  expect_equal(built(plot, 1)$y, distribution$proofs)
  # The bar of the median length is gold and the subtitle names the median; nothing is drawn over the bars.
  median <- maithicstats::proof_length_summary(corpus)$median
  expect_equal(length(ggplot2::ggplot_build(plot)$data), 1)
  expect_equal(unique(built(plot, 1)$fill[built(plot, 1)$x == median]), unname(maithic_colors()[["gold"]]))
  expect_equal(unique(built(plot, 1)$fill[built(plot, 1)$x != median]), unname(maithic_colors()[["teal"]]))
  expect_match(plot$labels$subtitle, sprintf("the median is %s step", format(median)), fixed = TRUE)
  expect_match(plot$labels$caption, "dataset demo, version 1: 28 theorem\\(s\\), 88 step pair\\(s\\)\\. 15 proof\\(s\\) are terms")
})

test_that("P11-06-03 the figure of tactics draws their uses and how often they closed a goal", {
  corpus <- maithicstats::read_corpus(file.path(root, "fixtures", "corpus", "demo"))
  table <- maithicstats::tactic_frequency(corpus, 15)
  plot <- plot_tactic_frequency(corpus)
  # Each bar is the uses of a tactic in two parts that do not cover each other: the steps that
  # closed a goal stand at the axis, and the others begin where those end.
  bars <- built(plot, 1)
  closed <- bars[bars$fill == maithic_colors()[["teal"]], ]
  open <- bars[bars$fill == maithic_colors()[["indigo"]], ]
  expect_equal(closed$ymax[order(-closed$x)], table$uses * table$closes)
  expect_true(all(closed$ymin == 0))
  expect_equal(open$ymin[order(-open$x)], table$uses * table$closes)
  expect_equal(open$ymax[order(-open$x)], table$uses)
  # The number of a bar begins to the right of its end.
  notes <- built(plot, 2)
  expect_match(notes$label[order(-notes$x)][1], "^22  \\([0-9]+%\\)$")
  expect_equal(notes$y[order(-notes$x)], table$uses + 0.012 * max(table$uses))
  expect_true(all(notes$hjust == 0))
  expect_equal(nrow(built(plot_tactic_frequency(corpus, n = 5), 2)), 5)
})

test_that("P03-06-02 the figure of review findings draws the counts by rule and severity", {
  runs <- maithicstats::read_review_runs(c(demo = file.path(root, "fixtures", "review", "report.json")))
  plot <- plot_review_findings(runs)
  counts <- maithicstats::finding_counts(runs, c("rule", "severity"))
  expect_equal(sum(built(plot, 1)$ymax - built(plot, 1)$ymin), sum(counts$findings))
  totals <- maithicstats::finding_counts(runs, "rule")
  expect_equal(downward(plot, 2, "label"), totals$findings)
  colors <- maithic_colors()
  expect_setequal(unique(built(plot, 1)$fill), unname(c(colors["coral"], colors["gold"], colors["indigo"])))
  expect_match(plot$labels$caption, "1 review\\(s\\) of 2 file\\(s\\) and 16 declaration\\(s\\); 19 finding\\(s\\), 2 suggested edit\\(s\\), 1 suppressed")
})

test_that("P07-06-02 the figures of the code graph, the dataset and the review are redrawn from the fixtures", {
  out <- file.path(tempdir(), "insights")
  paths <- render_insights(file.path(root, "fixtures", "intel"), file.path(root, "fixtures", "corpus", "demo"),
                           file.path(root, "fixtures", "review", "report.json"), out)
  expect_equal(basename(paths), c("intel-top-depended.svg", "corpus-proof-lengths.svg", "corpus-tactics.svg", "review-findings.svg"))
  expect_true(all(file.exists(paths)) && all(file.size(paths) > 2000))
  expect_true(any(grepl("insertionSort", readLines(paths[1], warn = FALSE))))
  expect_true(any(grepl("simpa", readLines(paths[3], warn = FALSE))))
})
