# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()

test_that("the figure of a council session draws each turn in the lane of its role", {
  sessions <- maithicstats::read_council_sessions(file.path(root, "fixtures", "council", "sum-odd.json"))
  plot <- plot_council_session(sessions)
  built <- ggplot2::ggplot_build(plot)
  marks <- built$data[[4]]
  # Seven turns, from left to right, each in the lane of its role: the critic at the bottom, the researcher at the top.
  expect_equal(nrow(marks), 7)
  expect_equal(marks$x, 1:7)
  expect_equal(as.integer(marks$y), c(2, 4, 3, 2, 1, 3, 2))
  expect_equal(plot$data$height, c(2, 4, 3, 2, 1, 3, 2))
  # Three bands: the examination and two rounds.
  expect_equal(built$data[[2]]$label, c("examination", "round 1", "round 2"))
  # What each turn did stands in a row of its own under the lanes, below its mark: the line that
  # joins the marks stays between the lanes and never reaches the words.
  notes <- built$data[[5]]
  expect_equal(notes$x, 1:7)
  expect_true(all(notes$y < 1) && length(unique(notes$y)) == 1)
  expect_true(all(built$data[[3]]$y >= 1))
  expect_match(notes$label[2], "4 of 5 names\nexist in the\nlibrary; 1 idea", fixed = TRUE)
  expect_match(notes$label[7], "Lean accepted a\nproof", fixed = TRUE)
  # No note is wider than its column, and the panel reaches below the longest of them.
  expect_true(all(nchar(unlist(strsplit(notes$label, "\n", fixed = TRUE))) <= 18))
  expect_lt(built$layout$panel_params[[1]]$y.range[1], unique(notes$y) - 0.34 * max(lengths(strsplit(notes$label, "\n", fixed = TRUE))))
  expect_match(plot$labels$caption, "the session on sumOdd_eq_square: 7 turn\\(s\\), 4 question\\(s\\) to models, 24 call\\(s\\) to Lean; the claim is verified")
  expect_s3_class(plot_council_session(sessions, dark = TRUE), "ggplot")
  expect_error(plot_council_session(list()), "must come from maithicstats::read_council_sessions")
  expect_error(plot_council_session(sessions, run = 3), "there is no session 3")

  out <- tempfile("figures")
  path <- render_council(file.path(root, "fixtures", "council", "sum-odd.json"), out)
  expect_equal(basename(path), "council-session.svg")
  expect_true(file.exists(path) && file.size(path) > 2000)
})
