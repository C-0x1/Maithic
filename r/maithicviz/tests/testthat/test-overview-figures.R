# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

root <- repo_root()
built <- function(plot, i) ggplot2::ggplot_build(plot)$data[[i]]

test_that("the figure of the register has one bar for each pillar, divided by language", {
  status <- maithicstats::read_register_status(file.path(root, "registry", "status.json"))
  plot <- plot_register(status)
  bars <- built(plot, 1)
  # One segment for each pillar and language, and the segments of a pillar add up to its capabilities.
  expect_equal(nrow(bars), 4 * nrow(status$pillars))
  expect_equal(sum(bars$ymax - bars$ymin), status$total)
  # The first pillar is the top bar, and the number beside each bar is its total.
  expect_equal(rev(levels(plot$data$pillar))[1], sprintf("%s  %s", status$pillars$name[1], status$pillars$id[1]))
  expect_equal(as.integer(built(plot, 2)$label[order(-built(plot, 2)$x)]), status$pillars$capabilities)
  expect_match(plot$labels$caption, sprintf("%d capabilities in %d pillars, %d of them done", status$total, nrow(status$pillars), status$done))
  expect_s3_class(plot_register(status, dark = TRUE), "ggplot")
})

test_that("the figure of the areas of a library has one bar for each area, divided by kind", {
  snapshot <- maithicstats::read_intel_snapshot(file.path(root, "fixtures", "intel"))
  table <- maithicstats::declarations_by_area(snapshot)
  plot <- plot_library_areas(snapshot)
  bars <- built(plot, 1)
  expect_equal(nrow(bars), 3 * nrow(table))
  expect_equal(sum(bars$ymax - bars$ymin), sum(table$declarations))
  expect_equal(rev(levels(plot$data$area)), table$area)
  expect_equal(built(plot, 2)$label[order(-built(plot, 2)$x)], c("30 in 3 files", "11 in 1 file"))
  expect_match(plot$labels$caption, "41 declaration\\(s\\): 28 theorem\\(s\\), 6 definition\\(s\\), 7 other\\(s\\)")
  expect_equal(nrow(built(plot_library_areas(snapshot, depth = 4), 1)), 9)
  expect_s3_class(plot_library_areas(snapshot, dark = TRUE), "ggplot")
})

test_that("P04-06-02 the figures of the register, the library and the search are redrawn from the repository", {
  out <- file.path(tempdir(), "overview")
  register <- render_register(file.path(root, "registry", "status.json"), out)
  library <- render_library(file.path(root, "fixtures", "intel"), out, label = "the fixture")
  search <- render_search(file.path(root, "fixtures", "search", "queries.json"),
                          file.path(root, "fixtures", "search", c("rankings-lexical.json", "rankings-name.json")), out)
  paths <- c(register, library, search)
  expect_equal(basename(paths), c("register-by-pillar.svg", "library-areas.svg", "search-recall.svg"))
  expect_true(all(file.exists(paths)) && all(file.size(paths) > 2000))
  expect_true(any(grepl("Platform", readLines(register, warn = FALSE))))
  expect_true(any(grepl("the fixture", readLines(library, warn = FALSE))))
  expect_true(any(grepl("maithic-lexical", readLines(search, warn = FALSE))))
})
