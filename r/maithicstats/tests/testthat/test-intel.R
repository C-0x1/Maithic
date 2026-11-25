# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P07-06-01 a snapshot is loaded from the graph, the health snapshot and the metrics", {
  snapshot <- read_intel_snapshot(write_snapshot(), label = "demo")
  expect_s3_class(snapshot, "maithic_intel_snapshot")
  expect_equal(snapshot$label, "demo")
  expect_equal(snapshot$nodes$name, c("a", "b", "c", "d", "Nat", "Demo"))
  expect_equal(snapshot$nodes$kind, c(rep("declaration", 5), "module"))
  expect_equal(snapshot$nodes$internal, c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(snapshot$nodes$decl_kind, c("definition", "theorem", "theorem", "theorem", "inductive", NA))
  expect_equal(snapshot$nodes$uses_sorry, c(FALSE, FALSE, TRUE, FALSE, FALSE, NA))
  expect_equal(snapshot$nodes$line, c(1L, 5L, 9L, 12L, 1L, NA))
  expect_equal(nrow(snapshot$edges), 9)
  expect_equal(snapshot$edges$via[c(1, 2, 9)], c("type", "both", NA))
  expect_equal(snapshot$health$declarations, 4)
  expect_equal(snapshot$health$by_kind, c(definition = 1L, theorem = 3L))
  expect_equal(snapshot$health$resting_on_sorry, 1)
  expect_equal(snapshot$unfinished$name, "c")
  expect_equal(snapshot$unfinished$goals, 1)
  expect_equal(snapshot$unfinished$through, NA_character_)
  expect_equal(snapshot$axioms$axiom, c("propext", "shortcut"))
  expect_equal(snapshot$axioms$standard, c(TRUE, FALSE))
  expect_equal(snapshot$axioms$used_by, c(1, 2))
  expect_equal(snapshot$axioms$users[[2]], c("b", "c"))
  expect_null(snapshot$metrics)

  # The label is the name of the folder unless one is given.
  dir <- write_snapshot()
  expect_equal(read_intel_snapshot(dir)$label, basename(dir))

  # With metrics, which must be of the same graph.
  figures <- function(names) list(schema = "maithic.intel.metrics/1", nodes = 5, internal = length(names), edges = 6, components = list(count = 1, sizes = list(5)),
                                  declarations = lapply(names, function(name) list(name = name, kind = "theorem", usedBy = 1, uses = 2, pagerank = 0.25, betweenness = 1.5)))
  with_metrics <- read_intel_snapshot(write_snapshot(metrics = figures(c("a", "b", "c", "d"))))
  expect_equal(with_metrics$metrics$name, c("a", "b", "c", "d"))
  expect_equal(with_metrics$metrics$pagerank, rep(0.25, 4))
  expect_equal(attr(with_metrics$metrics, "components"), 5L)
  expect_error(read_intel_snapshot(write_snapshot(metrics = figures(c("a", "b", "c", "z")))), "the metrics are not of this graph \\(z is not one of its declarations\\)")
  expect_error(read_intel_snapshot(write_snapshot(metrics = figures(c("a", "b", "c")))), "the metrics are not of this graph")

  # Documents that do not belong together, or are not what they should be, are refused.
  expect_error(read_intel_snapshot(file.path(tempdir(), "no-such-folder")), "folder not found")
  expect_error(read_intel_snapshot(c("a", "b")), "one folder")
  loose <- demo_graph()
  loose$edges <- c(loose$edges, list(graph_use("a", "ghost")))
  expect_error(read_intel_snapshot(write_snapshot(graph = loose)), "d:ghost, which is not a node of the graph")
  twice <- demo_graph()
  twice$nodes <- c(twice$nodes, list(graph_declaration("a")))
  expect_error(read_intel_snapshot(write_snapshot(graph = twice)), "the node d:a occurs twice")
  odd <- demo_graph()
  odd$nodes[[1]]$kind <- "planet"
  expect_error(read_intel_snapshot(write_snapshot(graph = odd)), "unknown kind 'planet'")
  other <- demo_health()
  other$declarations <- 5
  other$byKind$theorem <- 4
  expect_error(read_intel_snapshot(write_snapshot(health = other)), "they are not of the same scan")
  miscounted <- demo_health()
  miscounted$byKind$theorem <- 7
  expect_error(read_intel_snapshot(write_snapshot(health = miscounted)), "8 declaration\\(s\\) by kind, but a stated total of 4")
  unlisted <- demo_health()
  unlisted$restingOnSorry <- 2
  expect_error(read_intel_snapshot(write_snapshot(health = unlisted)), "1 unfinished declaration\\(s\\) listed, but a stated count of 2")
  wrong <- demo_graph()
  wrong$schema <- "maithic.prove.run/1"
  expect_error(read_intel_snapshot(write_snapshot(graph = wrong)), "is not a code graph")
})

test_that("P07-06-01 the snapshot of the repository's fixture is read whole", {
  snapshot <- read_intel_snapshot(file.path(fixture_root(), "intel"))
  mine <- snapshot$nodes[snapshot$nodes$kind == "declaration" & snapshot$nodes$internal, ]
  expect_equal(nrow(mine), 41)
  expect_equal(snapshot$health$declarations, 41)
  expect_equal(snapshot$health$by_kind[["theorem"]], 28)
  expect_equal(sum(mine$uses_sorry), snapshot$health$resting_on_sorry)
  expect_equal(sum(mine$documented), snapshot$health$documented)
  expect_equal(snapshot$unfinished$name, c("open_one", "uses_open", "term_gap"))
  expect_equal(snapshot$unfinished$through, c(NA, "open_one", NA))
  expect_equal(snapshot$axioms$axiom[!snapshot$axioms$standard], "shortcut")
  expect_equal(snapshot$axioms$users[[which(snapshot$axioms$axiom == "shortcut")]], "by_shortcut")
  expect_equal(nrow(snapshot$metrics), 41)
})

test_that("P07-06-02 the most depended-upon declarations are ranked by how many others use them", {
  snapshot <- read_intel_snapshot(write_snapshot())
  table <- top_depended(snapshot)
  expect_equal(names(table), c("rank", "name", "kind", "file", "line", "used_by", "in_statements", "in_proofs", "share"))
  # a is used by b, c and d; c uses it twice and counts once; d uses itself, which does not count.
  expect_equal(table$name, c("a", "b", "c", "d"))
  expect_equal(table$used_by, c(3, 1, 0, 0))
  expect_equal(table$rank, c(1, 2, 3, 3))
  expect_equal(table$in_statements, c(2, 0, 0, 0))
  expect_equal(table$in_proofs, c(2, 1, 0, 0))
  expect_equal(table$share, c(0.75, 0.25, 0, 0))
  expect_equal(table$kind[1], "definition")
  expect_equal(table$line, c(1, 5, 9, 12))

  # With the libraries the code leans on.
  everything <- top_depended(snapshot, internal_only = FALSE)
  expect_equal(everything$name, c("a", "Nat", "b", "c", "d"))
  expect_equal(everything$used_by, c(3, 2, 1, 0, 0))
  expect_equal(everything$rank, c(1, 2, 3, 4, 4))

  expect_equal(top_depended(snapshot, 2)$name, c("a", "b"))
  expect_equal(nrow(top_depended(snapshot, 0)), 0)
  expect_equal(nrow(top_depended(snapshot, Inf)), 4)
  expect_error(top_depended(snapshot, -1), "must be a count")
  expect_error(top_depended(list()), "read_intel_snapshot")

  # The count agrees with the one the graph package computes from the same graph.
  fixture <- read_intel_snapshot(file.path(fixture_root(), "intel"))
  ranked <- top_depended(fixture, Inf)
  expect_equal(ranked$name[1], "Maithic.Algo.Sorted")
  expect_equal(ranked$used_by[1], 12)
  expect_equal(ranked$used_by, fixture$metrics$used_by[match(ranked$name, fixture$metrics$name)])
  expect_equal(top_depended(fixture, 3, internal_only = FALSE)$name, c("List", "LE", "Eq"))

  report <- intel_report_table(snapshot, n = 2)
  expect_equal(report[1], "4 declaration(s) in 1 file(s): 2 documented, 1 resting on `sorry`, 0 file(s) with errors.")
  expect_true("| 1 | a | definition | 3 | 75.0% |" %in% report)
  expect_true("| c | Demo.lean | 9 |  |" %in% report)
  expect_true("| shortcut | 2 |" %in% report)
  expect_false(any(grepl("propext", report)))
})

test_that("declarations are counted by the area they are in and by what they are", {
  snapshot <- read_intel_snapshot(file.path(fixture_root(), "intel"))
  table <- declarations_by_area(snapshot)
  # The scanned files share no folder, so an area is named by the first two folders of a file.
  expect_equal(table$area, c("lean/Maithic", "fixtures/intel"))
  expect_equal(table$declarations, c(30L, 11L))
  expect_equal(table$theorems + table$definitions + table$others, table$declarations)
  expect_equal(table$files, c(3L, 1L))
  expect_equal(sum(table$declarations), snapshot$health$declarations)
  deeper <- declarations_by_area(snapshot, depth = 4)
  expect_setequal(deeper$area, c("lean/Maithic/Algo/Sort", "lean/Maithic/Algo/Spec", "fixtures/intel/lean"))
  expect_equal(sum(deeper$declarations), 41)
  expect_true(all(diff(deeper$declarations) <= 0))
  # Files of one folder: the folder is left out and each file is an area of its own.
  flat <- snapshot
  flat$nodes$file <- ifelse(is.na(flat$nodes$file), NA, file.path("src", basename(flat$nodes$file)))
  expect_setequal(declarations_by_area(flat)$area, c("Demo", "Sorted", "Insertion", "Linear"))
  expect_error(declarations_by_area(snapshot, depth = 0), "`depth` must be a count of at least 1")
  expect_error(declarations_by_area(list()), "read_intel_snapshot")
})
