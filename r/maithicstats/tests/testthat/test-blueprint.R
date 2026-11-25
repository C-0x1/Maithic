# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P16-05-01 status snapshots are loaded as a series, each checked against itself", {
  series <- read_blueprint_snapshots(demo_blueprint_paths())
  expect_s3_class(series, "maithic_blueprint_series")
  expect_equal(series$snapshots$label, c("start", "middle", "end"))
  expect_equal(series$snapshots$nodes, c(4, 4, 5))
  expect_equal(series$snapshots$planned, c(1, 0, 1))
  expect_equal(series$snapshots$stated, c(2, 1, 0))
  expect_equal(series$snapshots$proved, c(0, 1, 0))
  expect_equal(series$snapshots$done, c(1, 2, 4))
  expect_equal(series$snapshots$complete, c(FALSE, FALSE, FALSE))
  expect_equal(series$snapshots$ready, c(1, 1, 1))
  expect_equal(series$snapshots$blocked, c(2, 0, 0))
  expect_equal(nrow(series$nodes), 13)
  expect_equal(series$nodes$id[1:4], c("d", "a", "b", "t"))
  expect_equal(series$nodes$status[series$nodes$id == "t"], c("planned", "proved", "done"))
  expect_equal(series$nodes$blockers[series$nodes$snapshot == 1], c(0, 0, 1, 2))
  expect_equal(series$nodes$unblocks[2], 2)
  expect_equal(series$nodes$notes[series$nodes$id == "t"], c(0, 0, 1))

  # Labels: the names of the paths win over the label a report carries; without either, the file name.
  named <- read_blueprint_snapshots(c(first = demo_blueprint_paths()[[1]]))
  expect_equal(named$snapshots$label, "first")
  bare <- write_blueprint_status(list(blueprint_node("x", "lemma", "done")))
  expect_equal(read_blueprint_snapshots(bare)$snapshots$label, tools::file_path_sans_ext(basename(bare)))
  expect_true(read_blueprint_snapshots(bare)$snapshots$complete)

  expect_error(read_blueprint_snapshots(character()), "no status report was given")
  expect_error(read_blueprint_snapshots(demo_blueprint_paths()[c(1, 1)]), "two snapshots carry the label 'start'")
  expect_error(read_blueprint_snapshots(demo_blueprint_paths(), label = "one"), "one entry for each path")
  expect_error(read_blueprint_snapshots(write_blueprint_status(list(blueprint_node("x", "lemma", "finished")))), "unknown status 'finished'")
  expect_error(read_blueprint_snapshots(write_blueprint_status(list(blueprint_node("x", "riddle", "done")))), "unknown kind 'riddle'")
  expect_error(read_blueprint_snapshots(write_blueprint_status(list(blueprint_node("x", "lemma", "done"), blueprint_node("x", "lemma", "done")))), "'x' is listed twice")
  expect_error(read_blueprint_snapshots(write_blueprint_status(list(blueprint_node("x", "lemma", "done")), by_status = list(planned = 1, stated = 0, proved = 0, done = 0))),
               "does not count the nodes")
  expect_error(read_blueprint_snapshots(write_blueprint_status(list(blueprint_node("x", "lemma", "done", ready = TRUE)))), "settled is listed as ready")
  expect_error(read_blueprint_snapshots(write_blueprint_status(list(blueprint_node("x", "lemma", "stated", ready = TRUE, blockers = "y")))), "waits for another is listed as ready")
  expect_error(read_blueprint_snapshots(write_document(list(schema = "maithic.council.session/1"))), "blueprint status")
})

test_that("P16-05-02 the share of nodes in each status is computed for every snapshot", {
  series <- read_blueprint_snapshots(demo_blueprint_paths())
  shares <- status_composition(series)
  expect_equal(nrow(shares), 12)
  expect_equal(names(shares), c("snapshot", "label", "status", "nodes", "share"))
  expect_equal(shares$status[1:4], c("planned", "stated", "proved", "done"))
  expect_equal(shares$nodes[shares$snapshot == 1], c(1, 2, 0, 1))
  expect_equal(shares$share[shares$snapshot == 1], c(0.25, 0.5, 0, 0.25))
  expect_equal(shares$share[shares$snapshot == 3], c(0.2, 0, 0, 0.8))
  # The shares of a snapshot add up to one.
  expect_equal(as.vector(tapply(shares$share, shares$snapshot, sum)), c(1, 1, 1))

  kinds <- status_composition(series, by = "kind")
  expect_equal(unique(kinds$kind[kinds$snapshot == 3]), c("definition", "lemma", "theorem", "corollary"))
  lemmas <- kinds[kinds$kind == "lemma" & kinds$snapshot == 2, ]
  expect_equal(lemmas$nodes, c(0, 1, 0, 1))
  expect_equal(lemmas$share, c(0, 0.5, 0, 0.5))

  # What moved between snapshots, and in which direction.
  changes <- status_changes(series)
  expect_equal(changes$id, c("a", "t", "b", "t", "c"))
  expect_equal(changes$from, c("stated", "planned", "stated", "proved", NA))
  expect_equal(changes$to, c("done", "proved", "done", "done", "planned"))
  expect_equal(changes$label, c("middle", "middle", "end", "end", "end"))
  expect_true(all(changes$forward))
  back <- read_blueprint_snapshots(rev(demo_blueprint_paths())[2:3])
  expect_equal(status_changes(back)$forward, c(FALSE, FALSE))
  expect_equal(nrow(status_changes(read_blueprint_snapshots(demo_blueprint_paths()[[1]]))), 0)

  table <- blueprint_report_table(series)
  expect_match(table[1], "snapshot.*nodes.*planned.*stated.*proved.*done.*done \\(share\\).*ready.*waiting")
  expect_match(table[3], "^\\| start \\| 4 \\| 1 \\| 2 \\| 0 \\| 1 \\| 25(\\.0)?% \\| 1 \\| 2 \\|$")
  expect_match(table[5], "^\\| end \\| 5 \\| 1 \\| 0 \\| 0 \\| 4 \\| 80(\\.0)?% \\| 1 \\| 0 \\|$")
  expect_error(status_composition(list()), "must come from read_blueprint_snapshots")
})

test_that("the snapshots the repository keeps are a series that ends with everything done", {
  root <- fixture_root()
  skip_if(is.null(root), "the repository fixtures are not at hand")
  paths <- file.path(root, "blueprint", "snapshots", c("1-skeleton.json", "2-half-done.json", "3-one-proof-left.json", "4-finished.json"))
  series <- read_blueprint_snapshots(paths)
  expect_equal(series$snapshots$label, c("skeleton", "half done", "one proof left", "finished"))
  expect_equal(series$snapshots$done, c(2, 3, 5, 8))
  expect_equal(series$snapshots$complete, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(series$snapshots$ready, c(2, 2, 1, 0))
  changes <- status_changes(series)
  expect_true(all(changes$forward[changes$label != "half done" | changes$id != "odd-triangles"]))
})
