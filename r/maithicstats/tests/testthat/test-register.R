# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

register_path <- function() file.path(dirname(fixture_root()), "registry", "status.json")

# A summary of two pillars whose every number can be checked by hand.
small_register <- function(change = identity) {
  pillar <- function(id, name, done, planned, languages) {
    list(id = id, name = name, done = done, in_progress = 0, planned = planned, languages = as.list(languages))
  }
  status <- list(total = 7, done = 5, in_progress = 0, planned = 2,
                 pillars = list(pillar("P00", "Platform", 3, 0, c(OCaml = 3, Lean = 0, Julia = 0, R = 0)),
                                pillar("P01", "Prove", 2, 2, c(OCaml = 1, Lean = 2, Julia = 0, R = 1))))
  write_document(change(status))
}

test_that("the summary of the register is read with its pillars and their languages", {
  status <- read_register_status(small_register())
  expect_s3_class(status, "maithic_register")
  expect_equal(c(status$total, status$done), c(7, 5))
  expect_equal(status$pillars$capabilities, c(3L, 4L))
  expect_equal(nrow(status$languages), 8)
  expect_equal(status$languages$capabilities[status$languages$id == "P01"], c(1L, 2L, 0L, 1L))
  by_language <- register_by_language(status)
  expect_equal(by_language$language, register_languages())
  expect_equal(by_language$capabilities, c(4L, 2L, 0L, 1L))
  expect_equal(by_language$pillars, c(2L, 1L, 0L, 1L))
  expect_equal(sum(by_language$share), 1)
  expect_error(register_by_language(list()), "must come from read_register_status")
})

test_that("a summary that does not add up is refused", {
  expect_error(read_register_status(small_register(function(s) { s$total <- 8; s })), "the pillars hold 7 capabilities, 5 of them done, and the totals say 8 and 5")
  expect_error(read_register_status(small_register(function(s) { s$pillars[[2]]$languages$Lean <- 1; s })),
               "pillar P01 has 4 capabilities and its languages add up to 3")
  expect_error(read_register_status(small_register(function(s) { s$pillars[[1]]$languages$Fortran <- 0; s })), "unknown language 'Fortran'")
  expect_error(read_register_status(small_register(function(s) { s$pillars[[2]]$id <- "P00"; s })), "the pillar P00 occurs twice")
  expect_error(read_register_status(small_register(function(s) { s$pillars <- list(); s })), "names no pillars")
  expect_error(read_register_status(file.path(tempdir(), "no-such-summary.json")), "file not found")
  broken <- tempfile(fileext = ".json")
  writeLines("{ not json", broken)
  expect_error(read_register_status(broken), "is not JSON")
  expect_error(read_register_status(c("a", "b")), "must be one file")
})

test_that("the register of the repository adds up, with every language in use", {
  status <- read_register_status(register_path())
  expect_equal(status$done, status$total)
  expect_true(all(status$pillars$capabilities >= 20))
  by_language <- register_by_language(status)
  expect_true(all(by_language$capabilities > 0))
  expect_equal(sum(by_language$capabilities), status$total)
})
