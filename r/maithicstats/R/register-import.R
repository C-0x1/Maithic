# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# The summary of the capability register: how many capabilities each pillar
# has, how many of them are done, and in which language they are written.

#' The languages of the capability register
#'
#' @return The four languages a capability can be counted under, in the
#'   order in which the register reports them.
#' @export
register_languages <- function() c("OCaml", "Lean", "Julia", "R")

#' Read the summary of the capability register
#'
#' Reads `registry/status.json`, which `make cap-sync` writes from the
#' register and the test suites, and holds it to what it states about
#' itself: the pillars add up to the totals, and the capabilities of each
#' pillar by language add up to the capabilities of that pillar. A summary
#' that does not add up is an error, not a slightly wrong figure.
#'
#' @param path Path of the summary.
#' @return A list of class `maithic_register` with `total` and `done`, the
#'   table `pillars` (`id`, `name`, `capabilities`, `done`) and the table
#'   `languages` with one row for each pillar and language (`id`, `name`,
#'   `language`, `capabilities`).
#' @export
read_register_status <- function(path) {
  if (!is.character(path) || length(path) != 1) stop("`path` must be one file", call. = FALSE)
  if (!file.exists(path)) stop(sprintf("file not found: %s", path), call. = FALSE)
  status <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                     error = function(e) stop(sprintf("%s is not JSON: %s", path, conditionMessage(e)), call. = FALSE))
  if (!is.list(status$pillars) || length(status$pillars) == 0) stop(sprintf("%s names no pillars: it is not a summary of the register", path), call. = FALSE)
  pillars <- data.frame(
    id = vapply(status$pillars, text_field, character(1), "id"), name = vapply(status$pillars, text_field, character(1), "name"),
    done = vapply(status$pillars, count_field, integer(1), "done"),
    capabilities = vapply(status$pillars, function(p) count_field(p, "done") + count_field(p, "in_progress") + count_field(p, "planned"), integer(1)),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(pillars$id) > 0) stop(sprintf("%s: the pillar %s occurs twice", path, pillars$id[anyDuplicated(pillars$id)]), call. = FALSE)
  if (sum(pillars$capabilities) != count_field(status, "total") || sum(pillars$done) != count_field(status, "done")) {
    stop(sprintf("%s: the pillars hold %d capabilities, %d of them done, and the totals say %d and %d", path, sum(pillars$capabilities),
                 sum(pillars$done), count_field(status, "total"), count_field(status, "done")), call. = FALSE)
  }
  languages <- do.call(rbind, lapply(status$pillars, function(p) {
    unknown <- setdiff(names(p$languages), register_languages())
    if (length(unknown) > 0) stop(sprintf("%s: pillar %s counts capabilities in the unknown language '%s'", path, text_field(p, "id"), unknown[1]), call. = FALSE)
    data.frame(id = text_field(p, "id"), name = text_field(p, "name"), language = register_languages(),
               capabilities = vapply(register_languages(), function(l) count_field(p$languages, l), integer(1)), stringsAsFactors = FALSE)
  }))
  rownames(languages) <- NULL
  counted <- tapply(languages$capabilities, factor(languages$id, levels = pillars$id), sum)
  wrong <- which(as.integer(counted) != pillars$capabilities)
  if (length(wrong) > 0) {
    stop(sprintf("%s: pillar %s has %d capabilities and its languages add up to %d", path, pillars$id[wrong[1]],
                 pillars$capabilities[wrong[1]], as.integer(counted)[wrong[1]]), call. = FALSE)
  }
  structure(list(total = sum(pillars$capabilities), done = sum(pillars$done), pillars = pillars, languages = languages), class = "maithic_register")
}

need_register <- function(status) {
  if (!inherits(status, "maithic_register")) stop("`status` must come from read_register_status()", call. = FALSE)
  status
}

#' Capabilities by language
#'
#' @param status A summary from [read_register_status()].
#' @return A data frame with one row for each language: `language`,
#'   `capabilities`, `pillars` (in how many pillars the language has at least
#'   one capability) and `share` (its part of all capabilities).
#' @export
register_by_language <- function(status) {
  status <- need_register(status)
  table <- status$languages
  data.frame(
    language = register_languages(),
    capabilities = vapply(register_languages(), function(l) sum(table$capabilities[table$language == l]), integer(1)),
    pillars = vapply(register_languages(), function(l) sum(table$capabilities[table$language == l] > 0), integer(1)),
    share = vapply(register_languages(), function(l) sum(table$capabilities[table$language == l]) / status$total, numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}
