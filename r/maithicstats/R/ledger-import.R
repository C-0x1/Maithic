# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading what the ledger knows about results: check reports and prove runs.

#' Read check reports
#'
#' Reads reports written by `maithic check FILE --json`: for every theorem
#' of a Lean file its status under the trust policy, the axioms its proof
#' rests on, and the reasons when it is not verified.
#'
#' @param paths Paths of JSON files with schema `maithic.check.report/1`.
#' @return A list of class `maithic_check_reports` with two data frames:
#'   `reports`, one row per file (`report`, `path`, `file`, `ok`, `errors`,
#'   `warnings`, `declarations`, `theorems`), and `theorems`, one row per
#'   theorem (`report`, `file`, `name`, `status`, and the list columns
#'   `axioms` and `reasons`).
#' @export
read_check_reports <- function(paths) {
  if (length(paths) == 0) stop("no report files were given", call. = FALSE)
  statuses <- result_statuses()
  axioms <- list()
  reasons <- list()
  reports <- list()
  theorems <- list()
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    report <- read_document(path, "maithic.check.report/1", "a check report")
    file <- text_field(report, "file")
    for (theorem in report$theorems) {
      name <- text_field(theorem, "name")
      status <- text_field(theorem, "status")
      if (!nzchar(name)) stop(sprintf("%s: a theorem has no name", path), call. = FALSE)
      if (!status %in% statuses) stop(sprintf("%s: theorem %s has unknown status '%s'", path, name, status), call. = FALSE)
      theorems[[length(theorems) + 1]] <- data.frame(report = i, file = file, name = name, status = status, stringsAsFactors = FALSE)
      axioms[[length(axioms) + 1]] <- texts_field(theorem, "axioms")
      reasons[[length(reasons) + 1]] <- texts_field(theorem, "reasons")
    }
    reports[[i]] <- data.frame(report = i, path = path, file = file, ok = flag_field(report, "ok", NA),
                               errors = count_field(report, "errors"), warnings = count_field(report, "warnings"),
                               declarations = count_field(report, "declarations"), theorems = length(report$theorems),
                               stringsAsFactors = FALSE)
  }
  table <- bind_frames(theorems, data.frame(report = integer(), file = character(), name = character(), status = character(), stringsAsFactors = FALSE))
  table$axioms <- axioms
  table$reasons <- reasons
  structure(list(reports = do.call(rbind, reports), theorems = table), class = "maithic_check_reports")
}

#' Ledger records
#'
#' Puts check reports and prove runs into one table of results, the form
#' every ledger summary takes. A result from a check report has no re-check
#' of its own; a result from a prove run has one whenever the trust policy
#' asked for it.
#'
#' @param checks Check reports from [read_check_reports()], or `NULL`.
#' @param runs Prove runs from [read_prove_runs()], or `NULL`.
#' @return A data frame with one row per result: `source` (`"check"` or
#'   `"prove"`), `file`, `name`, `status`, `recheck_passed` and `recheck_ms`
#'   (`NA` without a re-check), and the list columns `axioms`,
#'   `recheck_axioms` and `problems` (the reasons or policy violations).
#' @export
ledger_records <- function(checks = NULL, runs = NULL) {
  if (is.null(checks) && is.null(runs)) stop("give check reports, prove runs or both", call. = FALSE)
  frames <- list()
  axioms <- list()
  recheck_axioms <- list()
  problems <- list()
  if (!is.null(checks)) {
    if (!inherits(checks, "maithic_check_reports")) stop("`checks` must come from read_check_reports()", call. = FALSE)
    theorems <- checks$theorems
    frames[[1]] <- data.frame(source = rep("check", nrow(theorems)), file = theorems$file, name = theorems$name, status = theorems$status,
                              recheck_passed = rep(NA, nrow(theorems)), recheck_ms = rep(NA_real_, nrow(theorems)), stringsAsFactors = FALSE)
    axioms <- c(axioms, theorems$axioms)
    recheck_axioms <- c(recheck_axioms, rep(list(character()), nrow(theorems)))
    problems <- c(problems, theorems$reasons)
  }
  if (!is.null(runs)) {
    results <- need_runs(runs)$results
    frames[[length(frames) + 1]] <- data.frame(source = rep("prove", nrow(results)), file = results$file, name = results$theorem,
                                               status = results$status, recheck_passed = results$recheck_passed,
                                               recheck_ms = results$recheck_ms, stringsAsFactors = FALSE)
    axioms <- c(axioms, results$axioms)
    recheck_axioms <- c(recheck_axioms, results$recheck_axioms)
    problems <- c(problems, results$violations)
  }
  out <- do.call(rbind, frames)
  out$axioms <- axioms
  out$recheck_axioms <- recheck_axioms
  out$problems <- problems
  rownames(out) <- NULL
  out
}
