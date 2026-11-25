# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Summaries of the ledger: trust levels, axioms, re-checks and policy decisions.

need_records <- function(records) {
  required <- c("source", "name", "status", "recheck_passed", "recheck_ms", "axioms", "recheck_axioms", "problems")
  if (!is.data.frame(records) || !all(required %in% names(records))) stop("`records` must come from ledger_records()", call. = FALSE)
  if (nrow(records) == 0) stop("there are no results", call. = FALSE)
  records
}

#' Distribution of trust levels
#'
#' The share of results at each status, with Wilson intervals. The statuses
#' appear in their fixed order from `verified` down, and only those that
#' occur.
#'
#' @param records Ledger records from [ledger_records()].
#' @param conf Confidence level of the intervals.
#' @return A data frame with `status`, `count`, `share`, `lower` and `upper`.
#' @export
trust_distribution <- function(records, conf = 0.95) {
  records <- need_records(records)
  counts <- table(factor(records$status, levels = result_statuses()))
  counts <- counts[counts > 0]
  interval <- wilson_interval(as.numeric(counts), nrow(records), conf)
  data.frame(status = names(counts), count = as.integer(counts), share = interval$estimate, lower = interval$lower,
             upper = interval$upper, stringsAsFactors = FALSE)
}

#' The class of an axiom
#'
#' Sorts axiom names the way the Lean audit does: the three `standard`
#' axioms of Lean's logic, `incomplete` for `sorryAx`, `native` for axioms
#' that trust compiled evaluation, and `other`.
#'
#' @param axiom A character vector of axiom names.
#' @return A character vector of classes.
#' @export
axiom_class <- function(axiom) {
  axiom <- as.character(axiom)
  ifelse(axiom %in% c("propext", "Quot.sound", "Classical.choice"), "standard",
         ifelse(axiom == "sorryAx", "incomplete",
                ifelse(grepl("(^|\\.)_native(\\.|$)|^Lean\\.(ofReduceBool|trustCompiler)$", axiom), "native", "other")))
}

#' Census of axioms
#'
#' Which axioms the results rest on and how many results rest on each, with
#' a row `(none)` for the results that use no axiom at all.
#'
#' @param records Ledger records from [ledger_records()].
#' @return A data frame with `axiom`, `class`, `results` and `share`, by
#'   falling number of results; ties in the order of the names.
#' @export
axiom_census <- function(records) {
  records <- need_records(records)
  used <- unlist(lapply(records$axioms, unique))
  none <- sum(vapply(records$axioms, length, integer(1)) == 0)
  counts <- count_values(used)
  out <- data.frame(axiom = names(counts), class = axiom_class(names(counts)), results = as.integer(counts), stringsAsFactors = FALSE)
  if (none > 0) out <- rbind(out, data.frame(axiom = "(none)", class = "none", results = none, stringsAsFactors = FALSE))
  out <- out[order(-out$results, out$axiom, method = "radix"), , drop = FALSE]
  out$share <- out$results / nrow(records)
  rownames(out) <- NULL
  out
}

#' Agreement of the re-check
#'
#' Among the results that were re-checked in a fresh process: how many
#' passed, how many reported the same axioms as the first check, and the
#' share that agreed on both counts, with a Wilson interval. Anything short
#' of full agreement deserves a look: it means two runs of the checker saw
#' different things.
#'
#' @param records Ledger records from [ledger_records()].
#' @param conf Confidence level of the interval.
#' @return A one-row data frame with `rechecked`, `passed`, `same_axioms`,
#'   `agreed`, `rate`, `lower` and `upper`; the last three are `NA` when
#'   nothing was re-checked.
#' @export
recheck_agreement <- function(records, conf = 0.95) {
  records <- need_records(records)
  rechecked <- !is.na(records$recheck_passed)
  n <- sum(rechecked)
  passed <- rechecked & records$recheck_passed
  same <- rechecked & mapply(function(a, b) setequal(a, b), records$axioms, records$recheck_axioms)
  agreed <- sum(passed & same)
  interval <- if (n > 0) wilson_interval(agreed, n, conf) else data.frame(estimate = NA_real_, lower = NA_real_, upper = NA_real_)
  data.frame(rechecked = n, passed = sum(passed), same_axioms = sum(same), agreed = agreed, rate = interval$estimate,
             lower = interval$lower, upper = interval$upper)
}

#' Time of the re-check
#'
#' The distribution of the time one fresh re-check takes, over the results
#' that have one.
#'
#' @param records Ledger records from [ledger_records()].
#' @param probs The upper quantiles to report.
#' @return The one-row data frame of [distribution_summary()], in
#'   milliseconds.
#' @export
recheck_time <- function(records, probs = c(0.9, 0.99)) {
  distribution_summary(need_records(records)$recheck_ms, probs)
}

#' Policy decisions
#'
#' How the trust policy decided: `passed` for verified results, `failed` for
#' results the policy or the kernel rejected, `waived` for results accepted
#' under a recorded waiver, and `not decided` for the rest, such as results
#' that were only checked or never finished.
#'
#' @param records Ledger records from [ledger_records()]; a logical column
#'   `waived`, if present, marks waived results.
#' @return A data frame with `decision`, `count` and `share`, all four
#'   decisions in a fixed order.
#' @export
policy_decisions <- function(records) {
  records <- need_records(records)
  waived <- if ("waived" %in% names(records)) records$waived %in% TRUE else rep(FALSE, nrow(records))
  rejected <- records$status == "failed" | vapply(records$problems, length, integer(1)) > 0
  decision <- ifelse(waived, "waived", ifelse(records$status == "verified", "passed", ifelse(rejected, "failed", "not decided")))
  levels <- c("passed", "failed", "waived", "not decided")
  counts <- table(factor(decision, levels = levels))
  data.frame(decision = levels, count = as.integer(counts), share = as.numeric(counts) / nrow(records), stringsAsFactors = FALSE)
}

#' Report table of the ledger
#'
#' The state of the ledger as Markdown, for a report: trust levels, the
#' axiom census, and the agreement and time of the re-check.
#'
#' @param records Ledger records from [ledger_records()].
#' @param conf Confidence level of the intervals.
#' @return A character vector of Markdown lines.
#' @export
ledger_report_table <- function(records, conf = 0.95) {
  records <- need_records(records)
  sources <- count_values(records$source)
  agreement <- recheck_agreement(records, conf)
  out <- c(
    sprintf("%d result(s): %s.", nrow(records), paste(sprintf("%d from %s", as.integer(sources), names(sources)), collapse = ", ")), "",
    "**Trust levels**", "",
    markdown_table(trust_distribution(records, conf), percent = c("share", "lower", "upper"),
                   labels = c(status = "Status", count = "Results", share = "Share", lower = "Lower", upper = "Upper")), "",
    "**Axioms**", "",
    markdown_table(axiom_census(records), percent = "share", labels = c(axiom = "Axiom", class = "Class", results = "Results", share = "Share"))
  )
  if (agreement$rechecked > 0) {
    out <- c(out, "", "**Re-check**", "",
             markdown_table(agreement, percent = c("rate", "lower", "upper"),
                            labels = c(rechecked = "Re-checked", passed = "Passed", same_axioms = "Same axioms", agreed = "Agreed",
                                       rate = "Rate", lower = "Lower", upper = "Upper")), "",
             markdown_table(recheck_time(records), digits = 1, labels = c(n = "Re-checks", median = "Median (ms)", mean = "Mean (ms)", max = "Max (ms)")))
  }
  out
}
