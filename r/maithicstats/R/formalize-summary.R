# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# How often a drafted statement compiles, and what the checks say about the ones that do.

#' Share of candidate statements that elaborate
#'
#' A statement a model drafts either elaborates as it is, elaborates after
#' the model repaired it with Lean's error before it, or never does. This
#' counts the candidates of formalization runs and gives the share that
#' elaborates before any repair and the share after, each with its Wilson
#' interval. The difference between the two is what the repair loop is
#' worth.
#'
#' @param runs Formalization runs from [read_formalize_runs()].
#' @param by The properties to group by: any of `"label"`, `"run"` and
#'   `"origin"` (who wrote the statement: `draft` or `given`); none for all
#'   candidates together.
#' @param conf Confidence level of the intervals.
#' @return A data frame with the columns named in `by`, then `candidates`,
#'   `first_try` and `after_repair` (how many elaborate before and after
#'   repair), `repaired` (the difference), `rate_before`, `lower_before`,
#'   `upper_before`, `rate_after`, `lower_after` and `upper_after`.
#' @export
compile_rate <- function(runs, by = character(), conf = 0.95) {
  runs <- need_formalize_runs(runs)
  allowed <- c("label", "run", "origin")
  if (!is.character(by) || anyDuplicated(by) > 0 || !all(by %in% allowed)) {
    stop(sprintf("`by` must name some of: %s", paste(allowed, collapse = ", ")), call. = FALSE)
  }
  candidates <- runs$candidates
  if (nrow(candidates) == 0) {
    out <- candidates[by]
    for (column in c("candidates", "first_try", "after_repair", "repaired")) out[[column]] <- integer()
    for (column in c("rate_before", "lower_before", "upper_before", "rate_after", "lower_after", "upper_after")) out[[column]] <- numeric()
    return(out)
  }
  summarise_groups(candidates, by, function(rows) {
    before <- wilson_interval(sum(rows$first_try), nrow(rows), conf)
    after <- wilson_interval(sum(rows$elaborated), nrow(rows), conf)
    data.frame(candidates = nrow(rows), first_try = sum(rows$first_try), after_repair = sum(rows$elaborated),
               repaired = sum(rows$elaborated & !rows$first_try), rate_before = before$estimate, lower_before = before$lower, upper_before = before$upper,
               rate_after = after$estimate, lower_after = after$lower, upper_after = after$upper)
  })
}

#' What the checks found
#'
#' The checks made on the statements of formalization runs, counted by
#' outcome: how often each check passed, failed (which rejects a statement),
#' flagged something, or could not be made.
#'
#' @param runs Formalization runs from [read_formalize_runs()].
#' @return A data frame with one row for each check that was made, in the
#'   order the checks are made: `check`, `made`, `passed`, `failed`,
#'   `flagged` and `skipped`.
#' @export
check_summary <- function(runs) {
  runs <- need_formalize_runs(runs)
  checks <- runs$checks
  made <- intersect(faith_checks(), checks$check)
  count <- function(outcome) vapply(made, function(name) sum(checks$check == name & checks$outcome == outcome), integer(1), USE.NAMES = FALSE)
  data.frame(check = made, made = vapply(made, function(name) sum(checks$check == name), integer(1), USE.NAMES = FALSE), passed = count("passed"),
             failed = count("failed"), flagged = count("flagged"), skipped = count("skipped"), stringsAsFactors = FALSE)
}

#' Report table of formalization runs
#'
#' The outcome of one or more formalization runs as Markdown, for a report:
#' what became of the candidates of each problem, how many elaborate before
#' and after repair, and what the checks found.
#'
#' @param runs Formalization runs from [read_formalize_runs()].
#' @param conf Confidence level of the intervals.
#' @return A character vector of Markdown lines.
#' @export
formalize_report_table <- function(runs, conf = 0.95) {
  runs <- need_formalize_runs(runs)
  table <- runs$runs
  per_run <- data.frame(run = table$label, drafted = table$drafted, first_try = table$elaborated_first_try, repaired = table$repaired, failed = table$failed,
                        duplicates = table$duplicates, rejected = table$rejected, kept = table$kept,
                        outcome = ifelse(table$status == "formalized", "a statement", ifelse(table$status == "flagged", "a statement, flagged", "no statement")),
                        stringsAsFactors = FALSE)
  lines <- c(
    sprintf("%d formalization run(s): %d candidate statement(s), %d run(s) ended with a statement that no check flagged.", nrow(table), sum(table$drafted),
            sum(table$status == "formalized")), "",
    "**Runs**", "",
    markdown_table(per_run, labels = c(run = "Run", drafted = "Drafted", first_try = "Elaborate as drafted", repaired = "After repair", failed = "Never",
                                       duplicates = "Duplicates", rejected = "Rejected by Lean", kept = "Left", outcome = "At the end"))
  )
  if (nrow(runs$candidates) > 0) {
    rate <- compile_rate(runs, conf = conf)
    rates <- data.frame(when = c("as drafted", "after repair"), elaborate = c(rate$first_try, rate$after_repair), of = rate$candidates,
                        rate = c(rate$rate_before, rate$rate_after),
                        interval = sprintf("%.0f%% to %.0f%%", 100 * c(rate$lower_before, rate$lower_after), 100 * c(rate$upper_before, rate$upper_after)),
                        stringsAsFactors = FALSE)
    lines <- c(lines, "", "**Candidates that elaborate**", "",
               markdown_table(rates, percent = "rate",
                              labels = c(when = "When", elaborate = "Elaborate", of = "Of", rate = "Share", interval = sprintf("%g%% interval", 100 * conf))))
  }
  checks <- check_summary(runs)
  if (nrow(checks) > 0) {
    lines <- c(lines, "", "**What the checks found**", "",
               markdown_table(checks, labels = c(check = "Check", made = "Made", passed = "Passed", failed = "Rejected", flagged = "Flagged", skipped = "Not made")))
  }
  lines
}
