# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Tables from review reports.

#' Count the findings of reviews
#'
#' Counts findings by any of their properties: the rule that found them,
#' their severity, the repository, where the finding comes from (`origin`: a
#' lint rule, a semantic check, a suggestion Lean confirmed, or a model), the
#' file, or the run.
#'
#' @param runs Review runs from [read_review_runs()].
#' @param by The properties to count by, in the order of the columns wanted:
#'   any of `"rule"`, `"severity"`, `"repository"`, `"origin"`, `"file"` and
#'   `"run"`.
#' @return A data frame with one row for each combination that occurs: the
#'   columns named in `by`, `findings`, and `share` (of all findings). The
#'   most frequent comes first; severities are ordered from error down, and
#'   other ties by name.
#' @export
finding_counts <- function(runs, by = c("rule", "severity")) {
  runs <- need_review_runs(runs)
  allowed <- c("rule", "severity", "repository", "origin", "file", "run")
  if (!is.character(by) || length(by) == 0 || anyDuplicated(by) > 0 || !all(by %in% allowed)) {
    stop(sprintf("`by` must name one or more of: %s", paste(allowed, collapse = ", ")), call. = FALSE)
  }
  findings <- runs$findings
  if (nrow(findings) == 0) {
    out <- findings[by]
    out$findings <- integer()
    out$share <- numeric()
    return(out)
  }
  out <- summarise_groups(findings, by, function(rows) data.frame(findings = nrow(rows)))
  out$share <- out$findings / nrow(findings)
  keys <- lapply(by, function(column) if (column == "severity") match(out$severity, finding_severities()) else out[[column]])
  out <- out[do.call(order, c(list(-out$findings), keys, list(method = "radix"))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Report table of reviews
#'
#' The findings of one or more reviews as Markdown, for a report: the outcome
#' of each review, and the findings by rule with a column for each severity.
#'
#' @param runs Review runs from [read_review_runs()].
#' @return A character vector of Markdown lines.
#' @export
review_report_table <- function(runs) {
  runs <- need_review_runs(runs)
  findings <- runs$findings
  rules <- sort(unique(findings$rule), method = "radix")
  by_rule <- data.frame(rule = rules, origin = vapply(rules, function(rule) paste(sort(unique(findings$origin[findings$rule == rule]), method = "radix"), collapse = ", "), character(1)),
                        stringsAsFactors = FALSE)
  for (level in finding_severities()) by_rule[[level]] <- vapply(rules, function(rule) sum(findings$rule == rule & findings$severity == level), integer(1))
  by_rule$total <- vapply(rules, function(rule) sum(findings$rule == rule), integer(1))
  by_rule <- by_rule[order(-by_rule$total, by_rule$rule, method = "radix"), , drop = FALSE]
  c(
    sprintf("%d review(s): %d passed, %d failed.", nrow(runs$runs), sum(runs$runs$outcome == "passed"), sum(runs$runs$outcome == "failed")), "",
    "**Reviews**", "",
    markdown_table(runs$runs[c("repository", "profile", "files", "declarations", "errors", "warnings", "notes", "suppressed", "suggestions", "outcome")],
                   labels = c(repository = "Repository", profile = "Profile", files = "Files", declarations = "Declarations", errors = "Errors",
                              warnings = "Warnings", notes = "Notes", suppressed = "Suppressed", suggestions = "Suggested edits", outcome = "Outcome")), "",
    "**Findings by rule**", "",
    markdown_table(by_rule, labels = c(rule = "Rule", origin = "From", error = "Errors", warning = "Warnings", info = "Notes", total = "Total"))
  )
}
