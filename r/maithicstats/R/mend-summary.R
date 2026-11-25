# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# How much of what broke was repaired.

#' Share of breakages repaired
#'
#' The breakages of repair runs, counted by any of their properties, with
#' the share that was repaired and its Wilson interval. By class, the table
#' says which kinds of breakage the automation handles: names and
#' suggestions are repaired by rules nearly always, proofs that no longer
#' close their goal more rarely.
#'
#' @param runs Repair runs from [read_mend_runs()].
#' @param by The properties to group by: any of `"class"`, `"label"`,
#'   `"severity"`, `"file"` and `"run"`; none for all breakages together.
#' @param conf Confidence level of the intervals.
#' @return A data frame with the columns named in `by`, then `breakages`,
#'   `repaired`, `by_rule`, `by_model`, `by_prover`, `rate` (the share
#'   repaired), `lower` and `upper`. Classes come in the order the command
#'   lists them, other groups in the order of their names.
#' @export
repair_success <- function(runs, by = "class", conf = 0.95) {
  runs <- need_mend_runs(runs)
  allowed <- c("class", "label", "severity", "file", "run")
  if (!is.character(by) || anyDuplicated(by) > 0 || !all(by %in% allowed)) {
    stop(sprintf("`by` must name some of: %s", paste(allowed, collapse = ", ")), call. = FALSE)
  }
  breakages <- runs$breakages
  if (nrow(breakages) == 0) {
    out <- breakages[by]
    for (column in c("breakages", "repaired", "by_rule", "by_model", "by_prover")) out[[column]] <- integer()
    for (column in c("rate", "lower", "upper")) out[[column]] <- numeric()
    return(out)
  }
  out <- summarise_groups(breakages, by, function(rows) {
    interval <- wilson_interval(sum(rows$repaired), nrow(rows), conf)
    data.frame(breakages = nrow(rows), repaired = sum(rows$repaired), by_rule = sum(rows$stage == "rule"), by_model = sum(rows$stage == "model"),
               by_prover = sum(rows$stage == "prove"), rate = interval$estimate, lower = interval$lower, upper = interval$upper)
  })
  if ("class" %in% by) {
    keys <- lapply(by, function(column) if (column == "class") match(out$class, breakage_classes()) else out[[column]])
    out <- out[do.call(order, c(keys, list(method = "radix"))), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' Report table of repair runs
#'
#' The outcome of one or more repair runs as Markdown, for a report: what
#' each run repaired and at which stage, and the share repaired by class of
#' breakage.
#'
#' @param runs Repair runs from [read_mend_runs()].
#' @param conf Confidence level of the intervals.
#' @return A character vector of Markdown lines.
#' @export
mend_report_table <- function(runs, conf = 0.95) {
  runs <- need_mend_runs(runs)
  table <- runs$runs
  per_run <- data.frame(run = table$label, file = table$file, breakages = table$breakages, by_rule = table$by_rule, by_model = table$by_model,
                        by_prover = table$by_prover, left = table$unrepaired, outcome = ifelse(table$clean, "compiles", "still broken"),
                        stringsAsFactors = FALSE)
  by_class <- repair_success(runs, "class", conf)
  classes <- data.frame(class = by_class$class, breakages = by_class$breakages, repaired = by_class$repaired, rate = by_class$rate,
                        interval = sprintf("%.0f%% to %.0f%%", 100 * by_class$lower, 100 * by_class$upper), by_rule = by_class$by_rule,
                        by_model = by_class$by_model, by_prover = by_class$by_prover, stringsAsFactors = FALSE)
  c(
    sprintf("%d repair run(s): %d breakage(s), %d repaired; %d run(s) ended with a file that compiles.", nrow(table), sum(table$breakages),
            sum(table$repaired), sum(table$clean)), "",
    "**Runs**", "",
    markdown_table(per_run, labels = c(run = "Run", file = "File", breakages = "Breakages", by_rule = "By a rule", by_model = "By a model",
                                       by_prover = "By the prover", left = "Left", outcome = "At the end")), "",
    "**Repaired, by class of breakage**", "",
    markdown_table(classes, percent = "rate",
                   labels = c(class = "Class", breakages = "Breakages", repaired = "Repaired", rate = "Share", interval = sprintf("%g%% interval", 100 * conf),
                              by_rule = "By a rule", by_model = "By a model", by_prover = "By the prover"))
  )
}
