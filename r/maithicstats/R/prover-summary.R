# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Summaries of prove runs: success, time, tokens, steps and failures.

need_runs <- function(runs) {
  if (!inherits(runs, "maithic_prove_runs")) stop("`runs` must come from read_prove_runs()", call. = FALSE)
  runs
}

#' Success rate of the prover
#'
#' The share of theorems proved, overall or by group, with Wilson intervals.
#' A theorem counts as proved only with status `verified`.
#'
#' @param runs Prove runs from [read_prove_runs()].
#' @param by Columns of `runs$results` to group by, such as `"file"`,
#'   `"proposer"`, `"provider"` or `"model"`; none for one overall row.
#' @param conf Confidence level of the intervals.
#' @return A data frame with the group columns, `n`, `proved`, `rate`,
#'   `lower` and `upper`.
#' @export
prover_success <- function(runs, by = character(), conf = 0.95) {
  results <- need_runs(runs)$results
  if (nrow(results) == 0) stop("the runs hold no results", call. = FALSE)
  summarise_groups(results, by, function(rows) {
    interval <- wilson_interval(sum(rows$proved), nrow(rows), conf)
    data.frame(n = nrow(rows), proved = sum(rows$proved), rate = interval$estimate, lower = interval$lower, upper = interval$upper)
  })
}

#' Time to proof
#'
#' Median and upper quantiles of three durations per theorem: `wall`, the
#' whole time of the proof attempt; `lean`, the time spent in Lean checking
#' candidates and re-checking the result; and `recheck`, the time of the
#' fresh re-check alone. Model time is known per run, not per theorem, and
#' is reported by [prover_tokens()].
#'
#' @param runs Prove runs from [read_prove_runs()].
#' @param by Columns of `runs$results` to group by.
#' @param probs The upper quantiles to report.
#' @param proved_only Whether to summarise only the theorems that were proved.
#' @return A data frame with the group columns, `metric`, and the columns of
#'   [distribution_summary()], in milliseconds.
#' @export
prover_time <- function(runs, by = character(), probs = c(0.9, 0.99), proved_only = FALSE) {
  results <- need_runs(runs)$results
  if (proved_only) results <- results[results$proved, , drop = FALSE]
  metrics <- c(wall = "elapsed_ms", lean = "lean_ms", recheck = "recheck_ms")
  pieces <- lapply(names(metrics), function(metric) {
    summarise_groups(results, by, function(rows) cbind(metric = metric, distribution_summary(rows[[metrics[[metric]]]], probs)))
  })
  out <- do.call(rbind, pieces)
  if (length(by) > 0) out <- out[plain_order(out[by]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Tokens and cost of the prover
#'
#' Per run: model calls, tokens in and out, tokens per call and per proved
#' theorem, the time spent waiting for the model, and, when prices are given,
#' the cost in total and per proved theorem.
#'
#' @param runs Prove runs from [read_prove_runs()].
#' @param prices An optional price table as [usage_cost()] takes it.
#' @return A data frame with one row per run: `run`, `file`, `provider`,
#'   `model`, `mode`, `calls`, `input_tokens`, `output_tokens`,
#'   `tokens_per_call`, `proved`, `tokens_per_proved`, `model_ms`, and with
#'   prices also `cost` and `cost_per_proved`. Ratios with a zero denominator
#'   are `NA`.
#' @export
prover_tokens <- function(runs, prices = NULL) {
  runs <- need_runs(runs)
  table <- runs$runs
  ratio <- function(a, b) ifelse(b > 0, a / b, NA_real_)
  total <- table$input_tokens + table$output_tokens
  out <- data.frame(
    run = table$run, file = table$file, provider = table$provider, model = table$model, mode = table$mode,
    calls = table$calls, input_tokens = table$input_tokens, output_tokens = table$output_tokens,
    tokens_per_call = ratio(total, table$calls), proved = table$verified, tokens_per_proved = ratio(total, table$verified),
    model_ms = table$model_ms, stringsAsFactors = FALSE
  )
  if (!is.null(prices)) {
    costs <- usage_cost(runs$usage, prices, by = "run")
    out$cost <- costs$cost[match(out$run, costs$run)]
    out$cost[is.na(out$cost) & out$calls == 0] <- 0
    out$cost_per_proved <- ratio(out$cost, out$proved)
  }
  out
}

#' Step statistics of the prover
#'
#' The distribution of four counts per theorem: the tactic steps of the
#' accepted proof (proved theorems only), the candidate proofs checked, the
#' candidates rejected before one was accepted, and the model rounds used.
#'
#' @param runs Prove runs from [read_prove_runs()].
#' @param probs The upper quantiles to report.
#' @return A data frame with `metric` and the columns of
#'   [distribution_summary()].
#' @export
prover_steps <- function(runs, probs = c(0.9, 0.99)) {
  results <- need_runs(runs)$results
  rejected <- results$attempts - as.integer(results$proved)
  samples <- list(
    proof_steps = results$proof_steps[results$proved], candidates_checked = results$attempts,
    candidates_rejected = rejected, model_rounds = results$rounds
  )
  out <- do.call(rbind, lapply(names(samples), function(metric) {
    cbind(metric = metric, distribution_summary(as.numeric(samples[[metric]]), probs))
  }))
  rownames(out) <- NULL
  out
}

#' Failure breakdown of the prover
#'
#' Two counts in one table: how the theorems ended (`kind = "status"`) and
#' why the rejected candidate proofs were rejected (`kind = "error_class"`),
#' each with its share within its kind.
#'
#' @param runs Prove runs from [read_prove_runs()].
#' @return A data frame with `kind`, `name`, `count` and `share`, the
#'   statuses in their fixed order and the error classes by falling count.
#' @export
prover_failures <- function(runs) {
  runs <- need_runs(runs)
  statuses <- table(factor(runs$results$status, levels = result_statuses()))
  statuses <- statuses[statuses > 0]
  rejected <- runs$attempts[!runs$attempts$accepted, , drop = FALSE]
  classes <- rejected$error_class
  classes[is.na(classes)] <- "unclassified"
  classes <- count_values(classes)
  part <- function(kind, counts) {
    if (length(counts) == 0) return(data.frame(kind = character(), name = character(), count = integer(), share = numeric()))
    data.frame(kind = kind, name = names(counts), count = as.integer(counts), share = as.numeric(counts) / sum(counts), stringsAsFactors = FALSE)
  }
  out <- rbind(part("status", statuses), part("error_class", classes))
  rownames(out) <- NULL
  out
}

#' Report table of the prover
#'
#' The summary of prove runs as Markdown, for a report: success by file and
#' provider with intervals, wall time, and the failure breakdown. The first
#' line states the gateway mode of the runs, because numbers from replayed
#' model answers must not be read as numbers from live models.
#'
#' @param runs Prove runs from [read_prove_runs()].
#' @param conf Confidence level of the intervals.
#' @return A character vector of Markdown lines.
#' @export
prover_report_table <- function(runs, conf = 0.95) {
  runs <- need_runs(runs)
  success <- prover_success(runs, by = c("file", "provider"), conf = conf)
  time <- prover_time(runs, by = "provider")
  time <- time[time$metric == "wall", setdiff(names(time), "metric"), drop = FALSE]
  modes <- paste(sort(unique(runs$runs$mode), method = "radix"), collapse = ", ")
  c(
    sprintf("Gateway mode: %s. %d run(s), %d theorem(s).", modes, nrow(runs$runs), nrow(runs$results)), "",
    "**Success**", "",
    markdown_table(success, percent = c("rate", "lower", "upper"),
                   labels = c(file = "File", provider = "Provider", n = "Theorems", proved = "Proved", rate = "Rate", lower = "Lower", upper = "Upper")), "",
    "**Wall time per theorem (ms)**", "",
    markdown_table(time, digits = 1, labels = c(provider = "Provider", n = "Theorems", median = "Median", mean = "Mean", max = "Max")), "",
    "**Outcomes and rejected candidates**", "",
    markdown_table(prover_failures(runs), percent = "share", labels = c(kind = "Kind", name = "Name", count = "Count", share = "Share"))
  )
}
