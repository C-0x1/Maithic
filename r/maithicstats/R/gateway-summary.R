# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Summaries of gateway usage: cost, tokens, latency, errors and daily spend.

need_usage <- function(usage) {
  required <- c("purpose", "provider", "model", "ok", "input_tokens", "output_tokens", "elapsed_ms")
  if (!is.data.frame(usage) || !all(required %in% names(usage))) stop("`usage` must come from read_usage()", call. = FALSE)
  usage$pillar <- usage_pillar(usage$purpose)
  usage
}

# The cost of each call, from a price table in currency units per million tokens.
call_costs <- function(usage, prices) {
  required <- c("provider", "model", "input_per_mtok", "output_per_mtok")
  if (!is.data.frame(prices) || !all(required %in% names(prices))) {
    stop("`prices` needs the columns provider, model, input_per_mtok and output_per_mtok", call. = FALSE)
  }
  key <- function(table) paste(table$provider, table$model, sep = "\r")
  if (anyDuplicated(key(prices))) stop("`prices` lists a model twice", call. = FALSE)
  position <- match(key(usage), key(prices))
  if (anyNA(position)) {
    missing <- usage[which(is.na(position))[1], ]
    stop(sprintf("no price for %s / %s", missing$provider, missing$model), call. = FALSE)
  }
  cached_price <- if ("cached_per_mtok" %in% names(prices)) prices$cached_per_mtok[position] else prices$input_per_mtok[position]
  cached <- if ("cached_tokens" %in% names(usage)) usage$cached_tokens else 0
  ((usage$input_tokens - cached) * prices$input_per_mtok[position] + cached * cached_price +
     usage$output_tokens * prices$output_per_mtok[position]) / 1e6
}

#' Cost of model calls
#'
#' Applies a price table to the usage records and sums by group. Maithic
#' ships no prices: they change often and differ by contract, so the table
#' is yours to supply, and a call to a model without a price is an error
#' rather than a silent zero.
#'
#' @param usage Usage records from [read_usage()].
#' @param prices A data frame with `provider`, `model`, `input_per_mtok` and
#'   `output_per_mtok`, in one currency per million tokens; optionally
#'   `cached_per_mtok` for input tokens read from a cache, which otherwise
#'   cost as much as other input tokens.
#' @param by Columns to group by: any of `provider`, `model`, `pillar`,
#'   `purpose`, `served_by` and `run`.
#' @return A data frame with the group columns, `calls`, `input_tokens`,
#'   `output_tokens` and `cost`.
#' @export
usage_cost <- function(usage, prices, by = c("provider", "model")) {
  usage <- need_usage(usage)
  usage$cost <- call_costs(usage, prices)
  if (nrow(usage) == 0) {
    return(cbind(usage[by], data.frame(calls = integer(), input_tokens = numeric(), output_tokens = numeric(), cost = numeric())))
  }
  summarise_groups(usage, by, function(rows) {
    data.frame(calls = nrow(rows), input_tokens = sum(rows$input_tokens), output_tokens = sum(rows$output_tokens), cost = sum(rows$cost))
  })
}

#' Token breakdown
#'
#' Tokens by kind and group: input, output, the input tokens served from a
#' provider's cache, and the output tokens spent on reasoning, with the
#' share of output in the total.
#'
#' @param usage Usage records from [read_usage()].
#' @param by Columns to group by, as in [usage_cost()].
#' @return A data frame with the group columns, `calls`, `input_tokens`,
#'   `output_tokens`, `cached_tokens`, `reasoning_tokens`, `total_tokens`
#'   and `output_share` (`NA` when no token was counted).
#' @export
token_breakdown <- function(usage, by = c("provider", "model")) {
  usage <- need_usage(usage)
  for (column in c("cached_tokens", "reasoning_tokens")) if (!column %in% names(usage)) usage[[column]] <- 0
  if (nrow(usage) == 0) stop("there are no usage records", call. = FALSE)
  summarise_groups(usage, by, function(rows) {
    total <- sum(rows$input_tokens) + sum(rows$output_tokens)
    data.frame(calls = nrow(rows), input_tokens = sum(rows$input_tokens), output_tokens = sum(rows$output_tokens),
               cached_tokens = sum(rows$cached_tokens), reasoning_tokens = sum(rows$reasoning_tokens), total_tokens = total,
               output_share = if (total > 0) sum(rows$output_tokens) / total else NA_real_)
  })
}

#' Latency of model calls
#'
#' The distribution of call durations by group, successful calls only unless
#' `include_failed` is set: a call that failed fast would otherwise make a
#' model look quick.
#'
#' @param usage Usage records from [read_usage()].
#' @param by Columns to group by, as in [usage_cost()].
#' @param probs The upper quantiles to report.
#' @param include_failed Whether to include failed calls.
#' @return A data frame with the group columns and the columns of
#'   [distribution_summary()], in milliseconds.
#' @export
call_latency <- function(usage, by = "model", probs = c(0.9, 0.99), include_failed = FALSE) {
  usage <- need_usage(usage)
  if (!include_failed) usage <- usage[usage$ok, , drop = FALSE]
  if (nrow(usage) == 0) stop("there are no calls to summarise", call. = FALSE)
  summarise_groups(usage, by, function(rows) distribution_summary(rows$elapsed_ms, probs))
}

#' Error rates of providers
#'
#' For each provider the share of calls that failed, with a Wilson interval,
#' followed by one row per kind of failure. The kinds are the gateway's:
#' `rate_limited`, `overloaded`, `timeout`, `authentication`, and so on.
#'
#' @param usage Usage records from [read_usage()].
#' @param conf Confidence level of the intervals.
#' @return A data frame with `provider`, `error_class` (`"(any)"` on the row
#'   for all failures), `calls`, `failed`, `rate`, `lower` and `upper`.
#' @export
provider_errors <- function(usage, conf = 0.95) {
  usage <- need_usage(usage)
  if (nrow(usage) == 0) stop("there are no usage records", call. = FALSE)
  classes <- ifelse(usage$ok, NA_character_, ifelse(is.na(usage$error_class), "unclassified", usage$error_class))
  rows <- list()
  for (provider in sort(unique(usage$provider), method = "radix")) {
    here <- usage$provider == provider
    calls <- sum(here)
    counts <- c("(any)" = sum(here & !usage$ok), count_values(classes[here]))
    interval <- wilson_interval(as.numeric(counts), calls, conf)
    rows[[length(rows) + 1]] <- data.frame(provider = provider, error_class = names(counts), calls = calls, failed = as.integer(counts),
                                            rate = interval$estimate, lower = interval$lower, upper = interval$upper, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Spend per day
#'
#' Calls and cost per UTC day, every day from the first call to the last
#' (days without calls count as zero), with the mean of the trailing
#' `window` days.
#'
#' @param usage Usage records from [read_usage()], with start times.
#' @param prices A price table as [usage_cost()] takes it.
#' @param window The number of days of the rolling mean.
#' @return A data frame with `date`, `calls`, `cost` and `rolling_mean`; the
#'   rolling mean of the first days covers the days there are.
#' @export
daily_spend <- function(usage, prices, window = 7) {
  usage <- need_usage(usage)
  if (length(window) != 1 || window < 1 || window != round(window)) stop("`window` must be a positive whole number", call. = FALSE)
  if (nrow(usage) == 0) stop("there are no usage records", call. = FALSE)
  if (!"started_at" %in% names(usage) || anyNA(usage$started_at)) stop("every usage record needs a start time", call. = FALSE)
  usage$cost <- call_costs(usage, prices)
  day <- as.Date(usage$started_at, tz = "UTC")
  days <- seq(min(day), max(day), by = "day")
  position <- match(day, days)
  calls <- tabulate(position, nbins = length(days))
  cost <- vapply(seq_along(days), function(i) sum(usage$cost[position == i]), numeric(1))
  rolling <- vapply(seq_along(days), function(i) mean(cost[max(1, i - window + 1):i]), numeric(1))
  data.frame(date = days, calls = calls, cost = cost, rolling_mean = rolling)
}

#' Report table of gateway usage
#'
#' The usage of the gateway as Markdown, for a report: tokens by provider
#' and model, latency, and error rates; with prices, cost as well. The first
#' line states who served the calls, because replayed calls cost nothing and
#' say nothing about a provider's speed.
#'
#' @param usage Usage records from [read_usage()].
#' @param prices An optional price table as [usage_cost()] takes it.
#' @return A character vector of Markdown lines.
#' @export
usage_report_table <- function(usage, prices = NULL) {
  usage <- need_usage(usage)
  if (nrow(usage) == 0) return("No model call was made.")
  served <- count_values(usage$served_by)
  tokens <- token_breakdown(usage)[c("provider", "model", "calls", "input_tokens", "output_tokens", "total_tokens")]
  if (!is.null(prices)) tokens$cost <- usage_cost(usage, prices)$cost
  errors <- provider_errors(usage)
  out <- c(
    sprintf("Served by: %s.", paste(sprintf("%s (%d)", names(served), as.integer(served)), collapse = ", ")), "",
    "**Tokens**", "",
    markdown_table(tokens, labels = c(provider = "Provider", model = "Model", calls = "Calls", input_tokens = "Input",
                                      output_tokens = "Output", total_tokens = "Total", cost = "Cost")), "",
    "**Errors**", "",
    markdown_table(errors[errors$error_class == "(any)", c("provider", "calls", "failed", "rate", "lower", "upper")],
                   percent = c("rate", "lower", "upper"),
                   labels = c(provider = "Provider", calls = "Calls", failed = "Failed", rate = "Rate", lower = "Lower", upper = "Upper"))
  )
  if (any(usage$ok)) {
    out <- c(out, "", "**Latency of successful calls (ms)**", "",
             markdown_table(call_latency(usage), digits = 1, labels = c(model = "Model", n = "Calls", median = "Median", mean = "Mean", max = "Max")))
  }
  out
}
