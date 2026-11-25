# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

#' Success rate by group, with intervals
#'
#' @param problems The `problems` data frame of [read_bench_run()].
#' @param by One of `"tag"`, `"suite"`, `"difficulty"` or `"proposer"`. A
#'   problem with several tags counts once under each of them.
#' @param conf Confidence level.
#' @return A data frame with `group`, `n`, `solved`, `rate`, `lower`, `upper`.
#' @export
success_by <- function(problems, by = c("tag", "suite", "difficulty", "proposer"), conf = 0.95) {
  by <- match.arg(by)
  if (by == "tag") {
    counts <- vapply(problems$tags, length, integer(1))
    groups <- unlist(problems$tags)
    solved <- rep(problems$solved, counts)
  } else {
    groups <- as.character(problems[[by]])
    solved <- problems$solved
  }
  if (length(groups) == 0) return(data.frame(group = character(), n = integer(), solved = integer(), rate = numeric(), lower = numeric(), upper = numeric()))
  levels <- sort(unique(groups))
  rows <- lapply(levels, function(level) {
    hit <- solved[groups == level]
    interval <- wilson_interval(sum(hit), length(hit), conf)
    data.frame(group = level, n = length(hit), solved = sum(hit), rate = interval$estimate, lower = interval$lower, upper = interval$upper)
  })
  do.call(rbind, rows)
}

#' The standard analysis of a benchmark run
#'
#' One call that produces everything a report about a run needs: the overall
#' success rate with its interval, the same by tag, difficulty and proposer,
#' and a summary of the time per problem with a bootstrap interval for the
#' median.
#'
#' @param path Path of a result file, or the list returned by
#'   [read_bench_run()].
#' @param conf Confidence level.
#' @return A list with `meta`, `overall`, `by_tag`, `by_difficulty`,
#'   `by_proposer` and `time`.
#' @export
analyze_run <- function(path, conf = 0.95) {
  run <- if (is.character(path)) read_bench_run(path) else path
  problems <- run$problems
  times <- problems$elapsed_ms[is.finite(problems$elapsed_ms)]
  time <- if (length(times) == 0) {
    NULL
  } else {
    interval <- bootstrap_interval(times, stats::median, conf = conf)
    list(
      n = length(times), total_ms = sum(times), mean_ms = mean(times), median_ms = stats::median(times),
      median_lower = interval$lower, median_upper = interval$upper, max_ms = max(times)
    )
  }
  list(
    meta = run$meta,
    overall = wilson_interval(sum(problems$solved), nrow(problems), conf),
    by_tag = success_by(problems, "tag", conf),
    by_difficulty = success_by(problems, "difficulty", conf),
    by_proposer = success_by(problems[problems$solved, , drop = FALSE], "proposer", conf),
    time = time
  )
}
