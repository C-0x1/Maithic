# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

#' Wilson score interval for a success rate
#'
#' The interval Maithic reports for every share of problems solved. Unlike the
#' textbook interval it behaves well for small suites and for rates near 0 or
#' 1: it never leaves the unit interval and never collapses to a point.
#'
#' @param successes Number of successes; may be a vector.
#' @param n Number of trials; recycled against `successes`.
#' @param conf Confidence level.
#' @return A data frame with columns `successes`, `n`, `estimate`, `lower` and
#'   `upper`, one row per element.
#' @examples
#' wilson_interval(8, 10)
#' wilson_interval(c(0, 24), 24)
#' @export
wilson_interval <- function(successes, n, conf = 0.95) {
  if (!is.numeric(successes) || !is.numeric(n)) stop("`successes` and `n` must be numeric", call. = FALSE)
  if (length(conf) != 1 || !is.finite(conf) || conf <= 0 || conf >= 1) stop("`conf` must be strictly between 0 and 1", call. = FALSE)
  size <- max(length(successes), length(n))
  successes <- rep_len(successes, size)
  n <- rep_len(n, size)
  if (any(n <= 0) || any(successes < 0) || any(successes > n) || anyNA(successes) || anyNA(n)) {
    stop("need 0 <= successes <= n and n > 0", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- successes / n
  denominator <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denominator
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / denominator
  data.frame(
    successes = successes, n = n, estimate = p,
    lower = pmax(0, centre - half), upper = pmin(1, centre + half)
  )
}

#' Bootstrap percentile interval for any statistic
#'
#' Resamples the observations with replacement. In a benchmark the
#' observations are problems, so the interval reflects which problems happened
#' to be in the suite.
#'
#' The function is deterministic: it uses its own seed and leaves the state of
#' the caller's random number generator untouched.
#'
#' @param x Observations, one per problem.
#' @param statistic Function from a vector of observations to one number.
#' @param reps Number of resamples.
#' @param conf Confidence level.
#' @param seed Seed of the resampling.
#' @return A list with `estimate`, `lower`, `upper`, `reps` and `conf`.
#' @examples
#' bootstrap_interval(c(120, 80, 95, 400, 60), stats::median)
#' @export
bootstrap_interval <- function(x, statistic = mean, reps = 2000, conf = 0.95, seed = 1) {
  if (length(x) == 0) stop("`x` must not be empty", call. = FALSE)
  if (anyNA(x)) stop("`x` must not contain missing values", call. = FALSE)
  if (reps < 1) stop("`reps` must be at least 1", call. = FALSE)
  if (length(conf) != 1 || conf <= 0 || conf >= 1) stop("`conf` must be strictly between 0 and 1", call. = FALSE)
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  saved <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(
    if (had_seed) assign(".Random.seed", saved, envir = globalenv()) else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()),
    add = TRUE
  )
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  n <- length(x)
  draws <- vapply(seq_len(reps), function(i) statistic(x[sample.int(n, n, replace = TRUE)]), numeric(1))
  alpha <- 1 - conf
  bounds <- stats::quantile(draws, c(alpha / 2, 1 - alpha / 2), names = FALSE, type = 7)
  list(estimate = statistic(x), lower = bounds[1], upper = bounds[2], reps = reps, conf = conf)
}
