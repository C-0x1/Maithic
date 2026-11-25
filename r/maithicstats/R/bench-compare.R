# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

#' Exact McNemar test
#'
#' Tests whether two configurations differ on the same problems, from the
#' problems on which they disagree. The exact binomial form is used, which is
#' valid for the small counts that benchmark suites produce.
#'
#' @param only_a Number of problems solved by the first configuration only.
#' @param only_b Number of problems solved by the second configuration only.
#' @return The two-sided p-value; 1 when the configurations never disagree.
#' @examples
#' mcnemar_exact(9, 2)
#' @export
mcnemar_exact <- function(only_a, only_b) {
  if (length(only_a) != 1 || length(only_b) != 1 || only_a < 0 || only_b < 0) stop("counts must be single non-negative numbers", call. = FALSE)
  discordant <- only_a + only_b
  if (discordant == 0) return(1)
  stats::binom.test(only_a, discordant, p = 0.5, alternative = "two.sided")$p.value
}

#' Paired comparison of two configurations
#'
#' Both configurations must have been run on the same problems, in the same
#' order. The result holds the table of agreement, the exact McNemar p-value
#' and the effect size: the difference of the two success rates with an
#' interval that accounts for the pairing.
#'
#' @param solved_a,solved_b Logical vectors, one element per problem.
#' @param conf Confidence level of the interval for the difference.
#' @return A list with `n`, `both`, `only_a`, `only_b`, `neither`, `rate_a`,
#'   `rate_b`, `difference` (a minus b), `lower`, `upper` and `p_value`.
#' @examples
#' paired_comparison(c(TRUE, TRUE, FALSE, TRUE), c(TRUE, FALSE, FALSE, FALSE))
#' @export
paired_comparison <- function(solved_a, solved_b, conf = 0.95) {
  if (!is.logical(solved_a) || !is.logical(solved_b)) stop("`solved_a` and `solved_b` must be logical", call. = FALSE)
  if (length(solved_a) != length(solved_b)) stop("both configurations must cover the same problems", call. = FALSE)
  if (length(solved_a) == 0) stop("no problems to compare", call. = FALSE)
  if (anyNA(solved_a) || anyNA(solved_b)) stop("missing outcomes are not allowed", call. = FALSE)
  n <- length(solved_a)
  both <- sum(solved_a & solved_b)
  only_a <- sum(solved_a & !solved_b)
  only_b <- sum(!solved_a & solved_b)
  neither <- sum(!solved_a & !solved_b)
  difference <- (only_a - only_b) / n
  z <- stats::qnorm(1 - (1 - conf) / 2)
  half <- z * sqrt(only_a + only_b - (only_a - only_b)^2 / n) / n
  list(
    n = n, both = both, only_a = only_a, only_b = only_b, neither = neither,
    rate_a = mean(solved_a), rate_b = mean(solved_b),
    difference = difference, lower = max(-1, difference - half), upper = min(1, difference + half),
    p_value = mcnemar_exact(only_a, only_b)
  )
}

#' Table of configurations with intervals
#'
#' @param runs A named list of logical vectors: for each configuration, which
#'   problems it solved.
#' @param conf Confidence level.
#' @return A data frame with one row per configuration: `configuration`, `n`,
#'   `solved`, `rate`, `lower` and `upper`, ordered by rate, best first.
#' @examples
#' comparison_table(list(portfolio = c(TRUE, TRUE, FALSE), model = c(TRUE, TRUE, TRUE)))
#' @export
comparison_table <- function(runs, conf = 0.95) {
  if (!is.list(runs) || length(runs) == 0 || is.null(names(runs)) || any(names(runs) == "")) {
    stop("`runs` must be a named list of logical vectors", call. = FALSE)
  }
  rows <- lapply(names(runs), function(name) {
    solved <- runs[[name]]
    if (!is.logical(solved) || length(solved) == 0) stop(sprintf("configuration '%s' must be a non-empty logical vector", name), call. = FALSE)
    interval <- wilson_interval(sum(solved), length(solved), conf)
    data.frame(configuration = name, n = length(solved), solved = sum(solved), rate = interval$estimate, lower = interval$lower, upper = interval$upper)
  })
  table <- do.call(rbind, rows)
  table <- table[order(-table$rate, table$configuration), , drop = FALSE]
  rownames(table) <- NULL
  table
}
