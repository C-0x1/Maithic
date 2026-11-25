# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# How the cost of an algorithm grows with its input: fitting the classic
# growth models to measured counts, and laying the result beside the bound
# that is proved in Lean.

#' The growth models that are fitted
#'
#' Five models of how a cost grows with the size `n` of the input, from the
#' slowest to the fastest: constant, logarithmic, linear, linearithmic
#' (`n log n`) and quadratic.
#'
#' @return The names of the models, in order of growth.
#' @export
growth_models <- function() c("constant", "logarithmic", "linear", "linearithmic", "quadratic")

# What a model says the cost is proportional to.
growth_basis <- function(model, n) {
  switch(model,
         constant = rep(1, length(n)),
         logarithmic = log2(n),
         linear = n,
         linearithmic = n * log2(n),
         quadratic = n^2,
         stop("'", model, "' is not a growth model", call. = FALSE))
}

#' The place of a growth model in the order of growth
#'
#' @param model Names of growth models.
#' @return Their positions in [growth_models()]: 1 for constant, 5 for quadratic.
#' @export
growth_order <- function(model) {
  position <- match(model, growth_models())
  if (anyNA(position)) stop("'", model[is.na(position)][1], "' is not a growth model", call. = FALSE)
  position
}

#' Fit growth models to measured costs
#'
#' Fits each model of [growth_models()] to the costs by least squares, as
#' `cost = a + b * g(n)` with `g` the growth of the model, and says which
#' model describes the measurements best. The models are compared by the
#' Bayesian information criterion, so that a model with a slope has to earn
#' it against the constant one. A model whose fitted slope is negative
#' describes no growth and is not chosen.
#'
#' Measurements show how a cost grew on the inputs that were tried. They
#' prove nothing about inputs that were not; that is what the bounds in
#' Lean are for.
#'
#' @param n The sizes of the inputs, each positive.
#' @param cost The cost measured at each size.
#' @return A data frame with a row for each model: `model`, `intercept`,
#'   `slope`, `rss` (the residual sum of squares), `r_squared`, `bic` and
#'   `best`, which is true for one row.
#' @export
fit_growth <- function(n, cost) {
  if (length(n) != length(cost)) stop("there are ", length(n), " size(s) and ", length(cost), " cost(s)", call. = FALSE)
  if (!is.numeric(n) || !is.numeric(cost) || anyNA(n) || anyNA(cost)) stop("sizes and costs are numbers, with none missing", call. = FALSE)
  if (any(n <= 0)) stop("a size is positive", call. = FALSE)
  if (length(unique(n)) < 3) stop("growth needs measurements at three sizes or more", call. = FALSE)
  m <- length(n)
  total <- sum((cost - mean(cost))^2)
  floor_rss <- 1e-12 * max(total, 1)
  rows <- lapply(growth_models(), function(model) {
    if (model == "constant") {
      intercept <- mean(cost)
      slope <- 0
      rss <- total
      k <- 1
    } else {
      g <- growth_basis(model, n)
      centered <- g - mean(g)
      slope <- sum(centered * (cost - mean(cost))) / sum(centered^2)
      intercept <- mean(cost) - slope * mean(g)
      rss <- sum((cost - intercept - slope * g)^2)
      k <- 2
    }
    usable <- slope >= 0
    data.frame(model = model, intercept = intercept, slope = slope, rss = rss,
               r_squared = if (total == 0) 1 else 1 - rss / total,
               bic = if (usable) m * log(max(rss, floor_rss) / m) + k * log(m) else Inf,
               stringsAsFactors = FALSE)
  })
  fits <- do.call(rbind, rows)
  fits$best <- seq_len(nrow(fits)) == which.min(fits$bic)
  fits
}

#' The growth model that fits measured costs best
#'
#' @inheritParams fit_growth
#' @return The name of the model.
#' @export
best_growth <- function(n, cost) {
  fits <- fit_growth(n, cost)
  fits$model[fits$best]
}

#' Read a table of measured costs
#'
#' Reads the CSV that `bin/costs.jl` of the Julia package MaithicAlgoRef
#' prints: for an algorithm and a size, the comparisons that the compiled
#' Lean code made and the bound that is proved for that size.
#'
#' @param path The path of the CSV.
#' @return A data frame with the columns `algorithm`, `n`, `comparisons`
#'   and `bound`.
#' @export
read_algo_costs <- function(path) {
  if (!file.exists(path)) stop("there is no file ", path, call. = FALSE)
  costs <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing <- setdiff(c("algorithm", "n", "comparisons", "bound"), names(costs))
  if (length(missing) > 0) stop(path, " has no column ", paste(missing, collapse = ", "), call. = FALSE)
  if (nrow(costs) == 0) stop(path, " holds no measurement", call. = FALSE)
  costs[, c("algorithm", "n", "comparisons", "bound")]
}

#' Measured growth beside proved bounds
#'
#' For each algorithm of a table of costs: the growth model that fits the
#' measured comparisons, the model that fits the proved bound, and whether
#' the two tell the same story. `within` says that no measurement exceeds
#' its bound, which the proof guarantees and the table confirms; `tight`
#' says that the measured growth is of the same order as the bound, so the
#' bound cannot be improved by more than a constant factor on these inputs.
#'
#' @param costs A table from [read_algo_costs()].
#' @return A data frame with a row for each algorithm: `algorithm`, `sizes`,
#'   `largest`, `measured`, `r_squared`, `proved`, `within`, `tight` and
#'   `ratio`, the largest share of its bound that a measurement reached.
#' @export
growth_table <- function(costs) {
  algorithms <- unique(costs$algorithm)
  rows <- lapply(algorithms, function(name) {
    one <- costs[costs$algorithm == name, ]
    fits <- fit_growth(one$n, one$comparisons)
    measured <- fits$model[fits$best]
    proved <- best_growth(one$n, one$bound)
    data.frame(algorithm = name, sizes = nrow(one), largest = max(one$n), measured = measured,
               r_squared = fits$r_squared[fits$best], proved = proved,
               within = all(one$comparisons <= one$bound),
               tight = growth_order(measured) == growth_order(proved),
               ratio = max(ifelse(one$bound > 0, one$comparisons / one$bound, 0)),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
