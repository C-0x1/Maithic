# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# How often tactic models are right, model by model.

#' Accuracy of tactic models on held-out proofs
#'
#' For each model, the share of test examples whose tactic template it put
#' first (`top1`) and among its first `k` (`topk`), pooled over the runs of
#' the model, each with a Wilson interval; beside them the same for the
#' baseline that knows no goal and always answers with the templates most
#' frequent in training. The runs of a model differ by their seed, and a
#' seed decides the split of the data, so pooling adds up test examples from
#' different splits: the interval speaks of examples, with the caution that
#' the steps of one proof are not independent of each other.
#'
#' @param runs Runs from [read_guide_runs()].
#' @param conf Confidence level of the intervals.
#' @return A data frame with one row per model, the best first: `name`,
#'   `runs`, `examples`, `parameters`, `top1` with `top1_lower` and
#'   `top1_upper`, `topk` with `topk_lower` and `topk_upper`, `k`,
#'   `baseline_top1`, `baseline_topk`, and `lift`, the top-1 accuracy less
#'   that of the baseline.
#' @export
model_accuracy <- function(runs, conf = 0.95) {
  runs <- need_guide_runs(runs)
  table <- runs$runs[runs$runs$test > 0, , drop = FALSE]
  if (nrow(table) == 0) stop("no run has a test part to measure on", call. = FALSE)
  out <- summarise_groups(table, "name", function(rows) {
    if (length(unique(rows$k)) != 1) stop(sprintf("the runs of %s report top-k accuracy for different k", rows$name[1]), call. = FALSE)
    examples <- sum(rows$test)
    top1 <- wilson_interval(sum(rows$top1_correct), examples, conf)
    topk <- wilson_interval(sum(rows$topk_correct), examples, conf)
    data.frame(runs = nrow(rows), examples = examples, parameters = round(mean(rows$parameters)), top1 = top1$estimate, top1_lower = top1$lower,
               top1_upper = top1$upper, topk = topk$estimate, topk_lower = topk$lower, topk_upper = topk$upper, k = rows$k[1],
               baseline_top1 = sum(rows$baseline_top1_correct) / examples, baseline_topk = sum(rows$baseline_topk_correct) / examples)
  })
  out$lift <- out$top1 - out$baseline_top1
  out <- out[order(-out$top1, -out$topk, out$name, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Compare two tactic models seed by seed
#'
#' Runs with the same seed were trained and tested on the same split of the
#' data, so two models can be compared within each seed, and what the split
#' happened to be cancels out. The function takes the difference of the
#' accuracies seed by seed and gives its mean with a t interval and the
#' p-value of the paired t-test. With `reference = "baseline"`, the model is
#' compared with the baseline that knows no goal, run by run.
#'
#' A handful of seeds is a small sample: the interval is wide, and it is the
#' honest size of what a few runs can show.
#'
#' @param runs Runs from [read_guide_runs()].
#' @param model The name of a model.
#' @param reference The name of another model, or `"baseline"`.
#' @param measure `"top1"` or `"topk"`.
#' @param conf Confidence level of the interval.
#' @return A list with `model`, `reference`, `measure`, `seeds` (the seeds
#'   both have), `differences` (model less reference, one for each seed),
#'   `mean`, `lower`, `upper`, `p_value` and `conf`. With fewer than two
#'   seeds, or differences that are all equal, the interval and the p-value
#'   are `NA`.
#' @export
accuracy_difference <- function(runs, model, reference = "baseline", measure = c("top1", "topk"), conf = 0.95) {
  runs <- need_guide_runs(runs)
  measure <- match.arg(measure)
  if (length(conf) != 1 || !is.finite(conf) || conf <= 0 || conf >= 1) stop("`conf` must be strictly between 0 and 1", call. = FALSE)
  table <- runs$runs
  known <- guide_models(runs)
  if (!is.character(model) || length(model) != 1 || !(model %in% known)) {
    stop(sprintf("there is no run of a model called '%s'; the models are %s", format(model), paste(known, collapse = ", ")), call. = FALSE)
  }
  if (!is.character(reference) || length(reference) != 1 || !(reference %in% c(known, "baseline"))) {
    stop(sprintf("there is no run of a model called '%s'; the models are %s", format(reference), paste(known, collapse = ", ")), call. = FALSE)
  }
  own <- table[table$name == model, , drop = FALSE]
  if (anyDuplicated(own$seed) > 0) stop(sprintf("the model %s has more than one run with the same seed", model), call. = FALSE)
  if (reference == "baseline") {
    seeds <- own$seed
    differences <- own[[measure]] - own[[sprintf("baseline_%s", measure)]]
  } else {
    other <- table[table$name == reference, , drop = FALSE]
    if (anyDuplicated(other$seed) > 0) stop(sprintf("the model %s has more than one run with the same seed", reference), call. = FALSE)
    seeds <- sort(intersect(own$seed, other$seed))
    if (length(seeds) == 0) stop(sprintf("%s and %s have no seed in common", model, reference), call. = FALSE)
    differences <- own[[measure]][match(seeds, own$seed)] - other[[measure]][match(seeds, other$seed)]
  }
  ordered <- order(seeds)
  seeds <- seeds[ordered]
  differences <- differences[ordered]
  out <- list(model = model, reference = reference, measure = measure, seeds = seeds, differences = differences, mean = mean(differences),
              lower = NA_real_, upper = NA_real_, p_value = NA_real_, conf = conf)
  if (length(differences) >= 2 && stats::sd(differences) > 0) {
    test <- stats::t.test(differences, conf.level = conf)
    out$lower <- test$conf.int[1]
    out$upper <- test$conf.int[2]
    out$p_value <- test$p.value
  }
  out
}

#' Report table of training runs
#'
#' The models of a set of runs as Markdown, for a report: their accuracy on
#' held-out proofs with intervals beside the baseline, and for each model its
#' difference from the baseline seed by seed.
#'
#' @param runs Runs from [read_guide_runs()].
#' @param conf Confidence level of the intervals.
#' @return A character vector of Markdown lines.
#' @export
guide_report_table <- function(runs, conf = 0.95) {
  runs <- need_guide_runs(runs)
  accuracy <- model_accuracy(runs, conf)
  share <- function(x) sprintf("%.1f%%", 100 * x)
  between <- function(lower, upper) sprintf("%s to %s", share(lower), share(upper))
  models <- data.frame(model = accuracy$name, runs = accuracy$runs, examples = accuracy$examples, weights = accuracy$parameters,
                       top1 = share(accuracy$top1), top1_interval = between(accuracy$top1_lower, accuracy$top1_upper),
                       topk = share(accuracy$topk), topk_interval = between(accuracy$topk_lower, accuracy$topk_upper),
                       baseline = sprintf("%s / %s", share(accuracy$baseline_top1), share(accuracy$baseline_topk)), stringsAsFactors = FALSE)
  against <- do.call(rbind, lapply(accuracy$name, function(name) {
    difference <- accuracy_difference(runs, name, "baseline", "top1", conf)
    data.frame(model = name, seeds = length(difference$seeds), mean = sprintf("%+.1f points", 100 * difference$mean),
               interval = if (is.na(difference$lower)) "-" else sprintf("%+.1f to %+.1f", 100 * difference$lower, 100 * difference$upper),
               p = if (is.na(difference$p_value)) "-" else format.pval(difference$p_value, digits = 2, eps = 0.001), stringsAsFactors = FALSE)
  }))
  table <- runs$runs
  k <- accuracy$k[1]
  c(
    sprintf("%d training run(s) of %d model(s) on %s.", nrow(table), nrow(accuracy), paste(unique(table$dataset), collapse = ", ")), "",
    "**Accuracy on held-out proofs**", "",
    markdown_table(models, labels = c(model = "Model", runs = "Runs", examples = "Test examples", weights = "Weights", top1 = "Top-1",
                                      top1_interval = sprintf("%g%% interval", 100 * conf), topk = sprintf("Top-%d", k),
                                      topk_interval = sprintf("%g%% interval", 100 * conf), baseline = sprintf("Baseline top-1 / top-%d", k))), "",
    "**Top-1 accuracy against the baseline, seed by seed**", "",
    markdown_table(against, labels = c(model = "Model", seeds = "Seeds", mean = "Mean difference", interval = sprintf("%g%% interval", 100 * conf),
                                       p = "p-value"))
  )
}
