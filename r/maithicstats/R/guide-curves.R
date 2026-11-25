# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Learning curves of tactic models.

#' Learning curves of training runs
#'
#' The loss or the accuracy of each run after every epoch, in the long form a
#' plot wants: one row for each run, epoch and part of the data. The training
#' loss is the mean over the examples as they were seen during the epoch,
#' while the model was still changing; the validation numbers are measured
#' after the epoch, on proofs the model was not trained on.
#'
#' @param runs Runs from [read_guide_runs()].
#' @param measure `"loss"`, for the training and the validation loss, or
#'   `"top1"` or `"topk"`, for the validation accuracy.
#' @return A data frame with `run`, `name`, `seed`, `epoch`, `part`
#'   (`"training"` or `"validation"`), `value`, and `best`, which marks the
#'   epoch whose model was kept. Epochs without a validation number, as in a
#'   run without a validation part, have no validation row.
#' @export
learning_curves <- function(runs, measure = c("loss", "top1", "topk")) {
  runs <- need_guide_runs(runs)
  measure <- match.arg(measure)
  epochs <- runs$epochs
  part <- function(name, column) {
    out <- epochs[c("run", "name", "seed", "epoch")]
    out$part <- rep(name, nrow(epochs))
    out$value <- epochs[[column]]
    out$best <- epochs$best
    out[!is.na(out$value), , drop = FALSE]
  }
  out <- if (measure == "loss") rbind(part("training", "train_loss"), part("validation", "validation_loss"))
         else part("validation", sprintf("validation_%s", measure))
  out <- out[order(out$run, match(out$part, c("training", "validation")), out$epoch), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Where the training of each run stood at its best and at its end
#'
#' One row for each run: the epoch whose model was kept, the losses there and
#' after the last epoch, and how far the validation loss rose again after its
#' best. A rise is the sign of a model that went on to learn its training
#' examples by heart; keeping the model of the best epoch is what protects a
#' run from it.
#'
#' @param runs Runs from [read_guide_runs()].
#' @return A data frame with `run`, `name`, `seed`, `epochs`, `best_epoch`,
#'   `train_loss_best`, `validation_loss_best`, `train_loss_last`,
#'   `validation_loss_last` and `rise` (the last validation loss less the
#'   best; `NA` without a validation part).
#' @export
curve_summary <- function(runs) {
  runs <- need_guide_runs(runs)
  epochs <- runs$epochs
  rows <- lapply(seq_len(nrow(runs$runs)), function(i) {
    run <- runs$runs[i, ]
    own <- epochs[epochs$run == run$run, , drop = FALSE]
    best <- own[own$epoch == run$best_epoch, , drop = FALSE]
    last <- own[own$epoch == max(own$epoch), , drop = FALSE]
    data.frame(run = run$run, name = run$name, seed = run$seed, epochs = nrow(own), best_epoch = run$best_epoch,
               train_loss_best = best$train_loss, validation_loss_best = best$validation_loss, train_loss_last = last$train_loss,
               validation_loss_last = last$validation_loss, rise = last$validation_loss - best$validation_loss, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
