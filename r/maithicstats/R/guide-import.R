# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the records of training runs that `maithic guide train` writes.

empty_guide_epochs <- function() {
  data.frame(run = integer(), name = character(), seed = integer(), epoch = integer(), train_loss = numeric(), validation_loss = numeric(),
             validation_top1 = numeric(), validation_topk = numeric(), best = logical(), stringsAsFactors = FALSE)
}

# How many examples a share of them is. A share that is no whole number of
# examples means the record does not describe a count, and is refused.
share_count <- function(path, what, share, examples) {
  if (is.na(share) || examples == 0) return(NA_integer_)
  count <- share * examples
  if (abs(count - round(count)) > 1e-6 || share < 0 || share > 1) {
    stop(sprintf("%s: the %s, %s, is not a share of its %d example(s)", path, what, format(share), examples), call. = FALSE)
  }
  as.integer(round(count))
}

parse_guide_run <- function(path, index) {
  record <- read_document(path, "maithic.guide.run/1", "the record of a training run")
  config <- record$config
  dataset <- record$dataset
  logged <- record$epochs
  if (length(logged) == 0) stop(sprintf("%s: the record has no epoch", path), call. = FALSE)
  numbers <- counts_of(logged, "epoch", NA_integer_)
  if (!identical(numbers, seq_along(logged))) stop(sprintf("%s: the epochs are not numbered from 1 in order", path), call. = FALSE)
  best_epoch <- count_field(record, "best_epoch", NA_integer_)
  if (is.na(best_epoch) || best_epoch < 1 || best_epoch > length(logged)) {
    stop(sprintf("%s: the best epoch, %s, is not one of its %d epoch(s)", path, format(best_epoch), length(logged)), call. = FALSE)
  }
  parts <- c(train = count_field(dataset, "train"), validation = count_field(dataset, "validation"), test = count_field(dataset, "test"))
  if (sum(parts) != count_field(dataset, "pairs")) {
    stop(sprintf("%s: the parts of the data hold %d example(s), and the dataset %d", path, sum(parts), count_field(dataset, "pairs")), call. = FALSE)
  }
  name <- text_field(record, "name")
  seed <- count_field(record, "seed", NA_integer_)
  measured <- function(member) {
    evaluation <- record[[member]]
    examples <- count_field(evaluation, "examples")
    if (examples != parts[["test"]]) {
      stop(sprintf("%s: the %s was measured on %d example(s), and the test part holds %d", path, member, examples, parts[["test"]]), call. = FALSE)
    }
    top1 <- number_field(evaluation, "top1")
    topk <- number_field(evaluation, "topk")
    list(examples = examples, loss = number_field(evaluation, "loss"), top1 = top1, topk = topk, k = count_field(evaluation, "k", NA_integer_),
         top1_correct = share_count(path, sprintf("top-1 accuracy of the %s", member), top1, examples),
         topk_correct = share_count(path, sprintf("top-k accuracy of the %s", member), topk, examples))
  }
  test <- measured("test")
  baseline <- measured("baseline")
  run <- data.frame(
    run = index, path = path, name = name, seed = seed, dim = count_field(config, "dim"), max_length = count_field(config, "max_length"),
    pooling = text_field(config, "pooling"), mixer = text_field(config, "mixer"), epochs = length(logged), batch = count_field(config, "batch"),
    rate = number_field(config, "rate"), weight_decay = number_field(config, "weight_decay"), dataset = text_field(dataset, "name"),
    version = text_field(dataset, "version"), pairs = count_field(dataset, "pairs"), theorems = count_field(dataset, "theorems"),
    train = parts[["train"]], validation = parts[["validation"]], test = parts[["test"]], vocabulary = count_field(record, "vocabulary"),
    classes = count_field(record, "classes"), parameters = count_field(record, "parameters"), best_epoch = best_epoch, k = test$k,
    test_loss = test$loss, top1 = test$top1, topk = test$topk, top1_correct = test$top1_correct, topk_correct = test$topk_correct,
    baseline_loss = baseline$loss, baseline_top1 = baseline$top1, baseline_topk = baseline$topk, baseline_top1_correct = baseline$top1_correct,
    baseline_topk_correct = baseline$topk_correct, stringsAsFactors = FALSE
  )
  epochs <- data.frame(
    run = rep(index, length(logged)), name = rep(name, length(logged)), seed = rep(seed, length(logged)), epoch = numbers,
    train_loss = numbers_of(logged, "train_loss"), validation_loss = numbers_of(logged, "validation_loss"),
    validation_top1 = numbers_of(logged, "validation_top1"), validation_topk = numbers_of(logged, "validation_topk"),
    best = numbers == best_epoch, stringsAsFactors = FALSE
  )
  list(run = run, epochs = epochs)
}

#' Read training runs of tactic models
#'
#' Loads the records that `maithic guide train` and the comparison script of
#' the Julia package `MaithicGuide` write: one record for each training run,
#' with its configuration, the data that went in, the loss and the accuracy
#' after every epoch, and the chosen model on the test part of the data
#' beside a baseline that knows no goal.
#'
#' Each record is checked against itself: its epochs are numbered in order,
#' its best epoch is one of them, the parts of its data add up, and its
#' accuracies are shares of the examples they were measured on. From those
#' shares the counts are recovered, which is what intervals need.
#'
#' @param paths Paths of run records, or one directory, of which every
#'   `.json` file is read, in the order of the file names.
#' @return An object of class `maithic_guide_runs`, a list of two data
#'   frames. `runs` has one row per run: `name` (the model), `seed`, the
#'   shape of the model and the settings of the training, the size of the
#'   data and of its three parts, `best_epoch`, and on the test part the
#'   loss, `top1` and `topk` (with `k`) for the model and, with the prefix
#'   `baseline_`, for the baseline, each also as a count of examples
#'   (`top1_correct`, ...). `epochs` has one row per epoch of each run: the
#'   training loss, the validation loss and accuracies, and whether the
#'   epoch is the one whose model was kept (`best`).
#' @export
read_guide_runs <- function(paths) {
  if (!is.character(paths) || length(paths) == 0) stop("no run records given", call. = FALSE)
  if (length(paths) == 1 && dir.exists(paths)) {
    found <- sort(list.files(paths, pattern = "\\.json$", full.names = TRUE), method = "radix")
    if (length(found) == 0) stop(sprintf("no run records in %s", paths), call. = FALSE)
    paths <- found
  }
  parsed <- lapply(seq_along(paths), function(i) parse_guide_run(paths[[i]], i))
  runs <- do.call(rbind, lapply(parsed, `[[`, "run"))
  rownames(runs) <- NULL
  epochs <- bind_frames(lapply(parsed, `[[`, "epochs"), empty_guide_epochs())
  rownames(epochs) <- NULL
  structure(list(runs = runs, epochs = epochs), class = "maithic_guide_runs")
}

need_guide_runs <- function(runs) {
  if (!inherits(runs, "maithic_guide_runs")) stop("`runs` must come from read_guide_runs()", call. = FALSE)
  runs
}

# The models of a set of runs, in the order they first occur.
guide_models <- function(runs) unique(runs$runs$name)
