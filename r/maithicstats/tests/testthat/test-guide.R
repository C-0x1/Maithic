# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P12-06-01 run records are loaded with their settings, their data and every epoch", {
  runs <- read_guide_runs(demo_guide_paths())
  expect_s3_class(runs, "maithic_guide_runs")
  expect_equal(runs$runs$name, c("mean-8", "mean-8", "attention-mean-8", "attention-mean-8", "first-8"))
  expect_equal(runs$runs$seed, c(1, 2, 1, 2, 2))
  expect_equal(runs$runs$mixer, c("none", "none", "attention", "attention", "none"))
  expect_equal(runs$runs$pooling, c(rep("mean", 4), "first"))
  expect_equal(runs$runs$dim, rep(8, 5))
  expect_equal(runs$runs$epochs, c(3, 3, 2, 2, 3))
  expect_equal(runs$runs$best_epoch, c(2, 2, 1, 1, 2))
  expect_equal(runs$runs$dataset, rep("toy", 5))
  expect_equal(runs$runs$pairs, rep(100, 5))
  expect_equal(runs$runs[1, c("train", "validation", "test")], data.frame(train = 60L, validation = 20L, test = 20L), ignore_attr = TRUE)
  expect_equal(runs$runs$parameters, c(1000, 1000, 1200, 1200, 1000))
  # Accuracies come back as shares and as the counts they are shares of.
  expect_equal(runs$runs$top1, c(5, 9, 4, 6, 2) / 20)
  expect_equal(runs$runs$top1_correct, c(5, 9, 4, 6, 2))
  expect_equal(runs$runs$topk_correct, c(10, 16, 12, 12, 10))
  expect_equal(runs$runs$baseline_top1_correct, rep(2, 5))
  expect_equal(runs$runs$baseline_topk, c(8, 10, 8, 10, 10) / 20)
  expect_equal(runs$runs$k, rep(5, 5))
  expect_equal(runs$runs$test_loss, rep(1.9, 5))

  expect_equal(nrow(runs$epochs), 13)
  first <- runs$epochs[runs$epochs$run == 1, ]
  expect_equal(first$epoch, 1:3)
  expect_equal(first$train_loss, c(2.0, 1.5, 1.2))
  expect_equal(first$validation_loss, c(2.1, 1.8, 1.9))
  expect_equal(first$validation_top1, c(0.1, 0.2, 0.3))
  expect_equal(first$best, c(FALSE, TRUE, FALSE))
  expect_equal(unique(first$name), "mean-8")

  # A directory is read file by file, in the order of the names.
  dir <- tempfile("runs")
  dir.create(dir)
  file.copy(demo_guide_paths()[c(3, 1)], file.path(dir, c("b.json", "a.json")))
  expect_equal(read_guide_runs(dir)$runs$name, c("mean-8", "attention-mean-8"))
  expect_error(read_guide_runs(tempfile("nothing")), "file not found")
  empty <- tempfile("empty")
  dir.create(empty)
  expect_error(read_guide_runs(empty), "no run records in")
  expect_error(read_guide_runs(character()), "no run records given")

  # A record that contradicts itself is refused, with what is wrong.
  broken <- function(change) {
    record <- guide_run("mean-8", 1)
    write_document(change(record))
  }
  expect_error(read_guide_runs(broken(function(r) { r$schema <- "maithic.prove.run/1"; r })), "is not the record of a training run")
  expect_error(read_guide_runs(broken(function(r) { r$epochs[[2]]$epoch <- 5; r })), "not numbered from 1 in order")
  expect_error(read_guide_runs(broken(function(r) { r$best_epoch <- 4; r })), "the best epoch, 4, is not one of its 3 epoch")
  expect_error(read_guide_runs(broken(function(r) { r$dataset$pairs <- 99; r })), "hold 100 example\\(s\\), and the dataset 99")
  expect_error(read_guide_runs(broken(function(r) { r$test$top1 <- 0.33; r })), "is not a share of its 20 example")
  expect_error(read_guide_runs(broken(function(r) { r$baseline$examples <- 19; r })), "measured on 19 example\\(s\\), and the test part holds 20")
  expect_error(read_guide_runs(broken(function(r) { r$epochs <- list(); r })), "has no epoch")
  expect_error(model_accuracy(list()), "must come from read_guide_runs")
})

test_that("P12-06-01 the runs the repository keeps are read whole", {
  skip_if(is.null(fixture_root()), "the repository fixtures are not available")
  runs <- read_guide_runs(file.path(fixture_root(), "guide", "runs"))
  expect_equal(nrow(runs$runs), 20)
  expect_equal(sort(unique(runs$runs$name)), c("attention-first-32", "attention-mean-32", "first-32", "mean-32"))
  expect_equal(sort(unique(runs$runs$seed)), 1:5)
  expect_true(all(table(runs$runs$name) == 5))
  expect_equal(nrow(runs$epochs), sum(runs$runs$epochs))
  expect_true(all(runs$runs$top1_correct <= runs$runs$topk_correct))
  expect_equal(runs$runs$top1_correct / runs$runs$test, runs$runs$top1)
  # Runs of one seed met on one split of the data.
  for (seed in 1:5) expect_equal(length(unique(runs$runs$test[runs$runs$seed == seed])), 1)
  expect_equal(sum(runs$epochs$best), 20)
  library_run <- read_guide_runs(file.path(fixture_root(), "guide", "library", "run.json"))
  expect_equal(library_run$runs$name, "library")
  expect_equal(library_run$runs$dataset, "maithic-library")
})

test_that("P12-06-02 learning curves follow the loss of training and validation epoch by epoch", {
  runs <- read_guide_runs(demo_guide_paths())
  curves <- learning_curves(runs)
  expect_equal(names(curves), c("run", "name", "seed", "epoch", "part", "value", "best"))
  expect_equal(nrow(curves), 2 * 13)
  first <- curves[curves$run == 1, ]
  expect_equal(first$part, rep(c("training", "validation"), each = 3))
  expect_equal(first$epoch, c(1:3, 1:3))
  expect_equal(first$value, c(2.0, 1.5, 1.2, 2.1, 1.8, 1.9))
  expect_equal(first$best, rep(c(FALSE, TRUE, FALSE), 2))
  expect_equal(unique(curves$part), c("training", "validation"))

  # Accuracy is measured on the validation part alone.
  accuracy <- learning_curves(runs, "top1")
  expect_equal(unique(accuracy$part), "validation")
  expect_equal(accuracy$value[accuracy$run == 3], c(0.1, 0.2))
  expect_equal(learning_curves(runs, "topk")$value[1:3], c(0.2, 0.4, 0.6))
  expect_error(learning_curves(runs, "f1"), "should be one of")

  # A run without a validation part has a training curve and nothing else.
  alone <- read_guide_runs(write_document(guide_run("mean-8", 1, validation = 0, train = 80, best = 3)))
  expect_equal(learning_curves(alone)$part, rep("training", 3))
  expect_equal(nrow(learning_curves(alone, "top1")), 0)

  summary <- curve_summary(runs)
  expect_equal(summary$best_epoch, c(2, 2, 1, 1, 2))
  expect_equal(summary$epochs, c(3, 3, 2, 2, 3))
  expect_equal(summary$validation_loss_best, c(1.8, 1.8, 2.0, 2.0, 1.8))
  expect_equal(summary$train_loss_best, c(1.5, 1.5, 2.2, 2.2, 1.5))
  expect_equal(summary$validation_loss_last, c(1.9, 1.9, 2.4, 2.4, 1.9))
  expect_equal(summary$train_loss_last, c(1.2, 1.2, 1.0, 1.0, 1.2))
  # The validation loss of the attention model rose by 0.4 after its best epoch while its training loss fell.
  expect_equal(summary$rise, c(0.1, 0.1, 0.4, 0.4, 0.1))
  expect_true(is.na(curve_summary(alone)$rise))
})

test_that("P12-06-03 accuracy is compared model by model, with intervals and against the baseline", {
  runs <- read_guide_runs(demo_guide_paths())
  accuracy <- model_accuracy(runs)
  # The best first.
  expect_equal(accuracy$name, c("mean-8", "attention-mean-8", "first-8"))
  expect_equal(accuracy$runs, c(2, 2, 1))
  expect_equal(accuracy$examples, c(40, 40, 20))
  expect_equal(accuracy$top1, c(14 / 40, 10 / 40, 2 / 20))
  expect_equal(accuracy$topk, c(26 / 40, 24 / 40, 10 / 20))
  expect_equal(accuracy$baseline_top1, c(4 / 40, 4 / 40, 2 / 20))
  expect_equal(accuracy$baseline_topk, c(18 / 40, 18 / 40, 10 / 20))
  expect_equal(accuracy$lift, c(10 / 40, 6 / 40, 0))
  expect_equal(accuracy$parameters, c(1000, 1200, 1000))
  expect_equal(accuracy$k, c(5, 5, 5))
  interval <- wilson_interval(14, 40)
  expect_equal(c(accuracy$top1_lower[1], accuracy$top1_upper[1]), c(interval$lower, interval$upper))
  wide <- wilson_interval(26, 40, conf = 0.99)
  expect_equal(model_accuracy(runs, conf = 0.99)$topk_upper[1], wide$upper)
  expect_true(all(accuracy$top1_lower <= accuracy$top1 & accuracy$top1 <= accuracy$top1_upper))

  # Against the baseline, run by run: (5 - 2) / 20 and (9 - 2) / 20.
  against <- accuracy_difference(runs, "mean-8")
  expect_equal(against$reference, "baseline")
  expect_equal(against$seeds, c(1, 2))
  expect_equal(against$differences, c(0.15, 0.35))
  expect_equal(against$mean, 0.25)
  half <- stats::qt(0.975, 1) * 0.1
  expect_equal(c(against$lower, against$upper), c(0.25 - half, 0.25 + half))
  expect_equal(against$p_value, 2 * stats::pt(-2.5, 1))
  # Between two models, seed by seed: (5 - 4) / 20 and (9 - 6) / 20.
  between <- accuracy_difference(runs, "mean-8", "attention-mean-8")
  expect_equal(between$differences, c(0.05, 0.15))
  expect_equal(between$mean, 0.1)
  expect_equal(accuracy_difference(runs, "attention-mean-8", "mean-8")$mean, -0.1)
  expect_equal(accuracy_difference(runs, "mean-8", "attention-mean-8", "topk")$differences, c(-0.1, 0.2))
  # Only the seeds both models have are compared; one seed gives a difference and no interval.
  single <- accuracy_difference(runs, "mean-8", "first-8")
  expect_equal(single$seeds, 2)
  expect_equal(single$differences, 7 / 20)
  expect_true(is.na(single$lower) && is.na(single$p_value))
  expect_error(accuracy_difference(runs, "median-8"), "there is no run of a model called 'median-8'; the models are mean-8, attention-mean-8, first-8")
  expect_error(accuracy_difference(runs, "mean-8", "median-8"), "there is no run of a model called 'median-8'")
  expect_error(accuracy_difference(runs, "mean-8", conf = 1), "strictly between 0 and 1")
  twice <- read_guide_runs(demo_guide_paths()[c(1, 1)])
  expect_error(accuracy_difference(twice, "mean-8"), "more than one run with the same seed")
  mixed <- read_guide_runs(c(demo_guide_paths()[1], write_document(guide_run("mean-8", 2, k = 3))))
  expect_error(model_accuracy(mixed), "report top-k accuracy for different k")

  report <- guide_report_table(runs)
  expect_equal(report[1], "5 training run(s) of 3 model(s) on toy.")
  expect_true(any(grepl("| mean-8 | 2 | 40 | 1,000 | 35.0% | 22.1% to 50.5% | 65.0% |", report, fixed = TRUE)))
  expect_true(any(grepl("10.0% / 45.0%", report, fixed = TRUE)))
  expect_true(any(grepl("| mean-8 | 2 | +25.0 points |", report, fixed = TRUE)))
  expect_true(any(grepl("| first-8 | 1 | +0.0 points | - | - |", report, fixed = TRUE)))
})

test_that("P12-06-03 on the recorded runs every model is measured against the same baseline", {
  skip_if(is.null(fixture_root()), "the repository fixtures are not available")
  runs <- read_guide_runs(file.path(fixture_root(), "guide", "runs"))
  accuracy <- model_accuracy(runs)
  expect_equal(nrow(accuracy), 4)
  expect_true(all(accuracy$runs == 5))
  # The four models met on the same five splits, so they share their test examples and their baseline.
  expect_equal(length(unique(accuracy$examples)), 1)
  expect_equal(length(unique(accuracy$baseline_top1)), 1)
  expect_true(all(accuracy$top1 <= accuracy$topk))
  expect_true(all(diff(accuracy$top1) <= 0))
  # The model the repository ships is the mean-pooling one; it beats the baseline on every seed.
  against <- accuracy_difference(runs, "mean-32")
  expect_equal(against$seeds, 1:5)
  expect_true(all(against$differences > 0))
  expect_true(against$lower > 0)
  expect_true(length(guide_report_table(runs)) > 8)
})
