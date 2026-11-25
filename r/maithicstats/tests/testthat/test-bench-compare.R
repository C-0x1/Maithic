# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P15-02-03 exact McNemar test matches known values", {
  # Two-sided exact binomial: 2 * P(X <= 2) for X ~ Binomial(11, 1/2).
  expect_equal(mcnemar_exact(9, 2), 2 * sum(dbinom(0:2, 11, 0.5)), tolerance = 1e-12)
  expect_equal(mcnemar_exact(9, 2), 0.06542969, tolerance = 1e-7)
  expect_equal(mcnemar_exact(2, 9), mcnemar_exact(9, 2))
  expect_equal(mcnemar_exact(5, 5), 1)
  expect_equal(mcnemar_exact(0, 0), 1)
  expect_equal(mcnemar_exact(6, 0), 2 * 0.5^6, tolerance = 1e-12)
  expect_error(mcnemar_exact(-1, 2), "non-negative")
})

test_that("P15-02-03 paired comparison counts agreement and disagreement", {
  a <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
  b <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  result <- paired_comparison(a, b)
  expect_equal(result[c("n", "both", "only_a", "only_b", "neither")], list(n = 8, both = 3, only_a = 3, only_b = 1, neither = 1))
  expect_equal(result$p_value, mcnemar_exact(3, 1))
  expect_equal(result$rate_a, 6 / 8)
  expect_equal(result$rate_b, 4 / 8)
  expect_error(paired_comparison(a, b[-1]), "same problems")
  expect_error(paired_comparison(c(1, 0), c(TRUE, FALSE)), "logical")
  expect_error(paired_comparison(logical(0), logical(0)), "no problems")
})

test_that("P15-02-04 effect size is the paired difference with an interval", {
  a <- rep(c(TRUE, FALSE), c(30, 10))
  b <- c(rep(TRUE, 20), rep(FALSE, 10), rep(TRUE, 2), rep(FALSE, 8))
  result <- paired_comparison(a, b)
  expect_equal(result$difference, (10 - 2) / 40)
  expect_equal(result$difference, result$rate_a - result$rate_b)
  half <- qnorm(0.975) * sqrt(10 + 2 - (10 - 2)^2 / 40) / 40
  expect_equal(c(result$lower, result$upper), result$difference + c(-1, 1) * half, tolerance = 1e-12)
  identical_runs <- paired_comparison(a, a)
  expect_equal(c(identical_runs$difference, identical_runs$lower, identical_runs$upper, identical_runs$p_value), c(0, 0, 0, 1))
  swapped <- paired_comparison(b, a)
  expect_equal(swapped$difference, -result$difference)
  expect_equal(c(swapped$lower, swapped$upper), -c(result$upper, result$lower), tolerance = 1e-12)
})

test_that("P15-02-05 comparison table lists configurations with intervals, best first", {
  table <- comparison_table(list(portfolio = rep(c(TRUE, FALSE), c(18, 6)), model = rep(c(TRUE, FALSE), c(22, 2)), nothing = rep(FALSE, 24)))
  expect_equal(table$configuration, c("model", "portfolio", "nothing"))
  expect_equal(table$solved, c(22, 18, 0))
  expect_equal(table$rate, c(22, 18, 0) / 24)
  expect_equal(table$lower, wilson_interval(c(22, 18, 0), 24)$lower)
  expect_named(table, c("configuration", "n", "solved", "rate", "lower", "upper"))
  expect_error(comparison_table(list(c(TRUE, FALSE))), "named list")
  expect_error(comparison_table(list(a = 1:3)), "logical")
})

test_that("P15-02-06 statistical helpers reproduce textbook examples", {
  # Agresti and Coull (1998): 15 successes in 148 trials, 95 percent score interval.
  i <- wilson_interval(15, 148)
  expect_equal(c(i$lower, i$upper), c(0.0624, 0.1605), tolerance = 5e-4)
  # The sign test behind McNemar: 1 against 9 disagreements.
  expect_equal(mcnemar_exact(1, 9), 22 / 1024, tolerance = 1e-12)
  # A bootstrap interval for the mean of 1..100 contains the mean and is roughly symmetric.
  b <- bootstrap_interval(1:100, mean, reps = 3000)
  expect_equal((b$lower + b$upper) / 2, 50.5, tolerance = 0.02)
})
