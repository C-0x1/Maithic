# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

test_that("P15-02-01 Wilson interval matches known values", {
  i <- wilson_interval(8, 10)
  expect_equal(i$estimate, 0.8)
  expect_equal(i$lower, 0.4901625, tolerance = 1e-6)
  expect_equal(i$upper, 0.9433178, tolerance = 1e-6)
  # With every trial a success the lower bound is n / (n + z^2).
  all_solved <- wilson_interval(24, 24)
  expect_equal(all_solved$upper, 1)
  expect_equal(all_solved$lower, 24 / (24 + qnorm(0.975)^2), tolerance = 1e-12)
  none <- wilson_interval(0, 24)
  expect_equal(none$lower, 0)
  expect_equal(none$upper, 1 - all_solved$lower, tolerance = 1e-12)
})

test_that("P15-02-01 Wilson interval agrees with the score interval of base R", {
  for (case in list(c(1, 3), c(5, 20), c(17, 40), c(99, 100), c(0, 7), c(250, 1000))) {
    ours <- wilson_interval(case[1], case[2])
    reference <- suppressWarnings(prop.test(case[1], case[2], correct = FALSE))$conf.int
    expect_equal(c(ours$lower, ours$upper), as.numeric(reference), tolerance = 1e-9)
  }
  narrow <- wilson_interval(17, 40, conf = 0.80)
  wide <- wilson_interval(17, 40, conf = 0.99)
  expect_lt(narrow$upper - narrow$lower, wide$upper - wide$lower)
})

test_that("P15-02-01 Wilson interval is vectorized and validates its input", {
  table <- wilson_interval(c(0, 12, 24), 24)
  expect_equal(nrow(table), 3)
  expect_true(all(table$lower <= table$estimate & table$estimate <= table$upper))
  expect_true(all(table$lower >= 0 & table$upper <= 1))
  expect_error(wilson_interval(5, 3), "successes <= n")
  expect_error(wilson_interval(1, 0), "n > 0")
  expect_error(wilson_interval(-1, 3), "successes")
  expect_error(wilson_interval(1, 3, conf = 1), "conf")
  expect_error(wilson_interval("1", 3), "numeric")
})

test_that("P15-02-02 bootstrap interval is deterministic and brackets the estimate", {
  x <- c(120, 80, 95, 400, 60, 75, 110, 90, 85, 300)
  first <- bootstrap_interval(x, median)
  second <- bootstrap_interval(x, median)
  expect_identical(first, second)
  expect_equal(first$estimate, median(x))
  expect_lte(first$lower, first$estimate)
  expect_gte(first$upper, first$estimate)
  expect_false(identical(bootstrap_interval(x, median, seed = 2)[c("lower", "upper")], first[c("lower", "upper")]) &&
    identical(bootstrap_interval(x, median, seed = 3)[c("lower", "upper")], first[c("lower", "upper")]))
  # A constant sample has a degenerate interval.
  constant <- bootstrap_interval(rep(5, 12), mean)
  expect_equal(c(constant$lower, constant$upper), c(5, 5))
})

test_that("P15-02-02 bootstrap interval for a mean is close to the normal-theory interval", {
  x <- qnorm(ppoints(200), mean = 10, sd = 2)
  interval <- bootstrap_interval(x, mean, reps = 4000)
  theory <- mean(x) + c(-1, 1) * qnorm(0.975) * sd(x) / sqrt(length(x))
  expect_equal(c(interval$lower, interval$upper), theory, tolerance = 0.01)
})

test_that("P15-02-02 bootstrap interval leaves the caller's random numbers alone", {
  set.seed(42)
  expected <- runif(3)
  set.seed(42)
  invisible(bootstrap_interval(1:20, mean))
  expect_identical(runif(3), expected)
  expect_error(bootstrap_interval(numeric(0)), "empty")
  expect_error(bootstrap_interval(c(1, NA)), "missing")
  expect_error(bootstrap_interval(1:3, reps = 0), "reps")
})
