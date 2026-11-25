# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

sizes <- c(8, 16, 32, 64, 128, 256, 512)

test_that("P05-06-01 each of the five growth models is recognized from costs that follow it", {
  expect_equal(growth_models(), c("constant", "logarithmic", "linear", "linearithmic", "quadratic"))
  expect_equal(growth_order(c("quadratic", "constant", "linear")), c(5, 1, 3))
  expect_error(growth_order("cubic"), "'cubic' is not a growth model")

  exact <- list(constant = rep(7, 7), logarithmic = 3 * log2(sizes) + 2, linear = 5 * sizes + 11,
                linearithmic = 2 * sizes * log2(sizes) + 40, quadratic = sizes^2 / 2 + 3)
  for (model in names(exact)) {
    fits <- fit_growth(sizes, exact[[model]])
    expect_equal(fits$model, growth_models())
    expect_equal(sum(fits$best), 1)
    expect_equal(fits$model[fits$best], model)
    expect_equal(fits$r_squared[fits$best], 1)
    expect_equal(best_growth(sizes, exact[[model]]), model)
  }
  # The coefficients of the model that was used come back.
  linear <- fit_growth(sizes, exact$linear)
  expect_equal(linear$slope[linear$model == "linear"], 5)
  expect_equal(linear$intercept[linear$model == "linear"], 11)
  expect_equal(linear$rss[linear$model == "linear"], 0)
  expect_true(all(linear$rss[linear$model != "linear"] > 1))
  # The constant model has no slope, and its intercept is the mean.
  expect_equal(linear$slope[linear$model == "constant"], 0)
  expect_equal(linear$intercept[linear$model == "constant"], mean(exact$linear))
})

test_that("P05-06-01 growth is recognized through noise, and a slope has to earn its place", {
  wobble <- c(1.03, 0.98, 1.01, 0.97, 1.02, 0.99, 1.01)
  expect_equal(best_growth(sizes, wobble * sizes * log2(sizes)), "linearithmic")
  expect_equal(best_growth(sizes, wobble * sizes^2), "quadratic")
  expect_equal(best_growth(sizes, wobble * 4 * sizes), "linear")
  expect_equal(best_growth(sizes, wobble * 5 * log2(sizes)), "logarithmic")
  # Costs that only wobble around a level are constant: no model with a slope explains them better.
  expect_equal(best_growth(sizes, c(10, 11, 10, 9, 10, 11, 9)), "constant")
  # Costs that fall describe no growth.
  falling <- fit_growth(sizes, rev(sizes))
  expect_equal(falling$model[falling$best], "constant")
  expect_true(all(is.infinite(falling$bic[falling$slope < 0])))
  # n (n - 1) / 2 is quadratic although it is not a multiple of n squared.
  triangle <- fit_growth(sizes, sizes * (sizes - 1) / 2)
  expect_equal(triangle$model[triangle$best], "quadratic")
  expect_gt(triangle$r_squared[triangle$best], 0.99999)

  expect_error(fit_growth(c(1, 2), c(1, 2)), "three sizes or more")
  expect_error(fit_growth(c(4, 4, 4), c(1, 2, 3)), "three sizes or more")
  expect_error(fit_growth(c(0, 2, 4), c(1, 2, 3)), "a size is positive")
  expect_error(fit_growth(c(1, 2, 4), c(1, 2)), "3 size\\(s\\) and 2 cost\\(s\\)")
  expect_error(fit_growth(c(1, 2, 4), c(1, NA, 3)), "none missing")
  expect_error(fit_growth(c("1", "2", "4"), c(1, 2, 3)), "numbers")
})

test_that("P05-06-01 the measured comparisons of the verified algorithms grow as the proved bounds say", {
  root <- repo_root()
  skip_if(is.null(root), "the repository is not around the tests")
  costs <- read_algo_costs(file.path(root, "fixtures", "algolab", "costs.csv"))
  expect_equal(names(costs), c("algorithm", "n", "comparisons", "bound"))
  expect_equal(nrow(costs), 28)
  table <- growth_table(costs)
  expect_equal(table$algorithm, c("linear_search", "binary_search", "insertion_sort", "merge_sort"))
  expect_equal(table$measured, c("linear", "logarithmic", "quadratic", "linearithmic"))
  expect_equal(table$proved, c("linear", "logarithmic", "quadratic", "linearithmic"))
  expect_true(all(table$within))
  expect_true(all(table$tight))
  expect_equal(table$sizes, rep(7, 4))
  expect_equal(table$largest, rep(512, 4))
  expect_true(all(table$r_squared > 0.999))
  # A failed search and a falling list reach their bounds; the other two stay below theirs.
  expect_equal(table$ratio[c(1, 3)], c(1, 1))
  expect_true(all(table$ratio[c(2, 4)] < 1 & table$ratio[c(2, 4)] > 0.5))
})

test_that("P05-06-01 a table of costs is checked when it is read, and a count above its bound is seen", {
  path <- tempfile(fileext = ".csv")
  rows <- data.frame(algorithm = "linear_search", n = c(4, 8, 16), comparisons = c(4, 8, 17), bound = c(4, 8, 16))
  utils::write.csv(rows, path, row.names = FALSE)
  table <- growth_table(read_algo_costs(path))
  expect_false(table$within)
  expect_gt(table$ratio, 1)
  # A cost that grows slower than its bound is within it and not tight.
  slack <- data.frame(algorithm = "search", n = sizes, comparisons = log2(sizes), bound = sizes)
  loose <- growth_table(slack)
  expect_true(loose$within)
  expect_false(loose$tight)
  expect_equal(c(loose$measured, loose$proved), c("logarithmic", "linear"))

  utils::write.csv(rows[, c("algorithm", "n")], path, row.names = FALSE)
  expect_error(read_algo_costs(path), "has no column comparisons, bound")
  utils::write.csv(rows[0, ], path, row.names = FALSE)
  expect_error(read_algo_costs(path), "holds no measurement")
  expect_error(read_algo_costs(file.path(tempdir(), "no-such-costs.csv")), "there is no file")
})
