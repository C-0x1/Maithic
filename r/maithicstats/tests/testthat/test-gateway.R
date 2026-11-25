# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

usage_demo <- function() {
  records <- c(demo_records, list(
    usage_record("formalize.draft", "openai", "g1", input = 4000, output = 1000, cached = 1000, reasoning = 400, ms = 2000, at = "2026-11-22T12:00:00Z"),
    usage_record("review.summary", "openai", "g1", ok = FALSE, ms = 60000, class = "timeout", at = "2026-11-22T12:05:00Z"),
    usage_record("experiment", "openai", "g1", ok = FALSE, ms = 5, at = "2026-11-23T08:00:00Z")
  ))
  read_usage(write_prove_run(list(prove_result("a")), records = records))
}

test_that("P10-09-01 usage records are imported from result files", {
  usage <- usage_demo()
  expect_equal(nrow(usage), 6)
  expect_equal(usage$provider, rep(c("anthropic", "openai"), each = 3))
  expect_equal(usage$ok, c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(usage$error_class, c(NA, NA, "rate_limited", NA, "timeout", NA))
  expect_equal(usage$cached_tokens, c(0, 0, 0, 1000, 0, 0))
  expect_s3_class(usage$started_at, "POSIXct")
  expect_equal(format(usage$started_at[2], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-11-20 23:59:59")
  expect_equal(usage_pillar(c("prove.repair", "formalize.draft", "experiment", "nnv.explain")), c("Prove", "Formalize", "Other", "NNV"))
  two <- read_usage(c(write_prove_run(list(prove_result("a")), records = demo_records), write_prove_run(list(prove_result("b")))))
  expect_equal(two$run, c(1, 1, 1))
  expect_error(read_usage(character()), "no result files")
  expect_error(read_usage(write_document(list(schema = "maithic.check.report/1"))), "not a result file with usage records")
  expect_error(read_usage(write_document(list(schema = "maithic.prove.run/1"))), "no gateway account")
  miscounted <- write_document(list(schema = "maithic.prove.run/1", gateway = list(mode = "live", calls = 5, records = demo_records)))
  expect_error(read_usage(miscounted), "stated number of calls")
  nameless <- write_document(list(schema = "maithic.prove.run/1", gateway = list(mode = "live", records = list(list(purpose = "x", ok = TRUE)))))
  expect_error(read_usage(nameless), "names no provider")
})

test_that("P10-09-01 the usage of the recorded runs is read as it is", {
  skip_if(is.null(fixture_root()), "the repository fixtures are not available")
  usage <- read_usage(run_fixture(c("prove-even.json", "prove-triangle-anthropic.json", "prove-triangle-openai.json", "prove-triangle-xai.json")))
  expect_equal(nrow(usage), 6)
  expect_equal(unique(usage$run), 2:4)
  expect_true(all(usage$ok) && all(usage$served_by == "replay") && !anyNA(usage$started_at))
  expect_equal(token_breakdown(usage, by = "provider")$calls, c(2, 2, 2))
})

test_that("P10-09-02 cost is summed by provider, model and pillar from a price table", {
  usage <- usage_demo()
  cost <- usage_cost(usage, demo_prices)
  expect_equal(cost$provider, c("anthropic", "openai"))
  expect_equal(cost$calls, c(3, 3))
  expect_equal(cost$cost, c(0.015, 0.0145))
  by_pillar <- usage_cost(usage, demo_prices, by = "pillar")
  expect_equal(by_pillar$pillar, c("Formalize", "Other", "Prove", "Review"))
  expect_equal(by_pillar$cost, c(0.0145, 0, 0.015, 0))
  without_cache_price <- usage_cost(usage, demo_prices[c("provider", "model", "input_per_mtok", "output_per_mtok")])
  expect_equal(without_cache_price$cost, c(0.015, 0.016))
  expect_error(usage_cost(usage, demo_prices[1, ]), "no price for openai / g1")
  expect_error(usage_cost(usage, demo_prices[c("provider", "model")]), "needs the columns")
  expect_error(usage_cost(usage, rbind(demo_prices, demo_prices[1, ])), "lists a model twice")
  expect_error(usage_cost(data.frame(a = 1), demo_prices), "read_usage")
})

test_that("P10-09-03 tokens are broken down by kind", {
  tokens <- token_breakdown(usage_demo())
  expect_equal(tokens$input_tokens, c(2500, 4000))
  expect_equal(tokens$output_tokens, c(500, 1000))
  expect_equal(tokens$cached_tokens, c(0, 1000))
  expect_equal(tokens$reasoning_tokens, c(0, 400))
  expect_equal(tokens$total_tokens, c(3000, 5000))
  expect_equal(tokens$output_share, c(1 / 6, 0.2))
  quiet <- read_usage(write_prove_run(list(prove_result("a")), records = list(usage_record("prove.propose", "mock", "mock"))))
  expect_true(is.na(token_breakdown(quiet)$output_share))
})

test_that("P10-09-04 latency is summarised over successful calls unless asked otherwise", {
  usage <- usage_demo()
  latency <- call_latency(usage)
  expect_equal(latency$model, c("g1", "m1"))
  expect_equal(latency$n, c(1, 2))
  expect_equal(latency$median, c(2000, 1000))
  everything <- call_latency(usage, by = "provider", include_failed = TRUE)
  expect_equal(everything$n, c(3, 3))
  expect_equal(everything$max, c(1200, 60000))
  expect_error(call_latency(usage[!usage$ok, ]), "no calls to summarise")
})

test_that("P10-09-05 error rates are reported per provider and per kind of failure", {
  errors <- provider_errors(usage_demo())
  expect_equal(errors$provider, c("anthropic", "anthropic", "openai", "openai", "openai"))
  expect_equal(errors$error_class, c("(any)", "rate_limited", "(any)", "timeout", "unclassified"))
  expect_equal(errors$failed, c(1, 1, 2, 1, 1))
  expect_equal(errors$rate, c(1 / 3, 1 / 3, 2 / 3, 1 / 3, 1 / 3))
  expect_equal(errors$lower[1], wilson_interval(1, 3)$lower)
  healthy <- provider_errors(read_usage(write_prove_run(list(prove_result("a")), records = demo_records[1:2])))
  expect_equal(healthy[c("error_class", "failed", "rate")], data.frame(error_class = "(any)", failed = 0L, rate = 0, stringsAsFactors = FALSE))
})

test_that("P10-09-06 spend is reported per day with a rolling mean", {
  spend <- daily_spend(usage_demo(), demo_prices, window = 2)
  expect_equal(format(spend$date), c("2026-11-20", "2026-11-21", "2026-11-22", "2026-11-23"))
  expect_equal(spend$calls, c(2, 0, 3, 1))
  expect_equal(spend$cost, c(0.015, 0, 0.0145, 0))
  expect_equal(spend$rolling_mean, c(0.015, 0.0075, 0.00725, 0.00725))
  expect_equal(daily_spend(usage_demo(), demo_prices, window = 10)$rolling_mean[4], 0.0295 / 4)
  expect_error(daily_spend(usage_demo(), demo_prices, window = 0), "positive whole number")
  undated <- usage_demo()
  undated$started_at[2] <- NA
  expect_error(daily_spend(undated, demo_prices), "needs a start time")
})

test_that("P10-09-08 the report table says who served the calls", {
  lines <- usage_report_table(usage_demo(), demo_prices)
  expect_equal(lines[1], "Served by: live (6).")
  expect_true("| Provider | Model | Calls | Input | Output | Total | Cost |" %in% lines)
  expect_true("| anthropic | m1 | 3 | 2,500 | 500 | 3,000 | 0.015 |" %in% lines)
  expect_true("| openai | 3 | 2 | 66.7% | 20.8% | 93.9% |" %in% lines)
  expect_true("| m1 | 2 | 1,000 | 1,160 | 1,196 | 1,000 | 1,200 |" %in% lines)
  expect_false(any(grepl("Cost", usage_report_table(usage_demo()))))
  silent <- read_usage(write_prove_run(list(prove_result("a"))))
  expect_equal(usage_report_table(silent), "No model call was made.")
})
