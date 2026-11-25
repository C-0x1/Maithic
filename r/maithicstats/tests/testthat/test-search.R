# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

write_query_set <- function(queries, name = "demo") {
  write_document(list(schema = "maithic.search.queryset/1", name = name, queries = queries))
}

write_rankings <- function(engine, results, queryset = "demo") {
  write_document(list(schema = "maithic.search.rankings/1", engine = engine, queryset = queryset, results = results))
}

ranked <- function(query, ids, ms = 1) list(query = query, ranked = as.list(ids), elapsed_ms = ms)

demo_queries <- list(
  list(id = "q1", text = "addition is commutative", relevant = list(list(id = "A", grade = 3), "B")),
  list(id = "q2", text = "length of an append", relevant = list("C")),
  list(id = "q3", text = "sorted lists", relevant = list(list(id = "D", grade = 2), list(id = "E", grade = 2), "F"))
)

demo_set <- function() read_query_set(write_query_set(demo_queries))
engine_x <- function() read_rankings(write_rankings("x", list(ranked("q1", c("A", "Z", "B"), 1), ranked("q2", c("Y", "C"), 2), ranked("q3", c("Q", "R", "S"), 3))))
engine_y <- function() read_rankings(write_rankings("y", list(ranked("q1", c("A", "B"), 0.5), ranked("q2", "C", 0.5), ranked("q3", c("D", "E", "F"), 0.5))))

test_that("P04-06-01 query sets and rankings are read and validated", {
  set <- demo_set()
  expect_s3_class(set, "maithic_query_set")
  expect_equal(set$name, "demo")
  expect_equal(set$queries$id, c("q1", "q2", "q3"))
  expect_equal(set$relevant$id, c("A", "B", "C", "D", "E", "F"))
  expect_equal(set$relevant$grade, c(3, 1, 1, 2, 2, 1))
  x <- engine_x()
  expect_equal(x$engine, "x")
  expect_equal(x$ranked$q2, c("Y", "C"))
  expect_equal(x$results$elapsed_ms, c(1, 2, 3))
  query <- function(...) list(list(id = "q", text = "t", ...))
  expect_error(read_query_set(write_query_set(list())), "lists no queries")
  expect_error(read_query_set(write_query_set(query(relevant = list()))), "no relevant declaration")
  expect_error(read_query_set(write_query_set(query(relevant = list(list(id = "A", grade = 0))))), "positive whole numbers")
  expect_error(read_query_set(write_query_set(query(relevant = list(list(id = "A", grade = 1.5))))), "positive whole numbers")
  expect_error(read_query_set(write_query_set(query(relevant = list("A", "A")))), "lists a declaration twice")
  expect_error(read_query_set(write_query_set(c(demo_queries, demo_queries[1]))), "query id appears twice")
  expect_error(read_query_set(write_rankings("x", list())), "not a query set")
  expect_error(read_rankings(write_rankings("", list(ranked("q1", "A")))), "names no engine")
  expect_error(read_rankings(write_rankings("x", list())), "holds no results")
  expect_error(read_rankings(write_rankings("x", list(ranked("q1", "A"), ranked("q1", "B")))), "its own query id")
  expect_error(read_rankings(write_rankings("x", list(ranked("q1", c("A", "A"))))), "lists a declaration twice")
  partial <- read_rankings(write_rankings("partial", list(ranked("q1", "A"))))
  expect_error(recall_at_k(set, partial), "engine partial has no result for query q2")
  extra <- read_rankings(write_rankings("extra", list(ranked("q1", "A"), ranked("q2", "C"), ranked("q3", "D"), ranked("q9", "A"))))
  expect_error(recall_at_k(set, extra), "answers query q9")
})

test_that("P04-06-02 recall at k averages the share of relevant declarations found", {
  recall <- recall_at_k(demo_set(), engine_x(), k = c(1, 2, 3))
  expect_equal(recall$k, 1:3)
  expect_equal(recall$recall, c(1 / 6, 1 / 2, 2 / 3))
  expect_equal(recall$queries, c(3, 3, 3))
  expect_true(all(recall$lower <= recall$recall & recall$recall <= recall$upper))
  expect_equal(recall_at_k(demo_set(), engine_x(), k = c(1, 2, 3)), recall)
  expect_equal(recall_at_k(demo_set(), engine_y(), k = 3)[c("recall", "lower", "upper")], data.frame(recall = 1, lower = 1, upper = 1))
  expect_error(recall_at_k(demo_set(), engine_x(), k = 0), "positive whole numbers")
  expect_error(recall_at_k(list(), engine_x()), "read_query_set")
  expect_error(recall_at_k(demo_set(), list()), "read_rankings")
})

test_that("P04-06-03 mean reciprocal rank uses the first relevant result", {
  mrr <- mean_reciprocal_rank(demo_set(), engine_x())
  expect_equal(mrr[c("engine", "queries", "mrr")], data.frame(engine = "x", queries = 3L, mrr = 0.5, stringsAsFactors = FALSE))
  expect_true(mrr$lower <= 0.5 && 0.5 <= mrr$upper)
  expect_equal(mean_reciprocal_rank(demo_set(), engine_y())$mrr, 1)
})

test_that("P04-06-04 nDCG rewards high grades at high ranks", {
  ndcg <- ndcg_at_k(demo_set(), engine_x(), k = 3)
  first <- (7 + 1 / log2(4)) / (7 + 1 / log2(3))
  second <- (1 / log2(3)) / 1
  expect_equal(ndcg$ndcg, (first + second + 0) / 3)
  expect_equal(ndcg_at_k(demo_set(), engine_y(), k = c(1, 10))$ndcg, c(1, 1))
  swapped <- read_rankings(write_rankings("swapped", list(ranked("q1", c("B", "A")), ranked("q2", "C"), ranked("q3", c("F", "E", "D")))))
  expect_lt(ndcg_at_k(demo_set(), swapped, k = 3)$ndcg, 1)
  expect_equal(recall_at_k(demo_set(), swapped, k = 3)$recall, 1)
  expect_equal(maithicstats:::discounted_gain(c(3, 0, 1), 3), 7 + 0.5)
  expect_equal(maithicstats:::discounted_gain(c(3, 0, 1), 1), 7)
  expect_equal(maithicstats:::discounted_gain(integer(), 5), 0)
})

test_that("P04-06-05 engines are compared side by side on one query set", {
  table <- engine_comparison(demo_set(), list(engine_x(), engine_y()), k = 3)
  expect_equal(table$engine, c("y", "x"))
  expect_equal(table$recall, c(1, 2 / 3))
  expect_equal(table$mrr, c(1, 0.5))
  expect_equal(table$median_ms, c(0.5, 2))
  expect_equal(engine_comparison(demo_set(), engine_x(), k = 3)$engine, "x")
  expect_error(engine_comparison(demo_set(), list(engine_x(), engine_x())), "two rankings are called x")
  expect_error(engine_comparison(demo_set(), list()), "read_rankings")
  expect_error(engine_comparison(demo_set(), engine_x(), k = c(1, 2)), "one cutoff")
})

test_that("P04-06-06 query latency is summarised per engine", {
  latency <- query_latency(list(engine_x(), engine_y()))
  expect_equal(latency$engine, c("x", "y"))
  expect_equal(latency$n, c(3, 3))
  expect_equal(latency$median, c(2, 0.5))
  expect_equal(latency$max, c(3, 0.5))
  expect_equal(names(query_latency(engine_x(), probs = 0.5)), c("engine", "n", "median", "p50", "mean", "max"))
})

test_that("P04-06-08 the report table names the query set and its size", {
  lines <- retrieval_report_table(demo_set(), list(engine_x(), engine_y()), k = 3)
  expect_equal(lines[1], "Query set demo: 3 queries, 6 relevant declarations.")
  expect_equal(lines[3], "| Engine | Queries | Recall@3 | MRR | nDCG@3 | Median (ms) |")
  expect_equal(lines[5], "| y | 3 | 100.0% | 1 | 1 | 0.5 |")
  expect_equal(lines[6], "| x | 3 | 66.7% | 0.5 | 0.538 | 2 |")
})
