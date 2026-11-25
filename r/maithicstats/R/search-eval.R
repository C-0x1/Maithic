# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Evaluation of retrieval: query sets, rankings, and the metrics that compare them.

#' Read a query set
#'
#' A query set is the ground truth of a retrieval evaluation: queries, and
#' for each the declarations that answer it, optionally graded.
#'
#' ```json
#' {"schema": "maithic.search.queryset/1", "name": "demo", "queries": [
#'   {"id": "q1", "text": "addition is commutative",
#'    "relevant": [{"id": "Nat.add_comm", "grade": 3}, "Nat.add_left_comm"]}]}
#' ```
#'
#' A relevant declaration is an object with an `id` and a positive whole
#' `grade`, or just its name, which means grade 1.
#'
#' @param path Path of a JSON file with schema `maithic.search.queryset/1`.
#' @return A list of class `maithic_query_set` with `name`, `queries` (a data
#'   frame with `id` and `text`) and `relevant` (a data frame with `query`,
#'   `id` and `grade`).
#' @export
read_query_set <- function(path) {
  document <- read_document(path, "maithic.search.queryset/1", "a query set")
  if (length(document$queries) == 0) stop(sprintf("%s lists no queries", path), call. = FALSE)
  queries <- list()
  relevant <- list()
  for (query in document$queries) {
    id <- text_field(query, "id")
    if (!nzchar(id)) stop(sprintf("%s: a query has no id", path), call. = FALSE)
    if (length(query$relevant) == 0) stop(sprintf("%s: query %s has no relevant declaration", path, id), call. = FALSE)
    queries[[length(queries) + 1]] <- data.frame(id = id, text = text_field(query, "text"), stringsAsFactors = FALSE)
    for (entry in query$relevant) {
      name <- if (is.list(entry)) text_field(entry, "id") else as.character(entry)
      grade <- if (is.list(entry)) number_field(entry, "grade", 1) else 1
      if (!nzchar(name)) stop(sprintf("%s: query %s lists a declaration without a name", path, id), call. = FALSE)
      if (is.na(grade) || grade < 1 || grade != round(grade)) stop(sprintf("%s: query %s grades %s with %s; grades are positive whole numbers", path, id, name, format(grade)), call. = FALSE)
      relevant[[length(relevant) + 1]] <- data.frame(query = id, id = name, grade = as.integer(grade), stringsAsFactors = FALSE)
    }
  }
  queries <- do.call(rbind, queries)
  relevant <- do.call(rbind, relevant)
  if (anyDuplicated(queries$id)) stop(sprintf("%s: a query id appears twice", path), call. = FALSE)
  if (anyDuplicated(relevant[c("query", "id")])) stop(sprintf("%s: a query lists a declaration twice", path), call. = FALSE)
  structure(list(name = text_field(document, "name"), queries = queries, relevant = relevant), class = "maithic_query_set")
}

#' Read the rankings of an engine
#'
#' What a retrieval engine returned for the queries of a query set: for each
#' query the declarations in the order the engine ranked them, and how long
#' the query took.
#'
#' ```json
#' {"schema": "maithic.search.rankings/1", "engine": "hashed-ngram", "queryset": "demo",
#'  "results": [{"query": "q1", "ranked": ["Nat.add_comm", "Nat.mul_comm"], "elapsed_ms": 0.4}]}
#' ```
#'
#' @param path Path of a JSON file with schema `maithic.search.rankings/1`.
#' @return A list of class `maithic_rankings` with `engine`, `queryset`,
#'   `results` (a data frame with `query` and `elapsed_ms`) and `ranked` (a
#'   list of character vectors named by query).
#' @export
read_rankings <- function(path) {
  document <- read_document(path, "maithic.search.rankings/1", "a rankings file")
  engine <- text_field(document, "engine")
  if (!nzchar(engine)) stop(sprintf("%s names no engine", path), call. = FALSE)
  if (length(document$results) == 0) stop(sprintf("%s holds no results", path), call. = FALSE)
  queries <- vapply(document$results, function(result) text_field(result, "query"), character(1))
  if (any(!nzchar(queries)) || anyDuplicated(queries)) stop(sprintf("%s: every result needs its own query id", path), call. = FALSE)
  ranked <- lapply(document$results, function(result) texts_field(result, "ranked"))
  if (any(vapply(ranked, anyDuplicated, integer(1)) > 0)) stop(sprintf("%s: a ranking lists a declaration twice", path), call. = FALSE)
  names(ranked) <- queries
  results <- data.frame(query = queries, elapsed_ms = vapply(document$results, function(result) number_field(result, "elapsed_ms"), numeric(1)),
                        stringsAsFactors = FALSE)
  structure(list(engine = engine, queryset = text_field(document, "queryset"), results = results, ranked = ranked), class = "maithic_rankings")
}

# The grades of a query's ranked declarations, in rank order, zero for those that are not relevant.
ranked_grades <- function(queryset, rankings) {
  if (!inherits(queryset, "maithic_query_set")) stop("`queryset` must come from read_query_set()", call. = FALSE)
  if (!inherits(rankings, "maithic_rankings")) stop("`rankings` must come from read_rankings()", call. = FALSE)
  ids <- queryset$queries$id
  missing <- setdiff(ids, names(rankings$ranked))
  if (length(missing) > 0) stop(sprintf("engine %s has no result for query %s", rankings$engine, missing[1]), call. = FALSE)
  extra <- setdiff(names(rankings$ranked), ids)
  if (length(extra) > 0) stop(sprintf("engine %s answers query %s, which the query set does not hold", rankings$engine, extra[1]), call. = FALSE)
  lapply(stats::setNames(ids, ids), function(id) {
    truth <- queryset$relevant[queryset$relevant$query == id, , drop = FALSE]
    grades <- truth$grade[match(rankings$ranked[[id]], truth$id)]
    grades[is.na(grades)] <- 0L
    list(grades = grades, ideal = sort(truth$grade, decreasing = TRUE))
  })
}

check_cutoffs <- function(k) {
  if (!is.numeric(k) || length(k) == 0 || any(k < 1) || any(k != round(k))) stop("`k` must hold positive whole numbers", call. = FALSE)
  as.integer(k)
}

mean_with_interval <- function(values, conf, reps, seed) {
  interval <- bootstrap_interval(values, mean, reps = reps, conf = conf, seed = seed)
  data.frame(queries = length(values), value = mean(values), lower = interval$lower, upper = interval$upper)
}

#' Recall at k
#'
#' For each cutoff `k`, the share of a query's relevant declarations found
#' among the first `k` results, averaged over the queries, with a bootstrap
#' interval over queries.
#'
#' @param queryset A query set from [read_query_set()].
#' @param rankings Rankings from [read_rankings()].
#' @param k Cutoffs.
#' @param conf Confidence level of the intervals.
#' @param reps Bootstrap replicates.
#' @param seed Seed of the bootstrap.
#' @return A data frame with `engine`, `k`, `queries`, `recall`, `lower` and
#'   `upper`.
#' @export
recall_at_k <- function(queryset, rankings, k = c(1, 5, 10), conf = 0.95, reps = 2000, seed = 1) {
  k <- check_cutoffs(k)
  graded <- ranked_grades(queryset, rankings)
  rows <- lapply(k, function(cutoff) {
    values <- vapply(graded, function(q) sum(utils::head(q$grades, cutoff) > 0) / length(q$ideal), numeric(1))
    summary <- mean_with_interval(values, conf, reps, seed)
    data.frame(engine = rankings$engine, k = cutoff, queries = summary$queries, recall = summary$value, lower = summary$lower,
               upper = summary$upper, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Mean reciprocal rank
#'
#' The reciprocal of the rank of the first relevant result, zero when the
#' ranking holds none, averaged over the queries, with a bootstrap interval.
#'
#' @inheritParams recall_at_k
#' @return A one-row data frame with `engine`, `queries`, `mrr`, `lower` and
#'   `upper`.
#' @export
mean_reciprocal_rank <- function(queryset, rankings, conf = 0.95, reps = 2000, seed = 1) {
  graded <- ranked_grades(queryset, rankings)
  values <- vapply(graded, function(q) {
    hit <- which(q$grades > 0)
    if (length(hit) == 0) 0 else 1 / hit[1]
  }, numeric(1))
  summary <- mean_with_interval(values, conf, reps, seed)
  data.frame(engine = rankings$engine, queries = summary$queries, mrr = summary$value, lower = summary$lower, upper = summary$upper,
             stringsAsFactors = FALSE)
}

# Discounted cumulative gain of grades in rank order, cut at k.
discounted_gain <- function(grades, k) {
  grades <- utils::head(grades, k)
  if (length(grades) == 0) return(0)
  sum((2^grades - 1) / log2(seq_along(grades) + 1))
}

#' Normalized discounted cumulative gain
#'
#' The graded-relevance metric: each result contributes `2^grade - 1`,
#' discounted by the logarithm of its rank, and the sum is divided by the
#' best sum any ranking could reach. One is a perfect ranking.
#'
#' @inheritParams recall_at_k
#' @return A data frame with `engine`, `k`, `queries`, `ndcg`, `lower` and
#'   `upper`.
#' @export
ndcg_at_k <- function(queryset, rankings, k = 10, conf = 0.95, reps = 2000, seed = 1) {
  k <- check_cutoffs(k)
  graded <- ranked_grades(queryset, rankings)
  rows <- lapply(k, function(cutoff) {
    values <- vapply(graded, function(q) discounted_gain(q$grades, cutoff) / discounted_gain(q$ideal, cutoff), numeric(1))
    summary <- mean_with_interval(values, conf, reps, seed)
    data.frame(engine = rankings$engine, k = cutoff, queries = summary$queries, ndcg = summary$value, lower = summary$lower,
               upper = summary$upper, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

check_engines <- function(engines) {
  if (inherits(engines, "maithic_rankings")) engines <- list(engines)
  if (length(engines) == 0 || !all(vapply(engines, inherits, logical(1), "maithic_rankings"))) {
    stop("`engines` must be rankings from read_rankings(), or a list of them", call. = FALSE)
  }
  names <- vapply(engines, function(e) e$engine, character(1))
  if (anyDuplicated(names)) stop(sprintf("two rankings are called %s", names[anyDuplicated(names)]), call. = FALSE)
  engines
}

#' Latency of queries
#'
#' The distribution of the time one query takes, per engine.
#'
#' @param engines Rankings from [read_rankings()], or a list of them.
#' @param probs The upper quantiles to report.
#' @return A data frame with `engine` and the columns of
#'   [distribution_summary()], in milliseconds.
#' @export
query_latency <- function(engines, probs = c(0.9, 0.99)) {
  engines <- check_engines(engines)
  out <- do.call(rbind, lapply(engines, function(e) cbind(engine = e$engine, distribution_summary(e$results$elapsed_ms, probs))))
  rownames(out) <- NULL
  out
}

#' Compare engines side by side
#'
#' One row per engine on the same query set: recall and nDCG at `k`, mean
#' reciprocal rank, and the median query time.
#'
#' @param queryset A query set from [read_query_set()].
#' @param engines A list of rankings from [read_rankings()].
#' @param k The cutoff of recall and nDCG.
#' @return A data frame with `engine`, `queries`, `recall`, `mrr`, `ndcg`
#'   and `median_ms`, best recall first.
#' @export
engine_comparison <- function(queryset, engines, k = 10) {
  engines <- check_engines(engines)
  k <- check_cutoffs(k)
  if (length(k) != 1) stop("`k` must be one cutoff", call. = FALSE)
  rows <- lapply(engines, function(e) {
    graded <- ranked_grades(queryset, e)
    data.frame(
      engine = e$engine, queries = length(graded),
      recall = mean(vapply(graded, function(q) sum(utils::head(q$grades, k) > 0) / length(q$ideal), numeric(1))),
      mrr = mean(vapply(graded, function(q) if (any(q$grades > 0)) 1 / which(q$grades > 0)[1] else 0, numeric(1))),
      ndcg = mean(vapply(graded, function(q) discounted_gain(q$grades, k) / discounted_gain(q$ideal, k), numeric(1))),
      median_ms = stats::median(e$results$elapsed_ms, na.rm = TRUE), stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$recall, -out$mrr, out$engine, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Report table of a retrieval evaluation
#'
#' The comparison of engines as Markdown, for a report, headed by the name
#' and the size of the query set.
#'
#' @inheritParams engine_comparison
#' @return A character vector of Markdown lines.
#' @export
retrieval_report_table <- function(queryset, engines, k = 10) {
  table <- engine_comparison(queryset, engines, k)
  labels <- c(engine = "Engine", queries = "Queries", recall = sprintf("Recall@%d", k), mrr = "MRR", ndcg = sprintf("nDCG@%d", k),
              median_ms = "Median (ms)")
  c(sprintf("Query set %s: %d queries, %d relevant declarations.", queryset$name, nrow(queryset$queries), nrow(queryset$relevant)), "",
    markdown_table(table, percent = "recall", labels = labels))
}
