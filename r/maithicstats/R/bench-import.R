# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

#' Read a benchmark run
#'
#' Reads a result file written by `maithic bench run --out FILE` and checks
#' that it is what it claims to be before any number is computed from it.
#'
#' @param path Path of a JSON file with schema `maithic.bench.run/1`.
#' @return A list with `meta` (suite-level fields) and `problems`, a data
#'   frame with one row per problem: `id`, `suite`, `tags` (a list column),
#'   `difficulty`, `status`, `solved`, `proposer`, `proof`, `attempts`,
#'   `rounds` and `elapsed_ms`.
#' @export
read_bench_run <- function(path) {
  if (!file.exists(path)) stop(sprintf("result file not found: %s", path), call. = FALSE)
  run <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(run$schema, "maithic.bench.run/1")) {
    stop(sprintf("%s is not a benchmark run (schema '%s')", path, format(run$schema)), call. = FALSE)
  }
  if (length(run$problems) == 0) stop(sprintf("%s lists no problems", path), call. = FALSE)
  statuses <- c("verified", "checked", "refuted", "conjectured", "unverified", "unknown", "failed", "abstained")
  field <- function(problem, name, default) if (is.null(problem[[name]])) default else problem[[name]]
  rows <- lapply(run$problems, function(problem) {
    if (is.null(problem$id) || is.null(problem$status)) stop("every problem needs an id and a status", call. = FALSE)
    if (!problem$status %in% statuses) stop(sprintf("problem %s has unknown status '%s'", problem$id, problem$status), call. = FALSE)
    data.frame(
      id = problem$id, suite = field(problem, "suite", ""), difficulty = as.integer(field(problem, "difficulty", 1L)),
      status = problem$status, solved = identical(problem$status, "verified"),
      proposer = field(problem, "proposer", ""), proof = field(problem, "proof", ""),
      attempts = as.integer(field(problem, "attempts", 0L)), rounds = as.integer(field(problem, "rounds", 0L)),
      elapsed_ms = as.numeric(field(problem, "elapsed_ms", NA_real_)), stringsAsFactors = FALSE
    )
  })
  problems <- do.call(rbind, rows)
  problems$tags <- lapply(run$problems, function(problem) as.character(unlist(field(problem, "tags", list()))))
  if (anyDuplicated(problems$id)) stop("problem identifiers must be unique", call. = FALSE)
  if (!is.null(run$total) && run$total != nrow(problems)) stop("the stated total does not match the number of problems", call. = FALSE)
  if (!is.null(run$verified) && run$verified != sum(problems$solved)) stop("the stated number of verified problems does not match the problems", call. = FALSE)
  meta <- run[setdiff(names(run), "problems")]
  list(meta = meta, problems = problems)
}
