# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the records that `maithic prove --json` writes.

#' Count the steps of a tactic proof
#'
#' Counts the tactic steps of a proof text: the pieces between line breaks,
#' semicolons and `<;>`, without the leading `by` and without case labels
#' such as `| zero =>`. It is a measure of the length of a proof, not of its
#' difficulty.
#'
#' @param proof A character vector of proof texts.
#' @return An integer vector, zero for an empty proof.
#' @export
count_proof_steps <- function(proof) {
  vapply(as.character(proof), function(text) {
    if (is.na(text)) return(0L)
    pieces <- unlist(strsplit(gsub("<;>", ";", text, fixed = TRUE), "[\n;]"))
    pieces <- trimws(sub("^\\s*\\|[^=]*=>", "", pieces))
    pieces <- trimws(sub("^by\\b", "", pieces))
    pieces <- sub("\\s+with$", "", pieces)
    sum(nzchar(pieces))
  }, integer(1), USE.NAMES = FALSE)
}

empty_results <- function() {
  data.frame(run = integer(), file = character(), theorem = character(), status = character(), proved = logical(),
             proposer = character(), provider = character(), model = character(), rounds = integer(), attempts = integer(),
             elapsed_ms = numeric(), lean_ms = numeric(), recheck_passed = logical(), recheck_ms = numeric(),
             proof_steps = integer(), proof = character(), stringsAsFactors = FALSE)
}

empty_attempts <- function() {
  data.frame(run = integer(), file = character(), theorem = character(), index = integer(), round = integer(),
             proposer = character(), accepted = logical(), error_class = character(), elapsed_ms = numeric(),
             stringsAsFactors = FALSE)
}

empty_usage <- function() {
  data.frame(run = integer(), purpose = character(), provider = character(), model = character(), served_by = character(),
             ok = logical(), error_class = character(), input_tokens = numeric(), output_tokens = numeric(),
             cached_tokens = numeric(), reasoning_tokens = numeric(),
             started_at = as.POSIXct(character(), tz = "UTC"), elapsed_ms = numeric(), stringsAsFactors = FALSE)
}

# The usage records of one gateway account, as rows.
usage_rows <- function(gateway, run) {
  records <- gateway$records
  if (is.null(records) || length(records) == 0) return(empty_usage())
  rows <- lapply(records, function(record) {
    provider <- text_field(record, "provider")
    if (!nzchar(provider)) stop("a usage record names no provider", call. = FALSE)
    stamp <- text_field(record, "started_at", NA_character_)
    data.frame(
      run = run, purpose = text_field(record, "purpose"), provider = provider, model = text_field(record, "model"),
      served_by = text_field(record, "served_by"), ok = flag_field(record, "ok", NA),
      error_class = text_field(record, "error_class", NA_character_),
      input_tokens = number_field(record, "input_tokens", 0), output_tokens = number_field(record, "output_tokens", 0),
      cached_tokens = number_field(record, "cached_tokens", 0), reasoning_tokens = number_field(record, "reasoning_tokens", 0),
      started_at = as.POSIXct(stamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      elapsed_ms = number_field(record, "elapsed_ms"), stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (anyNA(out$ok)) stop("a usage record does not say whether the call succeeded", call. = FALSE)
  out
}

parse_prove_run <- function(path, index) {
  run <- read_document(path, "maithic.prove.run/1", "a prove run")
  gateway <- if (is.null(run$gateway)) list() else run$gateway
  usage <- usage_rows(gateway, index)
  label <- function(values) if (length(values) == 0) "none" else paste(sort(unique(values), method = "radix"), collapse = "+")
  provider <- label(usage$provider)
  model <- label(usage$model)
  statuses <- result_statuses()
  attempts <- list()
  axioms <- list()
  recheck_axioms <- list()
  violations <- list()
  results <- lapply(run$results, function(result) {
    theorem <- text_field(result, "theorem")
    status <- text_field(result, "status")
    if (!nzchar(theorem)) stop(sprintf("%s: a result names no theorem", path), call. = FALSE)
    if (!status %in% statuses) stop(sprintf("%s: theorem %s has unknown status '%s'", path, theorem, status), call. = FALSE)
    tried <- if (is.null(result$attempts)) list() else result$attempts
    attempt_rows <- lapply(seq_along(tried), function(i) {
      data.frame(run = index, file = text_field(result, "file", text_field(run, "file")), theorem = theorem, index = i,
                 round = count_field(tried[[i]], "round"), proposer = text_field(tried[[i]], "proposer"),
                 accepted = flag_field(tried[[i]], "accepted", FALSE),
                 error_class = text_field(tried[[i]], "error_class", NA_character_),
                 elapsed_ms = number_field(tried[[i]], "elapsed_ms"), stringsAsFactors = FALSE)
    })
    attempts[[length(attempts) + 1]] <<- bind_frames(attempt_rows, empty_attempts())
    axioms[[length(axioms) + 1]] <<- texts_field(result, "axioms")
    recheck <- result$recheck
    recheck_axioms[[length(recheck_axioms) + 1]] <<- if (is.null(recheck)) character() else texts_field(recheck, "axioms")
    violations[[length(violations) + 1]] <<- if (is.null(result$decision)) character() else texts_field(result$decision, "violations")
    attempt_ms <- sum(vapply(tried, function(a) number_field(a, "elapsed_ms", 0), numeric(1)))
    recheck_ms <- if (is.null(recheck)) NA_real_ else number_field(recheck, "elapsed_ms")
    proof <- text_field(result, "proof")
    data.frame(
      run = index, file = text_field(result, "file", text_field(run, "file")), theorem = theorem, status = status,
      proved = identical(status, "verified"), proposer = text_field(result, "proposer"), provider = provider, model = model,
      rounds = count_field(result, "rounds_used"), attempts = length(tried), elapsed_ms = number_field(result, "elapsed_ms"),
      lean_ms = attempt_ms + if (is.na(recheck_ms)) 0 else recheck_ms,
      recheck_passed = if (is.null(recheck)) NA else flag_field(recheck, "passed", NA), recheck_ms = recheck_ms,
      proof_steps = count_proof_steps(proof), proof = proof, stringsAsFactors = FALSE
    )
  })
  results <- bind_frames(results, empty_results())
  results$axioms <- axioms
  results$recheck_axioms <- recheck_axioms
  results$violations <- violations
  if (anyDuplicated(results$theorem)) stop(sprintf("%s: a theorem is reported twice", path), call. = FALSE)
  summary <- run$summary
  if (!is.null(summary$total) && summary$total != nrow(results)) {
    stop(sprintf("%s: the stated total does not match the number of results", path), call. = FALSE)
  }
  if (!is.null(summary$verified) && summary$verified != sum(results$proved)) {
    stop(sprintf("%s: the stated number of verified results does not match the results", path), call. = FALSE)
  }
  run_row <- data.frame(
    run = index, path = path, file = text_field(run, "file"), mode = text_field(gateway, "mode", "unknown"),
    provider = provider, model = model, policy = if (is.null(run$policy)) "" else text_field(run$policy, "name"),
    results = nrow(results), verified = sum(results$proved), calls = nrow(usage), failed_calls = sum(!usage$ok),
    input_tokens = sum(usage$input_tokens), output_tokens = sum(usage$output_tokens), model_ms = sum(usage$elapsed_ms),
    stringsAsFactors = FALSE
  )
  list(run = run_row, results = results, attempts = bind_frames(attempts, empty_attempts()), usage = usage)
}

#' Read prove runs
#'
#' Reads result files written by `maithic prove FILE --json` and checks each
#' against what it states about itself before any number is computed: the
#' schema, the statuses, and the totals of its summary.
#'
#' @param paths Paths of JSON files with schema `maithic.prove.run/1`.
#' @return A list of class `maithic_prove_runs` with four data frames:
#'   * `runs`, one row per file: `run`, `path`, `file`, `mode`, `provider`,
#'     `model`, `policy`, `results`, `verified`, `calls`, `failed_calls`,
#'     `input_tokens`, `output_tokens`, `model_ms`;
#'   * `results`, one row per theorem: `run`, `file`, `theorem`, `status`,
#'     `proved`, `proposer`, `provider`, `model`, `rounds`, `attempts`,
#'     `elapsed_ms`, `lean_ms` (time in Lean: the attempts and the re-check),
#'     `recheck_passed`, `recheck_ms`, `proof_steps`, `proof`, and the list
#'     columns `axioms`, `recheck_axioms` and `violations`;
#'   * `attempts`, one row per candidate proof: `run`, `file`, `theorem`,
#'     `index`, `round`, `proposer`, `accepted`, `error_class`, `elapsed_ms`;
#'   * `usage`, one row per model call, as [read_usage()] returns it.
#'
#'   A run that made no model call has provider and model `"none"`.
#' @export
read_prove_runs <- function(paths) {
  if (length(paths) == 0) stop("no run files were given", call. = FALSE)
  pieces <- lapply(seq_along(paths), function(i) parse_prove_run(paths[[i]], i))
  results <- lapply(pieces, `[[`, "results")
  out <- list(
    runs = do.call(rbind, lapply(pieces, `[[`, "run")),
    results = bind_frames(results, empty_results()),
    attempts = bind_frames(lapply(pieces, `[[`, "attempts"), empty_attempts()),
    usage = bind_frames(lapply(pieces, `[[`, "usage"), empty_usage())
  )
  rownames(out$results) <- NULL
  structure(out, class = "maithic_prove_runs")
}
