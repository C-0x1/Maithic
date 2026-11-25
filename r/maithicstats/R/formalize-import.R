# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the runs that `maithic formalize` writes.

# What can become of a candidate statement, from the one that is chosen to
# the one that never elaborated.
candidate_verdicts <- function() c("chosen", "kept", "duplicate", "rejected", "failed")

# The checks made on a statement that elaborates, in the order they are made.
faith_checks <- function() c("vacuity", "counterexample", "triviality", "numbers", "variables", "traps", "back-translation", "round-trip")

# What a check can say.
check_outcomes <- function() c("passed", "failed", "flagged", "skipped")

# What a run counts in its summary.
formalize_counts <- function() c("drafted", "elaborated_first_try", "repaired", "elaborated", "failed", "duplicates", "rejected", "kept")

empty_candidates <- function() {
  data.frame(run = integer(), label = character(), id = integer(), verdict = character(), origin = character(), elaborated = logical(),
             first_try = logical(), repairs = integer(), rank = integer(), score = numeric(), agreement = integer(), reason = character(),
             stringsAsFactors = FALSE)
}

empty_checks <- function() {
  data.frame(run = integer(), label = character(), id = integer(), check = character(), outcome = character(), note = character(),
             stringsAsFactors = FALSE)
}

empty_attempts <- function() {
  data.frame(run = integer(), label = character(), id = integer(), round = integer(), elaborated = logical(), error_class = character(),
             stringsAsFactors = FALSE)
}

parse_formalize_run <- function(path, index, label) {
  document <- read_document(path, "maithic.formalize.run/1", "a formalization run")
  items <- document$candidates
  elaborations <- lapply(items, function(item) item$elaboration)
  candidates <- data.frame(
    run = rep(index, length(items)), label = rep(label, length(items)), id = counts_of(items, "id", NA_integer_), verdict = texts_of(items, "verdict"),
    origin = vapply(items, function(item) text_field(item$provenance, "origin"), character(1)),
    elaborated = texts_of(elaborations, "status") == "elaborated", first_try = flags_of(elaborations, "first_try"),
    repairs = counts_of(elaborations, "repairs"), rank = counts_of(items, "rank", NA_integer_), score = numbers_of(items, "score"),
    agreement = counts_of(items, "agreement", NA_integer_), reason = texts_of(items, "reason", NA_character_), stringsAsFactors = FALSE
  )
  unknown <- setdiff(candidates$verdict, candidate_verdicts())
  if (length(unknown) > 0) stop(sprintf("%s: a candidate with the unknown verdict '%s'", path, unknown[1]), call. = FALSE)
  if (any(candidates$elaborated == (candidates$verdict == "failed"))) {
    stop(sprintf("%s: a candidate that failed is said to elaborate, or one that elaborates to have failed", path), call. = FALSE)
  }
  checks <- bind_frames(lapply(seq_along(items), function(i) {
    made <- items[[i]]$checks
    if (length(made) == 0) return(NULL)
    data.frame(run = index, label = label, id = candidates$id[i], check = texts_of(made, "name"), outcome = texts_of(made, "outcome"),
               note = texts_of(made, "note"), stringsAsFactors = FALSE)
  }), empty_checks())
  unknown <- setdiff(checks$check, faith_checks())
  if (length(unknown) > 0) stop(sprintf("%s: a check with the unknown name '%s'", path, unknown[1]), call. = FALSE)
  unknown <- setdiff(checks$outcome, check_outcomes())
  if (length(unknown) > 0) stop(sprintf("%s: a check with the unknown outcome '%s'", path, unknown[1]), call. = FALSE)
  attempts <- bind_frames(lapply(seq_along(items), function(i) {
    history <- elaborations[[i]]$history
    if (length(history) == 0) return(NULL)
    data.frame(run = index, label = label, id = candidates$id[i], round = counts_of(history, "round"), elaborated = flags_of(history, "elaborated"),
               error_class = texts_of(history, "error_class", NA_character_), stringsAsFactors = FALSE)
  }), empty_attempts())
  counted <- c(drafted = nrow(candidates), elaborated_first_try = sum(candidates$first_try), repaired = sum(candidates$elaborated & !candidates$first_try),
               elaborated = sum(candidates$elaborated), failed = sum(candidates$verdict == "failed"), duplicates = sum(candidates$verdict == "duplicate"),
               rejected = sum(candidates$verdict == "rejected"), kept = sum(candidates$verdict %in% c("chosen", "kept")))
  stated <- vapply(names(counted), function(name) count_field(document$summary, name, NA_integer_), integer(1))
  if (!identical(unname(as.integer(counted)), unname(stated))) {
    differs <- names(counted)[which(as.integer(counted) != stated | is.na(stated))[1]]
    stop(sprintf("%s: the summary states %s for '%s', and the run holds %d", path, format(stated[[differs]]), differs, counted[[differs]]), call. = FALSE)
  }
  chosen <- candidates$id[candidates$verdict == "chosen"]
  if (length(chosen) > 1) stop(sprintf("%s: %d candidates are chosen", path, length(chosen)), call. = FALSE)
  status <- text_field(document, "status")
  if (!status %in% c("formalized", "flagged", "none")) stop(sprintf("%s: a run with the unknown status '%s'", path, status), call. = FALSE)
  if ((status == "none") != (length(chosen) == 0)) stop(sprintf("%s: the status is '%s', and %d candidate(s) are chosen", path, status, length(chosen)), call. = FALSE)
  gateway <- document$gateway
  run <- data.frame(run = index, path = path, label = label, origin = text_field(document$problem, "origin"), task = text_field(document$problem, "task"),
                    status = status, chosen = if (length(chosen) == 1) chosen else NA_integer_, stringsAsFactors = FALSE)
  for (name in names(counted)) run[[name]] <- counted[[name]]
  run$model_calls <- if (is.null(gateway)) NA_integer_ else count_field(gateway, "calls")
  list(run = run, candidates = candidates, checks = checks, attempts = attempts)
}

#' Read formalization runs
#'
#' Loads the runs that `maithic formalize` writes with `--out`: one for each
#' problem that was stated in Lean. Each run is checked against its own
#' summary: the candidates it holds must be the candidates it counts.
#'
#' @param paths Paths of formalization runs. When the vector has names, they
#'   label the runs.
#' @param label A label for each run; by default the names of `paths`, or
#'   the file names of the runs without their extension.
#' @return An object of class `maithic_formalize_runs`, a list of four data
#'   frames: `runs` (one row per problem: where it came from, what it asks,
#'   the `status` of the run, the candidate that was `chosen`, the counts of
#'   its summary and the model calls made), `candidates` (one row per
#'   statement: its `verdict`, whether it `elaborated` and whether at the
#'   `first_try`, the `repairs` that were compiled, its `rank`, `score` and
#'   `agreement` where it has them, and the `reason` it is out), `checks`
#'   (one row per check made on a statement, with its `outcome` and what was
#'   found) and `attempts` (one row per statement that was compiled, with
#'   the class of the error when Lean rejected it).
#' @export
read_formalize_runs <- function(paths, label = NULL) {
  if (!is.character(paths) || length(paths) == 0) stop("no formalization runs given", call. = FALSE)
  if (is.null(label)) label <- if (!is.null(names(paths)) && all(nzchar(names(paths)))) names(paths) else sub("\\.json$", "", basename(paths))
  if (!is.character(label) || length(label) != length(paths)) stop("`label` must name one label for each run", call. = FALSE)
  parsed <- lapply(seq_along(paths), function(i) parse_formalize_run(paths[[i]], i, label[[i]]))
  runs <- do.call(rbind, lapply(parsed, `[[`, "run"))
  rownames(runs) <- NULL
  bound <- function(part, template) {
    frame <- bind_frames(lapply(parsed, `[[`, part), template)
    rownames(frame) <- NULL
    frame
  }
  structure(list(runs = runs, candidates = bound("candidates", empty_candidates()), checks = bound("checks", empty_checks()),
                 attempts = bound("attempts", empty_attempts())),
            class = "maithic_formalize_runs")
}

need_formalize_runs <- function(runs) {
  if (!inherits(runs, "maithic_formalize_runs")) stop("`runs` must come from read_formalize_runs()", call. = FALSE)
  runs
}
