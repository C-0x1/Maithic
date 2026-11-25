# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the reports that `maithic mend repair` writes.

# The classes of breakage, in the order the command lists them.
breakage_classes <- function() c("unknown-identifier", "deprecated", "unsolved-goals", "type-mismatch", "suggestion", "other")

# The stages a breakage can be repaired at, from the cheapest remedy to the
# dearest, and `none` for a breakage that was left.
repair_stages <- function() c("rule", "model", "prove", "none")

empty_breakages <- function() {
  data.frame(run = integer(), label = character(), file = character(), declaration = character(), line = integer(), class = character(),
             severity = character(), repaired = logical(), stage = character(), rule = character(), round = integer(), proposer = character(),
             tried = integer(), changed_lines = integer(), reason = character(), stringsAsFactors = FALSE)
}

parse_mend_report <- function(path, index, label) {
  report <- read_document(path, "maithic.mend.report/1", "a repair report")
  summary <- report$summary
  items <- report$repairs
  breakages <- data.frame(
    run = rep(index, length(items)), label = rep(label, length(items)), file = rep(text_field(report, "file"), length(items)),
    declaration = texts_of(items, "declaration"), line = counts_of(items, "line", NA_integer_), class = texts_of(items, "class"),
    severity = texts_of(items, "severity"), repaired = texts_of(items, "outcome") == "repaired", stage = texts_of(items, "stage"),
    rule = texts_of(items, "rule", NA_character_), round = counts_of(items, "round", NA_integer_), proposer = texts_of(items, "proposer", NA_character_),
    tried = counts_of(items, "tried"), changed_lines = counts_of(items, "changed_lines"), reason = texts_of(items, "reason", NA_character_),
    stringsAsFactors = FALSE
  )
  unknown <- setdiff(breakages$class, breakage_classes())
  if (length(unknown) > 0) stop(sprintf("%s: a breakage of the unknown class '%s'", path, unknown[1]), call. = FALSE)
  unknown <- setdiff(breakages$stage, repair_stages())
  if (length(unknown) > 0) stop(sprintf("%s: a repair at the unknown stage '%s'", path, unknown[1]), call. = FALSE)
  if (any(breakages$repaired != (breakages$stage != "none"))) {
    stop(sprintf("%s: a breakage is repaired at no stage, or left at one", path), call. = FALSE)
  }
  counted <- c(breakages = nrow(breakages), repaired = sum(breakages$repaired), unrepaired = sum(!breakages$repaired),
               by_rule = sum(breakages$stage == "rule"), by_model = sum(breakages$stage == "model"), by_prover = sum(breakages$stage == "prove"))
  stated <- vapply(names(counted), function(name) count_field(summary, name, NA_integer_), integer(1))
  if (!identical(unname(as.integer(counted)), unname(stated))) {
    differs <- names(counted)[which(as.integer(counted) != stated | is.na(stated))[1]]
    stop(sprintf("%s: the summary states %s for '%s', and the report holds %d", path, format(stated[[differs]]), differs, counted[[differs]]), call. = FALSE)
  }
  gateway <- report$gateway
  run <- data.frame(run = index, path = path, label = label, file = text_field(report, "file"), breakages = counted[["breakages"]],
                    repaired = counted[["repaired"]], unrepaired = counted[["unrepaired"]], by_rule = counted[["by_rule"]],
                    by_model = counted[["by_model"]], by_prover = counted[["by_prover"]], remaining = count_field(summary, "remaining"),
                    clean = flag_field(summary, "clean"), checks = count_field(summary, "checks"),
                    model_calls = if (is.null(gateway)) NA_integer_ else count_field(gateway, "calls"), stringsAsFactors = FALSE)
  list(run = run, breakages = breakages)
}

#' Read repair runs
#'
#' Loads the reports that `maithic mend repair` writes: one for each file
#' that was repaired, or for each way the same file was repaired, as after a
#' migration that was run once with the rules alone and once with a model.
#' Each report is checked against its own summary: the repairs it holds must
#' be the repairs it counts.
#'
#' @param paths Paths of repair reports. When the vector has names, they
#'   label the runs.
#' @param label A label for each run; by default the names of `paths`, or
#'   the file names of the reports without their extension.
#' @return An object of class `maithic_mend_runs`, a list of two data frames:
#'   `runs` (one row per report: the file, the breakages found, how many were
#'   repaired at each stage and how many were left, what Lean still reported
#'   at the end, whether the file was `clean`, and the model calls made) and
#'   `breakages` (one row per breakage: the declaration, the `class` of the
#'   breakage, whether it was `repaired` and at which `stage`: `rule`,
#'   `model`, `prove`, or `none` for one that was left; the rule, the round or
#'   the proposer; how many edits or proofs were `tried`; and the lines the
#'   repair changed).
#' @export
read_mend_runs <- function(paths, label = NULL) {
  if (!is.character(paths) || length(paths) == 0) stop("no repair reports given", call. = FALSE)
  if (is.null(label)) label <- if (!is.null(names(paths)) && all(nzchar(names(paths)))) names(paths) else sub("\\.json$", "", basename(paths))
  if (!is.character(label) || length(label) != length(paths)) stop("`label` must name one label for each report", call. = FALSE)
  parsed <- lapply(seq_along(paths), function(i) parse_mend_report(paths[[i]], i, label[[i]]))
  runs <- do.call(rbind, lapply(parsed, `[[`, "run"))
  rownames(runs) <- NULL
  breakages <- bind_frames(lapply(parsed, `[[`, "breakages"), empty_breakages())
  rownames(breakages) <- NULL
  structure(list(runs = runs, breakages = breakages), class = "maithic_mend_runs")
}

need_mend_runs <- function(runs) {
  if (!inherits(runs, "maithic_mend_runs")) stop("`runs` must come from read_mend_runs()", call. = FALSE)
  runs
}
