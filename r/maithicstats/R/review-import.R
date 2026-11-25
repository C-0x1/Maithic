# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the reports that `maithic review --format json` writes.

# The severities of a finding, from the one that fails a review down.
finding_severities <- function() c("error", "warning", "info")

empty_findings <- function() {
  data.frame(run = integer(), repository = character(), file = character(), line = integer(), declaration = character(),
             rule = character(), origin = character(), severity = character(), message = character(), suggestion = logical(),
             before = character(), after = character(), stringsAsFactors = FALSE)
}

parse_review_report <- function(path, index, repository) {
  report <- read_document(path, "maithic.review.report/1", "a review report")
  summary <- report$summary
  files <- data.frame(run = rep(index, length(report$files)), repository = rep(repository, length(report$files)),
                      file = texts_of(report$files, "file"), declarations = counts_of(report$files, "declarations"),
                      findings = vapply(report$files, function(file) length(file$findings), integer(1), USE.NAMES = FALSE),
                      suppressed = counts_of(report$files, "suppressed"), stringsAsFactors = FALSE)
  items <- do.call(c, c(list(list()), lapply(report$files, function(file) file$findings)))
  edits <- lapply(items, function(item) item$edit)
  findings <- data.frame(
    run = rep(index, length(items)), repository = rep(repository, length(items)), file = texts_of(items, "file"),
    line = counts_of(items, "line", NA_integer_), declaration = texts_of(items, "declaration", NA_character_), rule = texts_of(items, "rule"),
    origin = texts_of(items, "origin"), severity = texts_of(items, "severity"), message = texts_of(items, "message"),
    suggestion = !vapply(edits, is.null, logical(1)),
    before = vapply(edits, function(edit) if (is.null(edit)) NA_character_ else text_field(edit, "before", NA_character_), character(1)),
    after = vapply(edits, function(edit) if (is.null(edit)) NA_character_ else text_field(edit, "after", NA_character_), character(1)),
    stringsAsFactors = FALSE
  )
  unknown <- setdiff(findings$severity, finding_severities())
  if (length(unknown) > 0) stop(sprintf("%s: a finding of the unknown severity '%s'", path, unknown[1]), call. = FALSE)
  counted <- vapply(finding_severities(), function(level) sum(findings$severity == level), integer(1))
  stated <- vapply(finding_severities(), function(level) count_field(summary, level, NA_integer_), integer(1))
  if (count_field(summary, "findings", NA_integer_) != nrow(findings) || !identical(unname(counted), unname(stated))) {
    stop(sprintf("%s: the summary states %d finding(s) (%s), and the report holds %d (%s)", path, count_field(summary, "findings", NA_integer_),
                 paste(sprintf("%d %s", stated, finding_severities()), collapse = ", "), nrow(findings),
                 paste(sprintf("%d %s", counted, finding_severities()), collapse = ", ")), call. = FALSE)
  }
  if (count_field(summary, "files", NA_integer_) != nrow(files)) {
    stop(sprintf("%s: the summary states %d file(s), and the report holds %d", path, count_field(summary, "files", NA_integer_), nrow(files)), call. = FALSE)
  }
  run <- data.frame(run = index, path = path, repository = repository, profile = text_field(report, "profile"), files = nrow(files),
                    declarations = sum(files$declarations), findings = nrow(findings), errors = counted[["error"]],
                    warnings = counted[["warning"]], notes = counted[["info"]], suppressed = count_field(summary, "suppressed"),
                    suggestions = sum(findings$suggestion), outcome = if (counted[["error"]] > 0) "failed" else "passed",
                    stringsAsFactors = FALSE)
  list(run = run, files = files, findings = findings)
}

#' Read review reports
#'
#' Loads the reports that `maithic review --format json` writes, one for each
#' reviewed repository or each review of the same repository over time. Each
#' report is checked against its own summary: the findings it holds must be
#' the findings it counts.
#'
#' A review fails when one of its findings is an error, which is also when
#' the command exits with a failure; that is the `outcome` of a run.
#'
#' @param paths Paths of review reports. When the vector has names, they name
#'   the repositories.
#' @param repository The repository each report is about, one name for each
#'   path; by default the names of `paths`, or the file names of the reports
#'   without their extension.
#' @return An object of class `maithic_review_runs`, a list of three data
#'   frames: `runs` (one row per report, with the counts by severity and the
#'   outcome), `files` (one row per reviewed file) and `findings` (one row per
#'   finding: where it is, the rule, where the finding comes from, its
#'   severity, the comment, and the suggested edit when there is one).
#' @export
read_review_runs <- function(paths, repository = NULL) {
  if (!is.character(paths) || length(paths) == 0) stop("no review reports given", call. = FALSE)
  if (is.null(repository)) {
    repository <- if (!is.null(names(paths)) && all(nzchar(names(paths)))) names(paths) else sub("\\.json$", "", basename(paths))
  }
  if (!is.character(repository) || length(repository) != length(paths)) stop("`repository` must name one repository for each report", call. = FALSE)
  parsed <- lapply(seq_along(paths), function(i) parse_review_report(paths[[i]], i, repository[[i]]))
  bind <- function(name, template) {
    out <- bind_frames(lapply(parsed, `[[`, name), template)
    rownames(out) <- NULL
    out
  }
  runs <- do.call(rbind, lapply(parsed, `[[`, "run"))
  rownames(runs) <- NULL
  structure(list(
    runs = runs,
    files = bind("files", data.frame(run = integer(), repository = character(), file = character(), declarations = integer(),
                                     findings = integer(), suppressed = integer(), stringsAsFactors = FALSE)),
    findings = bind("findings", empty_findings())
  ), class = "maithic_review_runs")
}

need_review_runs <- function(runs) {
  if (!inherits(runs, "maithic_review_runs")) stop("`runs` must come from read_review_runs()", call. = FALSE)
  runs
}
