# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the jobs that `maithic nnv verify` writes.

# What a verification job can end with.
nnv_statuses <- function() c("verified", "refuted", "unknown", "failed")

# What a verifier can claim.
nnv_claims <- function() c("verified", "violated", "unknown")

parse_nnv_job <- function(path, index, label) {
  document <- read_document(path, "maithic.nnv.job/1", "a verification job")
  status <- text_field(document, "status")
  if (!status %in% nnv_statuses()) stop(sprintf("%s: a job with the unknown status '%s'", path, status), call. = FALSE)
  claim <- text_field(document, "claim")
  if (!claim %in% nnv_claims()) stop(sprintf("%s: a job with the unknown claim '%s'", path, claim), call. = FALSE)
  check <- document$check
  accepted <- if (is.null(check)) NA else flag_field(check, "accepted", FALSE)
  # A job is checked against itself: a status says what was claimed and what Lean said about it.
  expected <- if (claim == "unknown") "unknown" else if (isTRUE(accepted)) c(verified = "verified", violated = "refuted")[[claim]] else "failed"
  if (claim == "unknown" && !is.null(check)) stop(sprintf("%s: nothing was claimed, and something was checked", path), call. = FALSE)
  if (claim != "unknown" && is.null(check)) stop(sprintf("%s: the verifier claims '%s', and Lean was not asked", path, claim), call. = FALSE)
  if (status != expected) stop(sprintf("%s: the status is '%s', and the claim and the check make it '%s'", path, status, expected), call. = FALSE)
  layers <- document$verdict$network
  data.frame(
    job = index, path = path, label = label, model = text_field(document, "model"), property = text_field(document, "property"), status = status, claim = claim,
    accepted = accepted, axioms = if (is.null(check)) NA_integer_ else length(check$axioms), layers = length(layers),
    neurons = sum(vapply(layers, function(layer) length(layer$neurons), integer(1))), inputs = length(document$verdict$input),
    outputs = length(document$verdict$output), verify_ms = count_field(document, "verify_ms"),
    check_ms = if (is.null(check)) NA_integer_ else count_field(check, "elapsed_ms"), reason = text_field(document, "reason"), stringsAsFactors = FALSE
  )
}

#' Read verification jobs
#'
#' Loads the jobs that `maithic nnv verify` writes with `--out`: one for each
#' property of a network that was put to the verifier. Each job is checked
#' against itself: it is verified or refuted only when Lean accepted the
#' theorem that states the verifier's claim, unknown only when nothing was
#' claimed, and failed when a claim was made that Lean did not accept.
#'
#' @param paths Paths of jobs. When the vector has names, they label the jobs.
#' @param label A label for each job; by default the names of `paths`, or
#'   the file names of the jobs without their extension.
#' @return An object of class `maithic_nnv_jobs`, a list with one data frame,
#'   `jobs`: one row per job with the network, the property, the `status`,
#'   what the verifier claimed, whether Lean accepted it and on how many
#'   axioms, the size of the network, and the time the verifier and the
#'   kernel took.
#' @export
read_nnv_jobs <- function(paths, label = NULL) {
  if (length(paths) == 0) stop("no job was given", call. = FALSE)
  if (is.null(label)) label <- if (!is.null(names(paths))) names(paths) else tools::file_path_sans_ext(basename(paths))
  if (length(label) != length(paths)) stop("`label` needs one entry for each path", call. = FALSE)
  jobs <- do.call(rbind, lapply(seq_along(paths), function(i) parse_nnv_job(paths[[i]], i, label[[i]])))
  rownames(jobs) <- NULL
  structure(list(jobs = jobs), class = "maithic_nnv_jobs")
}

#' How verification jobs ended
#'
#' Counts jobs by status, with the share of each and a Wilson interval for
#' it, and the median time the verifier and the kernel took.
#'
#' @param jobs Jobs from [read_nnv_jobs()].
#' @param level The confidence level of the intervals.
#' @return A data frame with one row per status that occurs: `status`,
#'   `jobs`, `share`, `lower`, `upper`, `median_verify_ms` and
#'   `median_check_ms`.
#' @export
verification_summary <- function(jobs, level = 0.95) {
  if (!inherits(jobs, "maithic_nnv_jobs")) stop("`jobs` must come from read_nnv_jobs()", call. = FALSE)
  all <- jobs$jobs
  rows <- lapply(intersect(nnv_statuses(), all$status), function(status) {
    part <- all[all$status == status, , drop = FALSE]
    interval <- wilson_interval(nrow(part), nrow(all), level)
    data.frame(status = status, jobs = nrow(part), share = nrow(part) / nrow(all), lower = interval$lower, upper = interval$upper,
               median_verify_ms = stats::median(part$verify_ms), median_check_ms = if (all(is.na(part$check_ms))) NA_real_ else stats::median(part$check_ms, na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
