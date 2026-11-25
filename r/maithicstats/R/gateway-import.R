# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the usage records of the gateway.

#' Read usage records
#'
#' Reads the usage records that the gateway writes into every result file
#' that involved it: one record per model call with its purpose, provider,
#' model, who served it (`live`, `replay`, `mock` or `scripted`), whether it
#' succeeded and how it failed, its tokens, its start time and its duration.
#' A record holds no prompt, no answer and no key.
#'
#' @param paths Paths of result files with a `gateway` member: prove runs
#'   (`maithic.prove.run/1`) and benchmark runs (`maithic.bench.run/1`).
#' @return A data frame with one row per call: `run` (the position of the
#'   file in `paths`), `purpose`, `provider`, `model`, `served_by`, `ok`,
#'   `error_class`, `input_tokens`, `output_tokens`, `cached_tokens`,
#'   `reasoning_tokens`, `started_at` (UTC) and `elapsed_ms`. Token kinds a
#'   record does not state are zero.
#' @export
read_usage <- function(paths) {
  if (length(paths) == 0) stop("no result files were given", call. = FALSE)
  known <- c("maithic.prove.run/1", "maithic.bench.run/1")
  frames <- lapply(seq_along(paths), function(i) {
    path <- paths[[i]]
    if (!file.exists(path)) stop(sprintf("file not found: %s", path), call. = FALSE)
    document <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!isTRUE(document$schema %in% known)) {
      stop(sprintf("%s is not a result file with usage records (schema '%s')", path, format(document$schema)), call. = FALSE)
    }
    gateway <- document$gateway
    if (is.null(gateway)) stop(sprintf("%s has no gateway account", path), call. = FALSE)
    rows <- usage_rows(gateway, i)
    if (!is.null(gateway$calls) && gateway$calls != nrow(rows)) {
      stop(sprintf("%s: the stated number of calls does not match the usage records", path), call. = FALSE)
    }
    rows
  })
  out <- bind_frames(frames, empty_usage())
  rownames(out) <- NULL
  out
}

# The pillar a call belongs to, from the first part of its purpose.
purpose_pillars <- function() {
  c(prove = "Prove", formalize = "Formalize", review = "Review", search = "Search", algolab = "AlgoLab",
    council = "Council", intel = "Intel", nnv = "NNV", corpus = "Corpus", guide = "Guide", oracle = "Oracle",
    refute = "Refute", bench = "Bench", blueprint = "Blueprint", mend = "Mend", ledger = "Ledger")
}

#' The pillar of a call
#'
#' Every model call states its purpose, such as `prove.repair`; the part
#' before the first dot names the pillar the call was made for.
#'
#' @param purpose A character vector of purposes.
#' @return The pillar names, `"Other"` where the purpose names none.
#' @export
usage_pillar <- function(purpose) {
  head <- sub("\\..*$", "", as.character(purpose))
  pillar <- unname(purpose_pillars()[head])
  pillar[is.na(pillar)] <- "Other"
  pillar
}
