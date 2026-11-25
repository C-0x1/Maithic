# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# What a dataset holds: how long its proofs are and which tactics they use.

#' How long the proofs of a dataset are
#'
#' The length of a proof is the number of its smallest steps: the tactics
#' that hold no other tactic. A proof written as a term has no steps; such
#' proofs are left out of the distribution and counted by
#' [proof_length_summary()].
#'
#' @param corpus A dataset from [read_corpus()].
#' @param complete_only Leave out the proofs that rest on `sorry`.
#' @return A data frame with one row for each length that occurs: `steps`,
#'   `proofs` (how many proofs have that length), `share`, and `cumulative`
#'   (the share of proofs of that length or shorter).
#' @export
proof_length_distribution <- function(corpus, complete_only = FALSE) {
  lengths <- tactic_proof_lengths(corpus, complete_only)
  if (length(lengths) == 0) {
    return(data.frame(steps = integer(), proofs = integer(), share = numeric(), cumulative = numeric()))
  }
  values <- sort(unique(lengths))
  proofs <- vapply(values, function(value) sum(lengths == value), integer(1))
  data.frame(steps = values, proofs = proofs, share = proofs / length(lengths), cumulative = cumsum(proofs) / length(lengths))
}

#' Summary of the proof lengths of a dataset
#'
#' @param corpus A dataset from [read_corpus()].
#' @param complete_only Leave out the proofs that rest on `sorry`.
#' @return A one-row data frame: `theorems`, `term_proofs` (proofs without a
#'   tactic), `tactic_proofs`, and over the tactic proofs the `median`, the
#'   `p90`, the `mean` and the `max` of the number of steps, and `one_step`,
#'   the share of them that a single tactic closes.
#' @export
proof_length_summary <- function(corpus, complete_only = FALSE) {
  corpus <- need_corpus(corpus)
  theorems <- corpus$theorems
  if (complete_only) theorems <- theorems[theorems$complete, , drop = FALSE]
  lengths <- tactic_proof_lengths(corpus, complete_only)
  summary <- distribution_summary(as.numeric(lengths), probs = 0.9)
  data.frame(theorems = nrow(theorems), term_proofs = nrow(theorems) - length(lengths), tactic_proofs = length(lengths),
             median = summary$median, p90 = summary$p90, mean = summary$mean, max = summary$max,
             one_step = if (length(lengths) == 0) NA_real_ else mean(lengths == 1))
}

tactic_proof_lengths <- function(corpus, complete_only) {
  corpus <- need_corpus(corpus)
  theorems <- corpus$theorems
  if (complete_only) theorems <- theorems[theorems$complete, , drop = FALSE]
  theorems$steps[theorems$steps > 0]
}

#' The tactic a step applies
#'
#' The first word of the text of a tactic, which names it: `simp` for
#' `simp [double]`, `exact` for `exact h.trans (le_of_lt hk)`, `rw` for
#' `rw[add_comm]`.
#'
#' @param tactic A character vector of tactic texts.
#' @return A character vector of the same length; empty for an empty text.
#' @export
tactic_head <- function(tactic) {
  text <- trimws(as.character(tactic))
  text[is.na(text)] <- ""
  found <- regexpr("^[^\\s\\[({]+", text, perl = TRUE)
  out <- rep("", length(text))
  out[found > 0] <- regmatches(text, found)
  out
}

#' The tactics a dataset uses
#'
#' Counts the smallest steps of the dataset by the tactic they apply. For
#' each tactic the table also says in how many proofs it occurs and how often
#' it closed a goal: how often fewer goals stood after it than before.
#'
#' @param corpus A dataset from [read_corpus()].
#' @param n How many rows to return, from the most used down; `Inf` for all.
#' @return A data frame with `tactic`, `uses`, `share` (of all smallest
#'   steps), `proofs` (how many proofs use it) and `closes` (the share of its
#'   uses that closed a goal). Ties are ordered by name.
#' @export
tactic_frequency <- function(corpus, n = Inf) {
  corpus <- need_corpus(corpus)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0) stop("`n` must be a count, or Inf", call. = FALSE)
  pairs <- corpus$pairs
  if (nrow(pairs) == 0) {
    return(data.frame(tactic = character(), uses = integer(), share = numeric(), proofs = integer(), closes = numeric(), stringsAsFactors = FALSE))
  }
  pairs$head <- tactic_head(pairs$tactic)
  pairs$proof <- paste(pairs$module, pairs$theorem, sep = "\r")
  counts <- count_values(pairs$head)
  out <- data.frame(
    tactic = names(counts), uses = as.integer(counts), share = as.integer(counts) / nrow(pairs),
    proofs = vapply(names(counts), function(head) length(unique(pairs$proof[pairs$head == head])), integer(1)),
    closes = vapply(names(counts), function(head) mean(pairs$goals_after[pairs$head == head] < pairs$goals_before[pairs$head == head]), numeric(1)),
    stringsAsFactors = FALSE
  )
  if (is.finite(n)) out <- utils::head(out, n)
  rownames(out) <- NULL
  out
}

#' Report table of a dataset
#'
#' What a dataset holds as Markdown, for a report or a dataset card: where it
#' came from, how many records of each kind, how long its proofs are, and its
#' most used tactics.
#'
#' @param corpus A dataset from [read_corpus()].
#' @param n How many of the most used tactics to list.
#' @return A character vector of Markdown lines.
#' @export
corpus_report_table <- function(corpus, n = 10) {
  corpus <- need_corpus(corpus)
  records <- data.frame(kind = names(corpus$records), records = as.integer(corpus$records), stringsAsFactors = FALSE)
  out <- c(
    sprintf("Dataset %s, version %s: %s.", corpus$name, corpus$version,
            paste(sprintf("%s %s record(s)", format(records$records, big.mark = ",", trim = TRUE), records$kind), collapse = ", ")), "")
  if (nrow(corpus$sources) > 0) {
    out <- c(out, "**Sources**", "",
             markdown_table(corpus$sources, labels = c(repository = "Repository", revision = "Revision", toolchain = "Lean", files = "Files")), "")
  }
  out <- c(out, "**Proof lengths**", "",
           markdown_table(proof_length_summary(corpus), digits = 1, percent = "one_step",
                          labels = c(theorems = "Theorems", term_proofs = "Term proofs", tactic_proofs = "Tactic proofs", median = "Median steps",
                                     p90 = "90th percentile", mean = "Mean", max = "Longest", one_step = "One step")), "",
           "**Most used tactics**", "",
           markdown_table(tactic_frequency(corpus, n), percent = c("share", "closes"),
                          labels = c(tactic = "Tactic", uses = "Uses", share = "Share", proofs = "Proofs", closes = "Closes a goal")))
  out
}
