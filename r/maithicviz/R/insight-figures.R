# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of what Maithic reads out of code: the declarations a codebase
# rests on, the proofs and tactics of a dataset, and the findings of a review.

horizontal_bars <- function(dark) labelled_bars()

# The last part of a dotted name, with enough of what stands before it to tell
# two declarations of the figure apart.
short_names <- function(names) {
  parts <- strsplit(names, ".", fixed = TRUE)
  keep <- rep(1L, length(names))
  label <- function() vapply(seq_along(parts), function(i) paste(utils::tail(parts[[i]], keep[i]), collapse = "."), character(1))
  repeat {
    labels <- label()
    clash <- (duplicated(labels) | duplicated(labels, fromLast = TRUE)) & keep < lengths(parts)
    if (!any(clash)) return(labels)
    keep[clash] <- keep[clash] + 1L
  }
}

#' The declarations a codebase rests on
#'
#' One bar for each of the most depended-upon declarations: how many
#' declarations of the scanned files use it, coloured by what it is.
#'
#' @param snapshot A snapshot from [maithicstats::read_intel_snapshot()].
#' @param n How many declarations to show.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_top_depended <- function(snapshot, n = 12, dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::top_depended(snapshot, n)
  table <- table[table$used_by > 0, , drop = FALSE]
  table$label <- short_names(table$name)
  table$label <- factor(table$label, levels = rev(table$label))
  table$note <- sprintf("%d  (%d%%)", table$used_by, round(100 * table$share))
  fills <- c(definition = colors[["indigo"]], theorem = colors[["teal"]], structure = colors[["gold"]], class = colors[["julia"]],
             inductive = colors[["ocaml"]], instance = colors[["r"]], axiom = colors[["coral"]], opaque = colors[["slate"]])
  health <- snapshot$health
  ggplot2::ggplot(table, ggplot2::aes(x = .data$label, y = .data$used_by, fill = .data$kind)) +
    ggplot2::geom_col(width = 0.62) +
    bar_end_text(ggplot2::aes(label = .data$note), max(table$used_by), dark) +
    ggplot2::scale_fill_manual(values = fills, breaks = intersect(names(fills), table$kind), na.value = colors[["slate"]]) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.16))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "The declarations the code rests on",
                  subtitle = "How many declarations use each one, in a statement, a definition or a proof, as Lean's kernel sees it",
                  x = NULL, y = "declarations that use it",
                  caption = sprintf("Source: %d declaration(s) in %d file(s); %d rest on sorry.", health$declarations, health$files, health$resting_on_sorry)) +
    theme_maithic(dark = dark) + horizontal_bars(dark)
}

corpus_caption <- function(corpus) {
  sprintf("Source: dataset %s, version %s: %s theorem(s), %s step pair(s).", corpus$name, corpus$version,
          format(corpus$records[["theorem"]], big.mark = ","), format(corpus$records[["pair"]], big.mark = ","))
}

#' How long the proofs of a dataset are
#'
#' One bar for each proof length that occurs, in smallest steps: the tactics
#' that hold no other tactic. The bar of the median length has its own
#' colour, and the subtitle names the median. Proofs written as terms have
#' no steps and are named in the caption.
#'
#' @param corpus A dataset from [maithicstats::read_corpus()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_proof_lengths <- function(corpus, dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::proof_length_distribution(corpus)
  summary <- maithicstats::proof_length_summary(corpus)
  if (nrow(table) == 0) stop("the dataset holds no tactic proof", call. = FALSE)
  # The bar of the median length is set apart by its colour, and the subtitle says which it is: nothing is written over the bars.
  table$role <- ifelse(table$steps == summary$median, "median", "other")
  ggplot2::ggplot(table, ggplot2::aes(x = .data$steps, y = .data$proofs)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$role), width = 0.7, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(median = colors[["gold"]], other = colors[["teal"]])) +
    ggplot2::scale_x_continuous(breaks = function(limits) unique(round(pretty(limits)))) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = "How long proofs are",
                  subtitle = sprintf("Tactic proofs by the number of their smallest steps; the median is %s step%s%s", format(summary$median),
                                     if (summary$median == 1) "" else "s", if (any(table$steps == summary$median)) ", the gold bar" else ""),
                  x = "steps", y = "proofs",
                  caption = sprintf("%s %d proof(s) are terms without a tactic and are not shown.", corpus_caption(corpus), summary$term_proofs)) +
    theme_maithic(dark = dark)
}

#' The tactics a dataset uses
#'
#' One bar for each of the most used tactics: how many of the smallest steps
#' apply it, with the share of its uses that closed a goal.
#'
#' @param corpus A dataset from [maithicstats::read_corpus()].
#' @param n How many tactics to show.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_tactic_frequency <- function(corpus, n = 15, dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::tactic_frequency(corpus, n)
  if (nrow(table) == 0) stop("the dataset holds no step pairs", call. = FALSE)
  table$tactic <- factor(table$tactic, levels = rev(table$tactic))
  table$closed <- table$uses * table$closes
  table$note <- sprintf("%d  (%d%%)", table$uses, round(100 * table$closes))
  # Each bar in two parts that do not cover each other: the steps that closed a goal stand at the axis, the others after them.
  parts <- c("closed a goal", "left goals open")
  long <- rbind(data.frame(tactic = table$tactic, part = parts[1], steps = table$closed),
                data.frame(tactic = table$tactic, part = parts[2], steps = table$uses - table$closed))
  long$part <- factor(long$part, levels = rev(parts))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$tactic, y = .data$steps)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$part), width = 0.62) +
    bar_end_text(ggplot2::aes(y = .data$uses, label = .data$note), max(table$uses), dark, data = table, size = 3.3) +
    ggplot2::scale_fill_manual(values = c(`closed a goal` = colors[["teal"]], `left goals open` = colors[["indigo"]]),
                               breaks = c("closed a goal", "left goals open")) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.14))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "The tactics proofs are made of",
                  subtitle = "Smallest steps by the tactic they apply; in brackets, how often fewer goals stood after the step than before",
                  x = NULL, y = "steps", caption = corpus_caption(corpus)) +
    theme_maithic(dark = dark) + horizontal_bars(dark) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(family = "mono"))
}

#' The findings of reviews, by rule and severity
#'
#' One bar for each rule, split by severity: errors fail a review, warnings
#' and notes do not.
#'
#' @param runs Review runs from [maithicstats::read_review_runs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_review_findings <- function(runs, dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::finding_counts(runs, c("rule", "severity"))
  if (nrow(table) == 0) stop("the reviews hold no findings", call. = FALSE)
  totals <- maithicstats::finding_counts(runs, "rule")
  table$rule <- factor(table$rule, levels = rev(totals$rule))
  totals$rule <- factor(totals$rule, levels = rev(totals$rule))
  labels <- c(error = "error", warning = "warning", info = "note")
  table$severity <- factor(labels[table$severity], levels = rev(unname(labels)))
  fills <- c(error = colors[["coral"]], warning = colors[["gold"]], note = colors[["indigo"]])
  summary <- runs$runs
  ggplot2::ggplot(table, ggplot2::aes(x = .data$rule, y = .data$findings)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$severity), width = 0.62) +
    bar_end_text(ggplot2::aes(label = .data$findings), max(totals$findings), dark, data = totals, size = 3.5) +
    ggplot2::scale_fill_manual(values = fills, breaks = intersect(names(fills), as.character(table$severity))) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "What a review finds",
                  subtitle = "Findings by rule and severity; every suggested edit was accepted by Lean before it was reported",
                  x = NULL, y = "findings",
                  caption = sprintf("Source: %d review(s) of %d file(s) and %d declaration(s); %d finding(s), %d suggested edit(s), %d suppressed.",
                                    nrow(summary), sum(summary$files), sum(summary$declarations), sum(summary$findings),
                                    sum(summary$suggestions), sum(summary$suppressed))) +
    theme_maithic(dark = dark) + horizontal_bars(dark) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(family = "mono"))
}

#' Redraw the figures of the code graph, the dataset and the review
#'
#' Reads a snapshot written by `maithic intel scan`, a dataset written by
#' `maithic corpus harvest` and reports written by `maithic review`, and
#' writes the figures made from them as SVG.
#'
#' @param intel_dir A folder with `graph.json` and `health.json`.
#' @param corpus_dir The folder of a dataset.
#' @param review_paths Paths of review reports.
#' @param out_dir Where to write the figures.
#' @return The paths of the figures, invisibly.
#' @export
render_insights <- function(intel_dir, corpus_dir, review_paths, out_dir) {
  corpus <- maithicstats::read_corpus(corpus_dir)
  figures <- list(
    `intel-top-depended` = plot_top_depended(maithicstats::read_intel_snapshot(intel_dir)),
    `corpus-proof-lengths` = plot_proof_lengths(corpus), `corpus-tactics` = plot_tactic_frequency(corpus),
    `review-findings` = plot_review_findings(maithicstats::read_review_runs(review_paths))
  )
  paths <- vapply(names(figures), function(name) save_figure(figures[[name]], file.path(out_dir, sprintf("%s.svg", name))), character(1))
  invisible(unname(paths))
}
