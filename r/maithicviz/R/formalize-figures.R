# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of formalization runs: what became of the statements a model drafted.

#' What became of the candidate statements
#'
#' One bar for each problem, split by what became of its candidates: the
#' statement that was chosen; statements that passed and were kept behind
#' it; duplicates, which Lean showed to be an earlier statement in other
#' notation; statements Lean rejected, as vacuous or with a counterexample;
#' and statements that never elaborated. Beside each bar stands how the run
#' ended.
#'
#' @param runs Formalization runs from [maithicstats::read_formalize_runs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_candidate_fates <- function(runs, dark = FALSE) {
  colors <- maithic_colors()
  if (!inherits(runs, "maithic_formalize_runs")) stop("`runs` must come from maithicstats::read_formalize_runs()", call. = FALSE)
  table <- runs$runs
  candidates <- runs$candidates
  if (nrow(candidates) == 0) stop("the runs hold no candidate", call. = FALSE)
  fates <- c(chosen = "chosen", kept = "kept behind it", duplicate = "a duplicate", rejected = "rejected by Lean", failed = "never elaborated")
  long <- do.call(rbind, lapply(names(fates), function(verdict) {
    data.frame(label = table$label, fate = fates[[verdict]],
               candidates = vapply(table$run, function(run) sum(candidates$run == run & candidates$verdict == verdict), integer(1)),
               stringsAsFactors = FALSE)
  }))
  long <- long[long$candidates > 0, , drop = FALSE]
  long$label <- factor(long$label, levels = rev(table$label))
  # Stacked from the chosen statement at the axis to what never elaborated at the end of the bar.
  long$fate <- factor(long$fate, levels = rev(unname(fates)))
  totals <- data.frame(label = factor(table$label, levels = rev(table$label)), candidates = table$drafted,
                       note = ifelse(table$status == "formalized", "a statement", ifelse(table$status == "flagged", "a statement, flagged", "no statement")))
  fills <- c(chosen = colors[["teal"]], `kept behind it` = colors[["indigo"]], `a duplicate` = colors[["gold"]], `rejected by Lean` = colors[["coral"]],
             `never elaborated` = colors[["slate"]])
  ggplot2::ggplot(long, ggplot2::aes(x = .data$label, y = .data$candidates)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$fate), width = 0.58) +
    bar_end_text(ggplot2::aes(label = .data$note), max(totals$candidates), dark, data = totals, size = 3.5) +
    ggplot2::scale_fill_manual(values = fills, breaks = intersect(unname(fates), as.character(long$fate))) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.34))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "What becomes of the statements a model drafts",
                  subtitle = "Candidate statements by outcome; a rejection rests on a proof that Lean accepted",
                  x = NULL, y = "candidate statements",
                  caption = sprintf("Source: %d formalization run(s), %d candidate(s); %d elaborate as drafted, %d after repair.", nrow(table), nrow(candidates),
                                    sum(candidates$first_try), sum(candidates$elaborated))) +
    theme_maithic(dark = dark) + horizontal_bars(dark)
}

#' Redraw the figure of the formalization runs
#'
#' Reads runs written by `maithic formalize` and writes the figure made from
#' them as SVG.
#'
#' @param paths Paths of formalization runs; their names label the runs.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_formalize <- function(paths, out_dir) {
  runs <- maithicstats::read_formalize_runs(paths)
  invisible(save_figure(plot_candidate_fates(runs), file.path(out_dir, "formalize-candidates.svg"), height = 3.6))
}
