# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of repair runs: what was broken, and which remedy repaired it.

#' How the breakages of repair runs were repaired
#'
#' One bar for each run, split by the stage that repaired a breakage: a rule,
#' which needs no model; a model, asked for the smallest change to a proof;
#' the prover, which proves the statement again; and what was left. Runs of
#' the same file with more remedies switched on show what each remedy adds.
#'
#' @param runs Repair runs from [maithicstats::read_mend_runs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_repair_stages <- function(runs, dark = FALSE) {
  colors <- maithic_colors()
  if (!inherits(runs, "maithic_mend_runs")) stop("`runs` must come from maithicstats::read_mend_runs()", call. = FALSE)
  table <- runs$runs
  if (sum(table$breakages) == 0) stop("the runs hold no breakage", call. = FALSE)
  stages <- c(by_rule = "by a rule", by_model = "by a model", by_prover = "by the prover", unrepaired = "left")
  long <- do.call(rbind, lapply(names(stages), function(column) {
    data.frame(label = table$label, stage = stages[[column]], breakages = table[[column]], stringsAsFactors = FALSE)
  }))
  long <- long[long$breakages > 0, , drop = FALSE]
  long$label <- factor(long$label, levels = rev(table$label))
  # Stacked from the cheapest remedy at the axis to what was left at the end of the bar.
  long$stage <- factor(long$stage, levels = rev(unname(stages)))
  totals <- data.frame(label = factor(table$label, levels = rev(table$label)), breakages = table$breakages,
                       note = ifelse(table$clean, "the file compiles", sprintf("%d left", table$unrepaired)))
  fills <- c(`by a rule` = colors[["teal"]], `by a model` = colors[["gold"]], `by the prover` = colors[["indigo"]], left = colors[["coral"]])
  ggplot2::ggplot(long, ggplot2::aes(x = .data$label, y = .data$breakages)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$stage), width = 0.58) +
    bar_end_text(ggplot2::aes(label = .data$note), max(totals$breakages), dark, data = totals, size = 3.5) +
    ggplot2::scale_fill_manual(values = fills, breaks = intersect(unname(stages), as.character(long$stage))) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.28))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "What repairs a file that stopped compiling",
                  subtitle = "Breakages by the remedy that repaired them; every repair was compiled by Lean before it was kept",
                  x = NULL, y = "breakages",
                  caption = sprintf("Source: %d repair run(s) of %s; %d breakage(s) in each.", nrow(table), paste(unique(table$file), collapse = ", "),
                                    max(table$breakages))) +
    theme_maithic(dark = dark) + horizontal_bars(dark)
}

#' Redraw the figure of the repair runs
#'
#' Reads repair reports written by `maithic mend repair` and writes the
#' figure made from them as SVG.
#'
#' @param paths Paths of repair reports; their names label the runs.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_mend <- function(paths, out_dir) {
  runs <- maithicstats::read_mend_runs(paths)
  invisible(save_figure(plot_repair_stages(runs), file.path(out_dir, "mend-repair-stages.svg"), height = 3.6))
}
