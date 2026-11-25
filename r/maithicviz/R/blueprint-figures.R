# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of a formalization project: how the nodes of its blueprint moved
# from planned to done.

#' The progress of a blueprint, snapshot by snapshot
#'
#' One bar for each snapshot, the earliest at the top, divided by the status
#' of the nodes: planned, stated, proved and done. Read from top to bottom,
#' the bars show the plan filling up.
#'
#' @param series Snapshots from [maithicstats::read_blueprint_snapshots()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_blueprint_progress <- function(series, dark = FALSE) {
  colors <- maithic_colors()
  composition <- maithicstats::status_composition(series)
  statuses <- c("done", "proved", "stated", "planned")
  composition$status <- factor(composition$status, levels = rev(statuses))
  composition$label <- factor(composition$label, levels = rev(series$snapshots$label))
  composition$text <- ifelse(composition$nodes == 0, "", as.character(composition$nodes))
  fills <- c(done = colors[["teal"]], proved = colors[["indigo"]], stated = colors[["gold"]], planned = if (dark) "#4A4E6B" else "#C9CCDA")
  inks <- c(done = "#FFFFFF", proved = "#FFFFFF", stated = colors[["ink"]], planned = colors[["ink"]])
  last <- series$snapshots[nrow(series$snapshots), ]
  ggplot2::ggplot(composition, ggplot2::aes(x = .data$nodes, y = .data$label, fill = .data$status)) +
    ggplot2::geom_col(width = 0.62, colour = if (dark) colors[["ink"]] else colors[["paper"]], linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$text, colour = .data$status), position = ggplot2::position_stack(vjust = 0.5), size = 3.6, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = fills, breaks = statuses, name = NULL) +
    ggplot2::scale_colour_manual(values = inks) +
    ggplot2::scale_x_continuous(breaks = seq(0, max(series$snapshots$nodes), by = max(1, ceiling(max(series$snapshots$nodes) / 8))), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = "How a formalization fills up",
                  subtitle = "The nodes of a blueprint by status, one bar for each snapshot; a status is what Lean reports, not what anyone says",
                  x = "nodes", y = NULL,
                  caption = sprintf("Source: %d snapshot(s) of the blueprint \"%s\"; at the last one %d of %d node(s) are done.", nrow(series$snapshots),
                                    last$title, last$done, last$nodes)) +
    theme_maithic(dark = dark) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "bottom")
}

#' Redraw the figure of a blueprint's progress
#'
#' Reads status snapshots written by `maithic blueprint status` and writes
#' the figure made from them as SVG.
#'
#' @param paths The paths of the snapshots, the earliest first.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_blueprint <- function(paths, out_dir) {
  series <- maithicstats::read_blueprint_snapshots(paths)
  invisible(save_figure(plot_blueprint_progress(series), file.path(out_dir, "blueprint-progress.svg"), height = 3.9))
}
