# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of the algorithm laboratory: how the cost of the verified
# algorithms grows, measured, beside the bounds that are proved.

#' Measured comparisons beside proved bounds
#'
#' One panel for each algorithm, on logarithmic axes: the comparisons that
#' the compiled Lean code made at each size as points, and the bound that
#' Lean proves as a line. No point lies above its line; how far below it
#' lies is what the measurement adds.
#'
#' @param costs A table from [maithicstats::read_algo_costs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_growth <- function(costs, dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::growth_table(costs)
  above <- sum(costs$comparisons > costs$bound)
  titles <- stats::setNames(sprintf("%s\nmeasured: %s", gsub("_", " ", table$algorithm), table$measured), table$algorithm)
  costs$panel <- factor(titles[costs$algorithm], levels = titles[table$algorithm])
  long <- rbind(data.frame(panel = costs$panel, n = costs$n, value = costs$bound, what = "proved bound"),
                data.frame(panel = costs$panel, n = costs$n, value = costs$comparisons, what = "measured"))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$n, y = .data$value)) +
    ggplot2::geom_line(data = long[long$what == "proved bound", ], ggplot2::aes(colour = .data$what), linewidth = 0.9) +
    ggplot2::geom_point(data = long[long$what == "measured", ], ggplot2::aes(colour = .data$what), size = 2.4) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free_y") +
    ggplot2::scale_x_continuous(trans = "log2", breaks = unique(costs$n)[c(TRUE, FALSE)]) +
    ggplot2::scale_y_continuous(trans = "log2", labels = function(v) format(v, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    ggplot2::scale_colour_manual(values = c("proved bound" = colors[["gold"]], "measured" = colors[["teal"]]), name = NULL) +
    ggplot2::labs(title = "What the proofs promise, and what the code does",
                  subtitle = "Comparisons made by the compiled Lean code, and the bounds proved in Lean (logarithmic axes)",
                  x = "elements in the input", y = "comparisons",
                  caption = sprintf("Source: %d measurement(s) of %d algorithm(s); the largest input has %s elements. %s", nrow(costs), nrow(table),
                                    format(max(costs$n), big.mark = ","),
                                    if (above == 0) "No measurement exceeds its bound." else sprintf("%d measurement(s) exceed their bound.", above))) +
    theme_maithic(dark = dark) +
    ggplot2::theme(legend.position = "bottom")
}

#' Redraw the figure of measured growth
#'
#' Reads a table of measured costs and writes the figure made from it as SVG.
#'
#' @param path The CSV of costs.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_growth <- function(path, out_dir) {
  costs <- maithicstats::read_algo_costs(path)
  invisible(save_figure(plot_growth(costs), file.path(out_dir, "algo-growth.svg"), height = 3.9))
}
