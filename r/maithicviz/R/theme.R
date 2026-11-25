# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

#' The Maithic colours
#'
#' The brand palette, by name. Teal stands for what the kernel has checked,
#' gold for what a model proposed, coral for failure.
#'
#' @return A named character vector of hexadecimal colours.
#' @export
maithic_colors <- function() {
  c(
    ink = "#14162B", indigo = "#2B2F77", teal = "#0FA89B", gold = "#F0A92E", coral = "#E5484D",
    slate = "#5B6477", mist = "#E8E9F3", paper = "#FAF8F4",
    ocaml = "#EC6813", lean = "#0FA89B", julia = "#9558B2", r = "#276DC3"
  )
}

#' The Maithic theme for ggplot2
#'
#' A quiet theme on the paper background of the brand: no box, horizontal
#' grid lines only, titles in ink and annotations in slate. Figures whose
#' bars carry their numbers drop the grid, so that no line runs through a
#' number.
#'
#' @param base_size Base font size in points.
#' @param dark Draw for a dark background instead.
#' @return A ggplot2 theme.
#' @export
theme_maithic <- function(base_size = 12, dark = FALSE) {
  colors <- maithic_colors()
  background <- if (dark) colors[["ink"]] else colors[["paper"]]
  foreground <- if (dark) colors[["paper"]] else colors[["ink"]]
  quiet <- if (dark) colors[["mist"]] else colors[["slate"]]
  grid <- if (dark) "#2C2F4A" else colors[["mist"]]
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = background, colour = NA),
      panel.background = ggplot2::element_rect(fill = background, colour = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = grid, linewidth = 0.4),
      text = ggplot2::element_text(colour = foreground),
      axis.text = ggplot2::element_text(colour = quiet),
      axis.title = ggplot2::element_text(colour = quiet),
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25), colour = foreground),
      plot.subtitle = ggplot2::element_text(colour = quiet, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = quiet, hjust = 0, size = ggplot2::rel(0.8)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(16, 20, 12, 16)
    )
}

# The grid of a figure whose bars carry their numbers: none. A number written at the end of a
# bar says what a grid line would, and no line runs through it.
labelled_bars <- function() ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank())

# The scale and the grid of a figure of rates in rows: percentages along the bottom, with
# vertical lines to read them by. Nothing is written inside the panel.
rate_rows <- function(dark, limit = 1) {
  colors <- maithic_colors()
  list(
    ggplot2::scale_x_continuous(limits = c(0, limit), breaks = seq(0, 1, 0.25), labels = function(x) sprintf("%d%%", round(100 * x)),
                                expand = ggplot2::expansion(mult = c(0.02, 0.03))),
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = if (dark) "#2C2F4A" else colors[["mist"]], linewidth = 0.4))
  )
}

# What stands at the end of a horizontal bar, a number or a few words: it begins a fixed part of
# the widest bar to the right of its own bar, so that a short number stands as clear of its bar
# as a long one. `widest` is the largest value of the bars.
bar_end_text <- function(mapping, widest, dark, data = NULL, size = 3.4) {
  ggplot2::geom_text(mapping = mapping, data = data, hjust = 0, position = ggplot2::position_nudge(y = 0.012 * widest),
                     colour = if (dark) maithic_colors()[["paper"]] else maithic_colors()[["ink"]], size = size)
}

# The height, in inches, of a figure of `n` rows under a title, a subtitle and a caption.
rows_height <- function(n) 1.9 + 0.42 * n

# A rate with its interval: a line from the lower to the upper end and a dot at the rate, with
# no bar under them, so that no mark covers another.
interval_marks <- function(colour = NULL) {
  line <- if (is.null(colour)) ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lower, xmax = .data$upper, colour = .data$shade), linewidth = 2.4, alpha = 0.45, show.legend = FALSE)
          else ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lower, xmax = .data$upper), colour = colour, linewidth = 2.4, alpha = 0.45)
  dot <- if (is.null(colour)) ggplot2::geom_point(ggplot2::aes(x = .data$rate, colour = .data$shade), size = 3.6, show.legend = FALSE)
         else ggplot2::geom_point(ggplot2::aes(x = .data$rate), colour = colour, size = 3.6)
  list(line, dot)
}
