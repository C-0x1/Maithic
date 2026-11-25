# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Where the numbers of a figure come from: the result file when it is known,
# otherwise the manifest of the suite.
run_source <- function(meta) if (is.null(meta$source)) meta$manifest else meta$source

run_caption <- function(meta) {
  mode <- if (isTRUE(meta$use_ai)) sprintf("gateway mode %s", meta$mode) else "no model used"
  sprintf("Source: %s, %d problems, %s. Lines show 95%% Wilson intervals.", run_source(meta), meta$total, mode)
}

# Whole-number axis breaks for counts.
count_breaks <- function(limits) {
  breaks <- pretty(c(0, limits[2]), n = 5)
  breaks[breaks == round(breaks)]
}

#' Success rate by group, with intervals
#'
#' One row for each group: a dot at the share of its problems that were
#' verified, on a line that spans the 95% Wilson interval. The label of the
#' row gives the counts.
#'
#' @param run A run as returned by [maithicstats::read_bench_run()].
#' @param by Grouping passed to [maithicstats::success_by()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_success_by_group <- function(run, by = "tag", dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::success_by(run$problems, by)
  table$row <- sprintf("%s (%d of %d)", table$group, table$solved, table$n)
  table$row <- factor(table$row, levels = rev(table$row))
  ggplot2::ggplot(table, ggplot2::aes(y = .data$row)) +
    interval_marks(colors[["teal"]]) +
    ggplot2::labs(
      title = sprintf("Verified problems by %s", by),
      subtitle = "Share of problems whose proof was accepted by the kernel, the trust policy and the re-check",
      x = NULL, y = NULL, caption = run_caption(run$meta)
    ) +
    theme_maithic(dark = dark) +
    rate_rows(dark)
}

#' Time per problem
#'
#' One bar per problem, in the order of the suite, coloured by who found the
#' proof. A problem that was not verified is drawn in coral.
#'
#' @param run A run as returned by [maithicstats::read_bench_run()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_time_per_problem <- function(run, dark = FALSE) {
  colors <- maithic_colors()
  problems <- run$problems
  problems$outcome <- ifelse(problems$solved, ifelse(problems$proposer == "model", "model", "portfolio"), "not verified")
  problems$outcome <- factor(problems$outcome, levels = intersect(c("portfolio", "model", "not verified"), problems$outcome))
  problems$id <- factor(problems$id, levels = problems$id)
  ggplot2::ggplot(problems, ggplot2::aes(x = .data$id, y = .data$elapsed_ms / 1000, fill = .data$outcome)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = c(portfolio = colors[["teal"]], model = colors[["gold"]], `not verified` = colors[["coral"]])) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Time per problem",
      subtitle = "Seconds from the request to the verified result, including the fresh-process re-check",
      x = NULL, y = "seconds",
      caption = sprintf("Source: %s. One run on one machine; timings vary between runs. The first problem includes loading the suite.", run_source(run$meta))
    ) +
    theme_maithic(dark = dark) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = ggplot2::rel(0.8)))
}

#' Attempts needed per problem
#'
#' How many candidate proofs were tried before one was accepted.
#'
#' @param run A run as returned by [maithicstats::read_bench_run()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_attempts <- function(run, dark = FALSE) {
  colors <- maithic_colors()
  counts <- as.data.frame(table(attempts = run$problems$attempts[run$problems$solved]), stringsAsFactors = FALSE)
  counts$attempts <- factor(as.integer(counts$attempts), levels = sort(unique(as.integer(counts$attempts))))
  ggplot2::ggplot(counts, ggplot2::aes(x = .data$attempts, y = .data$Freq)) +
    ggplot2::geom_col(fill = colors[["indigo"]], width = 0.62) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), vjust = -0.5, colour = if (dark) colors[["paper"]] else colors[["ink"]], size = 3.6) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title = "Candidates tried before a proof was accepted",
      subtitle = "Verified problems by the number of attempts; the portfolio tries its cheapest steps first",
      x = "attempts", y = "problems", caption = sprintf("Source: %s.", run_source(run$meta))
    ) +
    theme_maithic(dark = dark) +
    labelled_bars()
}

# The names svglite gives the sans and the monospaced face of this machine, read from a drawing
# of one letter in each.
device_fonts <- function() {
  drawing <- svglite::svgstring(width = 4, height = 4, id = "probe")
  closed <- FALSE
  on.exit(if (!closed) grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  graphics::text(0.5, 0.7, "a", family = "sans")
  graphics::text(0.5, 0.3, "a", family = "mono")
  grDevices::dev.off()
  closed <- TRUE
  text <- paste(as.character(drawing()), collapse = "\n")
  found <- regmatches(text, gregexpr('font-family: "[^"]*";', text))[[1]]
  if (length(found) < 2) stop("the faces of the drawing device could not be read", call. = FALSE)
  c(sans = found[1], mono = found[2])
}

# The faces a figure asks for, in the order a reader's machine is tried: svglite names the
# faces of the machine that drew the figure, which another machine may not have, and a browser
# then falls back to a serif face. The first names have the widths the figure was laid out
# with; the last is a face every system has.
figure_fonts <- function() {
  c(sans = 'font-family: Arial, "Helvetica Neue", "Liberation Sans", sans-serif;',
    mono = 'font-family: "Courier New", Courier, "Liberation Mono", monospace;')
}

# Rewrites the font names of an SVG that svglite wrote, so that it reads the same elsewhere.
portable_fonts <- function(path) {
  drawn <- device_fonts()
  wanted <- figure_fonts()
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!identical(drawn[["mono"]], drawn[["sans"]])) lines <- gsub(drawn[["mono"]], wanted[["mono"]], lines, fixed = TRUE)
  lines <- gsub(drawn[["sans"]], wanted[["sans"]], lines, fixed = TRUE)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(enc2utf8(lines), connection, sep = "\n", useBytes = TRUE)
  invisible(path)
}

#' Save a figure at a fixed size
#'
#' The format follows the file extension: `.svg` or `.png`. An SVG keeps text
#' as text and carries no timestamp or machine-specific identifier, so the
#' same data gives the same file on the same machine. Its text names faces
#' that every system has, in an order that keeps the widths the figure was
#' laid out with, and every piece of text carries its measured length, so that
#' a reader with other fonts sees the same layout. A PNG is rendered at 150
#' pixels per inch.
#'
#' @param plot A ggplot object.
#' @param path Output file; the directory is created if needed.
#' @param width,height Size in inches.
#' @return The path, invisibly.
#' @export
save_figure <- function(plot, path, width = 9, height = 4.8) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  extension <- tolower(tools::file_ext(path))
  if (extension == "svg") {
    svglite::svglite(path, width = width, height = height, bg = "transparent", fix_text_size = TRUE, id = "maithic")
  } else if (extension == "png") {
    type <- if (isTRUE(capabilities("cairo"))) "cairo" else getOption("bitmapType")
    grDevices::png(path, width = width, height = height, units = "in", res = 150, type = type)
  } else {
    stop(sprintf("unsupported figure format '%s'; use .svg or .png", extension), call. = FALSE)
  }
  tryCatch(print(plot), finally = grDevices::dev.off())
  if (extension == "svg") portable_fonts(path)
  invisible(path)
}

#' Regenerate every benchmark figure of a run
#'
#' @param run_path A result file written by `maithic bench run --out`.
#' @param out_dir Directory for the figures.
#' @param stem File-name stem, usually the name of the suite.
#' @return The paths of the files written.
#' @export
render_all <- function(run_path, out_dir, stem = "bench-basics") {
  run <- maithicstats::read_bench_run(run_path)
  run$meta$source <- basename(run_path)
  figures <- list(
    `by-tag` = plot_success_by_group(run, "tag"),
    time = plot_time_per_problem(run),
    attempts = plot_attempts(run)
  )
  heights <- c(`by-tag` = rows_height(nrow(maithicstats::success_by(run$problems, "tag"))), time = 4.8, attempts = 4.8)
  paths <- vapply(names(figures), function(name) {
    save_figure(figures[[name]], file.path(out_dir, sprintf("%s-%s.svg", stem, name)), height = heights[[name]])
  }, character(1))
  unname(paths)
}
