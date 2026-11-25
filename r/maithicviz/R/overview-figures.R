# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of the repository as a whole: what the capability register holds,
# where the declarations of a Lean library are, and how well a search finds
# what it is asked for.

#' Capabilities by pillar and language
#'
#' One bar for each pillar of the capability register, divided by the
#' language its capabilities are written in.
#'
#' @param status A summary from [maithicstats::read_register_status()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_register <- function(status, dark = FALSE) {
  colors <- maithic_colors()
  table <- status$languages
  order <- status$pillars$id
  label <- stats::setNames(sprintf("%s  %s", status$pillars$name, status$pillars$id), status$pillars$id)
  table$pillar <- factor(label[table$id], levels = rev(label[order]))
  languages <- maithicstats::register_languages()
  table$language <- factor(table$language, levels = rev(languages))
  totals <- data.frame(pillar = factor(label[order], levels = rev(label[order])), capabilities = status$pillars$capabilities)
  by_language <- maithicstats::register_by_language(status)
  fills <- c(OCaml = colors[["ocaml"]], Lean = colors[["lean"]], Julia = colors[["julia"]], R = colors[["r"]])
  ggplot2::ggplot(table, ggplot2::aes(x = .data$pillar, y = .data$capabilities)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$language), width = 0.66) +
    bar_end_text(ggplot2::aes(label = .data$capabilities), max(totals$capabilities), dark, data = totals) +
    ggplot2::scale_fill_manual(values = fills, breaks = languages, name = NULL) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "What the register holds, pillar by pillar",
                  subtitle = "Capabilities of each pillar, by the language they are written in",
                  x = NULL, y = "capabilities",
                  caption = sprintf("Source: the capability register, %d capabilities in %d pillars, %d of them done: %s.", status$total, nrow(status$pillars),
                                    status$done, paste(sprintf("%s %d", by_language$language, by_language$capabilities), collapse = ", "))) +
    theme_maithic(dark = dark) +
    horizontal_bars(dark) +
    ggplot2::theme(legend.position = "bottom")
}

#' Where the declarations of a library are
#'
#' One bar for each area of the scanned files, divided into theorems,
#' definitions and the other kinds of declaration.
#'
#' @param snapshot A snapshot from [maithicstats::read_intel_snapshot()].
#' @param depth How many folders name an area.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_library_areas <- function(snapshot, depth = 2, dark = FALSE) {
  colors <- maithic_colors()
  table <- maithicstats::declarations_by_area(snapshot, depth)
  if (nrow(table) == 0) stop("the snapshot holds no declaration with a file", call. = FALSE)
  kinds <- c("theorems", "definitions", "others")
  long <- do.call(rbind, lapply(kinds, function(kind) data.frame(area = table$area, kind = kind, declarations = table[[kind]], stringsAsFactors = FALSE)))
  long$area <- factor(long$area, levels = rev(table$area))
  long$kind <- factor(long$kind, levels = rev(kinds))
  totals <- data.frame(area = factor(table$area, levels = rev(table$area)), declarations = table$declarations,
                       note = sprintf("%d in %d file%s", table$declarations, table$files, ifelse(table$files == 1, "", "s")))
  fills <- c(theorems = colors[["teal"]], definitions = colors[["indigo"]], others = colors[["gold"]])
  ggplot2::ggplot(long, ggplot2::aes(x = .data$area, y = .data$declarations)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$kind), width = 0.66) +
    bar_end_text(ggplot2::aes(label = .data$note), max(totals$declarations), dark, data = totals) +
    ggplot2::scale_fill_manual(values = fills, breaks = kinds, labels = c("theorems", "definitions", "structures, classes, instances and others"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Where the declarations are",
                  subtitle = "Declarations of the scanned files by area, and what they are",
                  x = NULL, y = "declarations",
                  caption = sprintf("Source: code graph %s, %d declaration(s): %d theorem(s), %d definition(s), %d other(s).",
                                    snapshot$label, sum(table$declarations), sum(table$theorems), sum(table$definitions), sum(table$others))) +
    theme_maithic(dark = dark) +
    horizontal_bars(dark) +
    ggplot2::theme(legend.position = "bottom")
}

#' Redraw the figure of the capability register
#'
#' @param path The summary of the register, `registry/status.json`.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_register <- function(path, out_dir) {
  status <- maithicstats::read_register_status(path)
  invisible(save_figure(plot_register(status), file.path(out_dir, "register-by-pillar.svg"), height = 6.4))
}

#' Redraw the figure of the areas of a library
#'
#' @param intel_dir A folder with `graph.json` and `health.json`.
#' @param out_dir Where to write the figure.
#' @param label How the caption names the code graph.
#' @return The path of the figure, invisibly.
#' @export
render_library <- function(intel_dir, out_dir, label = basename(normalizePath(intel_dir, mustWork = FALSE))) {
  snapshot <- maithicstats::read_intel_snapshot(intel_dir, label = label)
  areas <- nrow(maithicstats::declarations_by_area(snapshot))
  invisible(save_figure(plot_library_areas(snapshot), file.path(out_dir, "library-areas.svg"), height = max(3.6, 1.5 + 0.3 * areas)))
}

#' Redraw the figure of search recall
#'
#' @param queries A query set, as `maithic search --queries` reads it.
#' @param rankings Paths of rankings written by `maithic search --rankings`.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_search <- function(queries, rankings, out_dir) {
  queryset <- maithicstats::read_query_set(queries)
  engines <- lapply(rankings, maithicstats::read_rankings)
  invisible(save_figure(plot_recall_curve(queryset, engines), file.path(out_dir, "search-recall.svg"), height = 4.2))
}
