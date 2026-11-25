# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of tactic models: how their training went, and how often they are
# right on proofs they were not trained on.

guide_caption <- function(runs, accuracy) {
  table <- runs$runs
  examples <- range(accuracy$examples)
  tested <- if (examples[1] == examples[2]) sprintf("each model was tested on %s proof step(s)", format(examples[1], big.mark = ","))
            else sprintf("the models were tested on %s to %s proof steps", format(examples[1], big.mark = ","), format(examples[2], big.mark = ","))
  sprintf("Source: %d training run(s) on %s; %s from proofs it was not trained on.", nrow(table), paste(unique(table$dataset), collapse = ", "),
          tested)
}

#' Learning curves of a tactic model
#'
#' One panel for each run of one model: its training loss and its
#' validation loss, epoch by epoch, with a mark at the epoch whose model was
#' kept. The training loss keeps falling; the validation loss turns up once
#' the model starts to learn its training examples by heart, and the model
#' that is kept is the one from before the turn.
#'
#' @param runs Runs from [maithicstats::read_guide_runs()].
#' @param model The model whose runs are drawn; by default the most accurate.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_learning_curves <- function(runs, model = NULL, dark = FALSE) {
  colors <- maithic_colors()
  if (is.null(model)) model <- maithicstats::model_accuracy(runs)$name[1]
  curves <- maithicstats::learning_curves(runs, "loss")
  curves <- curves[curves$name == model, , drop = FALSE]
  if (nrow(curves) == 0) stop(sprintf("there is no run of a model called '%s'", model), call. = FALSE)
  summary <- maithicstats::curve_summary(runs)
  summary <- summary[summary$name == model, , drop = FALSE]
  # One panel for each seed, named by the seed and the epoch that was kept.
  panels <- stats::setNames(sprintf("seed %s: epoch %d kept", format(summary$seed), summary$best_epoch), summary$run)
  curves$panel <- factor(panels[as.character(curves$run)], levels = unname(panels))
  kept <- curves[curves$best & curves$part == "validation", , drop = FALSE]
  ggplot2::ggplot(curves, ggplot2::aes(x = .data$epoch, y = .data$value, colour = .data$part)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(data = kept, ggplot2::aes(shape = "the model that was kept"), colour = colors[["teal"]], size = 2.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), nrow = 1) +
    ggplot2::scale_colour_manual(values = c(training = colors[["indigo"]], validation = colors[["gold"]]),
                                 labels = c(training = "training loss", validation = "validation loss")) +
    ggplot2::scale_shape_manual(values = c(`the model that was kept` = 16)) +
    ggplot2::scale_x_continuous(breaks = function(limits) unique(round(pretty(limits, n = 3)))) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = sprintf("How the training of %s went", model),
                  subtitle = "One panel for each seed. Training goes on after the validation loss has turned; the model of the turn is kept",
                  x = "epoch", y = "loss",
                  caption = sprintf("Source: %d run(s) of %s on %s; the models kept are those of epochs %s.", nrow(summary), model,
                                    paste(unique(runs$runs$dataset[runs$runs$name == model]), collapse = ", "),
                                    paste(summary$best_epoch, collapse = ", "))) +
    theme_maithic(dark = dark) +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1.4, "lines"), strip.text = ggplot2::element_text(colour = contrast(dark), hjust = 0))
}

#' Accuracy of tactic models on held-out proofs
#'
#' For each model, how often the tactic template of a held-out proof step was
#' its first answer and how often it was among its first `k`: a dot at the
#' share, on a line that spans the 95% Wilson interval, with the share written
#' at the end of the row. The last row is the baseline that knows no goal. A
#' model is worth using when its interval stands clear of the baseline.
#'
#' @param runs Runs from [maithicstats::read_guide_runs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_model_accuracy <- function(runs, dark = FALSE) {
  colors <- maithic_colors()
  accuracy <- maithicstats::model_accuracy(runs)
  k <- accuracy$k[1]
  measures <- c("first answer", sprintf("among the first %d", k))
  baseline <- "baseline"
  rows <- function(measure, share, lower, upper, base) {
    rbind(data.frame(name = accuracy$name, measure = measure, rate = share, lower = lower, upper = upper, kind = "model", stringsAsFactors = FALSE),
          data.frame(name = baseline, measure = measure, rate = base[1], lower = NA_real_, upper = NA_real_, kind = "baseline", stringsAsFactors = FALSE))
  }
  long <- rbind(rows(measures[1], accuracy$top1, accuracy$top1_lower, accuracy$top1_upper, accuracy$baseline_top1),
                rows(measures[2], accuracy$topk, accuracy$topk_lower, accuracy$topk_upper, accuracy$baseline_topk))
  long$name <- factor(long$name, levels = rev(c(accuracy$name, baseline)))
  long$measure <- factor(long$measure, levels = measures)
  long$note <- sprintf("%.0f%%", 100 * long$rate)
  models <- long[long$kind == "model", , drop = FALSE]
  ggplot2::ggplot(long, ggplot2::aes(y = .data$name)) +
    ggplot2::geom_linerange(data = models, ggplot2::aes(xmin = .data$lower, xmax = .data$upper), colour = colors[["teal"]], linewidth = 2.4, alpha = 0.45) +
    ggplot2::geom_point(ggplot2::aes(x = .data$rate, colour = .data$kind, shape = .data$kind), size = 3.4, stroke = 1.3, show.legend = FALSE) +
    # The shares stand in a column to the right of the last grid line, where no mark can reach them.
    ggplot2::geom_text(ggplot2::aes(x = 1.04, label = .data$note), hjust = 0, colour = contrast(dark), size = 3.4) +
    ggplot2::facet_wrap(ggplot2::vars(.data$measure), nrow = 1) +
    ggplot2::scale_colour_manual(values = c(model = colors[["teal"]], baseline = colors[["coral"]])) +
    ggplot2::scale_shape_manual(values = c(model = 16, baseline = 4)) +
    ggplot2::scale_y_discrete(limits = levels(long$name)) +
    ggplot2::labs(title = "How often a tactic model names the next tactic",
                  subtitle = "Share of held-out proof steps a model got right, with 95% Wilson intervals.\nThe baseline answers with the most frequent templates, whatever the goal.",
                  x = NULL, y = NULL, caption = guide_caption(runs, accuracy)) +
    theme_maithic(dark = dark) +
    rate_rows(dark, limit = 1.16) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(family = "mono"), panel.spacing.x = ggplot2::unit(2.2, "lines"),
                   strip.text = ggplot2::element_text(colour = contrast(dark), face = "bold", hjust = 0))
}

#' Redraw the figures of the tactic models
#'
#' Reads the records of training runs in a folder and writes the figures
#' made from them as SVG.
#'
#' @param runs_dir A folder of run records, as the comparison script of the
#'   Julia package `MaithicGuide` writes them.
#' @param out_dir Where to write the figures.
#' @return The paths of the figures, invisibly.
#' @export
render_guide <- function(runs_dir, out_dir) {
  runs <- maithicstats::read_guide_runs(runs_dir)
  figures <- list(`guide-accuracy` = plot_model_accuracy(runs), `guide-learning-curves` = plot_learning_curves(runs))
  paths <- vapply(names(figures), function(name) save_figure(figures[[name]], file.path(out_dir, sprintf("%s.svg", name))), character(1))
  invisible(unname(paths))
}
