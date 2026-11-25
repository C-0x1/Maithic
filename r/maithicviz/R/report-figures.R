# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures for the prover, the ledger, the gateway and retrieval, drawn from
# the summaries of maithicstats.

percent_scale <- function() {
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), labels = function(x) sprintf("%d%%", round(100 * x)),
                              expand = ggplot2::expansion(mult = c(0, 0.02)))
}

contrast <- function(dark) if (dark) maithic_colors()[["paper"]] else maithic_colors()[["ink"]]

#' Colours of the result statuses
#'
#' One colour per status: teal for what the kernel verified, indigo for what
#' it only checked, coral for what failed or was refuted, gold for what is
#' conjectured, and slate and mist for what is not known.
#'
#' @return A named character vector of hexadecimal colours.
#' @export
status_colors <- function() {
  colors <- maithic_colors()
  c(verified = colors[["teal"]], checked = colors[["indigo"]], refuted = colors[["coral"]], conjectured = colors[["gold"]],
    unverified = colors[["slate"]], unknown = colors[["mist"]], failed = colors[["coral"]], abstained = colors[["slate"]])
}

runs_caption <- function(runs) {
  modes <- paste(sort(unique(runs$runs$mode), method = "radix"), collapse = ", ")
  sprintf("Source: %d run(s), %d theorem(s), gateway mode %s.", nrow(runs$runs), nrow(runs$results), modes)
}

#' Success rate of the prover, with intervals
#'
#' One row for each group: a dot at the share of its theorems that were
#' proved, on a line that spans the 95% Wilson interval. The label of the
#' row gives the counts.
#'
#' @param runs Prove runs from [maithicstats::read_prove_runs()].
#' @param by The column of the results to group by.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_prover_success <- function(runs, by = "provider", dark = FALSE) {
  table <- maithicstats::prover_success(runs, by = by)
  table$row <- sprintf("%s (%d of %d)", table[[by]], table$proved, table$n)
  table$row <- factor(table$row, levels = rev(table$row))
  ggplot2::ggplot(table, ggplot2::aes(y = .data$row)) +
    interval_marks(maithic_colors()[["teal"]]) +
    ggplot2::labs(title = sprintf("Theorems proved by %s", by),
                  subtitle = "A theorem counts only when the kernel, the trust policy and the re-check all accept its proof",
                  x = NULL, y = NULL, caption = paste(runs_caption(runs), "Lines show 95% Wilson intervals.")) +
    theme_maithic(dark = dark) +
    rate_rows(dark)
}

#' Where the time of a proof goes
#'
#' One bar per theorem and run, in two parts that add up to the whole
#' attempt: the time spent in Lean checking candidates and re-checking the
#' result, and everything else.
#'
#' @param runs Prove runs from [maithicstats::read_prove_runs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_prover_time <- function(runs, dark = FALSE) {
  colors <- maithic_colors()
  results <- runs$results
  results$label <- sprintf("%s\n%s", results$theorem, results$provider)
  results$label <- factor(results$label, levels = unique(results$label))
  parts <- c("in Lean", "everything else")
  long <- rbind(
    data.frame(label = results$label, part = parts[1], seconds = results$lean_ms / 1000),
    data.frame(label = results$label, part = parts[2], seconds = pmax(0, results$elapsed_ms - results$lean_ms) / 1000)
  )
  # The part in Lean stands on the axis, and the rest on top of it.
  long$part <- factor(long$part, levels = rev(parts))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$label, y = .data$seconds, fill = .data$part)) +
    ggplot2::geom_col(width = 0.58) +
    ggplot2::scale_fill_manual(values = stats::setNames(c(colors[["teal"]], colors[["gold"]]), parts), breaks = parts) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = "Where the time of a proof goes",
                  subtitle = "Seconds per theorem: the part spent in Lean on candidates and the re-check, and everything else",
                  x = NULL, y = "seconds", caption = runs_caption(runs)) +
    theme_maithic(dark = dark)
}

#' Why candidate proofs were rejected
#'
#' @param runs Prove runs from [maithicstats::read_prove_runs()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_prover_failures <- function(runs, dark = FALSE) {
  failures <- maithicstats::prover_failures(runs)
  classes <- failures[failures$kind == "error_class", , drop = FALSE]
  if (nrow(classes) == 0) stop("no candidate proof was rejected in these runs", call. = FALSE)
  classes$name <- factor(classes$name, levels = rev(classes$name))
  ggplot2::ggplot(classes, ggplot2::aes(x = .data$name, y = .data$count)) +
    ggplot2::geom_col(fill = maithic_colors()[["coral"]], width = 0.62) +
    bar_end_text(ggplot2::aes(label = .data$count), max(classes$count), dark, size = 3.6) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Why candidate proofs were rejected",
                  subtitle = "Rejected candidates by the class of Lean's error; every rejection is fed back into the next round",
                  x = NULL, y = "candidates", caption = runs_caption(runs)) +
    theme_maithic(dark = dark) +
    labelled_bars()
}

records_caption <- function(records) {
  sources <- table(records$source)
  sprintf("Source: %d result(s), %s.", nrow(records), paste(sprintf("%d from %s", as.integer(sources), names(sources)), collapse = ", "))
}

#' Results by trust level
#'
#' One row for each status that occurs: a dot at its share of the results,
#' on a line that spans the 95% Wilson interval. The label of the row gives
#' the number of results.
#'
#' @param records Ledger records from [maithicstats::ledger_records()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_trust_distribution <- function(records, dark = FALSE) {
  table <- maithicstats::trust_distribution(records)
  table$row <- sprintf("%s (%d result%s)", table$status, table$count, ifelse(table$count == 1, "", "s"))
  table$row <- factor(table$row, levels = rev(table$row))
  table$rate <- table$share
  table$shade <- factor(table$status, levels = table$status)
  ggplot2::ggplot(table, ggplot2::aes(y = .data$row)) +
    interval_marks() +
    ggplot2::scale_colour_manual(values = status_colors()) +
    ggplot2::labs(title = "Results by trust level",
                  subtitle = "Share of results at each status, with the number of results beside its name",
                  x = NULL, y = NULL, caption = paste(records_caption(records), "Lines show 95% Wilson intervals.")) +
    theme_maithic(dark = dark) +
    rate_rows(dark)
}

#' The axioms that results rest on
#'
#' @param records Ledger records from [maithicstats::ledger_records()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_axiom_census <- function(records, dark = FALSE) {
  colors <- maithic_colors()
  census <- maithicstats::axiom_census(records)
  census$axiom <- factor(census$axiom, levels = rev(census$axiom))
  fills <- c(standard = colors[["indigo"]], none = colors[["teal"]], incomplete = colors[["coral"]], native = colors[["gold"]], other = colors[["slate"]])
  ggplot2::ggplot(census, ggplot2::aes(x = .data$axiom, y = .data$results, fill = .data$class)) +
    ggplot2::geom_col(width = 0.62) +
    bar_end_text(ggplot2::aes(label = .data$results), max(census$results), dark, size = 3.6) +
    ggplot2::scale_fill_manual(values = fills, breaks = intersect(names(fills), census$class)) +
    ggplot2::scale_y_continuous(breaks = count_breaks, expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "The axioms that results rest on",
                  subtitle = "Results per axiom, from the kernel's own account of each proof; a result may rest on several",
                  x = NULL, y = "results", caption = records_caption(records)) +
    theme_maithic(dark = dark) +
    labelled_bars()
}

usage_caption <- function(usage) {
  served <- table(usage$served_by)
  sprintf("Source: %d call(s), served by %s.", nrow(usage), paste(sprintf("%s (%d)", names(served), as.integer(served)), collapse = ", "))
}

#' Tokens by model
#'
#' @param usage Usage records from [maithicstats::read_usage()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_usage_tokens <- function(usage, dark = FALSE) {
  colors <- maithic_colors()
  tokens <- maithicstats::token_breakdown(usage, by = "model")
  long <- rbind(data.frame(model = tokens$model, kind = "input", tokens = tokens$input_tokens),
                data.frame(model = tokens$model, kind = "output", tokens = tokens$output_tokens))
  long$kind <- factor(long$kind, levels = c("output", "input"))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$model, y = .data$tokens, fill = .data$kind)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::scale_fill_manual(values = c(input = colors[["indigo"]], output = colors[["gold"]]), breaks = c("input", "output")) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = "Tokens by model", subtitle = "Input and output tokens of every call, as the providers counted them",
                  x = NULL, y = "tokens", caption = usage_caption(usage)) +
    theme_maithic(dark = dark)
}

#' Latency of model calls
#'
#' Every successful call as a point, with the median of each model marked.
#'
#' @param usage Usage records from [maithicstats::read_usage()].
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_call_latency <- function(usage, dark = FALSE) {
  colors <- maithic_colors()
  calls <- usage[usage$ok, , drop = FALSE]
  if (nrow(calls) == 0) stop("no successful call to draw", call. = FALSE)
  calls$seconds <- calls$elapsed_ms / 1000
  medians <- maithicstats::call_latency(usage, by = "model")
  medians$seconds <- medians$median / 1000
  ggplot2::ggplot(calls, ggplot2::aes(x = .data$model, y = .data$seconds)) +
    ggplot2::geom_point(colour = colors[["gold"]], size = 2.6, alpha = 0.8, position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1)) +
    ggplot2::geom_crossbar(data = medians, ggplot2::aes(ymin = .data$seconds, ymax = .data$seconds), width = 0.45, colour = contrast(dark), linewidth = 0.35) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = "Latency of model calls", subtitle = "Seconds per successful call; the bar marks the median of each model",
                  x = NULL, y = "seconds", caption = usage_caption(usage)) +
    theme_maithic(dark = dark)
}

engine_palette <- function(names) {
  colors <- maithic_colors()
  palette <- unname(colors[c("teal", "gold", "indigo", "coral", "julia", "r", "ocaml", "slate")])
  stats::setNames(rep_len(palette, length(names)), names)
}

as_engine_list <- function(engines) if (inherits(engines, "maithic_rankings")) list(engines) else engines

#' Recall curves of retrieval engines
#'
#' Recall at each cutoff, one line per engine, with bootstrap intervals as
#' bands.
#'
#' @param queryset A query set from [maithicstats::read_query_set()].
#' @param engines Rankings from [maithicstats::read_rankings()], or a list of them.
#' @param k The cutoffs.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_recall_curve <- function(queryset, engines, k = c(1, 3, 5, 10), dark = FALSE) {
  engines <- as_engine_list(engines)
  curve <- do.call(rbind, lapply(engines, function(e) maithicstats::recall_at_k(queryset, e, k = k)))
  palette <- engine_palette(unique(curve$engine))
  ggplot2::ggplot(curve, ggplot2::aes(x = .data$k, y = .data$recall, colour = .data$engine, fill = .data$engine)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_x_continuous(breaks = sort(unique(curve$k))) +
    percent_scale() +
    ggplot2::labs(title = "Recall of retrieval engines",
                  subtitle = "Share of the relevant declarations among the first k results, averaged over the queries",
                  x = "k", y = NULL,
                  caption = sprintf("Source: query set %s, %d queries. Bands show 95%% bootstrap intervals.", queryset$name, nrow(queryset$queries))) +
    theme_maithic(dark = dark)
}

#' Latency of queries
#'
#' Every query as a point, with the median of each engine marked.
#'
#' @param engines Rankings from [maithicstats::read_rankings()], or a list of them.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_query_latency <- function(engines, dark = FALSE) {
  engines <- as_engine_list(engines)
  points <- do.call(rbind, lapply(engines, function(e) data.frame(engine = e$engine, ms = e$results$elapsed_ms, stringsAsFactors = FALSE)))
  medians <- maithicstats::query_latency(engines)
  ggplot2::ggplot(points, ggplot2::aes(x = .data$engine, y = .data$ms)) +
    ggplot2::geom_point(colour = maithic_colors()[["teal"]], size = 2.6, alpha = 0.8, position = ggplot2::position_jitter(width = 0.12, height = 0, seed = 1)) +
    ggplot2::geom_crossbar(data = medians, ggplot2::aes(y = .data$median, ymin = .data$median, ymax = .data$median), width = 0.45, colour = contrast(dark), linewidth = 0.35) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = "Latency of queries", subtitle = "Milliseconds per query; the bar marks the median of each engine",
                  x = NULL, y = "milliseconds", caption = sprintf("Source: %d engine(s), %d queries each.", length(engines), nrow(engines[[1]]$results))) +
    theme_maithic(dark = dark)
}

#' Redraw the figures of the recorded runs
#'
#' Reads the prove runs and check reports in a folder and writes the figures
#' made from them as SVG: the prover's success, time and rejections, and the
#' ledger's trust levels and axioms.
#'
#' @param run_dir A folder with `prove-*.json` and `check-*.json` files.
#' @param out_dir Where to write the figures.
#' @return The paths of the figures, invisibly.
#' @export
render_reports <- function(run_dir, out_dir) {
  prove <- sort(list.files(run_dir, pattern = "^prove-.*\\.json$", full.names = TRUE), method = "radix")
  check <- sort(list.files(run_dir, pattern = "^check-.*\\.json$", full.names = TRUE), method = "radix")
  if (length(prove) == 0 || length(check) == 0) stop(sprintf("%s holds no prove runs or no check reports", run_dir), call. = FALSE)
  runs <- maithicstats::read_prove_runs(prove)
  records <- maithicstats::ledger_records(maithicstats::read_check_reports(check), runs)
  figures <- list(
    `prover-success` = plot_prover_success(runs), `prover-time` = plot_prover_time(runs), `prover-rejections` = plot_prover_failures(runs),
    `ledger-trust` = plot_trust_distribution(records), `ledger-axioms` = plot_axiom_census(records)
  )
  rejections <- maithicstats::prover_failures(runs)
  heights <- c(`prover-success` = rows_height(nrow(maithicstats::prover_success(runs, by = "provider"))), `prover-time` = 4.8,
               `prover-rejections` = rows_height(sum(rejections$kind == "error_class")) + 0.3,
               `ledger-trust` = rows_height(nrow(maithicstats::trust_distribution(records))),
               `ledger-axioms` = rows_height(nrow(maithicstats::axiom_census(records))) + 0.5)
  paths <- vapply(names(figures), function(name) save_figure(figures[[name]], file.path(out_dir, sprintf("%s.svg", name)), height = heights[[name]]),
                  character(1))
  invisible(unname(paths))
}
