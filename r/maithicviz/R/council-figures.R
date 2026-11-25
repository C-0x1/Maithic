# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Figures of council sessions: who took which turn, and what came of it.

#' A council session, turn by turn
#'
#' One lane for each role and one mark for each turn, in the order the turns
#' were taken, joined by a line. What each turn did is written in a row of
#' its own under the lanes, below the mark of the turn, so that the line
#' between the marks never runs through the words. Read from left to right,
#' the lanes show how the work passes from one role to the next: from the
#' researcher to the prover, from the prover to the verifier, and back
#' through the critic when Lean rejects what was proposed.
#'
#' @param sessions Council sessions from [maithicstats::read_council_sessions()].
#' @param run Which of the sessions to draw.
#' @param dark Draw for a dark background.
#' @return A ggplot object.
#' @export
plot_council_session <- function(sessions, run = 1, dark = FALSE) {
  colors <- maithic_colors()
  if (!inherits(sessions, "maithic_council_sessions")) stop("`sessions` must come from maithicstats::read_council_sessions()", call. = FALSE)
  session <- sessions$sessions[sessions$sessions$run == run, , drop = FALSE]
  if (nrow(session) != 1) stop(sprintf("there is no session %s", format(run)), call. = FALSE)
  turns <- sessions$turns[sessions$turns$run == run, , drop = FALSE]
  if (nrow(turns) == 0) stop("the session has no turn", call. = FALSE)
  lanes <- c("researcher", "prover", "verifier", "critic")
  # The lanes stand at the heights 4 down to 1; the notes hang from a line under the last lane.
  turns$height <- length(lanes) + 1 - match(turns$role, lanes)
  turns$note <- vapply(turns$summary, function(text) paste(strwrap(text, 18), collapse = "\n"), character(1))
  turns$task <- factor(turns$task, levels = c("examine", "research", "prove", "verify", "critique"))
  note_lines <- max(lengths(strsplit(turns$note, "\n", fixed = TRUE)))
  note_top <- 0.3
  bottom <- note_top - 0.34 * note_lines - 0.1
  # The rounds as bands behind the lanes, each with its name above; the notes stand on plain ground.
  rounds <- do.call(rbind, lapply(sort(unique(turns$round)), function(round) {
    numbers <- turns$number[turns$round == round]
    data.frame(round = round, from = min(numbers) - 0.5, to = max(numbers) + 0.5, name = if (round == 0) "examination" else sprintf("round %d", round))
  }))
  rounds$shade <- factor(rounds$round %% 2)
  fills <- c(examine = colors[["slate"]], research = colors[["indigo"]], prove = colors[["gold"]], verify = colors[["teal"]], critique = colors[["coral"]])
  outcome <- switch(session$status, verified = "the claim is verified", refuted = "the claim is refuted", sprintf("the claim is %s", session$status))
  ggplot2::ggplot(turns, ggplot2::aes(x = .data$number, y = .data$height)) +
    ggplot2::geom_rect(data = rounds, ggplot2::aes(xmin = .data$from, xmax = .data$to, ymin = 0.5, ymax = Inf, fill = NULL, alpha = .data$shade),
                       inherit.aes = FALSE, fill = if (dark) "#2C2F4A" else colors[["mist"]], show.legend = FALSE) +
    ggplot2::scale_alpha_manual(values = c(`0` = 0.55, `1` = 0.2)) +
    ggplot2::geom_text(data = rounds, ggplot2::aes(x = (.data$from + .data$to) / 2, y = length(lanes) + 0.8, label = .data$name), inherit.aes = FALSE,
                       colour = if (dark) colors[["mist"]] else colors[["slate"]], size = 3.4) +
    ggplot2::geom_path(ggplot2::aes(group = 1), colour = if (dark) colors[["mist"]] else colors[["slate"]], linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$task), shape = 21, size = 6.5, stroke = 0.8, colour = contrast(dark)) +
    ggplot2::geom_text(ggplot2::aes(y = note_top, label = .data$note), vjust = 1, size = 2.9, lineheight = 0.95, colour = contrast(dark)) +
    ggplot2::scale_fill_manual(values = fills, drop = TRUE, name = NULL) +
    ggplot2::scale_x_continuous(breaks = turns$number, expand = ggplot2::expansion(add = 0.1)) +
    ggplot2::scale_y_continuous(breaks = rev(seq_along(lanes)), labels = lanes, limits = c(bottom, length(lanes) + 1.05), expand = ggplot2::expansion(add = 0)) +
    ggplot2::labs(title = "A council session, turn by turn",
                  subtitle = "Each role takes its turn and posts to a shared board; only what Lean accepts changes the status of the claim",
                  x = "turn", y = NULL,
                  caption = sprintf("Source: the session on %s: %d turn(s), %d question(s) to models, %d call(s) to Lean; %s.", session$theorem, nrow(turns),
                                    session$model_calls, session$lean_calls, outcome)) +
    # The grid of the theme draws the lanes: one line at the height of each role, and none where the notes are.
    theme_maithic(dark = dark)
}

#' Redraw the figure of a council session
#'
#' Reads a session written by `maithic council` and writes the figure made
#' from it as SVG.
#'
#' @param path The path of a council session.
#' @param out_dir Where to write the figure.
#' @return The path of the figure, invisibly.
#' @export
render_council <- function(path, out_dir) {
  sessions <- maithicstats::read_council_sessions(path)
  invisible(save_figure(plot_council_session(sessions), file.path(out_dir, "council-session.svg"), height = 5))
}
