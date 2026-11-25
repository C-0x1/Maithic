# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# What a series of status snapshots says about a formalization.

check_series <- function(series) {
  if (!inherits(series, "maithic_blueprint_series")) stop("`series` must come from read_blueprint_snapshots()", call. = FALSE)
}

#' The share of nodes in each status
#'
#' For every snapshot, how many of its nodes are planned, stated, proved and
#' done, and which share of the snapshot that is. Every status has a row in
#' every snapshot, also when no node is in it, so that the rows of a snapshot
#' add up to 1.
#'
#' @param series Snapshots from [read_blueprint_snapshots()].
#' @param by `"snapshot"` for the whole blueprint, or `"kind"` to tell
#'   definitions, lemmas and the other kinds apart.
#' @return A data frame with `snapshot`, `label`, with `by = "kind"` also
#'   `kind`, then `status`, `nodes` and `share`.
#' @export
status_composition <- function(series, by = c("snapshot", "kind")) {
  check_series(series)
  by <- match.arg(by)
  nodes <- series$nodes
  rows <- list()
  for (i in seq_len(nrow(series$snapshots))) {
    of_snapshot <- nodes[nodes$snapshot == series$snapshots$snapshot[i], , drop = FALSE]
    groups <- if (by == "kind") intersect(blueprint_kinds(), of_snapshot$kind) else NA_character_
    for (group in groups) {
      part <- if (is.na(group)) of_snapshot else of_snapshot[of_snapshot$kind == group, , drop = FALSE]
      counted <- vapply(blueprint_statuses(), function(status) sum(part$status == status), integer(1))
      row <- data.frame(snapshot = series$snapshots$snapshot[i], label = series$snapshots$label[i], kind = group, status = blueprint_statuses(),
                        nodes = unname(counted), share = if (nrow(part) == 0) 0 else unname(counted) / nrow(part), stringsAsFactors = FALSE)
      rows[[length(rows) + 1]] <- row
    }
  }
  out <- do.call(rbind, rows)
  if (by == "snapshot") out$kind <- NULL
  rownames(out) <- NULL
  out
}

#' The nodes whose status changed between snapshots
#'
#' Compares each snapshot with the one before it and lists the nodes that
#' moved, with where they were and where they are: the work that was done
#' between two snapshots, node by node. A node that a snapshot lists for the
#' first time comes `from` `NA`.
#'
#' @param series Snapshots from [read_blueprint_snapshots()].
#' @return A data frame with `snapshot`, `label`, `id`, `from`, `to` and
#'   `forward`, which says whether the node moved towards done.
#' @export
status_changes <- function(series) {
  check_series(series)
  rows <- list()
  rank <- function(status) match(status, blueprint_statuses())
  if (nrow(series$snapshots) > 1) {
    for (i in 2:nrow(series$snapshots)) {
      before <- series$nodes[series$nodes$snapshot == series$snapshots$snapshot[i - 1], , drop = FALSE]
      after <- series$nodes[series$nodes$snapshot == series$snapshots$snapshot[i], , drop = FALSE]
      was <- before$status[match(after$id, before$id)]
      moved <- is.na(was) | was != after$status
      if (any(moved)) {
        rows[[length(rows) + 1]] <- data.frame(snapshot = series$snapshots$snapshot[i], label = series$snapshots$label[i], id = after$id[moved], from = was[moved],
                                               to = after$status[moved], forward = is.na(was[moved]) | rank(after$status[moved]) > rank(was[moved]),
                                               stringsAsFactors = FALSE)
      }
    }
  }
  if (length(rows) == 0) {
    return(data.frame(snapshot = integer(), label = character(), id = character(), from = character(), to = character(), forward = logical(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' A report table for a series of snapshots
#'
#' @param series Snapshots from [read_blueprint_snapshots()].
#' @return Lines of Markdown: one row per snapshot, with the number of nodes
#'   in each status and the share that is done.
#' @export
blueprint_report_table <- function(series) {
  check_series(series)
  s <- series$snapshots
  table <- data.frame(snapshot = s$label, nodes = s$nodes, planned = s$planned, stated = s$stated, proved = s$proved, done = s$done,
                      share_done = ifelse(s$nodes == 0, 0, s$done / s$nodes), ready = s$ready, waiting = s$blocked, stringsAsFactors = FALSE)
  markdown_table(table, percent = "share_done", labels = c(share_done = "done (share)"))
}
