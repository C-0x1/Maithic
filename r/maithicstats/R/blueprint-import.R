# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the status snapshots that `maithic blueprint status` writes.

# How far a node of a blueprint is, from the one that is not in Lean yet to
# the one with nothing open below it.
blueprint_statuses <- function() c("planned", "stated", "proved", "done")

# What a node of a blueprint can be.
blueprint_kinds <- function() c("definition", "lemma", "proposition", "theorem", "corollary")

empty_blueprint_nodes <- function() {
  data.frame(snapshot = integer(), label = character(), id = character(), kind = character(), status = character(), ready = logical(),
             unblocks = integer(), blockers = integer(), notes = integer(), stringsAsFactors = FALSE)
}

parse_blueprint_status <- function(path, index, label) {
  document <- read_document(path, "maithic.blueprint.status/1", "a blueprint status")
  if (is.null(label)) label <- text_field(document, "label")
  if (identical(label, "")) label <- tools::file_path_sans_ext(basename(path))
  listed <- document$nodes
  nodes <- data.frame(
    snapshot = rep(index, length(listed)), label = rep(label, length(listed)), id = texts_of(listed, "id"), kind = texts_of(listed, "kind"),
    status = texts_of(listed, "status"), ready = flags_of(listed, "ready", FALSE), unblocks = counts_of(listed, "unblocks"),
    blockers = vapply(listed, function(node) length(node$blockers), integer(1)), notes = vapply(listed, function(node) length(node$notes), integer(1)),
    stringsAsFactors = FALSE
  )
  unknown <- setdiff(nodes$status, blueprint_statuses())
  if (length(unknown) > 0) stop(sprintf("%s: a node with the unknown status '%s'", path, unknown[1]), call. = FALSE)
  unknown <- setdiff(nodes$kind, blueprint_kinds())
  if (length(unknown) > 0) stop(sprintf("%s: a node of the unknown kind '%s'", path, unknown[1]), call. = FALSE)
  if (anyDuplicated(nodes$id) > 0) stop(sprintf("%s: the node '%s' is listed twice", path, nodes$id[anyDuplicated(nodes$id)]), call. = FALSE)
  # A snapshot is checked against itself: its counts are counts of its nodes.
  counted <- vapply(blueprint_statuses(), function(status) sum(nodes$status == status), integer(1))
  stated <- vapply(blueprint_statuses(), function(status) count_field(document$summary$by_status, status), integer(1))
  if (count_field(document$summary, "nodes") != nrow(nodes) || !identical(unname(counted), unname(stated))) {
    stop(sprintf("%s: the summary does not count the nodes that are listed", path), call. = FALSE)
  }
  if (any(nodes$ready & nodes$status %in% c("proved", "done"))) stop(sprintf("%s: a node that is settled is listed as ready", path), call. = FALSE)
  if (any(nodes$ready & nodes$blockers > 0)) stop(sprintf("%s: a node that waits for another is listed as ready", path), call. = FALSE)
  snapshot <- data.frame(
    snapshot = index, path = path, label = label, title = text_field(document, "title"), nodes = nrow(nodes), planned = counted[["planned"]],
    stated = counted[["stated"]], proved = counted[["proved"]], done = counted[["done"]], complete = nrow(nodes) > 0 && counted[["done"]] == nrow(nodes),
    ready = sum(nodes$ready), blocked = sum(nodes$blockers > 0), lean_errors = count_field(document$lean, "errors"), stringsAsFactors = FALSE
  )
  list(snapshot = snapshot, nodes = nodes)
}

#' Read the status snapshots of a blueprint
#'
#' Loads the reports that `maithic blueprint status` writes with `--out`,
#' in the order given: a series of them shows how a formalization went.
#' Each snapshot is checked against itself: its summary must count the
#' nodes it lists, and a node that is settled or that waits for another
#' cannot be ready.
#'
#' @param paths Paths of status reports, the earliest first. When the
#'   vector has names, they label the snapshots.
#' @param label A label for each snapshot; by default the names of `paths`,
#'   then the label a report carries, then its file name.
#' @return An object of class `maithic_blueprint_series`, a list of two data
#'   frames: `snapshots` (one row per report: its label, the number of nodes
#'   in each status, whether everything is done, and how many nodes are
#'   ready and how many wait) and `nodes` (one row per node and snapshot,
#'   with its kind, its status and what stands in its way).
#' @export
read_blueprint_snapshots <- function(paths, label = NULL) {
  if (length(paths) == 0) stop("no status report was given", call. = FALSE)
  if (is.null(label) && !is.null(names(paths))) label <- names(paths)
  if (!is.null(label) && length(label) != length(paths)) stop("`label` needs one entry for each path", call. = FALSE)
  parsed <- lapply(seq_along(paths), function(i) parse_blueprint_status(paths[[i]], i, if (is.null(label)) NULL else label[[i]]))
  snapshots <- do.call(rbind, lapply(parsed, function(p) p$snapshot))
  if (anyDuplicated(snapshots$label) > 0) stop(sprintf("two snapshots carry the label '%s'", snapshots$label[anyDuplicated(snapshots$label)]), call. = FALSE)
  rownames(snapshots) <- NULL
  structure(list(snapshots = snapshots, nodes = bind_frames(lapply(parsed, function(p) p$nodes), empty_blueprint_nodes())), class = "maithic_blueprint_series")
}
