# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading what `maithic intel scan` leaves behind: the code graph, the health
# snapshot, and the metrics computed from the graph.

graph_node_kinds <- function() c("declaration", "module", "namespace", "file")
graph_edge_kinds <- function() c("uses", "imports", "contains", "instance-of", "extends")

# One column of a table from a list of JSON objects: the member `name` of each.
texts_of <- function(items, name, default = "") vapply(items, text_field, character(1), name, default, USE.NAMES = FALSE)
counts_of <- function(items, name, default = 0L) vapply(items, count_field, integer(1), name, default, USE.NAMES = FALSE)
numbers_of <- function(items, name, default = NA_real_) vapply(items, number_field, numeric(1), name, default, USE.NAMES = FALSE)
flags_of <- function(items, name, default = NA) vapply(items, flag_field, logical(1), name, default, USE.NAMES = FALSE)

parse_graph <- function(path) {
  graph <- read_document(path, "maithic.intel.graph/1", "a code graph")
  nodes <- data.frame(
    id = texts_of(graph$nodes, "id"), kind = texts_of(graph$nodes, "kind"), name = texts_of(graph$nodes, "name"),
    internal = flags_of(graph$nodes, "internal", FALSE), decl_kind = texts_of(graph$nodes, "declKind", NA_character_),
    file = texts_of(graph$nodes, "file", NA_character_), line = counts_of(graph$nodes, "nameLine", NA_integer_),
    uses_sorry = flags_of(graph$nodes, "usesSorry"), contains_sorry = flags_of(graph$nodes, "containsSorry"),
    documented = flags_of(graph$nodes, "documented"), stringsAsFactors = FALSE
  )
  # The line of a declaration is where its name stands; a graph that does not say takes where the declaration begins.
  unnamed <- is.na(nodes$line)
  nodes$line[unnamed] <- counts_of(graph$nodes, "line", NA_integer_)[unnamed]
  unknown <- setdiff(nodes$kind, graph_node_kinds())
  if (length(unknown) > 0) stop(sprintf("%s: a node of the unknown kind '%s'", path, unknown[1]), call. = FALSE)
  if (anyDuplicated(nodes$id) > 0) stop(sprintf("%s: the node %s occurs twice", path, nodes$id[anyDuplicated(nodes$id)]), call. = FALSE)
  nodes$axioms <- lapply(graph$nodes, texts_field, "axioms")
  edges <- data.frame(source = texts_of(graph$edges, "source"), target = texts_of(graph$edges, "target"), kind = texts_of(graph$edges, "kind"),
                      via = texts_of(graph$edges, "via", NA_character_), stringsAsFactors = FALSE)
  unknown <- setdiff(edges$kind, graph_edge_kinds())
  if (length(unknown) > 0) stop(sprintf("%s: an edge of the unknown kind '%s'", path, unknown[1]), call. = FALSE)
  loose <- setdiff(c(edges$source, edges$target), nodes$id)
  if (length(loose) > 0) stop(sprintf("%s: an edge touches %s, which is not a node of the graph", path, loose[1]), call. = FALSE)
  list(nodes = nodes, edges = edges)
}

parse_health <- function(path) {
  health <- read_document(path, "maithic.intel.health/1", "a health snapshot")
  by_kind <- vapply(health$byKind, function(count) as.integer(count), integer(1))
  by_kind <- by_kind[order(names(by_kind), method = "radix")]
  totals <- list(files = count_field(health, "files"), files_with_errors = count_field(health, "filesWithErrors"),
                 declarations = count_field(health, "declarations"), documented = count_field(health, "documented"),
                 resting_on_sorry = count_field(health, "restingOnSorry"), by_kind = by_kind)
  if (sum(by_kind) != totals$declarations) {
    stop(sprintf("%s: %d declaration(s) by kind, but a stated total of %d", path, sum(by_kind), totals$declarations), call. = FALSE)
  }
  unfinished <- data.frame(name = texts_of(health$unfinished, "name"), file = texts_of(health$unfinished, "file"),
                           line = counts_of(health$unfinished, "line", NA_integer_), through = texts_of(health$unfinished, "through", NA_character_),
                           goals = vapply(health$unfinished, function(item) length(item$goals), integer(1), USE.NAMES = FALSE),
                           stringsAsFactors = FALSE)
  if (nrow(unfinished) != totals$resting_on_sorry) {
    stop(sprintf("%s: %d unfinished declaration(s) listed, but a stated count of %d", path, nrow(unfinished), totals$resting_on_sorry), call. = FALSE)
  }
  axioms <- data.frame(axiom = texts_of(health$axioms, "axiom"), standard = flags_of(health$axioms, "standard"),
                       used_by = vapply(health$axioms, function(item) length(item$usedBy), integer(1), USE.NAMES = FALSE),
                       stringsAsFactors = FALSE)
  axioms$users <- lapply(health$axioms, function(item) texts_of(item$usedBy, "name"))
  list(totals = totals, unfinished = unfinished, axioms = axioms)
}

parse_metrics <- function(path) {
  metrics <- read_document(path, "maithic.intel.metrics/1", "the metrics of a code graph")
  items <- metrics$declarations
  table <- data.frame(name = texts_of(items, "name"), kind = texts_of(items, "kind"), used_by = counts_of(items, "usedBy"),
                      uses = counts_of(items, "uses"), pagerank = numbers_of(items, "pagerank"), betweenness = numbers_of(items, "betweenness"),
                      stringsAsFactors = FALSE)
  if (nrow(table) != count_field(metrics, "internal")) {
    stop(sprintf("%s: figures for %d declaration(s), but a stated count of %d", path, nrow(table), count_field(metrics, "internal")), call. = FALSE)
  }
  list(table = table, nodes = count_field(metrics, "nodes"), edges = count_field(metrics, "edges"),
       components = vapply(metrics$components$sizes, function(size) as.integer(size), integer(1), USE.NAMES = FALSE))
}

#' Read a snapshot of a codebase
#'
#' Loads the files that `maithic intel scan` leaves in a folder: `graph.json`,
#' the declarations and what they use; `health.json`, what is unfinished and
#' which axioms are relied on; and, when it is there, `metrics.json`, the
#' figures computed from the graph. The documents are checked against each
#' other, because a table made from files that do not belong together is
#' worse than no table: every edge must join two nodes of the graph, the
#' health snapshot must count the declarations the graph holds, and the
#' metrics must speak of those declarations and no others.
#'
#' @param dir A folder written by `maithic intel scan`.
#' @param label A name for the snapshot in tables and figures; the name of
#'   the folder unless given.
#' @return An object of class `maithic_intel_snapshot`, a list with `label`,
#'   `nodes` and `edges` (the graph), `health` (the totals), `unfinished` and
#'   `axioms` (data frames) and `metrics` (a data frame, or `NULL` when the
#'   folder holds no metrics).
#' @export
read_intel_snapshot <- function(dir, label = basename(normalizePath(dir, mustWork = FALSE))) {
  if (!is.character(dir) || length(dir) != 1) stop("`dir` must be one folder", call. = FALSE)
  if (!dir.exists(dir)) stop(sprintf("folder not found: %s", dir), call. = FALSE)
  graph <- parse_graph(file.path(dir, "graph.json"))
  health <- parse_health(file.path(dir, "health.json"))
  mine <- graph$nodes[graph$nodes$kind == "declaration" & graph$nodes$internal, , drop = FALSE]
  if (nrow(mine) != health$totals$declarations) {
    stop(sprintf("%s: the graph holds %d declaration(s) of the scanned files and the health snapshot counts %d: they are not of the same scan",
                 dir, nrow(mine), health$totals$declarations), call. = FALSE)
  }
  metrics <- NULL
  metrics_path <- file.path(dir, "metrics.json")
  if (file.exists(metrics_path)) {
    parsed <- parse_metrics(metrics_path)
    strangers <- setdiff(parsed$table$name, mine$name)
    if (length(strangers) > 0 || nrow(parsed$table) != nrow(mine)) {
      stop(sprintf("%s: the metrics are not of this graph%s", dir,
                   if (length(strangers) > 0) sprintf(" (%s is not one of its declarations)", strangers[1]) else ""), call. = FALSE)
    }
    metrics <- parsed$table
    attr(metrics, "components") <- parsed$components
  }
  structure(list(label = label, nodes = graph$nodes, edges = graph$edges, health = health$totals, unfinished = health$unfinished,
                 axioms = health$axioms, metrics = metrics),
            class = "maithic_intel_snapshot")
}

need_snapshot <- function(snapshot) {
  if (!inherits(snapshot, "maithic_intel_snapshot")) stop("`snapshot` must come from read_intel_snapshot()", call. = FALSE)
  snapshot
}
