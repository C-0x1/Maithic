# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Tables from a snapshot of a codebase.

#' The declarations that most others depend on
#'
#' Counts, for every declaration, the declarations of the scanned files that
#' use it: in their statement, in their definition or proof, or in both. A
#' declaration that uses another in several places counts once, and none
#' counts as a user of itself. These are the declarations whose change
#' reaches furthest, and the ones a newcomer should read first.
#'
#' @param snapshot A snapshot from [read_intel_snapshot()].
#' @param n How many rows to return, from the most used down; `Inf` for all.
#' @param internal_only Rank only the declarations of the scanned files,
#'   which is the default. Otherwise the declarations of the libraries the
#'   code leans on are ranked with them.
#' @return A data frame with `rank`, `name`, `kind`, `file`, `line`,
#'   `used_by` (how many declarations use it), `in_statements` and `in_proofs`
#'   (how many of them use it in their statement, and in their definition or
#'   proof), and `share` (the part of the scanned declarations that use it).
#'   Ties are ranked alike and ordered by name.
#' @export
top_depended <- function(snapshot, n = 10, internal_only = TRUE) {
  snapshot <- need_snapshot(snapshot)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 0) stop("`n` must be a count, or Inf", call. = FALSE)
  nodes <- snapshot$nodes[snapshot$nodes$kind == "declaration", , drop = FALSE]
  scanned <- sum(nodes$internal)
  uses <- snapshot$edges[snapshot$edges$kind == "uses" & snapshot$edges$source != snapshot$edges$target, , drop = FALSE]
  uses <- uses[!duplicated(uses[c("source", "target")]), , drop = FALSE]
  count_for <- function(edges) {
    counts <- table(factor(edges$target, levels = nodes$id))
    as.integer(counts)
  }
  out <- data.frame(
    name = nodes$name, kind = nodes$decl_kind, file = nodes$file, line = nodes$line, internal = nodes$internal,
    used_by = count_for(uses),
    in_statements = count_for(uses[uses$via %in% c("type", "both"), , drop = FALSE]),
    in_proofs = count_for(uses[uses$via %in% c("value", "both"), , drop = FALSE]),
    stringsAsFactors = FALSE
  )
  if (internal_only) out <- out[out$internal, , drop = FALSE]
  out$share <- if (scanned == 0) NA_real_ else out$used_by / scanned
  out <- out[order(-out$used_by, out$name, method = "radix"), , drop = FALSE]
  out$rank <- rank(-out$used_by, ties.method = "min")
  out <- out[c("rank", "name", "kind", "file", "line", "used_by", "in_statements", "in_proofs", "share")]
  if (is.finite(n)) out <- utils::head(out, n)
  rownames(out) <- NULL
  out
}

#' Declarations by area of a library
#'
#' Counts the declarations of the scanned files by where they are and by what
#' they are. An area is named by the folders of a file below the folder that
#' all scanned files share; a file directly in that folder is an area of its
#' own, named after the file.
#'
#' @param snapshot A snapshot from [read_intel_snapshot()].
#' @param depth How many folders name an area.
#' @return A data frame with one row for each area, from the largest down:
#'   `area`, `theorems`, `definitions`, `others` (structures, classes,
#'   inductive types, instances and the rest), `declarations` (their sum) and
#'   `files`.
#' @export
declarations_by_area <- function(snapshot, depth = 2) {
  snapshot <- need_snapshot(snapshot)
  if (!is.numeric(depth) || length(depth) != 1 || is.na(depth) || depth < 1) stop("`depth` must be a count of at least 1", call. = FALSE)
  nodes <- snapshot$nodes[snapshot$nodes$kind == "declaration" & snapshot$nodes$internal & !is.na(snapshot$nodes$file), , drop = FALSE]
  empty <- data.frame(area = character(), theorems = integer(), definitions = integer(), others = integer(), declarations = integer(),
                      files = integer(), stringsAsFactors = FALSE)
  if (nrow(nodes) == 0) return(empty)
  parts <- strsplit(nodes$file, "/", fixed = TRUE)
  folders <- lapply(parts, function(p) p[-length(p)])
  # The folders that every file shares are left out of the names.
  shared <- 0L
  while (all(lengths(folders) > shared) && length(unique(vapply(folders, function(f) f[shared + 1L], character(1)))) == 1) shared <- shared + 1L
  nodes$area <- vapply(seq_along(parts), function(i) {
    below <- if (shared == 0L) folders[[i]] else folders[[i]][-seq_len(shared)]
    if (length(below) == 0) sub("\\.[A-Za-z]+$", "", parts[[i]][length(parts[[i]])]) else paste(utils::head(below, depth), collapse = "/")
  }, character(1))
  areas <- sort(unique(nodes$area), method = "radix")
  count <- function(keep) vapply(areas, function(a) sum(nodes$area == a & keep), integer(1))
  theorem <- !is.na(nodes$decl_kind) & nodes$decl_kind == "theorem"
  definition <- !is.na(nodes$decl_kind) & nodes$decl_kind == "definition"
  table <- data.frame(area = areas, theorems = count(theorem), definitions = count(definition), others = count(!theorem & !definition),
                      declarations = count(TRUE), files = vapply(areas, function(a) length(unique(nodes$file[nodes$area == a])), integer(1)),
                      stringsAsFactors = FALSE, row.names = NULL)
  table <- table[order(-table$declarations, match(table$area, areas)), , drop = FALSE]
  rownames(table) <- NULL
  table
}

#' Report table of a codebase snapshot
#'
#' The state of a codebase as Markdown, for a report: what was scanned, what
#' is unfinished, which axioms are relied on beyond those of Lean's logic,
#' and the declarations most others depend on.
#'
#' @param snapshot A snapshot from [read_intel_snapshot()].
#' @param n How many of the most depended-upon declarations to list.
#' @return A character vector of Markdown lines.
#' @export
intel_report_table <- function(snapshot, n = 10) {
  snapshot <- need_snapshot(snapshot)
  health <- snapshot$health
  kinds <- data.frame(kind = names(health$by_kind), declarations = as.integer(health$by_kind), stringsAsFactors = FALSE)
  kinds$share <- if (health$declarations == 0) NA_real_ else kinds$declarations / health$declarations
  own <- snapshot$axioms[!snapshot$axioms$standard, , drop = FALSE]
  out <- c(
    sprintf("%d declaration(s) in %d file(s): %d documented, %d resting on `sorry`, %d file(s) with errors.",
            health$declarations, health$files, health$documented, health$resting_on_sorry, health$files_with_errors), "",
    "**Declarations by kind**", "",
    markdown_table(kinds, percent = "share", labels = c(kind = "Kind", declarations = "Declarations", share = "Share")), "",
    "**Most depended upon**", "",
    markdown_table(top_depended(snapshot, n)[c("rank", "name", "kind", "used_by", "share")], percent = "share",
                   labels = c(rank = "Rank", name = "Declaration", kind = "Kind", used_by = "Used by", share = "Share"))
  )
  if (nrow(snapshot$unfinished) > 0) {
    out <- c(out, "", "**Unfinished**", "",
             markdown_table(snapshot$unfinished[c("name", "file", "line", "through")],
                            labels = c(name = "Declaration", file = "File", line = "Line", through = "Through")))
  }
  if (nrow(own) > 0) {
    out <- c(out, "", "**Axioms beyond Lean's logic**", "",
             markdown_table(own[c("axiom", "used_by")], labels = c(axiom = "Axiom", used_by = "Used by")))
  }
  out
}
