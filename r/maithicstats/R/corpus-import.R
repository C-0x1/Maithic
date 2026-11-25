# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading a dataset written by `maithic corpus harvest`.

corpus_kinds <- function() c("theorem", "step", "premise", "pair")

# The records of one shard, parsed, after the shard was checked against what
# the manifest says about it.
read_shard <- function(dir, shard, verify) {
  file <- text_field(shard, "file")
  path <- file.path(dir, file)
  if (!file.exists(path)) stop(sprintf("%s: the shard %s is missing", dir, file), call. = FALSE)
  if (verify && exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) {
    found <- unname(get("sha256sum", envir = asNamespace("tools"))(path))
    if (!identical(found, text_field(shard, "sha256"))) {
      stop(sprintf("%s: the shard %s does not have the hash the manifest states", dir, file), call. = FALSE)
    }
  }
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) != count_field(shard, "records")) {
    stop(sprintf("%s: the shard %s holds %d record(s), and the manifest states %d", dir, file, length(lines), count_field(shard, "records")), call. = FALSE)
  }
  schema <- sprintf("maithic.corpus.%s/1", text_field(shard, "kind"))
  lapply(lines, function(line) {
    record <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    if (!identical(record$schema, schema)) stop(sprintf("%s: a record of %s is not a %s record", dir, file, text_field(shard, "kind")), call. = FALSE)
    record
  })
}

theorem_table <- function(records) {
  data.frame(id = texts_of(records, "id"), name = texts_of(records, "name"), module = texts_of(records, "module"), file = texts_of(records, "file"),
             line = counts_of(records, "line", NA_integer_), statement = texts_of(records, "statement"), proof = texts_of(records, "proof"),
             steps = counts_of(records, "steps"), premises = counts_of(records, "premises"), complete = flags_of(records, "complete"),
             stringsAsFactors = FALSE)
}

step_table <- function(records) {
  out <- data.frame(id = texts_of(records, "id"), theorem = texts_of(records, "theorem"), module = texts_of(records, "module"),
                    index = counts_of(records, "index"), tactic = texts_of(records, "tactic"), depth = counts_of(records, "depth"),
                    inner = counts_of(records, "inner"), line = counts_of(records, "line", NA_integer_),
                    column = counts_of(records, "column", NA_integer_), stringsAsFactors = FALSE)
  out$goals_before <- lapply(records, texts_field, "goals_before")
  out$goals_after <- lapply(records, texts_field, "goals_after")
  out
}

premise_table <- function(records) {
  data.frame(id = texts_of(records, "id"), theorem = texts_of(records, "theorem"), module = texts_of(records, "module"),
             premise = texts_of(records, "premise"), stringsAsFactors = FALSE)
}

pair_table <- function(records) {
  data.frame(id = texts_of(records, "id"), theorem = texts_of(records, "theorem"), module = texts_of(records, "module"),
             state = texts_of(records, "state"), tactic = texts_of(records, "tactic"), goals_before = counts_of(records, "goals_before"),
             goals_after = counts_of(records, "goals_after"), stringsAsFactors = FALSE)
}

#' Read a dataset
#'
#' Loads a dataset written by `maithic corpus harvest`: its manifest and the
#' records of its shards, as one table for each kind of record. The dataset
#' is checked against its manifest while it is read: every shard must be
#' there and hold the number of records the manifest states, the totals must
#' add up, and every step, premise and pair must belong to a theorem of the
#' dataset. The hashes of the shards are checked as well where R can compute
#' SHA-256 (R 4.5 and later); `maithic corpus verify` checks them everywhere.
#'
#' @param dir The folder of the dataset: the one that holds `manifest.json`.
#' @param verify Check the hashes of the shards.
#' @return An object of class `maithic_corpus`, a list with `name`,
#'   `version`, `records` (the count of each kind), `sources` and `shards`
#'   (data frames from the manifest), and the tables `theorems`, `steps`,
#'   `premises` and `pairs`.
#' @export
read_corpus <- function(dir, verify = TRUE) {
  if (!is.character(dir) || length(dir) != 1) stop("`dir` must be one folder", call. = FALSE)
  if (!dir.exists(dir)) stop(sprintf("folder not found: %s", dir), call. = FALSE)
  manifest <- read_document(file.path(dir, "manifest.json"), "maithic.corpus.manifest/1", "the manifest of a dataset")
  unknown <- setdiff(texts_of(manifest$shards, "kind"), corpus_kinds())
  if (length(unknown) > 0) stop(sprintf("%s: a shard of the unknown kind '%s'", dir, unknown[1]), call. = FALSE)
  by_kind <- lapply(stats::setNames(corpus_kinds(), corpus_kinds()), function(kind) {
    shards <- Filter(function(shard) identical(text_field(shard, "kind"), kind), manifest$shards)
    do.call(c, c(list(list()), lapply(shards, function(shard) read_shard(dir, shard, verify))))
  })
  records <- vapply(corpus_kinds(), function(kind) count_field(manifest$records, kind), integer(1))
  for (kind in corpus_kinds()) {
    if (length(by_kind[[kind]]) != records[[kind]]) {
      stop(sprintf("%s: the manifest counts %d %s record(s), and its shards hold %d", dir, records[[kind]], kind, length(by_kind[[kind]])), call. = FALSE)
    }
  }
  out <- list(
    name = text_field(manifest, "name"), version = text_field(manifest, "version"), records = records,
    sources = data.frame(repository = texts_of(manifest$sources, "repository"), revision = texts_of(manifest$sources, "revision"),
                         toolchain = texts_of(manifest$sources, "toolchain"), files = counts_of(manifest$sources, "files"),
                         stringsAsFactors = FALSE),
    shards = data.frame(kind = texts_of(manifest$shards, "kind"), file = texts_of(manifest$shards, "file"),
                        records = counts_of(manifest$shards, "records"), sha256 = texts_of(manifest$shards, "sha256"), stringsAsFactors = FALSE),
    theorems = theorem_table(by_kind$theorem), steps = step_table(by_kind$step), premises = premise_table(by_kind$premise),
    pairs = pair_table(by_kind$pair)
  )
  key <- function(table, name) paste(table$module, table[[name]], sep = "\r")
  known <- key(out$theorems, "name")
  for (kind in c("steps", "premises", "pairs")) {
    orphan <- which(!key(out[[kind]], "theorem") %in% known)
    if (length(orphan) > 0) {
      stop(sprintf("%s: a record of the %s belongs to %s, which is not a theorem of the dataset", dir, kind, out[[kind]]$theorem[orphan[1]]), call. = FALSE)
    }
  }
  structure(out, class = "maithic_corpus")
}

need_corpus <- function(corpus) {
  if (!inherits(corpus, "maithic_corpus")) stop("`corpus` must come from read_corpus()", call. = FALSE)
  corpus
}
