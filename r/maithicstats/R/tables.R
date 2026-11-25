# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Shared pieces of the summaries: field access, quantiles and report tables.

# The statuses a result can have, from the strongest claim to the weakest.
result_statuses <- function() {
  c("verified", "checked", "refuted", "conjectured", "unverified", "unknown", "failed", "abstained")
}

text_field <- function(x, name, default = "") {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) default else as.character(value[[1]])
}

number_field <- function(x, name, default = NA_real_) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) default else as.numeric(value[[1]])
}

count_field <- function(x, name, default = 0L) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) default else as.integer(value[[1]])
}

flag_field <- function(x, name, default = NA) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) default else isTRUE(value[[1]])
}

texts_field <- function(x, name) {
  value <- x[[name]]
  if (is.null(value)) character() else as.character(unlist(value))
}

# Reads a JSON document and refuses it unless it names the expected schema.
read_document <- function(path, schema, what) {
  if (!file.exists(path)) stop(sprintf("file not found: %s", path), call. = FALSE)
  document <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(document$schema, schema)) {
    stop(sprintf("%s is not %s (schema '%s')", path, what, format(document$schema)), call. = FALSE)
  }
  document
}

# Binds data frames by row; an empty list gives the empty frame `template`.
bind_frames <- function(frames, template) {
  frames <- Filter(function(frame) !is.null(frame) && nrow(frame) > 0, frames)
  if (length(frames) == 0) template else do.call(rbind, frames)
}

#' Summary of a sample of durations or counts
#'
#' The count, the median, upper quantiles, the mean and the maximum of the
#' values that are not missing.
#'
#' @param x A numeric vector.
#' @param probs The upper quantiles to report, as probabilities.
#' @return A one-row data frame with `n`, `median`, one column per quantile
#'   named `p90`, `p99`, ..., `mean` and `max`. With no values, `n` is zero and
#'   the other columns are `NA`.
#' @export
distribution_summary <- function(x, probs = c(0.9, 0.99)) {
  if (!is.numeric(x)) stop("`x` must be numeric", call. = FALSE)
  if (!is.numeric(probs) || any(probs <= 0) || any(probs >= 1)) stop("`probs` must lie strictly between 0 and 1", call. = FALSE)
  x <- x[!is.na(x)]
  names <- sprintf("p%s", format(100 * probs, trim = TRUE))
  quantiles <- if (length(x) == 0) rep(NA_real_, length(probs)) else unname(stats::quantile(x, probs, type = 7))
  out <- data.frame(n = length(x), median = if (length(x) == 0) NA_real_ else stats::median(x))
  for (i in seq_along(probs)) out[[names[i]]] <- quantiles[i]
  out$mean <- if (length(x) == 0) NA_real_ else mean(x)
  out$max <- if (length(x) == 0) NA_real_ else max(x)
  out
}

# The order of the rows of a data frame by its columns, the same on every
# machine: text is compared byte by byte, not by the rules of the locale.
plain_order <- function(keys) do.call(order, c(unname(as.list(keys)), list(method = "radix")))

# How often each value occurs, most frequent first, ties in plain text order.
count_values <- function(x) {
  x <- as.character(x[!is.na(x)])
  values <- sort(unique(x), method = "radix")
  counts <- vapply(values, function(value) sum(x == value), integer(1))
  counts[order(-counts, seq_along(counts))]
}

# Applies `summarise` to the rows of each group and puts the group keys in front.
summarise_groups <- function(data, by, summarise) {
  if (length(by) == 0) return(summarise(data))
  missing <- setdiff(by, names(data))
  if (length(missing) > 0) stop(sprintf("no column named '%s'", missing[1]), call. = FALSE)
  keys <- unique(data[by])
  keys <- keys[plain_order(keys), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    keep <- rep(TRUE, nrow(data))
    for (column in by) keep <- keep & data[[column]] == keys[[column]][i]
    cbind(keys[i, , drop = FALSE], summarise(data[keep, , drop = FALSE]), row.names = NULL)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

format_cell <- function(value, digits, as_percent) {
  if (is.list(value)) return(vapply(value, function(item) paste(as.character(unlist(item)), collapse = ", "), character(1)))
  if (is.logical(value)) return(ifelse(is.na(value), "", ifelse(value, "yes", "no")))
  if (!is.numeric(value)) return(ifelse(is.na(value), "", as.character(value)))
  if (as_percent) return(ifelse(is.na(value), "", sprintf("%.1f%%", 100 * value)))
  whole <- !is.na(value) & value == round(value)
  text <- formatC(value, format = "f", digits = digits)
  text <- sub("\\.$", "", sub("(\\.[0-9]*?)0+$", "\\1", text))
  text[whole] <- formatC(value[whole], format = "d", big.mark = ",")
  text[is.na(value)] <- ""
  text
}

#' A data frame as a Markdown table
#'
#' Formats a summary for a report: numbers are right-aligned, whole numbers
#' are written without decimals, shares can be written as percentages, and a
#' vertical bar inside a cell is escaped so that it cannot break the table.
#'
#' @param data A data frame.
#' @param digits Decimal places for numbers that are not whole.
#' @param percent Names of columns holding shares between 0 and 1, to be
#'   written as percentages.
#' @param labels A named character vector of column headings, by column name.
#' @return A character vector with one line of Markdown per element.
#' @export
markdown_table <- function(data, digits = 3, percent = character(), labels = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame", call. = FALSE)
  unknown <- setdiff(percent, names(data))
  if (length(unknown) > 0) stop(sprintf("no column named '%s'", unknown[1]), call. = FALSE)
  headings <- names(data)
  if (!is.null(labels)) headings[match(names(labels), names(data), nomatch = 0)] <- labels[names(labels) %in% names(data)]
  numeric_column <- vapply(data, is.numeric, logical(1))
  cells <- lapply(names(data), function(name) gsub("|", "\\|", format_cell(data[[name]], digits, name %in% percent), fixed = TRUE))
  line <- function(parts) paste0("| ", paste(parts, collapse = " | "), " |")
  rule <- ifelse(numeric_column, "---:", "---")
  rows <- if (nrow(data) == 0) character() else vapply(seq_len(nrow(data)), function(i) line(vapply(cells, `[`, character(1), i)), character(1))
  c(line(headings), line(rule), rows)
}
