# Copyright (c) 2026 The Maithic Authors. Apache-2.0; see LICENSE.

# Reading the sessions that `maithic council` writes.

# The roles of a council, in the order they act within a round.
council_roles <- function() c("researcher", "prover", "verifier", "critic")

# What a claim can be at the end of a session, from the weakest to the strongest.
claim_statuses <- function() c("conjectured", "formalized", "refuted", "verified")

# Why a session ended.
session_endings <- function() c("settled", "rounds", "budget", "stopped")

empty_council_turns <- function() {
  data.frame(run = integer(), label = character(), number = integer(), round = integer(), task = character(), role = character(), status = character(),
             summary = character(), posted = integer(), model_calls = integer(), lean_calls = integer(), seconds = numeric(), stringsAsFactors = FALSE)
}

empty_council_messages <- function() {
  data.frame(run = integer(), label = character(), seq = integer(), round = integer(), sender = character(), recipients = character(),
             performative = character(), subject = character(), refers = integer(), stringsAsFactors = FALSE)
}

empty_council_items <- function() {
  data.frame(run = integer(), label = character(), id = integer(), kind = character(), author = character(), round = integer(), title = character(),
             checked = logical(), status = character(), examined = character(), stringsAsFactors = FALSE)
}

parse_council_session <- function(path, index, label) {
  document <- read_document(path, "maithic.council.session/1", "a council session")
  status <- text_field(document, "status")
  if (!status %in% claim_statuses()) stop(sprintf("%s: a session with the unknown status '%s'", path, status), call. = FALSE)
  ending <- text_field(document$ending, "reason")
  if (!ending %in% session_endings()) stop(sprintf("%s: a session with the unknown ending '%s'", path, ending), call. = FALSE)
  taken <- document$turns
  turns <- data.frame(
    run = rep(index, length(taken)), label = rep(label, length(taken)), number = counts_of(taken, "number", NA_integer_), round = counts_of(taken, "round"),
    task = texts_of(taken, "task"), role = texts_of(taken, "role"), status = texts_of(taken, "status"), summary = texts_of(taken, "summary"),
    posted = vapply(taken, function(turn) length(turn$posted), integer(1)), model_calls = counts_of(taken, "model_calls"),
    lean_calls = counts_of(taken, "lean_calls"), seconds = numbers_of(taken, "seconds"), stringsAsFactors = FALSE
  )
  unknown <- setdiff(turns$role, council_roles())
  if (length(unknown) > 0) stop(sprintf("%s: a turn of the unknown role '%s'", path, unknown[1]), call. = FALSE)
  said <- document$messages
  messages <- data.frame(
    run = rep(index, length(said)), label = rep(label, length(said)), seq = counts_of(said, "seq", NA_integer_), round = counts_of(said, "round"),
    sender = texts_of(said, "sender"), recipients = vapply(said, function(m) paste(unlist(m$recipients), collapse = ", "), character(1)),
    performative = texts_of(said, "performative"), subject = texts_of(said, "subject"), refers = vapply(said, function(m) length(m$refers), integer(1)),
    stringsAsFactors = FALSE
  )
  if (!identical(messages$seq, seq_len(nrow(messages)))) stop(sprintf("%s: the messages are not in the order they were said", path), call. = FALSE)
  if (any(messages$performative == "verdict" & messages$sender != "verifier")) {
    stop(sprintf("%s: a verdict that the verifier did not give", path), call. = FALSE)
  }
  posted <- document$board
  items <- data.frame(
    run = rep(index, length(posted)), label = rep(label, length(posted)), id = counts_of(posted, "id", NA_integer_), kind = texts_of(posted, "kind"),
    author = texts_of(posted, "author"), round = counts_of(posted, "round"), title = texts_of(posted, "title"), checked = flags_of(posted, "checked", FALSE),
    status = texts_of(posted, "status", NA_character_), examined = texts_of(posted, "examined", NA_character_), stringsAsFactors = FALSE
  )
  claims <- items[items$kind == "claim", , drop = FALSE]
  if (nrow(claims) == 0 || !identical(claims$status[1], status)) {
    stop(sprintf("%s: the session is '%s', and its claim is '%s'", path, status, if (nrow(claims) == 0) "missing" else claims$status[1]), call. = FALSE)
  }
  accepted <- items[items$kind == "proof" & !is.na(items$examined) & items$examined == "accepted", , drop = FALSE]
  if ((status == "verified") != (nrow(accepted) > 0)) {
    stop(sprintf("%s: the claim is '%s', and %d proof(s) were accepted by Lean", path, status, nrow(accepted)), call. = FALSE)
  }
  spent <- document$budget$spent
  session <- data.frame(
    run = index, path = path, label = label, file = text_field(document$problem, "file"), theorem = text_field(document$problem, "theorem"), status = status,
    ending = ending, rounds = count_field(document$settings, "rounds"), with_model = flag_field(document$settings, "model"), turns = nrow(turns),
    messages = nrow(messages), items = nrow(items), model_calls = count_field(spent, "model_calls"), lean_calls = count_field(spent, "lean_calls"),
    tokens = count_field(spent, "input_tokens") + count_field(spent, "output_tokens"), cost = number_field(spent, "cost"), seconds = number_field(spent, "seconds"),
    proof_round = if (nrow(accepted) > 0) accepted$round[1] else NA_integer_, proof_author = if (nrow(accepted) > 0) accepted$author[1] else NA_character_,
    stringsAsFactors = FALSE
  )
  list(session = session, turns = turns, messages = messages, items = items)
}

#' Read council sessions
#'
#' Loads the sessions that `maithic council` writes with `--out`: one for
#' each theorem a council worked on. Each session is checked against
#' itself: its messages must be in the order they were said, only the
#' verifier may have given a verdict, and the claim is verified exactly when
#' Lean accepted a proof.
#'
#' @param paths Paths of council sessions. When the vector has names, they
#'   label the sessions.
#' @param label A label for each session; by default the names of `paths`,
#'   or the file names of the sessions without their extension.
#' @return An object of class `maithic_council_sessions`, a list of four
#'   data frames: `sessions` (one row per session: the theorem, the `status`
#'   of its claim, why the session ended, what it spent, and the round and
#'   the author of the proof Lean accepted), `turns` (one row per turn: the
#'   role, its task, how the turn ended, what it posted and what it cost),
#'   `messages` (one row per message of the transcript) and `items` (one row
#'   per item of the blackboard, with the status of a claim and what Lean
#'   said about a proof).
#' @export
read_council_sessions <- function(paths, label = NULL) {
  if (!is.character(paths) || length(paths) == 0) stop("no council sessions given", call. = FALSE)
  if (is.null(label)) label <- if (!is.null(names(paths)) && all(nzchar(names(paths)))) names(paths) else sub("\\.json$", "", basename(paths))
  if (!is.character(label) || length(label) != length(paths)) stop("`label` must name one label for each session", call. = FALSE)
  parsed <- lapply(seq_along(paths), function(i) parse_council_session(paths[[i]], i, label[[i]]))
  sessions <- do.call(rbind, lapply(parsed, `[[`, "session"))
  rownames(sessions) <- NULL
  bound <- function(part, template) {
    frame <- bind_frames(lapply(parsed, `[[`, part), template)
    rownames(frame) <- NULL
    frame
  }
  structure(list(sessions = sessions, turns = bound("turns", empty_council_turns()), messages = bound("messages", empty_council_messages()),
                 items = bound("items", empty_council_items())),
            class = "maithic_council_sessions")
}

need_council_sessions <- function(sessions) {
  if (!inherits(sessions, "maithic_council_sessions")) stop("`sessions` must come from read_council_sessions()", call. = FALSE)
  sessions
}

#' What each role contributed
#'
#' The work of council sessions, counted by role: the turns it took, the
#' messages it sent, the items it posted, the items of it that Lean
#' confirmed, and the calls it made to models and to Lean.
#'
#' @param sessions Council sessions from [read_council_sessions()].
#' @return A data frame with one row per role, in the order the roles act:
#'   `role`, `turns`, `messages`, `items`, `confirmed` (items Lean confirmed:
#'   definitions that were looked up, proofs that were accepted,
#'   counterexamples), `model_calls` and `lean_calls`.
#' @export
role_contributions <- function(sessions) {
  sessions <- need_council_sessions(sessions)
  roles <- council_roles()
  count <- function(values) vapply(roles, function(role) sum(values == role), integer(1), USE.NAMES = FALSE)
  total <- function(column) vapply(roles, function(role) as.integer(sum(sessions$turns[[column]][sessions$turns$role == role])), integer(1), USE.NAMES = FALSE)
  items <- sessions$items
  confirmed <- items$checked | (!is.na(items$examined) & items$examined == "accepted")
  data.frame(role = roles, turns = count(sessions$turns$role), messages = count(sessions$messages$sender), items = count(items$author),
             confirmed = count(items$author[confirmed]), model_calls = total("model_calls"), lean_calls = total("lean_calls"), stringsAsFactors = FALSE)
}
