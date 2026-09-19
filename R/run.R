# One pass over the day's checks. Safe to call many times a day: a check that
# has succeeded is never repeated, and a finding that has been emailed is never
# emailed again.
#
# `fetchers` and `send` are arguments so the tests can drive this without the
# network or Gmail.

# The last scheduled launchd run is 11:07. A check still failing by then gets a
# failure email, because a monitor that stops working silently looks exactly
# like a run of days with no rebus.
FAILURE_EMAIL_HOUR <- 11L

# name = check key stored in state, value = function(date) returning TRUE when
# the thing being watched for is present. One Wordle check that fires when the
# answer fits any pattern; which pattern matched is deliberately not surfaced,
# so the email never hints at the letter positions.
default_checks <- function() {
  checks <- list(
    wordle = function(date) wordle_any_match(fetch_wordle(date)$solution, WORDLE_PATTERNS)
  )
  for (path in unname(CROSSWORDS)) {
    local({
      p <- path
      checks[[p]] <<- function(date) has_rebus(fetch_crossword(p, date))
    })
  }
  checks
}

finding_key <- function(check) if (identical(check, "wordle")) "wordle" else paste0("rebus:", check)

run_checks <- function(date,
                       checks    = default_checks(),
                       send      = send_alert_email,
                       dry_run   = FALSE,
                       use_state = TRUE,
                       hour      = as.integer(format(Sys.time(), "%H")),
                       state_path = STATE_FILE) {
  date  <- as.character(date)
  state <- if (use_state) read_state(state_path) else list()
  day   <- get_day(state, date)
  save  <- function() if (use_state && !dry_run) write_state(state, date, day, state_path)

  errors <- list()
  for (check in setdiff(names(checks), unlist(day$checked))) {
    hit <- tryCatch(checks[[check]](date), error = function(e) e)
    if (inherits(hit, "error")) {
      errors[[check]] <- conditionMessage(hit)
      log_msg(sprintf("[error] %s for %s: %s", check, date, errors[[check]]))
      next
    }
    log_msg(sprintf("%s for %s: %s", check, date, if (hit) "MATCH" else "no match"))
    day$checked <- add_unique(day$checked, check)
    if (hit) day$findings <- add_unique(day$findings, finding_key(check))
    # Saved after every check, so a crash later in the run cannot cause this
    # one to be fetched and counted again.
    save()
  }

  unsent <- setdiff(unlist(day$findings), unlist(day$emailed))
  if (length(unsent)) {
    if (isTRUE(send(alert_subject(date, unsent), alert_body(date, unsent), dry_run = dry_run))) {
      day$emailed <- add_unique(day$emailed, unsent)
      save()
    }
  }

  if (length(errors) && hour >= FAILURE_EMAIL_HOUR && !isTRUE(day$failure_emailed)) {
    if (isTRUE(send(failure_subject(date), failure_body(date, errors), dry_run = dry_run))) {
      day$failure_emailed <- TRUE
      save()
    }
  }

  invisible(list(day = day, errors = errors))
}
