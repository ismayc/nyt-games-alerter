# Email, sent as the configured GMAIL_ACCOUNT through gmailr.
#
# gmail_token_cache() and gmail_auth() are copied from a sibling job's auth
# helper rather than sourced from it, so this job does not break when that repo
# moves. The call is kept identical on purpose: gargle keys the cached token off
# the client and scopes, so any change here would miss the cache and demand an
# interactive consent that launchd cannot give.

gmail_token_cache <- function(base = GMAIL_TOKEN_BASE) {
  node <- gsub("[^A-Za-z0-9._-]", "_", Sys.info()[["nodename"]])
  file.path(base, node)
}

.gmail_authed <- new.env(parent = emptyenv())

gmail_auth <- function() {
  if (isTRUE(.gmail_authed$done)) return(invisible(TRUE))
  if (!file.exists(GMAIL_CREDENTIALS)) {
    stop("Gmail credentials not found at ", GMAIL_CREDENTIALS)
  }
  cache <- gmail_token_cache()
  if (!dir.exists(cache)) stop("No Gmail token cache for this machine at ", cache)

  options(gargle_oauth_email = GMAIL_ACCOUNT)
  options(gargle_oauth_cache = cache)
  gmailr::gm_auth_configure(path = GMAIL_CREDENTIALS)
  gmailr::gm_auth(
    email  = GMAIL_ACCOUNT,
    token  = httr::config(GMAIL_CREDENTIALS),
    scopes = "full",
    cache  = gargle::gargle_oauth_cache()
  )
  .gmail_authed$done <- TRUE
  invisible(TRUE)
}

# Returns TRUE only when Gmail accepted the message, so the caller records a
# finding as emailed only when it really was.
send_alert_email <- function(subject, body, dry_run = FALSE) {
  if (dry_run) {
    log_msg("[dry-run] would email: ", subject)
    cat(body, "\n")
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    gmail_auth()
    msg <- gmailr::gm_mime() |>
      gmailr::gm_to(ALERT_TO) |>
      gmailr::gm_from(GMAIL_ACCOUNT) |>
      gmailr::gm_subject(subject) |>
      gmailr::gm_text_body(body)
    httr::with_config(httr::timeout(60), gmailr::gm_send_message(msg))
    TRUE
  }, error = function(e) {
    log_msg(sprintf("[warn] could not send '%s': %s", subject, conditionMessage(e)))
    FALSE
  })
  if (ok) log_msg("Emailed: ", subject)
  invisible(ok)
}

long_date <- function(date) {
  d <- as.Date(date)
  sprintf("%s, %s %d, %s", format(d, "%A"), format(d, "%B"),
          as.integer(format(d, "%d")), format(d, "%Y"))
}

# "a", "a and b", "a, b, and c".
and_list <- function(x) {
  n <- length(x)
  if (n <= 1) return(paste(x, collapse = ""))
  if (n == 2) return(paste(x, collapse = " and "))
  paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}

# Findings are keys like "rebus:daily" and "wordle". The email states the fact
# and nothing else: no answers, no square counts, no rebus contents.
# The Wordle line never names which pattern matched: knowing whether it was
# __A_E or _A_E_ would give away where the A and E sit in today's answer.
finding_phrase <- function(finding) {
  if (identical(finding, "wordle")) return("Wordle matches a pattern")
  path <- sub("^rebus:", "", finding)
  sprintf("rebus in %s", names(CROSSWORDS)[match(path, CROSSWORDS)])
}

finding_sentence <- function(finding) {
  if (identical(finding, "wordle")) {
    return("Today's Wordle solution fits one of the tracked patterns (which one is not shown, to avoid a hint).")
  }
  path <- sub("^rebus:", "", finding)
  sprintf("%s has at least one rebus square in its solution.",
          names(CROSSWORDS)[match(path, CROSSWORDS)])
}

alert_subject <- function(date, findings) {
  sprintf("[nyt-games] %s: %s", long_date(date),
          and_list(vapply(findings, finding_phrase, character(1))))
}

alert_body <- function(date, findings) {
  paste(c(
    sprintf("NYT games alert for %s.", long_date(date)),
    "",
    sprintf("  - %s", vapply(findings, finding_sentence, character(1))),
    "",
    "No answers are included in this email.",
    "Sent by the nyt-games-alert launchd job on this Mac."
  ), collapse = "\n")
}

failure_subject <- function(date) {
  sprintf("[nyt-games] checks failing on %s", long_date(date))
}

failure_body <- function(date, errors) {
  paste(c(
    sprintf("Some NYT games checks have not succeeded for %s:", long_date(date)),
    "",
    sprintf("  - %s: %s", names(errors), unlist(errors)),
    "",
    "A rebus or a Wordle match could have been missed today.",
    sprintf("Log: %s", LOG_FILE)
  ), collapse = "\n")
}
