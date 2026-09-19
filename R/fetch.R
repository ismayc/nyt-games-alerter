# Network layer. The 5 am run often fires seconds after the Mac wakes, before
# Wi-Fi is back, so every fetch retries with backoff before giving up.

fetch_json <- function(url, max_attempts = 4L, timeout_seconds = 30L) {
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      resp <- httr::GET(url, httr::timeout(timeout_seconds),
                        httr::user_agent("Mozilla/5.0 (nyt-games-alert)"))
      status <- httr::status_code(resp)
      if (status != 200L) stop(sprintf("HTTP %d", status))
      jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                         simplifyVector = FALSE)
    }, error = function(e) e)
    if (!inherits(result, "error")) return(result)
    last_error <- conditionMessage(result)
    # 404 means the puzzle does not exist (before an archive's first day, or
    # not yet released). Waiting will not change that.
    if (identical(last_error, "HTTP 404")) stop(sprintf("%s: HTTP 404", url))
    wait <- 2^(attempt - 1L) * 5L
    log_msg(sprintf("[retry] %s failed %d/%d (%s)%s", url, attempt, max_attempts,
                    last_error,
                    if (attempt < max_attempts) sprintf("; sleeping %ds", wait) else ""))
    if (attempt < max_attempts) Sys.sleep(wait)
  }
  stop(sprintf("%s failed after %d attempts: %s", url, max_attempts, last_error))
}

# Both endpoints echo the date they are serving. Checking it guards against the
# undated default (e.g. mini.json) or a cache handing back a different day.
assert_date <- function(got, want, what) {
  if (!identical(as.character(got), as.character(want))) {
    stop(sprintf("%s returned date '%s', expected '%s'", what, got, want))
  }
  invisible(TRUE)
}

fetch_wordle <- function(date) {
  wordle <- fetch_json(sprintf(WORDLE_URL, date))
  assert_date(wordle$print_date, date, "Wordle")
  wordle
}

fetch_crossword <- function(path, date) {
  puzzle <- fetch_json(sprintf(CROSSWORD_URL, path, date))
  assert_date(puzzle$publicationDate, date, paste("Crossword", path))
  puzzle
}
