#!/usr/bin/env Rscript
# One-time gentle backfill of data/difficulty.csv from XW Stats, for the daily
# crossword. Fetches each date from --from (default 2015-01-01) to today, skips
# dates already stored, and waits --delay seconds (default 2) between requests
# so it stays polite to a hobby site. Safe to stop and restart; checkpoints
# every 100 dates.
#
#   Rscript scripts/backfill_difficulty.R
#   Rscript scripts/backfill_difficulty.R --from=2015-01-01 --delay=2 --limit=5

args <- commandArgs(trailingOnly = FALSE)
REPO_ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grepl("^--file=", args)])), ".."))
for (f in c("config.R", "difficulty.R")) source(file.path(REPO_ROOT, "R", f))

ua <- commandArgs(trailingOnly = TRUE)
arg <- function(n, d) { h <- ua[grepl(paste0("^--", n, "="), ua)]; if (length(h)) sub(paste0("^--", n, "="), "", h[[1]]) else d }
from  <- as.Date(arg("from", "2015-01-01"))
delay <- as.numeric(arg("delay", "2"))
limit <- suppressWarnings(as.integer(arg("limit", NA)))

have  <- read_difficulty()
dates <- as.character(seq(from, Sys.Date(), by = "day"))
todo  <- setdiff(dates, have$date)
if (!is.na(limit)) todo <- utils::head(todo, limit)
log_msg(sprintf("[difficulty] %d dates to fetch (%s..%s); %d already stored",
                length(todo), from, Sys.Date(), nrow(have)))

acc <- have; start <- Sys.time(); got <- 0L; empty <- 0L; err <- 0L
add <- function(d, r) data.frame(date = d,
  difficulty = if (is.null(r)) NA_character_ else r$difficulty,
  median_sec = if (is.null(r)) NA_integer_ else r$median_sec,
  vs_weekday = if (is.null(r)) NA_integer_ else r$vs_weekday, stringsAsFactors = FALSE)

for (i in seq_along(todo)) {
  d <- todo[i]
  r <- tryCatch(fetch_difficulty(d), error = function(e) e)
  if (inherits(r, "error")) { err <- err + 1L; log_msg(sprintf("[difficulty] %s: %s", d, conditionMessage(r))) }
  else { if (is.null(r)) empty <- empty + 1L else got <- got + 1L; acc <- rbind(acc, add(d, r)) }
  if (i %% 100L == 0L) {
    write_difficulty(acc)
    el <- as.numeric(difftime(Sys.time(), start, units = "secs")); rate <- i / el
    log_msg(sprintf("[difficulty] %d/%d  %.2f/s  eta %.0f min  got=%d empty=%d err=%d",
                    i, length(todo), rate, (length(todo) - i) / max(rate, 1e-6) / 60, got, empty, err))
  }
  Sys.sleep(delay)
}
write_difficulty(acc)
log_msg(sprintf("[difficulty] DONE  got=%d empty=%d err=%d  total rows=%d", got, empty, err, nrow(acc)))
