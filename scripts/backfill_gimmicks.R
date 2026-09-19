#!/usr/bin/env Rscript
# One-time backfill of the gimmick_cells column across the whole crossword
# history, by re-fetching every daily, mini, and midi puzzle from the live NYT
# endpoints. It doubles as a full re-validation: the re-fetched rebus count is
# compared with the stored one and any drift is logged (stored values are kept,
# never silently overwritten).
#
#   Rscript scripts/backfill_gimmicks.R
#
# Safe to stop and restart: only rows whose gimmick_cells is still NA are
# fetched, and progress is checkpointed to data/history.csv every 200 rows.

args <- commandArgs(trailingOnly = FALSE)
REPO_ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grepl("^--file=", args)])), ".."))
for (f in c("config.R", "detect.R", "fetch.R", "history.R", "notify.R")) {
  source(file.path(REPO_ROOT, "R", f))
}

user_args <- commandArgs(trailingOnly = TRUE)
limit <- {
  hit <- user_args[grepl("^--limit=", user_args)]
  if (length(hit)) as.integer(sub("^--limit=", "", hit[[1]])) else NA_integer_
}

h <- read_history(HISTORY_FILE)
is_cw   <- h$game %in% c("daily", "mini", "midi")
todo    <- which(is_cw & is.na(h$gimmick_cells))
if (!is.na(limit)) todo <- head(todo, limit)
mism_log <- file.path(REPO_ROOT, "data", "rebus-mismatches.log")

log_msg(sprintf("[backfill] %d crossword rows need gimmick_cells (of %d total)", length(todo), nrow(h)))
start <- Sys.time(); done <- 0L; gim_rows <- 0L; mism <- 0L; errs <- 0L

for (i in todo) {
  game <- h$game[i]; date <- h$date[i]
  res <- tryCatch({
    puzzle <- fetch_json(sprintf(CROSSWORD_URL, game, date))
    assert_date(puzzle$publicationDate, date, game)
    list(rebus = as.integer(count_rebus_cells(puzzle)),
         gimmick = as.integer(count_gimmick_cells(puzzle)))
  }, error = function(e) e)

  if (inherits(res, "error")) {
    errs <- errs + 1L
    log_msg(sprintf("[backfill] %s %s: %s", game, date, conditionMessage(res)))
    next
  }
  if (!identical(res$rebus, h$rebus_cells[i])) {
    mism <- mism + 1L
    cat(sprintf("%s,%s,stored=%s,live=%d\n", date, game, h$rebus_cells[i], res$rebus),
        file = mism_log, append = TRUE)
  }
  h$gimmick_cells[i] <- res$gimmick
  if (res$gimmick > 0L) gim_rows <- gim_rows + 1L
  done <- done + 1L

  if (done %% 200L == 0L) {
    el   <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    rate <- done / el
    eta  <- (length(todo) - done) / max(rate, 1e-6)
    write_history(h, HISTORY_FILE)   # atomic checkpoint
    log_msg(sprintf("[backfill] %d/%d  %.1f/s  eta %.0f min  gimmick_rows=%d mism=%d err=%d",
                    done, length(todo), rate, eta / 60, gim_rows, mism, errs))
  }
}

write_history(h, HISTORY_FILE)
log_msg(sprintf("[backfill] DONE: scanned %d, gimmick puzzles=%d, rebus mismatches=%d, fetch errors=%d",
                done, sum(h$gimmick_cells > 0, na.rm = TRUE), mism, errs))
