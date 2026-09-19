#!/usr/bin/env Rscript
# One-time archive download for data/history.csv.
#
#   Rscript scripts/backfill_history.R --game=daily
#   Rscript scripts/backfill_history.R --game=mini --from=2020-01-01
#   Rscript scripts/backfill_history.R --merge
#
# Walks backward from yesterday to the game's first archive date. Each row is
# appended to data/backfill-<game>.csv as soon as it is fetched, so the script
# can be stopped and restarted and only fetches what is missing. Separate part
# files let several games download at once without sharing a file; --merge
# folds them into data/history.csv and deletes them.

args <- commandArgs(trailingOnly = FALSE)
REPO_ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grepl("^--file=", args)])), ".."))
for (f in c("config.R", "detect.R", "fetch.R", "history.R")) source(file.path(REPO_ROOT, "R", f))

user_args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name) {
  hit <- user_args[grepl(paste0("^--", name, "="), user_args)]
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else NULL
}
part_file <- function(game) file.path(REPO_ROOT, "data", sprintf("backfill-%s.csv", game))

if ("--merge" %in% user_args) {
  parts   <- Sys.glob(file.path(REPO_ROOT, "data", "backfill-*.csv"))
  history <- read_history()
  before  <- nrow(history)
  # Existing history rows go last so they win over a backfilled duplicate.
  merged  <- do.call(rbind, c(lapply(parts, read_history), list(history)))
  merged  <- write_history(merged)
  file.remove(parts)
  log_msg(sprintf("[backfill] merged %d part file(s): %d -> %d rows",
                  length(parts), before, nrow(merged)))
  quit(status = 0)
}

game <- arg_value("game")
if (is.null(game) || !game %in% names(GAME_START)) stop("--game= must be one of: ", paste(names(GAME_START), collapse = ", "))

today <- as.Date(format(Sys.time(), "%Y-%m-%d", tz = LOCAL_TZ))
from  <- as.Date(arg_value("from") %||% GAME_START[[game]] %||% NA)
to    <- as.Date(arg_value("to") %||% as.character(today - 1))
if (is.na(from)) from <- as.Date("1993-11-21")   # Midi: walk back until the archive ends

# --tag= names a separate part file, so one game's date range can be split
# across several processes (--game=daily --tag=daily-a --from=... --to=...).
part <- part_file(arg_value("tag") %||% game)
history <- read_history()
have <- unique(c(read_history(part)$date, history$date[history$game == game]))
todo <- setdiff(as.character(seq(to, from, by = "-1 day")), have)
log_msg(sprintf("[backfill] %s: %d date(s) to fetch, %s back to %s", game, length(todo), to, from))

if (!dir.exists(dirname(part))) dir.create(dirname(part), recursive = TRUE)
if (!file.exists(part)) utils::write.csv(empty_history(), part, row.names = FALSE)

missing_run <- 0L; fetched <- 0L; failed <- 0L
for (date in todo) {
  row <- tryCatch(history_row(game, date, include_solution = TRUE, max_attempts = 3L),
                  error = function(e) e)
  if (inherits(row, "error")) {
    if (grepl("HTTP 404", conditionMessage(row))) {
      missing_run <- missing_run + 1L
      # Seven straight missing days means the archive has ended.
      if (missing_run >= 7L) { log_msg(sprintf("[backfill] %s: archive ends before %s", game, date)); break }
    } else {
      failed <- failed + 1L
      log_msg(sprintf("[backfill] %s %s: %s", game, date, conditionMessage(row)))
    }
    next
  }
  missing_run <- 0L
  utils::write.table(row, part, sep = ",", append = TRUE, col.names = FALSE,
                     row.names = FALSE, qmethod = "double", na = "NA")
  fetched <- fetched + 1L
  if (fetched %% 500L == 0L) log_msg(sprintf("[backfill] %s: %d fetched, now at %s", game, fetched, date))
  Sys.sleep(0.2)
}
log_msg(sprintf("[backfill] %s done: %d fetched, %d failed (rerun to retry)", game, fetched, failed))
