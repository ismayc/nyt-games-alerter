#!/usr/bin/env Rscript
# Entry point for the nyt-games-alert launchd job.
#
#   Rscript scripts/check_today.R                  today's puzzles, email on a hit
#   Rscript scripts/check_today.R --dry-run        print instead of emailing, touch no state
#   Rscript scripts/check_today.R --date=2026-09-16
#       Check a specific date. Ignores and leaves alone the state file, so it
#       always re-checks, and it DOES send a real email unless --dry-run is
#       also given. Useful for an end-to-end test against a known rebus day.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
REPO_ROOT <- normalizePath(file.path(dirname(file_arg), ".."))
for (f in c("config.R", "detect.R", "fetch.R", "state.R", "notify.R", "run.R",
            "history.R", "report.R")) {
  source(file.path(REPO_ROOT, "R", f))
}

user_args <- commandArgs(trailingOnly = TRUE)
known     <- grepl("^--dry-run$|^--date=\\d{4}-\\d{2}-\\d{2}$", user_args)
if (!all(known)) stop("Unrecognized argument: ", paste(user_args[!known], collapse = " "))

dry_run  <- "--dry-run" %in% user_args
date_arg <- sub("^--date=", "", user_args[grepl("^--date=", user_args)])
date     <- if (length(date_arg)) date_arg else format(Sys.time(), "%Y-%m-%d", tz = LOCAL_TZ)

log_msg(sprintf("Run start: date=%s dry_run=%s explicit_date=%s",
                date, dry_run, length(date_arg) > 0))
result <- run_checks(date, dry_run = dry_run, use_state = length(date_arg) == 0)
log_msg(sprintf("Run end: checked=[%s] findings=[%s] emailed=[%s] errors=%d",
                paste(unlist(result$day$checked), collapse = ","),
                paste(unlist(result$day$findings), collapse = ","),
                paste(unlist(result$day$emailed), collapse = ","),
                length(result$errors)))

# History and the report come after the alert and can never block it. Skipped
# for --dry-run and --date, which are for testing the alert.
if (!dry_run && length(date_arg) == 0) {
  tryCatch({
    update_history(date)
    build_report()
  }, error = function(e) log_msg("[history] failed: ", conditionMessage(e)))
}

# Non-zero when any check could not complete, so launchd's log shows it.
if (length(result$errors)) quit(status = 1)
