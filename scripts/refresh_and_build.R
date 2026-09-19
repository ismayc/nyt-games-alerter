#!/usr/bin/env Rscript
# Fetch any missing recent days into data/history.csv and rebuild the report.
# No email and no Gmail: this is the publish path (the monthly GitHub Actions
# job), not the alert path. Needs only httr and jsonlite.
#
#   Rscript scripts/refresh_and_build.R

args <- commandArgs(trailingOnly = FALSE)
REPO_ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grepl("^--file=", args)])), ".."))
for (f in c("config.R", "detect.R", "fetch.R", "notify.R", "history.R", "difficulty.R", "report.R")) {
  source(file.path(REPO_ROOT, "R", f))
}

update_history(Sys.Date())   # append missing days (fills rebus_cells and gimmick_cells)
topup_difficulty()           # refresh recent crossword difficulty from XW Stats (gentle)
build_report()
