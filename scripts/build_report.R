#!/usr/bin/env Rscript
# Rebuild report/index.html from data/history.csv without running the checks.
#
#   Rscript scripts/build_report.R
#   Rscript scripts/build_report.R --history=some.csv --out=some.html

args <- commandArgs(trailingOnly = FALSE)
REPO_ROOT <- normalizePath(file.path(dirname(sub("^--file=", "", args[grepl("^--file=", args)])), ".."))
for (f in c("config.R", "detect.R", "fetch.R", "notify.R", "history.R", "difficulty.R", "report.R")) {
  source(file.path(REPO_ROOT, "R", f))
}

user_args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default) {
  hit <- user_args[grepl(paste0("^--", name, "="), user_args)]
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}
build_report(arg_value("history", HISTORY_FILE), arg_value("out", REPORT_FILE))
