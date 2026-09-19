#!/usr/bin/env Rscript
# Rscript tests/run-tests.R   (from the repo root)
REPO_ROOT <- getwd()
for (f in c("config.R", "detect.R", "fetch.R", "state.R", "notify.R", "run.R",
            "history.R", "report.R")) {
  source(file.path("R", f))
}
# Keep test runs out of the real log.
LOG_FILE <- tempfile(fileext = ".log")
testthat::test_dir("tests/testthat", env = globalenv(), stop_on_failure = TRUE)
