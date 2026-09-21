# Publishes report/ to Netlify from the daily Mac Mini job, so the live site
# shows today's puzzles each morning instead of waiting for the monthly refresh.
#
# The monthly GitHub Actions job deploys independently (via netlify-cli); this
# only fills the gap between those runs. It talks to the Netlify HTTP API
# directly with httr and jsonlite, which the alert already requires, so it needs
# no Node or netlify-cli on the machine. File digests come from the system
# shasum (present on macOS).
#
# It is deliberately best-effort: a missing token, a hashing problem, or a failed
# request is logged and skipped, and never blocks or fails the alert that is the
# job's real purpose.
#
# Needs config.R sourced first (STATE_DIR, REPORT_FILE, NETLIFY_SITE_ID, log_msg).

# Records the last day a deploy went out, so the three daily launchd runs
# publish at most once. Lives beside the alert state, outside the repo.
DEPLOY_STATE <- file.path(STATE_DIR, "last_deploy.txt")
NETLIFY_API  <- "https://api.netlify.com/api/v1"

# SHA1 of a file via the system shasum. Returns "" on any problem so the caller
# can skip cleanly rather than deploy a half-hashed set.
sha1_file <- function(path) {
  sh <- unname(Sys.which("shasum"))
  if (identical(sh, "")) return("")
  out <- tryCatch(system2(sh, c("-a", "1", path), stdout = TRUE, stderr = TRUE),
                  error = function(e) NULL)
  if (is.null(out) || !length(out) || !is.null(attr(out, "status"))) return("")
  sub("\\s.*$", "", out[1])
}

# Netlify's digest deploy: announce every file and its SHA1, upload only the
# ones Netlify says it is missing, and the deploy publishes to production.
deploy_report <- function(date = Sys.Date(), report_dir = dirname(REPORT_FILE),
                          site = NETLIFY_SITE_ID,
                          token = Sys.getenv("NETLIFY_AUTH_TOKEN"),
                          force = FALSE) {
  day <- format(as.Date(date))

  if (identical(token, "")) {
    log_msg("[deploy] no NETLIFY_AUTH_TOKEN set; skipped (monthly job still deploys)")
    return(invisible(FALSE))
  }
  if (!force && file.exists(DEPLOY_STATE) &&
      identical(readLines(DEPLOY_STATE, warn = FALSE)[1], day)) {
    log_msg("[deploy] already published today; skipped")
    return(invisible(FALSE))
  }
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    log_msg("[deploy] httr/jsonlite unavailable; skipped")
    return(invisible(FALSE))
  }

  # The whole published site is report/ (index.html and og.png today). Listing
  # only these files means Netlify serves exactly them, matching the CLI deploy.
  paths <- list.files(report_dir, recursive = TRUE)
  paths <- paths[!grepl("(^|/)\\.", paths)]            # no dotfiles
  if (!length(paths)) { log_msg("[deploy] nothing to publish; skipped"); return(invisible(FALSE)) }
  full    <- file.path(report_dir, paths)
  digests <- vapply(full, sha1_file, character(1))
  if (any(digests == "")) { log_msg("[deploy] could not hash a file; skipped"); return(invisible(FALSE)) }
  files_map <- stats::setNames(as.list(digests), paste0("/", paths))

  auth <- httr::add_headers(Authorization = paste("Bearer", token))

  log_msg(sprintf("[deploy] publishing %d file(s) to Netlify site %s", length(paths), site))
  cr <- tryCatch(
    httr::POST(sprintf("%s/sites/%s/deploys", NETLIFY_API, site), auth,
               body = list(files = files_map), encode = "json", httr::timeout(30)),
    error = function(e) { log_msg("[deploy] create failed: ", conditionMessage(e)); NULL })
  if (is.null(cr) || httr::status_code(cr) >= 300) {
    log_msg(sprintf("[deploy] create returned %s; live site unchanged",
                    if (is.null(cr)) "no response" else httr::status_code(cr)))
    return(invisible(FALSE))
  }
  dep       <- httr::content(cr, as = "parsed", type = "application/json")
  deploy_id <- dep$id
  required  <- unlist(dep$required)                    # SHA1s Netlify still needs
  if (is.null(deploy_id)) { log_msg("[deploy] no deploy id in response; skipped"); return(invisible(FALSE)) }

  to_send <- which(digests %in% required)
  ok <- TRUE
  for (i in to_send) {
    body <- readBin(full[i], "raw", file.size(full[i]))
    ur <- tryCatch(
      httr::PUT(sprintf("%s/deploys/%s/files/%s", NETLIFY_API, deploy_id, paths[i]),
                auth, httr::content_type("application/octet-stream"),
                body = body, httr::timeout(60)),
      error = function(e) { log_msg("[deploy] upload failed for ", paths[i], ": ", conditionMessage(e)); NULL })
    if (is.null(ur) || httr::status_code(ur) >= 300) {
      ok <- FALSE
      log_msg(sprintf("[deploy] upload of %s returned %s", paths[i],
                      if (is.null(ur)) "no response" else httr::status_code(ur)))
    }
  }

  if (ok) {
    if (!dir.exists(STATE_DIR)) dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
    writeLines(day, DEPLOY_STATE)
    log_msg(sprintf("[deploy] live site updated (%d uploaded, %d unchanged)",
                    length(to_send), length(paths) - length(to_send)))
  } else {
    log_msg("[deploy] one or more uploads failed; live site may be incomplete")
  }
  invisible(ok)
}
