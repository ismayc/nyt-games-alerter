# Difficulty signal for the daily crossword, scraped from XW Stats
# (https://xwstats.com), which aggregates community solve times. Kept apart from
# the alert path on purpose: a third-party outage must never affect the email.
#
# Per date, the public puzzle page carries a categorical difficulty, a median
# solve time, and how the median solver did versus their own average for that
# weekday, which is the "harder or easier than a normal <weekday>" signal.
#
# Stored in data/difficulty.csv (one row per date) with attribution shown on the
# report. XW Stats coverage starts around 2014 and fills in as people solve, so
# recent dates may not have enough data yet (fetch returns NULL for those).

XWSTATS_URL   <- "https://xwstats.com/puzzles/%s"
DIFFICULTY_FILE <- file.path(if (exists("REPO_ROOT")) REPO_ROOT else getwd(), "data", "difficulty.csv")
DIFFICULTY_COLUMNS <- c("date", "difficulty", "median_sec", "vs_weekday")

# Parse the tag-stripped page text. Returns NULL when the date has no data yet.
parse_xwstats <- function(html) {
  txt <- gsub("<[^>]+>", " ", html)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&gt;", ">", txt, fixed = TRUE)
  txt <- gsub("[[:space:]]+", " ", txt)
  grab <- function(re) {
    m <- regmatches(txt, regexpr(re, txt, perl = TRUE))
    if (length(m)) m[[1]] else NA_character_
  }
  diff <- grab("(?<=Difficulty )(Very Easy|Very Hard|Average|Easy|Hard)")
  med  <- grab("(?<=Median Solve Time )[0-9]+:[0-9]{2}")
  dir  <- grab("(?<=Median Solver )[0-9]+% (faster|slower)")
  if (is.na(diff) && is.na(med)) return(NULL)
  median_sec <- if (!is.na(med)) {
    p <- as.integer(strsplit(med, ":", fixed = TRUE)[[1]]); p[[1]] * 60L + p[[2]]
  } else NA_integer_
  # Signed so positive = harder than a normal weekday (slower), negative = easier.
  vs <- if (!is.na(dir)) {
    n <- as.integer(sub("%.*", "", dir)); if (grepl("slower", dir)) n else -n
  } else NA_integer_
  list(difficulty = diff, median_sec = median_sec, vs_weekday = as.integer(vs))
}

fetch_difficulty <- function(date, max_attempts = 3L) {
  for (a in seq_len(max_attempts)) {
    r <- tryCatch(
      httr::GET(sprintf(XWSTATS_URL, date), httr::timeout(30),
                httr::user_agent("nyt-games-alerter difficulty backfill (github.com/ismayc/nyt-games-alerter)")),
      error = function(e) e)
    if (!inherits(r, "error")) {
      sc <- httr::status_code(r)
      if (sc == 200L) return(parse_xwstats(httr::content(r, as = "text", encoding = "UTF-8")))
      if (sc == 404L) return(NULL)
    }
    if (a < max_attempts) Sys.sleep(2^(a - 1))
  }
  stop(sprintf("XW Stats fetch failed for %s", date))
}

read_difficulty <- function(path = DIFFICULTY_FILE) {
  if (!file.exists(path)) {
    return(data.frame(date = character(), difficulty = character(),
                      median_sec = integer(), vs_weekday = integer(),
                      stringsAsFactors = FALSE))
  }
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "NA",
                  colClasses = c(date = "character", difficulty = "character",
                                 median_sec = "integer", vs_weekday = "integer"))
}

# Re-fetch recent dates that are missing or not yet settled (stored NA), so the
# solve data that accumulates after a puzzle runs shows up. Gentle: a short
# window, one request every `delay` seconds, and a down XW Stats just leaves the
# affected dates NA rather than erroring. Used by the monthly refresh.
topup_difficulty <- function(days = 45, delay = 1.5, path = DIFFICULTY_FILE) {
  df     <- read_difficulty(path)
  window <- as.character(seq(Sys.Date() - days, Sys.Date(), by = "day"))
  settled <- df$date[!is.na(df$vs_weekday)]
  need   <- setdiff(window, settled)
  if (!length(need)) return(invisible(df))
  for (d in need) {
    r  <- tryCatch(fetch_difficulty(d), error = function(e) NULL)
    df <- df[df$date != d, ]
    df <- rbind(df, data.frame(date = d,
      difficulty = if (is.null(r)) NA_character_ else r$difficulty,
      median_sec = if (is.null(r)) NA_integer_ else r$median_sec,
      vs_weekday = if (is.null(r)) NA_integer_ else r$vs_weekday, stringsAsFactors = FALSE))
    Sys.sleep(delay)
  }
  write_difficulty(df, path)
  invisible(df)
}

write_difficulty <- function(df, path = DIFFICULTY_FILE) {
  df <- df[!duplicated(df["date"], fromLast = TRUE), ]
  df <- df[order(df$date), DIFFICULTY_COLUMNS]
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  utils::write.csv(df, tmp, row.names = FALSE, na = "NA")
  file.rename(tmp, path)
  invisible(df)
}
