# data/history.csv: one row per game per date, recording whether the thing
# being watched for was present.
#
#   date         2026-09-16
#   weekday      Wednesday
#   game          wordle | daily | mini | midi
#   hit           TRUE when the Wordle fits WORDLE_PATTERN, or the grid has a rebus
#   rebus_cells   number of rebus squares, multi-letter answers (crosswords only)
#   gimmick_cells number of alternate-answer gimmick squares (crosswords only)
#   solution      the Wordle word, recorded only once its day has passed, so
#                 opening this file never spoils today's puzzle
#
# This is kept apart from the alert path on purpose. The alert must go out even
# if the CSV is unwritable, and the CSV must fill in days the Mac was off,
# which the alert never looks at.

HISTORY_COLUMNS <- c("date", "weekday", "game", "hit", "rebus_cells", "gimmick_cells", "solution")

# First date each archive serves, verified 2026-09-19. Midi is found by
# walking back until the archive stops.
GAME_START <- c(wordle = "2021-06-19", daily = "1993-11-21", mini = "2014-08-21",
                midi = NA)

empty_history <- function() {
  data.frame(date = character(), weekday = character(), game = character(),
             hit = logical(), rebus_cells = integer(), gimmick_cells = integer(),
             solution = character(), stringsAsFactors = FALSE)
}

read_history <- function(path = HISTORY_FILE) {
  if (!file.exists(path)) return(empty_history())
  # suppressWarnings: an old CSV predating gimmick_cells triggers a harmless
  # "not all columns named in colClasses exist"; the upgrade below handles it.
  h <- suppressWarnings(
    utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "NA",
                    colClasses = c(date = "character", weekday = "character",
                                   game = "character", hit = "logical",
                                   rebus_cells = "integer", gimmick_cells = "integer",
                                   solution = "character")))
  # Upgrade a CSV written before gimmick_cells existed: add it as NA so the
  # rest of the code, and a re-fetch, can fill it in.
  if (!"gimmick_cells" %in% names(h)) {
    h$gimmick_cells <- NA_integer_
    h <- h[, HISTORY_COLUMNS]
  }
  h
}

# Later rows win, so a re-fetched row (a Wordle solution filled in the next
# day) replaces the earlier one. Written to a temp file and renamed.
write_history <- function(history, path = HISTORY_FILE) {
  history <- history[!duplicated(history[c("date", "game")], fromLast = TRUE), ]
  history <- history[order(history$date, history$game), HISTORY_COLUMNS]
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  utils::write.csv(history, tmp, row.names = FALSE, na = "NA")
  file.rename(tmp, path)
  invisible(history)
}

weekday_name <- function(date) {
  c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday")[as.POSIXlt(as.Date(date))$wday + 1]
}

# Fetch one game for one date and describe it as a one-row data frame.
# `include_solution` is FALSE for today's Wordle.
history_row <- function(game, date, include_solution = TRUE, max_attempts = 4L) {
  date <- as.character(date)
  if (identical(game, "wordle")) {
    wordle <- fetch_json(sprintf(WORDLE_URL, date), max_attempts = max_attempts)
    assert_date(wordle$print_date, date, "Wordle")
    hit <- wordle_any_match(wordle$solution, WORDLE_PATTERNS)
    cells <- NA_integer_
    gimmick <- NA_integer_
    solution <- if (include_solution) toupper(wordle$solution) else NA_character_
  } else {
    puzzle <- fetch_json(sprintf(CROSSWORD_URL, game, date), max_attempts = max_attempts)
    assert_date(puzzle$publicationDate, date, paste("Crossword", game))
    cells <- as.integer(count_rebus_cells(puzzle))
    gimmick <- as.integer(count_gimmick_cells(puzzle))
    hit <- cells > 0L
    solution <- NA_character_
  }
  data.frame(date = date, weekday = weekday_name(date), game = game, hit = hit,
             rebus_cells = cells, gimmick_cells = gimmick, solution = solution,
             stringsAsFactors = FALSE)
}

# Dates this game still needs: anything in the recent window with no row, plus
# past Wordle rows still waiting for their solution.
dates_needed <- function(history, game, today, window_days = 60L) {
  today  <- as.Date(today)
  start  <- today - window_days
  if (!is.na(GAME_START[[game]])) start <- max(start, as.Date(GAME_START[[game]]))
  rows   <- history[history$game == game, ]
  wanted <- as.character(seq(start, today, by = "day"))
  needed <- setdiff(wanted, rows$date)
  if (identical(game, "wordle")) {
    needed <- union(needed, rows$date[is.na(rows$solution) & rows$date < as.character(today)])
  }
  sort(needed)
}

# Called once per run, after the alert. Individual failures are logged and
# left for the next run; they never stop the rest.
update_history <- function(today, games = c("wordle", unname(CROSSWORDS)),
                           path = HISTORY_FILE, fetch_row = history_row) {
  today   <- as.character(today)
  history <- read_history(path)
  added   <- 0L
  for (game in games) {
    for (date in dates_needed(history, game, today)) {
      row <- tryCatch(fetch_row(game, date, include_solution = date < today),
                      error = function(e) e)
      if (inherits(row, "error")) {
        # Midi has no archive before its launch, so a 404 inside the window is
        # expected there and not worth a log line per day.
        if (!(identical(game, "midi") && grepl("HTTP 404", conditionMessage(row)))) {
          log_msg(sprintf("[history] %s %s: %s", game, date, conditionMessage(row)))
        }
        next
      }
      history <- rbind(history, row)
      added <- added + 1L
    }
  }
  if (added > 0L) write_history(history, path)
  log_msg(sprintf("[history] %d row(s) added or updated, %d total", added, nrow(history)))
  invisible(history)
}
