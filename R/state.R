# Per-day record of what has been checked and what has been emailed, so the
# job can run several times a day (to cover a sleeping Mac or a dead network at
# 5 am) and still send each finding exactly once.
#
# Shape of state.json:
#   { "2026-09-19": { "checked":  ["wordle", "daily"],
#                     "findings": ["rebus:daily"],
#                     "emailed":  ["rebus:daily"],
#                     "failure_emailed": false } }

KEEP_DAYS <- 30L

read_state <- function(path = STATE_FILE) {
  if (!file.exists(path)) return(list())
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

empty_day <- function() {
  list(checked = list(), findings = list(), emailed = list(), failure_emailed = FALSE)
}

# Not utils::modifyList(): it merges list values by name, so the unnamed
# entries of "checked" would be dropped and every run would look like the first.
get_day <- function(state, date) {
  day <- empty_day()
  saved <- state[[as.character(date)]]
  for (key in intersect(names(day), names(saved))) day[[key]] <- saved[[key]]
  day
}

# Written to a temp file and renamed, so a crash mid-write cannot leave a
# truncated state.json that every later run would fail to parse.
write_state <- function(state, date, day, path = STATE_FILE) {
  state[[as.character(date)]] <- day
  keep  <- names(state) >= as.character(as.Date(date) - KEEP_DAYS)
  state <- state[keep]
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  writeLines(jsonlite::toJSON(state, auto_unbox = TRUE, pretty = TRUE), tmp)
  file.rename(tmp, path)
  invisible(state)
}

add_unique <- function(existing, value) as.list(union(unlist(existing), value))
