# difficulty.R without the network: parse_xwstats against saved page markup,
# and the read/write CSV round trip. fetch_difficulty and topup_difficulty are
# the network-facing wrappers and are left to manual/backfill runs.

# A trimmed copy of the real XW Stats page shape: tags around the three fields,
# plus a &nbsp; and a &gt; so the entity handling is exercised too.
xwstats_page <- function(difficulty = "Hard", median = "12:34",
                         direction = "18% slower") {
  paste0(
    "<html><body>",
    "<div class='hdr'>Wednesday&nbsp;&gt; The Crossword</div>",
    "<section><span>Difficulty</span> <strong>", difficulty, "</strong></section>",
    "<section>Median Solve Time <b>", median, "</b></section>",
    "<section>Median Solver ", direction, " than their average Wednesday</section>",
    "</body></html>")
}

test_that("parse_xwstats pulls the three fields and converts the median to seconds", {
  r <- parse_xwstats(xwstats_page("Hard", "12:34", "18% slower"))
  expect_equal(r$difficulty, "Hard")
  expect_equal(r$median_sec, 12L * 60L + 34L)   # 754
  expect_equal(r$vs_weekday, 18L)               # slower -> positive
})

test_that("parse_xwstats signs vs_weekday: slower positive, faster negative", {
  expect_equal(parse_xwstats(xwstats_page(direction = "22% faster"))$vs_weekday, -22L)
  expect_equal(parse_xwstats(xwstats_page(direction = "3% slower"))$vs_weekday, 3L)
})

test_that("parse_xwstats reads each difficulty label, including the two-word ones", {
  for (lab in c("Very Easy", "Easy", "Average", "Hard", "Very Hard")) {
    expect_equal(parse_xwstats(xwstats_page(difficulty = lab))$difficulty, lab)
  }
})

test_that("parse_xwstats returns NULL when the date has no data yet", {
  expect_null(parse_xwstats("<html><body>Not enough solves yet.</body></html>"))
  expect_null(parse_xwstats(""))
})

test_that("parse_xwstats keeps partial data: median present, no difficulty or direction", {
  r <- parse_xwstats("<p>Median Solve Time 05:00</p>")
  expect_false(is.null(r))
  expect_equal(r$median_sec, 300L)
  expect_true(is.na(r$difficulty))
  expect_true(is.na(r$vs_weekday))
})

test_that("read_difficulty on a missing file returns a typed, empty frame", {
  df <- read_difficulty(file.path(tempdir(), "no-such-difficulty.csv"))
  expect_equal(nrow(df), 0L)
  expect_equal(names(df), DIFFICULTY_COLUMNS)
  expect_type(df$median_sec, "integer")
  expect_type(df$vs_weekday, "integer")
})

test_that("write_difficulty then read_difficulty round-trips values and NA", {
  path <- tempfile(fileext = ".csv")
  df <- data.frame(
    date       = c("2015-01-02", "2015-01-01"),
    difficulty = c("Hard", NA_character_),
    median_sec = c(754L, NA_integer_),
    vs_weekday = c(18L, NA_integer_),
    stringsAsFactors = FALSE)
  write_difficulty(df, path)
  back <- read_difficulty(path)
  expect_equal(back$date, c("2015-01-01", "2015-01-02"))   # sorted ascending
  expect_true(is.na(back$difficulty[back$date == "2015-01-01"]))
  expect_true(is.na(back$vs_weekday[back$date == "2015-01-01"]))
  expect_equal(back$median_sec[back$date == "2015-01-02"], 754L)
})

test_that("write_difficulty creates the data directory when it is missing", {
  path <- file.path(tempfile(), "difficulty.csv")   # parent dir does not exist yet
  expect_false(dir.exists(dirname(path)))
  write_difficulty(data.frame(date = "2015-01-01", difficulty = "Easy",
                              median_sec = 300L, vs_weekday = -5L,
                              stringsAsFactors = FALSE), path)
  expect_true(file.exists(path))
})

test_that("topup_difficulty refetches only unsettled dates and stores a failure as NA", {
  path <- tempfile(fileext = ".csv")
  today <- Sys.Date()
  # today-2 is already settled; today-1 is stored NA; today is absent. Window is
  # the last 2 days, so need = {today-1, today}; today-2 must be left untouched.
  write_difficulty(data.frame(
    date       = as.character(c(today - 2, today - 1)),
    difficulty = c("Average", NA_character_),
    median_sec = c(400L, NA_integer_),
    vs_weekday = c(5L, NA_integer_),
    stringsAsFactors = FALSE), path)

  # Stub the network wrapper in the env where topup_difficulty resolves it, so
  # the stub applies no matter which env the code was sourced into (and never
  # touches the network). today-1 settles; today still has no data (NULL).
  env  <- environment(topup_difficulty)
  orig <- get("fetch_difficulty", envir = env)
  assign("fetch_difficulty", function(d, ...) {
    if (d == as.character(today - 1))
      list(difficulty = "Hard", median_sec = 754L, vs_weekday = 18L) else NULL
  }, envir = env)
  on.exit(assign("fetch_difficulty", orig, envir = env), add = TRUE)

  out <- topup_difficulty(days = 2, delay = 0, path = path)
  expect_equal(out$vs_weekday[out$date == as.character(today - 2)], 5L)   # untouched
  expect_equal(out$vs_weekday[out$date == as.character(today - 1)], 18L)  # settled now
  expect_true(is.na(out$vs_weekday[out$date == as.character(today)]))     # failure -> NA
})

test_that("write_difficulty keeps the last row per date and orders the columns", {
  path <- tempfile(fileext = ".csv")
  df <- data.frame(
    date       = c("2015-01-01", "2015-01-01"),
    difficulty = c("Easy", "Hard"),        # later row should win
    median_sec = c(300L, 754L),
    vs_weekday = c(-5L, 18L),
    stringsAsFactors = FALSE)
  # Columns intentionally shuffled to prove write_difficulty reorders them.
  df <- df[, c("vs_weekday", "date", "median_sec", "difficulty")]
  out <- write_difficulty(df, path)
  expect_equal(names(out), DIFFICULTY_COLUMNS)
  back <- read_difficulty(path)
  expect_equal(nrow(back), 1L)
  expect_equal(back$difficulty, "Hard")
  expect_equal(back$median_sec, 754L)
})
