# history.R with a fake fetcher: no network.

fake_row <- function(hits = character(), rebus = list()) {
  function(game, date, include_solution = TRUE, max_attempts = 4L) {
    hit <- date %in% hits
    if (identical(game, "wordle")) {
      data.frame(date = date, weekday = weekday_name(date), game = game, hit = hit,
                 rebus_cells = NA_integer_, gimmick_cells = NA_integer_,
                 solution = if (include_solution && hit) "SHAKE" else NA_character_,
                 stringsAsFactors = FALSE)
    } else {
      n <- if (!is.null(rebus[[date]])) rebus[[date]] else 0L
      data.frame(date = date, weekday = weekday_name(date), game = game, hit = n > 0L,
                 rebus_cells = as.integer(n), gimmick_cells = 0L, solution = NA_character_,
                 stringsAsFactors = FALSE)
    }
  }
}

test_that("weekday_name matches the calendar", {
  expect_equal(weekday_name("2026-09-19"), "Saturday")
  expect_equal(weekday_name("2026-09-16"), "Wednesday")
})

test_that("dates_needed asks only for missing days in the window", {
  h <- empty_history()
  need <- dates_needed(h, "daily", today = "2026-09-19", window_days = 3L)
  expect_equal(need, c("2026-09-16", "2026-09-17", "2026-09-18", "2026-09-19"))
})

test_that("dates_needed does not go before a game's first archive date", {
  h <- empty_history()
  need <- dates_needed(h, "wordle", today = "2021-06-21", window_days = 30L)
  expect_equal(min(need), "2021-06-19")     # Wordle launch; nothing earlier
})

test_that("a past Wordle with no solution is re-fetched, a filled one is not", {
  h <- rbind(
    data.frame(date = "2026-09-10", weekday = "Thursday", game = "wordle", hit = TRUE,
               rebus_cells = NA_integer_, gimmick_cells = NA_integer_, solution = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(date = "2026-09-11", weekday = "Friday", game = "wordle", hit = FALSE,
               rebus_cells = NA_integer_, gimmick_cells = NA_integer_, solution = "AAAAA",
               stringsAsFactors = FALSE))
  need <- dates_needed(h, "wordle", today = "2026-09-11", window_days = 2L)
  expect_true("2026-09-10" %in% need)        # past, solution still NA
  expect_false("2026-09-11" %in% need)       # already has a solution
})

test_that("update_history fills the window and records a rebus count", {
  path <- tempfile(fileext = ".csv")
  fetch <- fake_row(rebus = list(`2026-09-16` = 7L))
  # Only the crossword, and a short synthetic window via a wrapper.
  with_mocked <- function() update_history("2026-09-16", games = "daily", path = path,
                                           fetch_row = fetch)
  # Shrink the window so the test does not fetch 60 days.
  local({
    orig <- dates_needed
    assign("dates_needed", function(history, game, today, window_days = 60L)
      orig(history, game, today, window_days = 2L), envir = globalenv())
    on.exit(assign("dates_needed", orig, envir = globalenv()), add = TRUE)
    with_mocked()
  })
  h <- read_history(path)
  expect_equal(nrow(h), 3)                                   # 14th, 15th, 16th
  expect_equal(h$rebus_cells[h$date == "2026-09-16"], 7L)
  expect_true(h$hit[h$date == "2026-09-16"])
  expect_false(any(h$hit[h$date != "2026-09-16"]))
})

test_that("write_history keeps the latest row per date+game and sorts", {
  path <- tempfile(fileext = ".csv")
  h <- rbind(
    data.frame(date = "2026-09-10", weekday = "Thursday", game = "wordle", hit = TRUE,
               rebus_cells = NA_integer_, gimmick_cells = NA_integer_, solution = NA_character_,
               stringsAsFactors = FALSE),
    data.frame(date = "2026-09-10", weekday = "Thursday", game = "wordle", hit = TRUE,
               rebus_cells = NA_integer_, gimmick_cells = NA_integer_, solution = "SHAKE",
               stringsAsFactors = FALSE))
  out <- write_history(h, path)
  expect_equal(nrow(out), 1)
  expect_equal(out$solution, "SHAKE")        # later row wins
})
