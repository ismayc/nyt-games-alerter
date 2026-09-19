# run_checks() with fake checks and a fake sender: no network, no Gmail.

new_outbox <- function(succeed = TRUE) {
  box <- new.env()
  box$sent <- list()
  box$send <- function(subject, body, dry_run = FALSE) {
    box$sent[[length(box$sent) + 1]] <- list(subject = subject, body = body)
    succeed
  }
  box
}

counted <- function(value) {
  calls <- 0
  list(fn = function(date) { calls <<- calls + 1; if (inherits(value, "error")) stop(value); value },
       calls = function() calls)
}

DATE <- "2026-09-17"

test_that("no findings means no email, and the day is not checked twice", {
  state <- tempfile(fileext = ".json"); box <- new_outbox()
  w <- counted(FALSE); d <- counted(FALSE)
  checks <- list(wordle = w$fn, daily = d$fn)
  run_checks(DATE, checks, box$send, hour = 5L, state_path = state)
  run_checks(DATE, checks, box$send, hour = 8L, state_path = state)
  expect_length(box$sent, 0)
  expect_equal(c(w$calls(), d$calls()), c(1, 1))
})

test_that("a rebus and a Wordle match go out as one email, once", {
  state <- tempfile(fileext = ".json"); box <- new_outbox()
  checks <- list(wordle = counted(TRUE)$fn, daily = counted(TRUE)$fn, mini = counted(FALSE)$fn)
  run_checks(DATE, checks, box$send, hour = 5L, state_path = state)
  run_checks(DATE, checks, box$send, hour = 8L, state_path = state)
  expect_length(box$sent, 1)
  expect_equal(box$sent[[1]]$subject,
               "[nyt-games] Thursday, September 17, 2026: Wordle matches a pattern and rebus in The Crossword")
  expect_match(box$sent[[1]]$body, "The Crossword has at least one rebus square", fixed = TRUE)
  expect_no_match(box$sent[[1]]$body, "The Mini", fixed = TRUE)
})

test_that("a failed send is retried on the next run", {
  state <- tempfile(fileext = ".json")
  checks <- list(daily = counted(TRUE)$fn)
  failing <- new_outbox(succeed = FALSE); working <- new_outbox()
  run_checks(DATE, checks, failing$send, hour = 5L, state_path = state)
  run_checks(DATE, checks, working$send, hour = 8L, state_path = state)
  expect_length(failing$sent, 1)
  expect_length(working$sent, 1)
})

test_that("a failed check is retried, and does not block the others", {
  state <- tempfile(fileext = ".json"); box <- new_outbox()
  broken <- counted(simpleError("HTTP 503")); fine <- counted(TRUE)
  res <- run_checks(DATE, list(daily = broken$fn, wordle = fine$fn), box$send,
                    hour = 5L, state_path = state)
  expect_named(res$errors, "daily")
  expect_length(box$sent, 1)                       # the Wordle match still goes out
  recovered <- counted(TRUE)
  run_checks(DATE, list(daily = recovered$fn, wordle = fine$fn), box$send,
             hour = 8L, state_path = state)
  expect_equal(fine$calls(), 1)
  expect_equal(recovered$calls(), 1)
  expect_length(box$sent, 2)
  expect_match(box$sent[[2]]$subject, "rebus in The Crossword")
})

test_that("a check still failing at the last run sends one failure email", {
  state <- tempfile(fileext = ".json"); box <- new_outbox()
  checks <- list(daily = counted(simpleError("HTTP 403"))$fn)
  run_checks(DATE, checks, box$send, hour = 5L, state_path = state)
  expect_length(box$sent, 0)
  run_checks(DATE, checks, box$send, hour = 11L, state_path = state)
  run_checks(DATE, checks, box$send, hour = 14L, state_path = state)
  expect_length(box$sent, 1)
  expect_match(box$sent[[1]]$subject, "checks failing")
  expect_match(box$sent[[1]]$body, "daily: HTTP 403", fixed = TRUE)
})

test_that("dry run writes no state", {
  state <- tempfile(fileext = ".json"); box <- new_outbox(succeed = FALSE)
  run_checks(DATE, list(daily = counted(TRUE)$fn), box$send, dry_run = TRUE,
             hour = 5L, state_path = state)
  expect_false(file.exists(state))
})

test_that("state older than the retention window is pruned", {
  state <- tempfile(fileext = ".json"); box <- new_outbox()
  checks <- list(wordle = counted(FALSE)$fn)
  run_checks("2026-07-01", checks, box$send, hour = 5L, state_path = state)
  run_checks(DATE, checks, box$send, hour = 5L, state_path = state)
  expect_equal(names(read_state(state)), DATE)
})

test_that("lists of three use the Oxford comma", {
  expect_equal(and_list(c("a", "b", "c")), "a, b, and c")
  expect_equal(and_list(c("a", "b")), "a and b")
})

test_that("default checks are one Wordle check plus each crossword", {
  keys <- names(default_checks())
  expect_true("wordle" %in% keys)
  for (path in unname(CROSSWORDS)) expect_true(path %in% keys)
})

test_that("the Wordle alert never names which pattern matched", {
  expect_equal(finding_phrase("wordle"), "Wordle matches a pattern")
  for (p in WORDLE_PATTERNS) {
    expect_no_match(finding_phrase("wordle"), p, fixed = TRUE)
    expect_no_match(finding_sentence("wordle"), p, fixed = TRUE)
  }
})
