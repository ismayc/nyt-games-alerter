parse <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)

# Same shape as the v6 endpoint: block squares are empty objects.
plain_puzzle <- parse('{"body":[{"cells":[
  {"answer":"C","type":1}, {}, {"answer":"A","type":1}, {"answer":"T","type":1}]}]}')
rebus_puzzle <- parse('{"body":[{"cells":[
  {"answer":"C","type":1}, {},
  {"answer":"HEART","moreAnswers":{"valid":["H"]},"type":1},
  {"answer":"T","type":1}]}]}')

# A gimmick puzzle: no multi-letter answer, but single-letter cells that carry a
# moreAnswers key (the grid accepts another entry there).
gimmick_puzzle <- parse('{"body":[{"cells":[
  {"answer":"S","moreAnswers":{"valid":["Z"]},"type":1}, {},
  {"answer":"A","type":1},
  {"answer":"E","moreAnswers":{"valid":["I"]},"type":1}]}]}')

test_that("the pattern becomes an anchored regex", {
  expect_equal(pattern_to_regex("__A_E"), "^..A.E$")
  expect_equal(pattern_to_regex("__a_e"), "^..A.E$")
  expect_error(pattern_to_regex("__A-E"), "only letters and underscores")
})

test_that("Wordle solutions match __A_E only with A third and E fifth", {
  for (word in c("shake", "BLAME", "crane", "whale")) {
    expect_true(wordle_matches(word, "__A_E"), info = word)
  }
  for (word in c("plant", "stone", "eagle", "abide", "areas")) {
    expect_false(wordle_matches(word, "__A_E"), info = word)
  }
})

test_that("a pattern does not match a word of another length", {
  expect_false(wordle_matches("shaken", "__A_E"))
  expect_false(wordle_matches("race", "__A_E"))
})

test_that("wordle_any_match is TRUE when any pattern fits", {
  pats <- c("__A_E", "_A_E_")
  expect_true(wordle_any_match("SHAKE", pats))    # __A_E
  expect_true(wordle_any_match("WAGER", pats))    # _A_E_
  expect_true(wordle_any_match("PACES", c("_A_E_"))) # A second, E fourth
  expect_false(wordle_any_match("PLANT", pats))
  expect_false(wordle_any_match("STONE", pats))
})

test_that("a missing Wordle solution is an error, not a non-match", {
  expect_error(wordle_matches(NULL, "__A_E"), "no solution")
  expect_error(wordle_matches("", "__A_E"), "no solution")
})

test_that("block squares are skipped", {
  expect_length(puzzle_cells(plain_puzzle), 3)
})

test_that("a rebus is a cell with a multi-letter answer", {
  expect_equal(count_rebus_cells(plain_puzzle), 0)
  expect_false(has_rebus(plain_puzzle))
  expect_equal(count_rebus_cells(rebus_puzzle), 1)
  expect_true(has_rebus(rebus_puzzle))
})

test_that("a gimmick square is a single-letter cell carrying moreAnswers", {
  expect_equal(count_gimmick_cells(gimmick_puzzle), 2)   # the S and E cells
  expect_equal(count_gimmick_cells(plain_puzzle), 0)     # nothing special
  expect_equal(count_gimmick_cells(rebus_puzzle), 0)     # HEART is a rebus, not a gimmick
})

test_that("a response without answers is an error, not 'no rebus'", {
  no_answers <- parse('{"body":[{"cells":[{"type":1},{},{"type":1}]}]}')
  expect_error(has_rebus(no_answers), "no answers")
  expect_error(has_rebus(parse('{"body":[{"board":"<svg/>"}]}')), "no body")
})
