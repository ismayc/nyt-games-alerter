# Pure detection logic. No network, no email, no clock: everything here takes
# parsed JSON (as R lists) and returns a plain answer, so it can be tested
# against fixtures.

# "__A_E" -> "^..A.E$". Anything other than letters and underscores is a typo
# in config.R, and failing loudly beats silently never matching.
pattern_to_regex <- function(pattern) {
  if (!grepl("^[A-Za-z_]+$", pattern)) {
    stop("Wordle pattern may contain only letters and underscores: ", pattern)
  }
  paste0("^", gsub("_", ".", toupper(pattern), fixed = TRUE), "$")
}

wordle_matches <- function(solution, pattern) {
  if (is.null(solution) || !nzchar(solution)) stop("Wordle response has no solution")
  grepl(pattern_to_regex(pattern), toupper(solution))
}

# TRUE if the solution fits any of the patterns. Used by the alert and the
# history: a day is a "hit" when at least one tracked pattern matches.
wordle_any_match <- function(solution, patterns) {
  any(vapply(patterns, function(p) wordle_matches(solution, p), logical(1)))
}

# The non-block cells of a v6 crossword. Block squares are empty objects, which
# jsonlite (simplifyVector = FALSE) reads as zero-length lists.
puzzle_cells <- function(puzzle) {
  cells <- puzzle$body[[1]]$cells
  if (is.null(cells)) stop("Crossword response has no body[[1]]$cells")
  Filter(function(cell) length(cell) > 0, cells)
}

# A rebus square is a cell whose answer is longer than one character. In the
# 120 puzzles scanned on 2026-09-19 those were exactly the cells that also
# carried a `moreAnswers` key.
count_rebus_cells <- function(puzzle) {
  cells   <- puzzle_cells(puzzle)
  answers <- vapply(cells, function(cell) {
    if (is.null(cell$answer)) "" else as.character(cell$answer)
  }, character(1))
  # A grid with no answers at all means the endpoint has stopped serving
  # solutions. That must be an error, not a quiet "no rebus today" forever.
  if (!any(nzchar(answers))) stop("Crossword response has cells but no answers")
  sum(nchar(answers) > 1)
}

has_rebus <- function(puzzle) count_rebus_cells(puzzle) > 0

# A "gimmick" square is a single-letter cell that still carries a `moreAnswers`
# key, meaning the grid accepts another entry there: a Schrodinger clue, an
# April Fools trick, or a symbol/shape square typed as one letter. This is a
# different trick from a rebus, whose square holds a multi-letter answer, so it
# is counted separately. In the 2026-09-19 scan, ordinary puzzles had zero such
# cells while these gimmick puzzles had several. Purely visual gimmicks (a
# drawn shape, circled squares) leave no answer-level trace and are not caught.
count_gimmick_cells <- function(puzzle) {
  cells <- puzzle_cells(puzzle)
  sum(vapply(cells, function(cell) {
    ans <- if (is.null(cell$answer)) "" else as.character(cell$answer)
    ("moreAnswers" %in% names(cell)) && nchar(ans) <= 1L
  }, logical(1)))
}
