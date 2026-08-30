# Locks the `calc_se` documentation on the three scoring help pages against what
# the scoring engine actually computes.
#
# The companion to test-vignette-se-prose.R. The vignettes gained an honest
# description of these numbers; the help pages did not, and said only "the
# standard error of each scale score" -- which reads as a standard error of
# measurement. `calc_sem()` (R/util.R) is the SD of one respondent's answered
# items over the square root of how many they answered: no reliability enters
# it, so it is not a standard error of measurement.
#
# The behavior itself is pinned elsewhere: calc_sem()'s NA handling in
# test-util.R, and the FULL/SF domain SE deriving from the 3 facet scores in
# test-score_pid5.R. This file guards the description.

se_param <- function(rd) {
  # Source-checkout only: man/ is not installed as .Rd.
  mdir <- testthat::test_path("..", "..", "man")
  skip_if(!dir.exists(mdir), "man/ not available")

  path <- file.path(mdir, rd)
  expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # The \item{calc_se} block: from its opening to the next \item at line start.
  m <- regmatches(
    text,
    regexpr("(?s)\\\\item\\{calc_se\\}.*?(?=\n\\\\item\\{)", text, perl = TRUE)
  )
  expect_length(m, 1L)
  # Rd wraps the roxygen prose, so a phrase can straddle a newline. Match
  # against the text with whitespace runs collapsed, which also keeps the
  # assertions stable when the source comment is reflowed.
  gsub("[[:space:]]+", " ", m)
}

score_help <- c("score_pid5.Rd", "score_hitopsr.Rd", "score_hitopbr.Rd")

test_that("the calc_se help text says what the standard error is computed over", {
  for (rd in score_help) {
    param <- se_param(rd)

    # The divisor is how many items the respondent answered, not how many the
    # scale has -- calc_sem() drops NA before dividing.
    expect_match(param, "items the respondent actually answered", info = rd)
    expect_match(param, "square root of how many of those items", info = rd)
    expect_false(
      grepl("square root of its number of items", param, fixed = TRUE),
      info = rd
    )
  }
})

test_that("the calc_se help text denies that it is a standard error of measurement", {
  for (rd in score_help) {
    param <- se_param(rd)

    expect_match(param, "not a standard error of measurement", info = rd)
    # Naming the alternative is the point: a reader who wanted measurement
    # precision has somewhere to go.
    expect_match(param, "reliability_", info = rd)
  }
})

test_that("the PID-5 calc_se help text derives domain SEs from the 3 facet scores", {
  param <- se_param("score_pid5.Rd")

  # score_pid5() computes a FULL/SF domain SE over its 3 primary facet scores,
  # never over the domain's 15 items.
  expect_match(param, "three contributing facet scores")
  expect_match(param, "square root of 3", fixed = TRUE)
})

# ---- The deprecation, and where each instrument's reader is sent ------------

test_that("the calc_se help text says the argument is deprecated", {
  for (rd in score_help) {
    param <- se_param(rd)

    expect_match(param, "Deprecated", info = rd)
    # A deprecation notice that does not say what goes away leaves a reader
    # guessing whether their `_se` columns survive it.
    expect_match(param, "removed in a future release", info = rd)
    expect_match(param, "_se", fixed = TRUE, info = rd)
  }
})

test_that("each calc_se help page names the replacement for its own instrument", {
  # The routing, not the wording: a HiTOP-BR reader sent to interval_hitopsr()
  # has been pointed at a function that does not take their data. The PID-5
  # page has no interval function to name and must say so rather than stay
  # silent, which would read as an oversight.
  expect_match(se_param("score_hitopsr.Rd"), "interval_hitopsr", fixed = TRUE)
  expect_false(
    grepl("interval_hitopbr", se_param("score_hitopsr.Rd"), fixed = TRUE)
  )

  expect_match(se_param("score_hitopbr.Rd"), "interval_hitopbr", fixed = TRUE)
  expect_false(
    grepl("interval_hitopsr", se_param("score_hitopbr.Rd"), fixed = TRUE)
  )

  pid <- se_param("score_pid5.Rd")
  expect_match(pid, "no interval function for the PID-5", fixed = TRUE)
  expect_false(grepl("interval_hitop", pid, fixed = TRUE))
})
