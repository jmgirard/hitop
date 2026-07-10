# Tests for internal helpers in R/util.R.

test_that("reverse() reflects values around the given range", {
  expect_equal(reverse(0, low = 0, high = 3), 3)
  expect_equal(reverse(3, low = 0, high = 3), 0)
  expect_equal(reverse(c(0, 1, 2, 3), low = 0, high = 3), c(3, 2, 1, 0))
})

test_that("reverse() preserves NA and infers range from data when unspecified", {
  expect_equal(reverse(c(0, NA, 3), low = 0, high = 3), c(3, NA, 0))
  # low/high inferred from observed values: low = 1, high = 5
  expect_equal(reverse(c(1, 2, 5)), c(5, 4, 1))
})

test_that("bind_columns() cbinds a list of equal-length vectors", {
  out <- bind_columns(list(a = c(1, 2), b = c(3, 4)))
  expect_equal(dim(out), c(2, 2))
  expect_equal(colnames(out), c("a", "b"))
  expect_equal(out[, "b"], c(3, 4))
})

test_that("adiff() takes the absolute difference of a paired-item row", {
  data <- data.frame(v1 = c(1, 10), v2 = c(5, 5), v3 = c(2, 2))
  # items row 1: use data columns 2 and 3
  items <- data.frame(id = 99, a = 2, b = 3)
  expect_equal(adiff(data, items, 1), c(abs(5 - 2), abs(5 - 2)))
})

test_that("drop_na() removes NA entries", {
  expect_equal(drop_na(c(1, NA, 2, NA)), c(1, 2))
  expect_length(drop_na(c(NA, NA)), 0)
  expect_equal(drop_na(1:3), 1:3)
})

test_that("calc_sem() returns the standard error, ignoring NA", {
  expect_equal(calc_sem(c(1, 2, 3)), stats::sd(c(1, 2, 3)) / sqrt(3))
  expect_equal(calc_sem(c(1, 2, 3, NA)), stats::sd(c(1, 2, 3)) / sqrt(3))
  expect_equal(calc_sem(c(0, 0, 0, 0)), 0)  # no variance
})

test_that("cli_assert() aborts with its message iff the condition is false", {
  expect_no_error(cli_assert(TRUE, "should not fire"))
  expect_error(cli_assert(FALSE, "boom"), "boom")
})

test_that("round_half_up() rounds halves away from zero, unlike base round()", {
  expect_equal(round_half_up(2.5), 3)   # base round(2.5) = 2 (round half to even)
  expect_equal(round_half_up(3.5), 4)   # base round(3.5) = 4 (agrees here)
  expect_equal(round_half_up(0.5), 1)   # base round(0.5) = 0
  expect_equal(round_half_up(c(1.4, 1.6, 2)), c(1, 2, 2))
  expect_equal(round_half_up(0), 0)
})

test_that("apa_mean() prorates and rounds the raw score per the APA rule", {
  # No missing: reduces to the plain mean (round(sum) = sum for integer sums).
  expect_equal(apa_mean(matrix(c(1, 2, 3, 2), nrow = 1)), 2)
  # 1 of 4 missing (25% <= 25%): prorate 5 over 3 answered ->
  # round(5 * 4 / 3) = round(6.667) = 7; average = 7/4 = 1.75.
  expect_equal(apa_mean(matrix(c(1, 2, 2, NA), nrow = 1)), 7 / 4)
  # Half-integer prorated raw rounds up: 5-item scale, 1 missing, answered sum 2
  # -> round(2 * 5 / 4) = round(2.5) = 3; average = 3/5 = 0.6.
  expect_equal(apa_mean(matrix(c(0, 0, 1, 1, NA), nrow = 1)), 0.6)
})

test_that("apa_mean() returns NA when more than 25% of items are missing", {
  # 2 of 4 missing (50% > 25%) -> NA.
  expect_true(is.na(apa_mean(matrix(c(1, 2, NA, NA), nrow = 1))))
  # Boundary: exactly 25% missing (2 of 8) is retained (prorated), not NA.
  expect_false(is.na(apa_mean(matrix(c(1, 1, 1, 1, 1, 1, NA, NA), nrow = 1))))
})

test_that("apa_mean() operates row-wise on a multi-respondent matrix", {
  m <- matrix(c(1, 2, 3, 2,      # row 1: complete -> 2
                1, 2, 2, NA,     # row 2: 1 missing -> 7/4
                0, NA, NA, NA),  # row 3: 3 missing (>25%) -> NA
              nrow = 3, byrow = TRUE)
  expect_equal(apa_mean(m), c(2, 7 / 4, NA))
})
