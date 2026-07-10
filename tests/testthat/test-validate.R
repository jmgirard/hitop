# Tests for the validate_*() input assertions in R/util.R.
# This API has no `id` argument, so there is no validate_id() (dropped from the
# fork). validate_items() requires a vector of the exact length (NULL is no
# longer accepted).

test_that("validate_data() requires a data frame", {
  expect_no_error(validate_data(data.frame(a = 1)))
  expect_error(validate_data(1:5))
  expect_error(validate_data(matrix(1:4, 2)))
})

test_that("validate_items() accepts a length-n character/integer vector", {
  expect_no_error(validate_items(1:220, n = 220))
  expect_no_error(validate_items(c("a", "b"), n = 2))
  expect_error(validate_items(1:100, n = 220))    # wrong length
  expect_error(validate_items(list(1, 2), n = 2)) # wrong type
  expect_error(validate_items(NULL, n = 220))     # NULL not accepted
})

test_that("validate_scales() accepts character or integerish of any length", {
  expect_no_error(validate_scales("PNA"))
  expect_no_error(validate_scales(1:3))
  expect_error(validate_scales(TRUE))
  expect_error(validate_scales(list("a")))
})

test_that("validate_range() requires a length-2 increasing integerish vector", {
  expect_no_error(validate_range(c(0, 3)))
  expect_error(validate_range(c(0, 3, 4)))  # wrong length
  expect_error(validate_range(c(3, 0)))     # not increasing
  expect_error(validate_range(0))           # length 1
})

test_that("exported functions reject bad input end-to-end", {
  expect_error(score_pid5(1:5))                                       # not a data frame
  expect_error(score_pid5(sim_pid5, items = 1:10, version = "FULL"))  # wrong item count
  expect_error(score_pid5(sim_pid5, items = 1:220, srange = c(3, 0))) # bad range
})
