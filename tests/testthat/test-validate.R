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

test_that("validate_items() distinguishes wrong type from wrong length", {
  # Wrong type: the message names the type problem, not a length
  expect_error(validate_items(list(1, 2), n = 2), "character or integer")
  expect_error(validate_items(TRUE, n = 1), "character or integer")
  # Wrong length: the message reports BOTH the expected count and the actual one
  err <- expect_error(validate_items(1:100, n = 220), "length")
  expect_match(conditionMessage(err), "220") # expected
  expect_match(conditionMessage(err), "100") # actual
  # A right-length vector of the wrong type still trips the type check first
  expect_error(validate_items(list(1, 2), n = 2), "character or integer")
})

test_that("validate_items_present() errors on names/positions absent from data", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  # Valid: all names present, all positions in range
  expect_no_error(validate_items_present(df, c("a", "c")))
  expect_no_error(validate_items_present(df, c(1L, 3L)))
  # Missing character column: message names the offender
  expect_error(validate_items_present(df, c("a", "zzz")), "zzz")
  # Out-of-range integer position: message names the bad position and ncol
  err <- expect_error(validate_items_present(df, c(1L, 99L)), "99")
  expect_match(conditionMessage(err), "3") # data has 3 columns
  # A position of 0 is also out of range
  expect_error(validate_items_present(df, c(0L, 1L)), "0")
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

test_that("missing `items` are caught before extraction, end-to-end", {
  # A bad NAME among otherwise-valid PID-5 items
  bad_name <- sprintf("pid_%d", 1:220)
  bad_name[5] <- "not_a_column"
  expect_error(
    score_pid5(sim_pid5, items = bad_name, version = "FULL"),
    "not_a_column"
  )
  expect_error(
    validity_pid5(sim_pid5, items = bad_name, version = "FULL"),
    "not_a_column"
  )

  # A bad POSITION among otherwise-valid PID-5 items (out of range)
  bad_pos <- c(1:219, ncol(sim_pid5) + 50L)
  expect_error(
    score_pid5(sim_pid5, items = bad_pos, version = "FULL"),
    as.character(ncol(sim_pid5) + 50L)
  )
  expect_error(
    validity_pid5(sim_pid5, items = bad_pos, version = "FULL"),
    as.character(ncol(sim_pid5) + 50L)
  )
})

test_that("input errors are attributed to the exported function, not internals", {
  # Threaded through score_engine(): the abort must blame the wrapper, not the
  # engine or the validate_* helper.
  cnd <- rlang::catch_cnd(score_pid5(sim_pid5, items = 1:10, version = "FULL"))
  expect_equal(rlang::call_name(cnd$call), "score_pid5")

  cnd <- rlang::catch_cnd(score_hitopsr(sim_hitopsr, items = 1:10))
  expect_equal(rlang::call_name(cnd$call), "score_hitopsr")

  cnd <- rlang::catch_cnd(score_hitopbr(sim_hitopbr, items = 1:10))
  expect_equal(rlang::call_name(cnd$call), "score_hitopbr")

  # Direct validate_* callers (no engine in between)
  cnd <- rlang::catch_cnd(validity_pid5(1:5))
  expect_equal(rlang::call_name(cnd$call), "validity_pid5")

  cnd <- rlang::catch_cnd(rank_scales(1:5, scales = "x"))
  expect_equal(rlang::call_name(cnd$call), "rank_scales")

  # Reliability surface (cli_assert throughout)
  cnd <- rlang::catch_cnd(calc_alpha(1L))
  expect_equal(rlang::call_name(cnd$call), "calc_alpha")

  cnd <- rlang::catch_cnd(calc_omega(1L))
  expect_equal(rlang::call_name(cnd$call), "calc_omega")
})

test_that("valid input still scores without error (no behavior regression)", {
  # The numeric output is locked by the oracle suites; here we only assert that
  # the hardened validation path does not reject a valid call.
  expect_no_error(score_pid5(sim_pid5, items = 1:220, version = "FULL"))
  expect_no_error(validity_pid5(sim_pid5, items = 1:220, version = "FULL"))
})
