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

test_that("validate_scales() reports the type actually supplied", {
  # The bullet is what distinguishes "wrong type" from "wrong value": without
  # it the message says only that the type was unexpected. Matched on the bare
  # class word, never the cli-styled `<logical>`, which carries ANSI escapes
  # when colors are on.
  expect_match(
    conditionMessage(expect_error(validate_scales(TRUE))),
    "logical"
  )
  expect_match(
    conditionMessage(expect_error(validate_scales(list("a")))),
    "list"
  )
})

test_that("validate_scales() names the caller's argument, not its default", {
  # norm_pid5() is the one caller that overrides `arg` (R/norm_pid5.R), so it
  # is the only place the interpolation can be observed: everywhere else the
  # default "scales" would render whether `{arg}` were interpolated or not.
  scored <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  cnd <- rlang::catch_cnd(norm_pid5(scored, scores = TRUE, version = "BF"))
  expect_equal(rlang::call_name(cnd$call), "norm_pid5")
  expect_match(conditionMessage(cnd), "scores")
})

test_that("validate_string() requires a single string, optionally NULL", {
  expect_no_error(validate_string("pid_", arg = "prefix"))
  expect_error(validate_string(1, arg = "prefix"), "prefix")
  expect_error(validate_string(c("a", "b"), arg = "prefix"), "prefix")
  expect_error(validate_string(NULL, arg = "prefix"), "prefix")
  # allow_null lets the one nullable caller (rank_scales's `prefix`) through
  expect_no_error(validate_string(NULL, arg = "prefix", allow_null = TRUE))
  expect_error(validate_string(1, arg = "prefix", allow_null = TRUE), "prefix")
  # The message reports what was supplied, as validate_items() does
  err <- expect_error(validate_string(1:3, arg = "name"))
  expect_match(conditionMessage(err), "integer")
  expect_match(conditionMessage(err), "3") # length
})

test_that("validate_flag() requires TRUE or FALSE", {
  expect_no_error(validate_flag(TRUE, arg = "append"))
  expect_no_error(validate_flag(FALSE, arg = "append"))
  expect_error(validate_flag(NA, arg = "append"), "append")
  expect_error(validate_flag(1, arg = "append"), "append")
  expect_error(validate_flag(c(TRUE, TRUE), arg = "append"), "append")
  expect_error(validate_flag(NULL, arg = "append"), "append")
})

test_that("validate_count() checks type and bounds separately", {
  expect_no_error(validate_count(3, arg = "top", max = 5))
  expect_no_error(validate_count(1, arg = "top", max = 5))
  expect_no_error(validate_count(5, arg = "top", max = 5))
  # Type failures name the type, not the range
  expect_error(validate_count("3", arg = "top", max = 5), "whole number")
  expect_error(validate_count(c(1, 2), arg = "top", max = 5), "whole number")
  expect_error(validate_count(NA_integer_, arg = "top", max = 5), "whole number")
  # Bounds failures report both the limit and what was supplied
  err <- expect_error(validate_count(9, arg = "top", max = 5), "range")
  expect_match(conditionMessage(err), "5")
  expect_match(conditionMessage(err), "9")
  expect_error(validate_count(0, arg = "top", max = 5), "range")
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
  bad_name <- sprintf("pid5_%03d", 1:220)
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

test_that("scalar-argument failures blame the exported function and the arg", {
  # These arguments were checked with bare stopifnot() before M031, which named
  # the failed predicate rather than the argument and blamed no function at all.
  # `call` carries the function name, so it is read from conditionCall(), never
  # from the message.
  blames <- function(expr, fn, arg) {
    cnd <- rlang::catch_cnd(expr)
    expect_equal(rlang::call_name(cnd$call), fn)
    expect_match(conditionMessage(cnd), arg)
  }

  blames(score_pid5(sim_pid5, items = 1:220, version = "FULL", prefix = 1),
         "score_pid5", "prefix")
  blames(score_pid5(sim_pid5, items = 1:220, version = "FULL", append = NA),
         "score_pid5", "append")
  blames(score_pid5(sim_pid5, items = 1:220, version = "FULL", calc_se = 1),
         "score_pid5", "calc_se")
  blames(validity_pid5(sim_pid5, items = 1:220, version = "FULL", prefix = 1),
         "validity_pid5", "prefix")
  blames(reliability_pid5(sim_pid5, items = 1:220, version = "FULL", alpha = 1),
         "reliability_pid5", "alpha")
  blames(reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = NA),
         "reliability_pid5", "omega")

  scored <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  blames(norm_pid5(scored, scores = names(scored), version = "BF", prefix = 1),
         "norm_pid5", "prefix")
  blames(norm_pid5(scored, scores = names(scored), version = "BF", append = NA),
         "norm_pid5", "append")

  blames(rank_scales(scored, scales = names(scored), prefix = 1),
         "rank_scales", "prefix")
  blames(rank_scales(scored, scales = names(scored), top = 99),
         "rank_scales", "top")
  blames(rank_scales(scored, scales = names(scored), name = 1),
         "rank_scales", "name")
  blames(rank_scales(scored, scales = names(scored), append = NA),
         "rank_scales", "append")

  blames(label_hitopsr(1:5), "label_hitopsr", "data")
  blames(label_hitopbr(1:5), "label_hitopbr", "data")
  blames(rename_hitopsr_items(1:5), "rename_hitopsr_items", "data")
})

test_that("rank_scales() `dir` reports the allowed values and suggests a match", {
  scored <- score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE)
  cnd <- rlang::catch_cnd(
    rank_scales(scored, scales = names(scored), dir = "hihg")
  )
  expect_equal(rlang::call_name(cnd$call), "rank_scales")
  # arg_match() lists the permitted values and offers the near miss
  expect_match(conditionMessage(cnd), "high")
  expect_match(conditionMessage(cnd), "low")
  expect_match(conditionMessage(cnd), "Did you mean")
  # arg_match() requires an exact match -- unlike match.arg(), it does NOT
  # accept an unambiguous abbreviation, so accepted input is unchanged from the
  # membership test it replaces and only the error improves.
  expect_error(
    rank_scales(scored, scales = names(scored), dir = "l"),
    "must be one of"
  )
  # arg_match() insists on a character vector, but the membership test it
  # replaced was satisfied by a factor -- `factor("high") %in% c("high","low")`
  # is TRUE -- so a `dir` taken from a factor column must keep working.
  expect_no_error(
    rank_scales(scored, scales = names(scored), dir = factor("high"))
  )
  expect_identical(
    rank_scales(scored, scales = names(scored), dir = factor("low"),
                append = FALSE),
    rank_scales(scored, scales = names(scored), dir = "low", append = FALSE)
  )
})

test_that("valid input still scores without error (no behavior regression)", {
  # The numeric output is locked by the oracle suites; here we only assert that
  # the hardened validation path does not reject a valid call.
  expect_no_error(score_pid5(sim_pid5, items = 1:220, version = "FULL"))
  expect_no_error(validity_pid5(sim_pid5, items = 1:220, version = "FULL"))
})
