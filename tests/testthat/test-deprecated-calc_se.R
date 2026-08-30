# The `calc_se` deprecation. Same contract shape as the subset -> module rename
# in test-deprecated.R: the argument keeps working, it says so by signalling a
# condition whose CLASS is stable, and no number moves (the values are pinned in
# test-score_pid5.R, test-score_hitopsr.R and test-score_hitopbr.R, which run
# under the warning and would fail if it changed what is returned).
#
# The warning fires from score_engine(), which all three wrappers share, so the
# axis worth varying is the wrapper: each supplies its own replacement sentence
# and each must blame itself rather than the engine.

# Collect every `hitop_deprecated_calc_se` condition one call signals, and let
# the rest of the call proceed. `expect_warning()` cannot count, and the point
# of these tests is that the count is exactly one -- a warning fired per scale,
# or per row, would still satisfy a presence assertion.
se_warnings <- function(expr) {
  found <- list()
  withCallingHandlers(
    force(expr),
    hitop_deprecated_calc_se = function(w) {
      found[[length(found) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  found
}

# One call per wrapper, in each of the three argument states AC1 names. The
# `items` and `version` arguments differ per instrument; nothing else does.
calls <- list(
  score_pid5 = function(...) {
    score_pid5(sim_pid5bf, items = 1:25, version = "BF", append = FALSE, ...)
  },
  score_hitopsr = function(...) {
    score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE, ...)
  },
  score_hitopbr = function(...) {
    score_hitopbr(sim_hitopbr, items = 1:45, append = FALSE, ...)
  }
)

test_that("calc_se = TRUE signals exactly one deprecation condition of a stable class", {
  for (fn in names(calls)) {
    w <- se_warnings(calls[[fn]](calc_se = TRUE))

    expect_length(w, 1L)
    expect_s3_class(w[[1]], "hitop_deprecated_calc_se")
  }
})

test_that("the deprecation condition is absent without calc_se = TRUE", {
  for (fn in names(calls)) {
    # Explicitly off, and omitted entirely (the default is FALSE).
    expect_length(se_warnings(calls[[fn]](calc_se = FALSE)), 0L)
    expect_length(se_warnings(calls[[fn]]()), 0L)
  }
})

test_that("the deprecation condition blames the wrapper the user called", {
  # Not score_engine(): the engine is unexported, so a caller reading the
  # warning would be pointed at a function they cannot look up.
  for (fn in names(calls)) {
    w <- se_warnings(calls[[fn]](calc_se = TRUE))
    blamed <- conditionCall(w[[1]])

    expect_true(is.call(blamed), info = fn)
    expect_identical(as.character(blamed[[1]]), fn)
  }
})

test_that("each wrapper's warning names the replacement for its own instrument", {
  # Message text carries no stability promise (the class does), but WHICH
  # function a caller is sent to is the substance of the deprecation: a PID-5
  # caller pointed at interval_hitopsr() has been sent to a function that does
  # not take their data. So the routing is asserted, not the wording.
  expected <- c(
    score_pid5 = "reliability_pid5",
    score_hitopsr = "interval_hitopsr",
    score_hitopbr = "interval_hitopbr"
  )
  wrong <- c(
    score_pid5 = "interval_hitopsr",
    score_hitopsr = "interval_hitopbr",
    score_hitopbr = "interval_hitopsr"
  )

  for (fn in names(calls)) {
    msg <- conditionMessage(se_warnings(calls[[fn]](calc_se = TRUE))[[1]])

    expect_match(msg, expected[[fn]], fixed = TRUE, info = fn)
    expect_false(grepl(wrong[[fn]], msg, fixed = TRUE), info = fn)
  }
})
