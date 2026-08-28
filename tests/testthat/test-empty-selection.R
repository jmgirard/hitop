# The three exports taking a variable-length column selection refuse an empty
# one, and say so ahead of the rest of the selection family (AC2).
#
# `data` is exempt by design: an invalid `data` is still reported first, because
# a selection cannot be checked against a frame that is not one.

empty_selection_calls <- function() {
  scored_pid <- suppressWarnings(
    hitop::score_pid5(hitop::sim_pid5, items = 1:220, append = FALSE)
  )
  scored_sr <- suppressWarnings(
    hitop::score_hitopsr(hitop::sim_hitopsr, items = 1:405, append = FALSE)
  )
  list(
    norm_pid5 = list(
      fn = hitop::norm_pid5,
      data = scored_pid,
      arg = "scores",
      empty = list(scores = character(0)),
      # One other invalid argument per member of the selection family this
      # function carries, each as the argument list that makes it invalid. Each
      # is invalid on its own -- asserted below -- so each is a real precedence
      # test and not a no-op.
      others = list(
        srange = list(srange = c(0, 3, 7)),
        prefix = list(prefix = 1L),
        append = list(append = "yes")
      )
    ),
    interval_hitopsr = list(
      fn = hitop::interval_hitopsr,
      data = scored_sr,
      arg = "scores",
      empty = list(scores = character(0)),
      others = list(
        srange = list(srange = c(1, 4, 9)),
        prefix = list(prefix = 1L),
        level = list(level = 2),
        append = list(append = "yes")
      )
    ),
    rank_scales = list(
      fn = hitop::rank_scales,
      data = scored_sr,
      arg = "scales",
      empty = list(scales = character(0)),
      # `srange` is read by rank_scales() only when `reverse` is set, so the
      # probe that makes it invalid supplies both. Without `reverse` a malformed
      # `srange` is simply unused, and the precedence claim would rest on an
      # argument nothing looks at.
      others = list(
        top = list(top = "3"),
        prefix = list(prefix = 1L),
        srange = list(reverse = "hsr_agoraphobia", srange = c(1, 4, 9)),
        append = list(append = "yes")
      )
    )
  )
}

test_that("an empty selection aborts, naming the argument the caller wrote", {
  calls <- empty_selection_calls()
  expect_true(length(calls) > 0)

  for (nm in names(calls)) {
    spec <- calls[[nm]]
    # The control: the same call on a non-empty selection succeeds, so a red
    # result is the new guard firing and not a broken call.
    # `message =` is testthat's filter on *which* errors count, not a failure
    # label, so the control is written as an explicit success expectation that
    # can carry `info`.
    expect_true(
      !inherits(
        try(
          suppressWarnings(
            do.call(spec$fn, list(data = spec$data, names(spec$data)))
          ),
          silent = TRUE
        ),
        "try-error"
      ),
      info = nm
    )

    err <- expect_error(
      suppressWarnings(do.call(spec$fn, c(list(data = spec$data), spec$empty))),
      class = "hitop_empty_selection"
    )
    expect_match(
      cli::ansi_strip(conditionMessage(err)),
      spec$arg,
      fixed = TRUE,
      info = nm
    )
  }
})

test_that("the empty selection is reported ahead of the rest of its family", {
  calls <- empty_selection_calls()
  for (nm in names(calls)) {
    spec <- calls[[nm]]
    expect_true(length(spec$others) > 0, info = nm)

    for (other in names(spec$others)) {
      # Each `other` is genuinely invalid on its own: without that, the
      # precedence assertion below would hold for a call with only one problem.
      bad_only <- c(
        list(data = spec$data, names(spec$data)),
        spec$others[[other]]
      )
      solo <- expect_error(
        suppressWarnings(do.call(spec$fn, bad_only)),
        class = "rlang_error"
      )
      expect_false(
        inherits(solo, "hitop_empty_selection"),
        info = paste(nm, other)
      )

      both <- c(list(data = spec$data), spec$empty, spec$others[[other]])
      err <- expect_error(
        suppressWarnings(do.call(spec$fn, both)),
        class = "hitop_empty_selection"
      )
      expect_match(
        cli::ansi_strip(conditionMessage(err)),
        spec$arg,
        fixed = TRUE,
        info = paste(nm, other)
      )
    }
  }
})

test_that("rank_scales() reports the empty selection, not `top` out of range", {
  # The specific misreport D-045 records: `validate_count(top, max = length(scales))`
  # fired first on a zero-length `scales` and blamed `top` for being outside
  # "between 1 and 0" -- a consequence of the empty selection, not its cause.
  scored <- suppressWarnings(
    hitop::score_hitopsr(hitop::sim_hitopsr, items = 1:405, append = FALSE)
  )
  err <- expect_error(
    hitop::rank_scales(scored, scales = character(0)),
    class = "hitop_empty_selection"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "scales", fixed = TRUE)
  expect_no_match(msg, "between 1 and 0", fixed = TRUE)
})

test_that("an invalid `data` is still reported before the empty selection", {
  calls <- empty_selection_calls()
  for (nm in names(calls)) {
    spec <- calls[[nm]]
    err <- expect_error(
      do.call(spec$fn, c(list(data = "not a data frame"), spec$empty)),
      class = "rlang_error"
    )
    expect_false(inherits(err, "hitop_empty_selection"), info = nm)
    expect_match(
      cli::ansi_strip(conditionMessage(err)),
      "data",
      fixed = TRUE,
      info = nm
    )
  }
})

test_that("a zero-length selection of the wrong type is still a type error", {
  # `list()` is empty too, but the type check owns it: the empty-selection guard
  # must not swallow a complaint the family already makes.
  scored <- suppressWarnings(
    hitop::score_hitopsr(hitop::sim_hitopsr, items = 1:405, append = FALSE)
  )
  err <- expect_error(
    hitop::rank_scales(scored, scales = list()),
    class = "rlang_error"
  )
  expect_false(inherits(err, "hitop_empty_selection"))
})
