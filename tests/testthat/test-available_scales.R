# available_scales() is a view of `hitopsr_scales`, so the test compares it
# against that table column by column rather than against a transcribed list:
# a change to the shipped table must move this function's output with it, and a
# hand-written expectation would silently stop tracking.

test_that("available_scales() returns one row per HiTOP-SR scale", {
  out <- available_scales("hitopsr")

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Scale", "camelCase", "nItems", "Brief"))
  expect_equal(nrow(out), nrow(hitopsr_scales))
  expect_equal(out$Scale, hitopsr_scales$Scale)
  expect_equal(out$camelCase, hitopsr_scales$camelCase)
  expect_equal(out$nItems, hitopsr_scales$nItems)
})

test_that("available_scales() defaults to the HiTOP-SR", {
  # NOT expect_equal(available_scales(), available_scales("hitopsr")): that
  # compares the function to itself and passes whatever the default is. Assert
  # the declared default, and that calling with no argument yields that
  # instrument's table.
  expect_identical(formals(available_scales)$instrument, "hitopsr")
  expect_equal(available_scales()$Scale, hitopsr_scales$Scale)
})

test_that("the supported set and the scale tables cannot drift apart", {
  # F6: `supported` is derived from the table map, so an instrument cannot be
  # declared supported without a table to serve it.
  tables <- module_scale_tables()
  expect_true(length(tables) >= 1L)
  for (nm in names(tables)) {
    expect_false(is.null(tables[[nm]]), info = nm)
    expect_true(all(c("Scale", "camelCase", "nItems") %in% names(tables[[nm]])), info = nm)
    expect_equal(available_scales(nm)$Scale, tables[[nm]]$Scale, info = nm)
  }
})

test_that("available_scales() accepts an instrument name in any case", {
  expect_equal(available_scales("HiTOPSR"), available_scales("hitopsr"))
})

test_that("every name available_scales() lists builds a module", {
  out <- available_scales("hitopsr")
  # Both name columns are advertised as accepted, so both are exercised over
  # every row rather than on a sampled few.
  for (i in seq_len(nrow(out))) {
    by_display <- hitop_module("hitopsr", out$Scale[[i]])
    by_stem <- hitop_module("hitopsr", out$camelCase[[i]])
    expect_equal(by_display$items, by_stem$items, info = out$Scale[[i]])
    expect_equal(
      by_display$nItems,
      as.integer(out$nItems[[i]]),
      info = out$Scale[[i]]
    )
  }
})

# --- The guard shared with hitop_module() ------------------------------------

test_that("available_scales() rejects an unsupported instrument as hitop_module() does", {
  # Asserted by class: the message is prose and may be reworded, but a caller
  # (and this test) discriminates on the condition.
  expect_error(available_scales("hitopbr"), class = "hitop_unsupported_instrument")
  expect_error(hitop_module("hitopbr", "agoraphobia"), class = "hitop_unsupported_instrument")

  expect_error(available_scales("pid5"), class = "hitop_unsupported_instrument")
  expect_error(hitop_module("pid5", "agoraphobia"), class = "hitop_unsupported_instrument")
})

test_that("available_scales() rejects an unknown instrument as hitop_module() does", {
  expect_error(available_scales("nosuchinstrument"), class = "hitop_unknown_instrument")
  expect_error(
    hitop_module("nosuchinstrument", "agoraphobia"),
    class = "hitop_unknown_instrument"
  )
})

test_that("available_scales() rejects a non-string instrument", {
  expect_error(available_scales(1L), "single string")
  expect_error(available_scales(c("hitopsr", "hitopbr")), "single string")
})

test_that("the two functions give the same message for the same bad input", {
  browser_msg <- tryCatch(available_scales("hitopbr"), error = conditionMessage)
  builder_msg <- tryCatch(
    hitop_module("hitopbr", "agoraphobia"),
    error = conditionMessage
  )
  expect_identical(browser_msg, builder_msg)
})

# --- The clinician definition (M057) -----------------------------------------

test_that("available_scales() carries each scale's clinician definition", {
  out <- available_scales("hitopsr")

  expect_named(out, c("Scale", "camelCase", "nItems", "Brief"))

  # The expected side is read from `hitopsr_definitions` itself, never back out
  # of `out`: an expectation derived from the artifact under test is blind in
  # exactly the dimension it derives. The scale rows are the ones with no
  # subscale; the other 17 rows define subscales, which this function does not
  # list.
  defs <- hitopsr_definitions[is.na(hitopsr_definitions$Subscale), ]

  # Total coverage, asserted as an equality over both tables rather than a walk
  # over whatever survived the join -- a per-row walk alone passes vacuously on
  # exactly the failure that matters, a scale silently dropped.
  expect_setequal(out$camelCase, hitopsr_scales$camelCase)
  expect_setequal(out$camelCase, defs$camelCase)
  expect_equal(nrow(out), nrow(hitopsr_scales))

  expect_false(anyNA(out$Brief))
  expect_false(any(trimws(out$Brief) == ""))

  # Pairing, row by row: the join is on the camelCase stem, so a definition
  # re-paired to a neighbouring scale is what this catches.
  for (i in seq_len(nrow(out))) {
    want <- defs$Brief[defs$camelCase == out$camelCase[[i]]]
    expect_length(want, 1L)
    expect_identical(out$Brief[[i]], want, info = out$camelCase[[i]])
  }
})

test_that("the definitions table keys on a stem, not a printed scale name", {
  # The two tables disagree on one display label, so a join on `Scale` would
  # lose that scale. This states the disagreement rather than assuming it away:
  # if a later milestone reconciles the labels, this test says so out loud
  # instead of the stem join quietly becoming untested.
  defs <- hitopsr_definitions[is.na(hitopsr_definitions$Subscale), ]
  expect_setequal(defs$camelCase, hitopsr_scales$camelCase)
  expect_false(anyDuplicated(defs$camelCase) > 0L)
  expect_false(anyDuplicated(hitopsr_definitions$camelCase) > 0L)
})

test_that("a scale with no definition aborts rather than returning a blank", {
  # Not reachable from the shipped tables -- the data-raw script stops before
  # writing an .rda whose stems disagree -- so the guard is fired directly.
  # Asserted by class, as the other guards in this file are.
  expect_error(
    scale_definitions("hitopsr", c("agoraphobia", "nosuchscale")),
    class = "hitop_missing_definition"
  )
  expect_error(
    scale_definitions("hitopbr", "agoraphobia"),
    class = "hitop_missing_definition"
  )
  # The passing control: every shipped stem resolves, so the guard is silent on
  # the case it must not fire on.
  expect_length(
    scale_definitions("hitopsr", hitopsr_scales$camelCase),
    nrow(hitopsr_scales)
  )
})

test_that("the missing-definition abort blames the function the user called", {
  # DESIGN's convention: aborts are attributed to the user-facing function, not
  # to the internal helper that raised them. `scale_definitions()` is called
  # from `available_scales()`, so a caller sees that call in the condition.
  outer <- function() scale_definitions("hitopbr", "agoraphobia")
  cnd <- tryCatch(outer(), hitop_missing_definition = function(c) c)
  expect_s3_class(cnd, "hitop_missing_definition")
  expect_identical(conditionCall(cnd), quote(outer()))
  # The control: the neighbouring guard in the same call path behaves the same
  # way, so this is the package's convention and not a one-off.
  unsupported <- tryCatch(
    available_scales("hitopbr"),
    hitop_unsupported_instrument = function(c) c
  )
  expect_identical(conditionCall(unsupported), quote(available_scales("hitopbr")))
})
