# available_scales() is a view of `hitopsr_scales`, so the test compares it
# against that table column by column rather than against a transcribed list:
# a change to the shipped table must move this function's output with it, and a
# hand-written expectation would silently stop tracking.

test_that("available_scales() returns one row per HiTOP-SR scale", {
  out <- available_scales("hitopsr")

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("Scale", "camelCase", "nItems"))
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
