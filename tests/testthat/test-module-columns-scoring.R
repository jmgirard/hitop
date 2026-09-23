# score_hitopsr() and reliability_hitopsr() with `items` omitted, taking the
# item columns from a module's `columns` attribute.
#
# Oracle: every call with `items` omitted is compared to the same call with the
# names passed as `items`, which is the behavior the package had before. The
# names come from sprintf(), or in the end-to-end test from the REDCap
# dictionary parsed back from the generated ZIP, never from item_names().

TWO_SCALES <- c("Agoraphobia", "Appetite Loss")

two_scale_module <- function() {
  hitop_module("hitopsr", scales = TWO_SCALES)
}

# Simulated responses for a module, named `hsr_NNN`.
module_data <- function(module) {
  sim_hitopsr[sprintf("hsr_%03d", module$items)]
}

test_that("score_hitopsr() and reliability_hitopsr() take a module's columns when `items` is omitted or NULL", {
  m <- two_scale_module()
  data <- module_data(m)
  attr(m, "columns") <- names(data)

  expected_scores <- score_hitopsr(data, items = names(data), module = m, append = FALSE)
  expected_rel <- reliability_hitopsr(data, items = names(data), module = m, omega = FALSE)

  expect_identical(score_hitopsr(data, module = m, append = FALSE), expected_scores)
  expect_identical(score_hitopsr(data, items = NULL, module = m, append = FALSE), expected_scores)
  expect_identical(reliability_hitopsr(data, module = m, omega = FALSE), expected_rel)
  expect_identical(reliability_hitopsr(data, items = NULL, module = m, omega = FALSE), expected_rel)

  # The columns are read by name: shuffling the data frame's columns and
  # adding another one changes nothing.
  shuffled <- cbind(extra = 1, data[rev(names(data))])
  expect_identical(
    score_hitopsr(shuffled, module = m, append = FALSE),
    expected_scores
  )
})

test_that("a supplied `items` wins over a module's columns", {
  m <- two_scale_module()
  data <- module_data(m)
  plain <- score_hitopsr(data, items = seq_along(data), module = m, append = FALSE)

  # Columns that name nothing in `data`: a call that read them would fail.
  attr(m, "columns") <- paste0("absent_", seq_along(data))
  expect_identical(
    score_hitopsr(data, items = seq_along(data), module = m, append = FALSE),
    plain
  )
  expect_identical(
    reliability_hitopsr(data, items = seq_along(data), module = m, omega = FALSE),
    reliability_hitopsr(data, items = seq_along(data), module = two_scale_module(), omega = FALSE)
  )
})

test_that("omitting `items` without columns to take is an error saying to pass `items`", {
  withr::local_options(cli.width = 10000)
  m <- two_scale_module()
  data <- module_data(m)
  with_columns <- m
  attr(with_columns, "columns") <- names(data)
  attr(with_columns, "item_order") <- rev(m$items)

  calls <- list(
    no_module = function(f) f(sim_hitopsr),
    no_columns = function(f) f(data, module = m),
    printed = function(f) f(data, module = with_columns, layout = "printed")
  )
  fns <- list(
    score_hitopsr = function(...) score_hitopsr(..., append = FALSE),
    reliability_hitopsr = function(...) reliability_hitopsr(..., omega = FALSE)
  )
  for (fn in names(fns)) {
    for (label in names(calls)) {
      e <- expect_error(calls[[label]](fns[[fn]]), class = "rlang_error",
                        info = paste(fn, label))
      expect_match(conditionMessage(e), "items", fixed = TRUE,
                   info = paste(fn, label))
      expect_match(conditionMessage(e), "Pass", fixed = TRUE,
                   info = paste(fn, label))
    }
  }
})

test_that("an explicit `items = NULL` is named as NULL, not as missing", {
  withr::local_options(cli.width = 10000)
  m <- two_scale_module()
  data <- module_data(m)
  fns <- list(
    score_hitopsr = function(...) score_hitopsr(..., append = FALSE),
    reliability_hitopsr = function(...) reliability_hitopsr(..., omega = FALSE)
  )
  for (fn in names(fns)) {
    e <- expect_error(fns[[fn]](data, items = NULL, module = m),
                      class = "rlang_error", info = fn)
    expect_match(conditionMessage(e), "is `NULL`", fixed = TRUE, info = fn)
    expect_no_match(conditionMessage(e), "is missing", fixed = TRUE, info = fn)
    # The control: an omitted `items` is still called missing.
    e <- expect_error(fns[[fn]](data, module = m),
                      class = "rlang_error", info = fn)
    expect_match(conditionMessage(e), "is missing", fixed = TRUE, info = fn)
  }
})

test_that("module columns absent from `data` are blamed on the module, with a pointer to `items`", {
  withr::local_options(cli.width = 10000)
  m <- two_scale_module()
  data <- module_data(m)
  # Names from another export: a Qualtrics prefix on REDCap-named data.
  attr(m, "columns") <- sprintf("HSR_%03d", m$items)
  fns <- list(
    score_hitopsr = function(...) score_hitopsr(..., append = FALSE),
    reliability_hitopsr = function(...) reliability_hitopsr(..., omega = FALSE)
  )
  for (fn in names(fns)) {
    e <- expect_error(fns[[fn]](data, module = m), class = "rlang_error",
                      info = fn)
    msg <- conditionMessage(e)
    expect_match(msg, "columns", fixed = TRUE, info = fn)
    expect_match(msg, "Pass", fixed = TRUE, info = fn)
    expect_match(msg, sprintf("HSR_%03d", m$items[[1]]), fixed = TRUE,
                 info = fn)
    expect_no_match(msg, "names must all be columns", fixed = TRUE, info = fn)
  }
})

test_that("a REDCap module export scores from its descriptor alone", {
  skip_if_not_installed("zip")
  m <- two_scale_module()
  zipfile <- withr::local_tempfile(fileext = ".zip")
  descriptor <- withr::local_tempfile(fileext = ".json")
  generate_redcap_hitopsr(file = zipfile, module = m, descriptor = descriptor)

  # Name the simulated responses by the dictionary the generator wrote, as a
  # REDCap export of that dictionary names them.
  dictionary <- read_redcap_csv(zipfile)
  parsed <- dictionary$`Variable / Field Name`[dictionary$`Field Type` == "radio"]
  expect_length(parsed, m$nItems)
  data <- stats::setNames(sim_hitopsr[m$items], parsed)

  from_file <- read_module(descriptor)
  expect_identical(
    score_hitopsr(data, module = from_file, append = FALSE),
    score_hitopsr(data, items = parsed, module = from_file, append = FALSE)
  )
  expect_identical(
    reliability_hitopsr(data, module = from_file, omega = FALSE),
    reliability_hitopsr(data, items = parsed, module = from_file, omega = FALSE)
  )
})
