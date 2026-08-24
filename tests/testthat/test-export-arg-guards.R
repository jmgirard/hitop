# Every argument of the two HiTOP-SR online-export generators that reaches the
# built artifact is guarded. The domain is read from formals() rather than
# hand-listed, so an argument added to either function later is swept without
# this file being edited.
#
# Three names are skipped deliberately: `file` is a path, not artifact content;
# `module` and `subset` are guarded by hitop_module() and resolve_module_arg()
# and carry their own tests (test-module.R, test-deprecated.R).
SKIP_ARGS <- c("file", "module", "subset")

# One probe that is the wrong type for every guard in play: not a string, not
# TRUE/FALSE, not a whole number, and not NULL, so it fails whichever of the
# three guards an argument uses -- including the ones that accept NULL.
WRONG_TYPE <- list(1, 2)

guarded_args <- function(fn) {
  setdiff(names(formals(fn)), SKIP_ARGS)
}

test_that("every artifact-reaching formal of generate_qualtrics_hitopsr() is guarded", {
  args <- guarded_args(generate_qualtrics_hitopsr)
  # The walk's domain must not be able to empty silently: an empty `args` would
  # make every expectation below vacuous and the test still green.
  expect_true(length(args) > 0)

  # The control: at its defaults the function still writes a file, so a red
  # result below means the guard fired, not that the call was broken.
  expect_no_error(
    generate_qualtrics_hitopsr(file = withr::local_tempfile(fileext = ".txt"))
  )

  for (arg in args) {
    call_args <- list(file = withr::local_tempfile(fileext = ".txt"))
    call_args[[arg]] <- WRONG_TYPE
    err <- expect_error(
      do.call(generate_qualtrics_hitopsr, call_args),
      class = "rlang_error"
    )
    # Which argument was refused, not merely that something was refused.
    expect_match(conditionMessage(err), arg, fixed = TRUE, info = arg)
  }
})

test_that("every artifact-reaching formal of generate_redcap_hitopsr() is guarded", {
  args <- guarded_args(generate_redcap_hitopsr)
  expect_true(length(args) > 0)

  expect_no_error(
    generate_redcap_hitopsr(file = withr::local_tempfile(fileext = ".zip"))
  )

  for (arg in args) {
    call_args <- list(file = withr::local_tempfile(fileext = ".zip"))
    call_args[[arg]] <- WRONG_TYPE
    err <- expect_error(
      do.call(generate_redcap_hitopsr, call_args),
      class = "rlang_error"
    )
    expect_match(conditionMessage(err), arg, fixed = TRUE, info = arg)
  }
})

test_that("the guards blame the exported function, not the internal builder", {
  err <- expect_error(
    generate_qualtrics_hitopsr(
      file = withr::local_tempfile(fileext = ".txt"),
      id_prefix = 1
    ),
    class = "rlang_error"
  )
  expect_equal(
    rlang::call_name(conditionCall(err)),
    "generate_qualtrics_hitopsr"
  )

  err <- expect_error(
    generate_redcap_hitopsr(
      file = withr::local_tempfile(fileext = ".zip"),
      form_name = 1
    ),
    class = "rlang_error"
  )
  expect_equal(rlang::call_name(conditionCall(err)), "generate_redcap_hitopsr")
})

test_that("`breaks` keeps its documented 0 and NULL disable values", {
  # These two are the reason validate_count() grew a `min` and an `allow_null`:
  # both are documented as turning pagination off, and both would have been
  # rejected by the >= 1 floor the helper had before.
  qual <- withr::local_tempfile(fileext = ".txt")
  expect_no_error(generate_qualtrics_hitopsr(file = qual, breaks = 0))
  expect_no_error(generate_qualtrics_hitopsr(file = qual, breaks = NULL))
  expect_false(any(grepl("[[PageBreak]]", readLines(qual), fixed = TRUE)))

  red <- withr::local_tempfile(fileext = ".zip")
  expect_no_error(generate_redcap_hitopsr(file = red, breaks = 0))
  expect_no_error(generate_redcap_hitopsr(file = red, breaks = NULL))

  # A negative page size is still refused -- 0 disables, it does not open the
  # floor.
  expect_error(
    generate_qualtrics_hitopsr(
      file = withr::local_tempfile(fileext = ".txt"),
      breaks = -1
    ),
    "range"
  )
})

test_that("every exported Qualtrics and REDCap generator guards its shared arguments", {
  # NEWS says these arguments are checked on *every* generate_qualtrics_*() and
  # generate_redcap_*(), so the claim is enforced over the whole exported
  # family rather than a sample of it. Ten of the eleven inherit the guards
  # from build_qualtrics_txt()/build_redcap_zip(); generate_redcap_hitophsum()
  # builds its dictionary itself and carries them directly.
  fns <- grep(
    "^generate_(qualtrics|redcap)_",
    getNamespaceExports("hitop"),
    value = TRUE
  )
  expect_true(length(fns) > 0)

  # The arguments NEWS names, checked on each generator that actually has it --
  # generate_redcap_hitophsum() has no `breaks`, for instance.
  shared <- c(
    "block_name", "id_prefix", "include_instructions",
    "breaks", "form_name", "required"
  )

  for (fn in fns) {
    f <- getExportedValue("hitop", fn)
    ext <- if (grepl("qualtrics", fn)) ".txt" else ".zip"
    for (arg in intersect(names(formals(f)), shared)) {
      call_args <- list(file = withr::local_tempfile(fileext = ext))
      call_args[[arg]] <- WRONG_TYPE
      err <- expect_error(
        do.call(f, call_args),
        class = "rlang_error",
        info = paste(fn, arg)
      )
      expect_match(
        conditionMessage(err), arg,
        fixed = TRUE, info = paste(fn, arg)
      )
    }
  }
})
