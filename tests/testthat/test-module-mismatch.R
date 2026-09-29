# Every function that takes a HiTOP-SR module refuses one that is not the
# hitop_module() build of its own `instrument` and `scales`, under the public
# class `hitop_module_mismatch` (D-081).
#
# The probes are edits a caller could make by hand to a module built from
# Appetite Loss and Dishonesty. Dishonesty is a parent scale with subscales, so
# the three probes that remove one of its items also reach the subscale path of
# the scoring functions. `pattern` is text the refusal must hold; `absent` is
# text it must not hold, which keeps a probe that edits one field from passing
# on a message that blames another.

probe_base <- function() {
  hitop_module("hitopsr", scales = c("appetiteLoss", "dishonesty"))
}

# Dishonesty holds items 7, 14, 75, 175, 226, 303, 327 and 377; Appetite Loss
# holds 144, 202 and 389; item 66 belongs to Agoraphobia.
drop_item <- function(m, item) {
  m$items <- m$items[m$items != item]
  m$nItems <- length(m$items)
  m
}

mismatch_probes <- list(
  lacks_start = list(
    edit = function(m) drop_item(m, 7L),
    pattern = c("Its items field", "lacks item 7,"), lacking = TRUE
  ),
  lacks_middle = list(
    edit = function(m) drop_item(m, 226L),
    pattern = c("Its items field", "lacks item 226,"), lacking = TRUE
  ),
  lacks_end = list(
    edit = function(m) drop_item(m, 377L),
    pattern = c("Its items field", "lacks item 377,"), lacking = TRUE
  ),
  extra = list(
    edit = function(m) {
      m$items <- c(m$items, 66L)
      m$nItems <- length(m$items)
      m
    },
    pattern = c("Its items field", "holds item 66 outside")
  ),
  swapped = list(
    edit = function(m) {
      m$items[1:2] <- m$items[2:1]
      m
    },
    pattern = "Its items field", absent = c("lacks item", "outside", "nItems")
  ),
  repeated = list(
    edit = function(m) {
      m$items <- c(m$items, 14L)
      m$nItems <- length(m$items)
      m
    },
    pattern = c("Its items field", "holds item 14 more than once"),
    absent = c("lacks item", "outside")
  ),
  missing_value = list(
    edit = function(m) {
      m$items[m$items == 175L] <- NA_integer_
      m
    },
    pattern = c("Its items field", "missing value", "lacks item 175,"),
    absent = "nItems"
  ),
  character = list(
    edit = function(m) {
      m$items <- as.character(m$items)
      m
    },
    pattern = c("Its items field", "not a vector of numbers"),
    absent = "nItems"
  ),
  n_items = list(
    edit = function(m) {
      m$nItems <- m$nItems + 1L
      m
    },
    pattern = "Its nItems field", absent = c("Its items field", "camelCase")
  ),
  camel_case = list(
    edit = function(m) {
      m$camelCase[[1L]] <- "agoraphobia"
      m
    },
    pattern = "Its camelCase field", absent = c("Its items field", "nItems")
  ),
  unknown_scale = list(
    edit = function(m) {
      m$scales <- c(m$scales, "Not A Scale")
      m
    },
    pattern = c("Cannot rebuild", "Not A Scale"), parent = TRUE
  )
)

# One runner per function that takes a module. A runner calls the function on
# `m` inside a fresh directory and returns the paths the call may write, so a
# refusal can be shown to leave none of them behind. `descriptor` is ignored by
# the functions that write no descriptor.
module_items <- function(m) as.integer(m$items)

generator_runner <- function(generate, ext) {
  function(m, dir, descriptor = FALSE) {
    f <- file.path(dir, paste0("form", ext))
    d <- if (descriptor) file.path(dir, "module.json")
    list(
      run = function() generate(file = f, module = m, descriptor = d),
      paths = c(f, d)
    )
  }
}

module_runners <- list(
  write_module = function(m, dir, descriptor = FALSE) {
    f <- file.path(dir, "module.json")
    list(run = function() write_module(m, f), paths = f)
  },
  score_hitopsr = function(m, dir, descriptor = FALSE) {
    list(
      run = function() {
        score_hitopsr(sim_hitopsr, items = module_items(m), module = m)
      },
      paths = NULL
    )
  },
  # Without omega: its fit can warn on simulated data, which is not a warning
  # about the module.
  reliability_hitopsr = function(m, dir, descriptor = FALSE) {
    list(
      run = function() {
        reliability_hitopsr(
          sim_hitopsr, items = module_items(m), omega = FALSE, module = m
        )
      },
      paths = NULL
    )
  },
  generate_docx_hitopsr = generator_runner(generate_docx_hitopsr, ".docx"),
  generate_qualtrics_hitopsr =
    generator_runner(generate_qualtrics_hitopsr, ".txt"),
  generate_redcap_hitopsr = generator_runner(generate_redcap_hitopsr, ".zip")
)

expect_mismatch <- function(run, probe, info) {
  e <- tryCatch(run(), error = identity)
  expect_s3_class(e, "hitop_module_mismatch")
  msg <- conditionMessage(e)
  for (p in probe$pattern) {
    expect_true(grepl(p, msg, fixed = TRUE), info = paste(info, "holds", p))
  }
  for (p in probe$absent) {
    expect_false(grepl(p, msg, fixed = TRUE), info = paste(info, "lacks", p))
  }
  if (isTRUE(probe$parent)) {
    expect_s3_class(e$parent, "rlang_error")
  }
  e
}

test_that("every module function refuses a module edited by hand, naming the fault", {
  withr::local_options(cli.width = 10000)
  base <- probe_base()

  for (fn in names(module_runners)) {
    for (desc in c(FALSE, TRUE)) {
      for (name in names(mismatch_probes)) {
        probe <- mismatch_probes[[name]]
        dir <- withr::local_tempdir()
        r <- module_runners[[fn]](probe$edit(base), dir, descriptor = desc)
        info <- paste(fn, name, if (desc) "with descriptor" else "")
        expect_mismatch(r$run, probe, info)
        for (p in r$paths) {
          expect_false(file.exists(p), info = paste(info, "wrote", p))
        }
      }
    }
  }
})

test_that("a module saved before a scale rename is refused, not scored without the scale", {
  # Appearance Focus was named Body Focus; a module saved under the old name
  # carries the old display name and stem.
  old <- hitop_module("hitopsr", scales = c("appetiteLoss", "appearanceFocus"))
  old$scales[old$scales == "Appearance Focus"] <- "Body Focus"
  old$camelCase[old$camelCase == "appearanceFocus"] <- "bodyFocus"

  e <- expect_error(
    score_hitopsr(sim_hitopsr, items = module_items(old), module = old),
    class = "hitop_module_mismatch"
  )
  expect_true(grepl("Body Focus", conditionMessage(e), fixed = TRUE))
})

test_that("every module function accepts a module built by hitop_module() or hitop_subset(), or with double items", {
  scale_sets <- list(
    c("appetiteLoss", "dishonesty"),
    c("agoraphobia", "Mistrust")
  )
  for (scales in scale_sets) {
    built <- hitop_module("hitopsr", scales = scales)
    withCallingHandlers(
      old <- hitop_subset("hitopsr", scales = scales),
      hitop_deprecated_subset = function(cnd) rlang::cnd_muffle(cnd)
    )
    doubled <- built
    doubled$items <- as.double(doubled$items)
    kinds <- list(hitop_module = built, hitop_subset = old, doubles = doubled)

    for (fn in names(module_runners)) {
      for (kind in names(kinds)) {
        dir <- withr::local_tempdir()
        r <- module_runners[[fn]](kinds[[kind]], dir)
        info <- paste(fn, kind, paste(scales, collapse = "+"))
        # Warnings are recorded and muffled rather than left to testthat, so a
        # warning raised before an error is still seen.
        warned <- character()
        res <- withCallingHandlers(
          tryCatch(suppressMessages(r$run()), error = identity),
          warning = function(w) {
            warned <<- c(warned, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        expect_false(inherits(res, "error"), info = info)
        expect_identical(warned, character(), info = info)
        if (!is.null(r$paths)) {
          expect_true(file.exists(r$paths[[1L]]), info = info)
        }
      }
    }
  }
})

test_that("with subscales, scoring refuses a module lacking a parent-scale item, not as an internal error", {
  withr::local_options(cli.width = 10000)
  base <- probe_base()
  lacking <- mismatch_probes[c("lacks_start", "lacks_middle", "lacks_end")]

  for (fn in c("score_hitopsr", "reliability_hitopsr")) {
    for (name in names(lacking)) {
      m <- lacking[[name]]$edit(base)
      run <- switch(fn,
        score_hitopsr = function() {
          score_hitopsr(sim_hitopsr, items = module_items(m), module = m,
                        include_subscales = TRUE)
        },
        reliability_hitopsr = function() {
          reliability_hitopsr(sim_hitopsr, items = module_items(m),
                              omega = FALSE, module = m,
                              include_subscales = TRUE)
        }
      )
      e <- expect_mismatch(run, lacking[[name]], paste(fn, name))
      expect_false(
        grepl("Internal error", conditionMessage(e), fixed = TRUE),
        info = paste(fn, name)
      )
    }
  }
})
