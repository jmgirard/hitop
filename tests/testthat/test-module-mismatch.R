# Every function that takes a HiTOP-SR module refuses one that is not the
# hitop_module() build of its own `instrument` and `scales`, under the public
# class `hitop_module_mismatch` (D-081).
#
# The probes are edits a caller could make by hand to a module built from
# Appetite Loss and Dishonesty. Dishonesty is a parent scale with subscales, so
# the three probes that remove one of its items are also run through the
# scoring functions with `include_subscales = TRUE`, where the refusal must
# come before the subscale code. `pattern` is text the refusal must hold; `absent` is
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
  ),
  # Found by the claim audit: the scoring functions checked the instrument
  # first, with no class, so this edit escaped the class there.
  instrument = list(
    edit = function(m) {
      m$instrument <- "pid5"
      m
    },
    pattern = "Cannot rebuild", absent = "wrong instrument", parent = TRUE
  ),
  # hitop_module() reads the instrument in any case, so this one rebuilds;
  # found by the claim audit's re-read.
  instrument_case = list(
    edit = function(m) {
      m$instrument <- "HiTOPSR"
      m
    },
    pattern = "Its instrument field",
    absent = c("wrong instrument", "Its items field", "nItems")
  ),
  # An object of the class that is not a list has no fields to rebuild from;
  # found by review pass 2, where it stopped with a base `$` error.
  not_a_list = list(
    edit = function(m) structure("a", class = "hitop_module"),
    pattern = c("not a list", "instrument", "scales", "items", "nItems",
                "camelCase"),
    absent = "Cannot rebuild"
  ),
  not_a_list_subset = list(
    edit = function(m) structure(1:3, class = "hitop_subset"),
    pattern = "not a list", absent = "Cannot rebuild"
  ),
  environment = list(
    edit = function(m) structure(list2env(unclass(m)), class = "hitop_module"),
    pattern = "not a list", absent = "Cannot rebuild"
  ),
  no_fields = list(
    edit = function(m) structure(list(), class = "hitop_module"),
    pattern = "Cannot rebuild", absent = "not a list", parent = TRUE
  )
)

# One runner per function that takes a module. A runner calls the function on
# `m` inside a fresh directory and returns the paths the call may write, so a
# refusal can be shown to leave none of them behind. `descriptor` is ignored by
# the functions that write no descriptor. A probe that is not a list may have
# no `items` to read, so the scoring runners pass three columns, which the
# refusal comes before.
module_items <- function(m) if (is.list(m)) as.integer(m$items) else 1:3

# The generator is called by its exported name, not through a local alias, so
# the call a refusal blames reads as the caller wrote it.
generator_runner <- function(name, ext) {
  function(m, dir, descriptor = FALSE) {
    f <- file.path(dir, paste0("form", ext))
    d <- if (descriptor) file.path(dir, "module.json")
    list(
      run = function() {
        eval(rlang::call2(name, file = f, module = m, descriptor = d))
      },
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
  generate_docx_hitopsr = generator_runner("generate_docx_hitopsr", ".docx"),
  generate_qualtrics_hitopsr =
    generator_runner("generate_qualtrics_hitopsr", ".txt"),
  generate_redcap_hitopsr =
    generator_runner("generate_redcap_hitopsr", ".zip")
)

expect_mismatch <- function(run, probe, info, fn) {
  e <- tryCatch(run(), error = identity)
  expect_s3_class(e, "hitop_module_mismatch")
  # The refusal blames the exported function the caller called, not a helper.
  call <- if (inherits(e, "condition")) conditionCall(e)
  expect_identical(
    if (is.call(call)) rlang::call_name(call) else NA_character_, fn,
    info = paste(info, "blames")
  )
  # The refusal's own text, without its parent's, so a probe whose parent is
  # the hitop_module() error cannot pass on what that error says.
  msg <- rlang::cnd_message(e, inherit = FALSE)
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
        expect_mismatch(r$run, probe, info, fn)
        for (p in r$paths) {
          expect_false(file.exists(p), info = paste(info, "wrote", p))
        }
      }
    }
  }
})

# The item numbers one bullet of the refusal names, with each "a-b" range
# expanded, read from the text "<lead> item(s) <list><tail>".
named_items <- function(msg, lead, tail) {
  line <- regmatches(msg, regexpr(paste0(lead, " items? .*?", tail), msg))
  list_text <- sub(paste0("^", lead, " items? "), "", sub(paste0(tail, "$"), "", line))
  tokens <- strsplit(list_text, ",? and |, ")[[1]]
  ranges <- grepl("-", tokens, fixed = TRUE)
  expanded <- lapply(tokens, function(t) {
    ends <- as.numeric(strsplit(t, "-", fixed = TRUE)[[1]])
    if (length(ends) == 2L) seq(ends[[1]], ends[[2]]) else ends
  })
  list(items = unlist(expanded), n_ranges = sum(ranges))
}

test_that("the refusal names every lacking or extra item, however many, with runs as ranges", {
  withr::local_options(cli.width = 10000)
  # Ten scales cover 51 items, so a module cut to three lacks 48, more than
  # the 20 a cli list shows before it cuts.
  full <- hitop_module("hitopsr", scales = hitopsr_scales$camelCase[1:10])
  cut <- full
  cut$items <- full$items[1:3]
  cut$nItems <- 3L
  lacking <- setdiff(full$items, cut$items)
  expect_gt(length(lacking), 20L)

  e <- expect_error(
    score_hitopsr(sim_hitopsr, items = module_items(cut), module = cut),
    class = "hitop_module_mismatch"
  )
  msg <- conditionMessage(e)
  expect_false(grepl("…", msg, fixed = TRUE))
  named <- named_items(msg, "lacks", ", which its scales cover")
  expect_setequal(named$items, lacking)
  expect_length(named$items, length(lacking))
  # Some lacking items are consecutive, so the list holds at least one range.
  expect_gt(named$n_ranges, 0L)

  # The extra items the same way: 25 items no scale of the module covers.
  outside <- setdiff(seq_len(405L), full$items)[1:25]
  padded <- full
  padded$items <- sort(c(full$items, outside))
  padded$nItems <- length(padded$items)
  e <- expect_error(write_module(padded, withr::local_tempfile(fileext = ".json")),
                    class = "hitop_module_mismatch")
  named <- named_items(conditionMessage(e), "holds", " outside its scales")
  expect_setequal(named$items, outside)
  expect_length(named$items, length(outside))
  expect_gt(named$n_ranges, 0L)
})

test_that("the refusal names every unknown scale, whatever the instrument's letter case", {
  withr::local_options(cli.width = 10000)
  unknown <- paste("Not A Scale", 1:25)
  for (instrument in c("hitopsr", "HiTOPSR")) {
    m <- probe_base()
    m$instrument <- instrument
    m$scales <- c(m$scales, unknown)
    e <- expect_error(
      score_hitopsr(sim_hitopsr, items = module_items(m), module = m),
      class = "hitop_module_mismatch"
    )
    own <- rlang::cnd_message(e, inherit = FALSE)
    for (s in unknown) {
      expect_true(grepl(paste0("\"", s, "\""), own, fixed = TRUE),
                  info = paste(instrument, s))
    }
  }
})

test_that("a pair of consecutive items prints as two numbers, a run of three as a range", {
  withr::local_options(cli.width = 10000)
  base <- probe_base()
  # Extra items 400 and 401 are a pair and 403 to 405 a run of three. No scale
  # of the base module covers any of them.
  m <- base
  m$items <- sort(c(m$items, c(400L, 401L, 403L, 404L, 405L)))
  m$nItems <- length(m$items)
  e <- expect_error(write_module(m, withr::local_tempfile(fileext = ".json")),
                    class = "hitop_module_mismatch")
  expect_match(conditionMessage(e), "holds items 400, 401, and 403-405 outside",
               fixed = TRUE)

  # Numbers one apart that are not whole numbers are not a run of items, so
  # each is named.
  m <- base
  m$items <- sort(c(m$items, c(1.5, 2.5, 3.5)))
  m$nItems <- length(m$items)
  e <- expect_error(write_module(m, withr::local_tempfile(fileext = ".json")),
                    class = "hitop_module_mismatch")
  expect_match(conditionMessage(e), "holds items 1.5, 2.5, and 3.5 outside",
               fixed = TRUE)

  # An item a hair off a whole number is named as it is, not rounded to the
  # item it replaced, which would read as lacking and holding the same item.
  m <- base
  m$items[m$items == 7L] <- 7 + 1e-9
  e <- expect_error(write_module(m, withr::local_tempfile(fileext = ".json")),
                    class = "hitop_module_mismatch")
  expect_match(conditionMessage(e), "holds item 7.000000001 outside",
               fixed = TRUE)
  expect_match(conditionMessage(e), "lacks item 7,", fixed = TRUE)
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
  # The check's own text, so the parent hitop_module() error, which also names
  # the scale, cannot pass it.
  own <- rlang::cnd_message(e, inherit = FALSE)
  expect_true(grepl("\"Body Focus\"", own, fixed = TRUE))
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
      e <- expect_mismatch(run, lacking[[name]], paste(fn, name), fn)
      expect_false(
        grepl("Internal error", conditionMessage(e), fixed = TRUE),
        info = paste(fn, name)
      )
    }
  }
})
