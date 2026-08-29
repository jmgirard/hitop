# The `descriptor` sidecar the three HiTOP-SR generators write.
#
# Oracle note: the expected field values are derived from the shipped tables --
# `hitopsr_items` for item membership, `hitopsr_scales` for the display names --
# and the written file is parsed with `jsonlite::fromJSON()` rather than through
# `read_module()`, so the writer is not checked only through this package's own
# reader. `read_module()` appears alongside, testing the round trip, never
# standing in for the independent parse.

sr_scales <- available_scales("hitopsr")

# The four scales every module case below is built from: non-adjacent rows of
# the scale table, so a module is not merely a leading block of items.
FOUR_STEMS <- sr_scales$camelCase[c(3L, 17L, 41L, 68L)]

# The display names for a set of camelCase stems, in scale-table row order --
# the order hitop_module() returns them in.
display_of <- function(stems) sr_scales$Scale[sort(match(stems, sr_scales$camelCase))]

# The item numbers a set of display scale names covers, ascending. Read from
# the ITEM-level table, not from `hitopsr_scales$itemNumbers`, which is what
# hitop_module() itself reads.
items_of <- function(display) {
  as.integer(hitopsr_items$HSR[hitopsr_items$Scale %in% display])
}

# Call one generator into temporary files, returning the descriptor's path.
# `writer` names which generator; the extension follows it so the artifact is
# the shape each builder expects.
GENERATORS <- list(
  generate_docx_hitopsr = ".docx",
  generate_qualtrics_hitopsr = ".txt",
  generate_redcap_hitopsr = ".zip"
)

# What each generator needs installed: officer/flextable for the Word builder,
# an external {zip} for the REDCap bundle, nothing at all for the Qualtrics text
# file. Filtering the loop rather than skipping the whole test keeps one absent
# dependency from taking the other generators' coverage with it. Skips only when
# nothing is runnable, so the loops below cannot silently pass over an empty
# domain.
GENERATOR_DEPS <- list(
  generate_docx_hitopsr = c("officer", "flextable"),
  generate_qualtrics_hitopsr = character(),
  generate_redcap_hitopsr = "zip"
)

available_generators <- function() {
  installed <- function(pkgs) {
    all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
  }
  runnable <- names(GENERATOR_DEPS)[vapply(GENERATOR_DEPS, installed, logical(1))]
  testthat::skip_if(
    length(runnable) == 0L,
    "no generator's dependencies are installed"
  )
  runnable
}

# Parse a written descriptor without going through read_module().
parse_descriptor <- function(file) {
  jsonlite::fromJSON(
    file,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}


# AC1 -------------------------------------------------------------------------

test_that("generate_docx_hitopsr(), generate_qualtrics_hitopsr(), and generate_redcap_hitopsr() write a module's descriptor beside the file", {
  module <- hitop_module("hitopsr", scales = FOUR_STEMS)
  display <- display_of(FOUR_STEMS)
  items <- items_of(display)
  # The domain must not be able to empty silently: a zero-length `items` would
  # make the field comparisons below vacuous and this test still green.
  expect_true(length(items) > 0)

  for (fn in available_generators()) {
    descriptor <- withr::local_tempfile(fileext = ".json")
    do.call(fn, list(
      file = withr::local_tempfile(fileext = GENERATORS[[fn]]),
      module = module,
      descriptor = descriptor
    ))

    parsed <- parse_descriptor(descriptor)
    # "1.0" as a literal, not module_format_version(): a version bump must be
    # visible here rather than tracking the writer's own output.
    expect_identical(parsed$format, "1.0", info = fn)
    expect_identical(parsed$instrument, "hitopsr", info = fn)
    expect_identical(parsed$scales, display, info = fn)
    expect_identical(as.integer(parsed$items), items, info = fn)
    expect_identical(as.integer(parsed$nItems), length(items), info = fn)

    expect_identical(read_module(descriptor), module, info = fn)
  }
})


# AC2 -------------------------------------------------------------------------

test_that("a generator call with no module writes a descriptor of the whole instrument", {
  full_items <- seq_len(405)
  expected <- score_hitopsr(sim_hitopsr, items = full_items, append = FALSE)

  for (fn in available_generators()) {
    descriptor <- withr::local_tempfile(fileext = ".json")
    do.call(fn, list(
      file = withr::local_tempfile(fileext = GENERATORS[[fn]]),
      descriptor = descriptor
    ))

    parsed <- parse_descriptor(descriptor)
    expect_identical(parsed$scales, sr_scales$Scale, info = fn)

    # The descriptor is not merely well-formed: scoring through it reproduces
    # the whole-instrument scoring it describes.
    module <- read_module(descriptor)
    expect_identical(
      score_hitopsr(
        sim_hitopsr,
        items = full_items,
        module = module,
        append = FALSE
      ),
      expected,
      info = fn
    )
  }
})


# AC3 -------------------------------------------------------------------------

test_that("a shuffled Word form's descriptor records the printed order in original HiTOP-SR numbers", {
  skip_if_no_docx()

  four <- hitop_module("hitopsr", scales = FOUR_STEMS)
  cases <- list(
    list(label = "module, renumbered", module = four, renumber = TRUE),
    list(label = "module, original numbering", module = four, renumber = FALSE),
    list(label = "whole instrument, renumbered", module = NULL, renumber = TRUE),
    list(
      label = "whole instrument, original numbering",
      module = NULL,
      renumber = FALSE
    )
  )

  for (case in cases) {
    descriptor <- withr::local_tempfile(fileext = ".json")
    set.seed(20260824)
    out <- generate_docx_hitopsr(
      file = withr::local_tempfile(fileext = ".docx"),
      module = case$module,
      renumber = case$renumber,
      randomize = TRUE,
      descriptor = descriptor
    )

    described <- read_module(descriptor)
    order <- attr(described, "item_order")
    covered <- if (is.null(case$module)) {
      as.integer(seq_len(405))
    } else {
      as.integer(case$module$items)
    }

    # Original instrument numbers, in printed order: a permutation of the items
    # the form covers, and the same order the generator returns.
    expect_identical(sort(order), covered, info = case$label)
    # The saved file and the returned attribute are the same computation, so
    # this says only that the sidecar preserved what the generator returned --
    # the contract the help pages state. Whether the order is RIGHT is settled
    # by the independent scoring round trip below.
    expect_identical(order, attr(out, "item_order"), info = case$label)
    # Discriminating control for the renumbered module form, where the printed
    # numbers are 1..8 and the original ones are not.
    if (!is.null(case$module)) {
      expect_false(
        identical(sort(order), seq_along(order)),
        info = case$label
      )
    }
    # The shuffle must actually have shuffled, or the reordering below would
    # pass on a form that was never out of order.
    expect_false(identical(order, covered), info = case$label)

    # Responses laid out in INSTRUMENT order, and the scores they earn. Both
    # are built by item number straight from `sim_hitopsr`, never through the
    # `itemOrder` under test.
    collected <- sim_hitopsr[sprintf("hsr_%d", covered)]
    # Items are addressed by POSITION on both sides: what the reordering has
    # to restore is the column order, and a name would let the scorer succeed
    # for a reason the printed form never supplies.
    expected <- score_hitopsr(
      collected,
      items = seq_along(collected),
      module = case$module,
      append = FALSE
    )

    # The same responses as they would be entered off the printed form: column
    # j holds the item printed in position j. The names are deliberately not
    # item names -- what is recovered is the position, not a label.
    printed <- collected[match(order, covered)]
    names(printed) <- sprintf("printed_%d", seq_along(printed))

    restored <- printed[order(order)]
    expect_identical(
      score_hitopsr(
        restored,
        items = seq_along(restored),
        module = case$module,
        append = FALSE
      ),
      expected,
      info = case$label
    )
  }
})


# AC4 -------------------------------------------------------------------------

test_that("a descriptor for an unshuffled form carries no printed order", {
  module <- hitop_module("hitopsr", scales = FOUR_STEMS)

  # The Word generator with randomize = FALSE, then the two online exports,
  # which never shuffle and have no `randomize` argument at all.
  calls <- list(
    docx = list(
      fn = "generate_docx_hitopsr",
      args = list(
        file = withr::local_tempfile(fileext = ".docx"),
        randomize = FALSE
      )
    ),
    qualtrics = list(
      fn = "generate_qualtrics_hitopsr",
      args = list(file = withr::local_tempfile(fileext = ".txt"))
    ),
    redcap = list(
      fn = "generate_redcap_hitopsr",
      args = list(file = withr::local_tempfile(fileext = ".zip"))
    )
  )

  runnable <- available_generators()
  for (label in names(calls)) {
    if (!calls[[label]]$fn %in% runnable) next
    descriptor <- withr::local_tempfile(fileext = ".json")
    args <- c(
      calls[[label]]$args,
      list(module = module, descriptor = descriptor)
    )
    do.call(calls[[label]]$fn, args)

    expect_null(parse_descriptor(descriptor)$itemOrder, info = label)
    expect_null(attr(read_module(descriptor), "item_order"), info = label)
  }
})


# AC5 -------------------------------------------------------------------------

test_that("every generator refuses a descriptor that is not a single path", {
  # Not a string, not NULL, and not length one: it fails the guard whichever
  # way the guard is written.
  bad <- list(list(1, 2), c("one.json", "two.json"), NA_character_)

  for (fn in available_generators()) {
    for (value in bad) {
      target <- withr::local_tempfile(fileext = GENERATORS[[fn]])
      err <- expect_error(
        do.call(fn, list(file = target, descriptor = value)),
        class = "rlang_error"
      )
      # Which argument was refused, not merely that something was refused.
      expect_match(conditionMessage(err), "descriptor", fixed = TRUE, info = fn)
    }
  }
})

test_that("an unwritable descriptor path is refused before any instrument file is written", {
  for (fn in available_generators()) {
    target <- withr::local_tempfile(fileext = GENERATORS[[fn]])
    # A directory that does not exist, so the descriptor cannot be opened.
    missing_dir <- file.path(
      withr::local_tempdir(),
      "no-such-directory",
      "module.json"
    )

    err <- expect_error(
      do.call(fn, list(file = target, descriptor = missing_dir)),
      class = "rlang_error"
    )
    expect_match(
      conditionMessage(err),
      "module.json",
      fixed = TRUE,
      info = fn
    )
    # The refusal came first: no form was left behind for a descriptor that
    # was never saved.
    expect_false(file.exists(target), info = fn)
  }
})

test_that("a descriptor is not left behind when the instrument file cannot be written", {
  for (fn in available_generators()) {
    descriptor <- file.path(withr::local_tempdir(), "module.json")
    # A target the builder cannot open, for the same reason: its directory
    # does not exist.
    target <- file.path(
      withr::local_tempdir(),
      "no-such-directory",
      paste0("form", GENERATORS[[fn]])
    )

    # suppressWarnings(): a failing builder may warn on the connection before
    # it aborts; the abort is what is being asserted.
    expect_error(
      suppressWarnings(
        do.call(fn, list(file = target, descriptor = descriptor))
      )
    )
    expect_false(file.exists(descriptor), info = fn)
  }
})


# Return repairs (2026-08-24 review findings) --------------------------------

test_that("a module that already carries a printed order does not stamp one on a form that was never shuffled", {
  # The state under test is exactly what read_module() returns from a shuffled
  # Word form's descriptor -- AC3 above asserts that attribute is there -- set
  # by hand so this regression covers the online generators on a machine with
  # no Word stack.
  module <- hitop_module("hitopsr", scales = FOUR_STEMS)
  attr(module, "item_order") <- rev(as.integer(module$items))

  extra <- list(
    generate_docx_hitopsr = list(randomize = FALSE),
    generate_qualtrics_hitopsr = list(),
    generate_redcap_hitopsr = list()
  )

  for (fn in available_generators()) {
    descriptor <- withr::local_tempfile(fileext = ".json")
    do.call(fn, c(
      list(
        file = withr::local_tempfile(fileext = GENERATORS[[fn]]),
        module = module,
        descriptor = descriptor
      ),
      extra[[fn]]
    ))

    expect_null(parse_descriptor(descriptor)$itemOrder, info = fn)
    expect_null(attr(read_module(descriptor), "item_order"), info = fn)
  }
})

test_that("a refused descriptor path blames the generator that was called", {
  for (fn in available_generators()) {
    missing_dir <- file.path(
      withr::local_tempdir(),
      "no-such-directory",
      "module.json"
    )
    err <- expect_error(
      do.call(fn, list(
        file = withr::local_tempfile(fileext = GENERATORS[[fn]]),
        descriptor = missing_dir
      )),
      class = "rlang_error"
    )
    # Which function is blamed, not merely that something aborted: the repo's
    # guards name the exported function the user called, never an internal
    # helper (tests/testthat/test-export-arg-guards.R).
    expect_identical(
      as.character(conditionCall(err)[[1]]),
      fn,
      info = fn
    )
  }
})

test_that("rollback removes the descriptor itself, not every path matching it", {
  for (fn in available_generators()) {
    dir <- withr::local_tempdir()
    # A file whose name a wildcard reading of the descriptor path would match.
    bystander <- file.path(dir, "modulea.json")
    writeLines("keep me", bystander)
    descriptor <- file.path(dir, "module[a].json")

    target <- file.path(dir, "no-such-directory", paste0("f", GENERATORS[[fn]]))
    expect_error(
      suppressWarnings(
        do.call(fn, list(file = target, descriptor = descriptor, module = NULL))
      )
    )

    expect_true(file.exists(bystander), info = fn)
    expect_false(file.exists(descriptor), info = fn)
  }
})


# Gate repairs (2026-08-24 review findings) -----------------------------------

test_that("every generator refuses an empty descriptor path", {
  # A length-one string, so the type guard admits it; the write then discards
  # the file silently. Refused before any instrument file is written.
  for (fn in available_generators()) {
    target <- withr::local_tempfile(fileext = GENERATORS[[fn]])
    err <- expect_error(
      do.call(fn, list(file = target, descriptor = "")),
      class = "rlang_error"
    )
    expect_match(conditionMessage(err), "descriptor", fixed = TRUE, info = fn)
    expect_false(file.exists(target), info = fn)
  }
})

test_that("every generator refuses a descriptor naming the instrument file", {
  # The descriptor is written first and the form second, so one path for both
  # leaves the form where the descriptor was, with no rollback and a success
  # message. Refused before either file is written.
  for (fn in available_generators()) {
    target <- file.path(withr::local_tempdir(), paste0("f", GENERATORS[[fn]]))
    err <- expect_error(
      do.call(fn, list(file = target, descriptor = target)),
      class = "rlang_error"
    )
    expect_match(conditionMessage(err), "descriptor", fixed = TRUE, info = fn)
    expect_false(file.exists(target), info = fn)
  }

  # The same path written two ways is still one path.
  for (fn in available_generators()) {
    dir <- withr::local_tempdir()
    target <- file.path(dir, paste0("f", GENERATORS[[fn]]))
    expect_error(
      do.call(fn, list(
        file = target,
        descriptor = file.path(dir, ".", paste0("f", GENERATORS[[fn]]))
      )),
      class = "rlang_error"
    )
    expect_false(file.exists(target), info = fn)
  }
})


# M065: announcing the descriptor -------------------------------------------

# Run one generator and return every message it emitted, in order.
#
# The cli options are pinned so the assertions below read the message text and
# not the terminal's rendering of it: an unpinned width wraps a long temporary
# path across two lines, and hyperlinking would replace the path with a label.
CLI_PLAIN <- list(
  cli.width = 10000L,
  cli.num_colors = 1L,
  cli.hyperlink = FALSE,
  cli.hyperlink_path = FALSE
)

capture_generator_messages <- function(fn, args) {
  withr::local_options(CLI_PLAIN)
  testthat::capture_messages(do.call(fn, args))
}

# The same, for a call expected to abort: the messages emitted on the way to
# the failure, plus the condition itself, so a test can say WHICH failure it
# saw rather than that something went wrong. A failing builder may warn on its
# connection before aborting; those warnings are muffled because the abort is
# the event under test.
capture_generator_failure <- function(fn, args) {
  withr::local_options(CLI_PLAIN)
  msgs <- character()
  err <- NULL
  withCallingHandlers(
    tryCatch(do.call(fn, args), error = function(e) err <<- e),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning")
  )
  list(messages = msgs, error = err)
}

# The three ways a call carrying a `descriptor` can fail, as (file, descriptor)
# pairs built under one temporary directory, each with the failure it must be.
# The first two are this package's own refusals and are named by condition
# class and message. The third is the builder's, whose message differs by
# generator and by platform; it is identified instead as a failure that does
# NOT name the descriptor -- the sidecar was written and rolled back, which the
# `control` flag then confirms by writing the same descriptor beside a target
# the builder can open.
descriptor_failure_cases <- function(ext) {
  list(
    list(
      label = "descriptor path refused before any write",
      control = FALSE,
      make = function(dir) {
        list(
          file = file.path(dir, paste0("form", ext)),
          descriptor = file.path(dir, "no-such-directory", "module.json")
        )
      },
      verify = function(err, paths, info) {
        testthat::expect_s3_class(err, "rlang_error")
        testthat::expect_match(
          conditionMessage(err),
          paths$descriptor,
          fixed = TRUE,
          info = info
        )
      }
    ),
    list(
      label = "descriptor and instrument paths collide",
      control = FALSE,
      make = function(dir) {
        both <- file.path(dir, paste0("form", ext))
        list(file = both, descriptor = both)
      },
      verify = function(err, paths, info) {
        testthat::expect_s3_class(err, "rlang_error")
        testthat::expect_match(
          conditionMessage(err),
          "different",
          fixed = TRUE,
          info = info
        )
      }
    ),
    list(
      label = "instrument write fails after the sidecar was written",
      control = TRUE,
      make = function(dir) {
        list(
          file = file.path(dir, "no-such-directory", paste0("form", ext)),
          descriptor = file.path(dir, "module.json")
        )
      },
      verify = function(err, paths, info) {
        testthat::expect_s3_class(err, "error")
        # Not the descriptor's own refusal: that failure names the descriptor
        # path, and this one must not, or the case would never have reached
        # the builder at all.
        testthat::expect_false(
          grepl(paths$descriptor, conditionMessage(err), fixed = TRUE),
          info = info
        )
      }
    )
  )
}


# AC1 -------------------------------------------------------------------------

test_that("every generator announces the descriptor it wrote, after the instrument file's own message", {
  module <- hitop_module("hitopsr", scales = FOUR_STEMS)

  for (fn in available_generators()) {
    target <- withr::local_tempfile(fileext = GENERATORS[[fn]])
    descriptor <- withr::local_tempfile(fileext = ".json")

    msgs <- capture_generator_messages(
      fn,
      list(file = target, module = module, descriptor = descriptor)
    )

    # The announcement names the descriptor's own path, so it is found by that
    # path and not by matching the word "descriptor" somewhere in the output.
    said <- grep(descriptor, msgs, fixed = TRUE)
    expect_length(said, 1L)
    expect_match(msgs[[said]], "descriptor", ignore.case = TRUE, info = fn)

    # After the instrument file's message, not before it: a reader following
    # the console top to bottom meets the form first and its sidecar second.
    instrument <- grep(target, msgs, fixed = TRUE)
    expect_length(instrument, 1L)
    expect_gt(said, instrument)

    # The announcement is true: the file it names is on disk.
    expect_true(file.exists(descriptor), info = fn)
  }
})

test_that("a generator called with no descriptor says nothing about one", {
  # The silent control. Without it the assertions above would pass on a
  # generator that announced a descriptor on every call.
  module <- hitop_module("hitopsr", scales = FOUR_STEMS)

  for (fn in available_generators()) {
    target <- withr::local_tempfile(fileext = GENERATORS[[fn]])
    msgs <- capture_generator_messages(
      fn,
      list(file = target, module = module)
    )

    # The instrument's own message is still there -- so this is silence about
    # the descriptor, not silence about everything.
    expect_length(grep(target, msgs, fixed = TRUE), 1L)
    expect_false(any(grepl("descriptor", msgs, ignore.case = TRUE)), info = fn)
  }
})


# AC2 -------------------------------------------------------------------------

test_that("no descriptor is announced on any of the three failure forms", {
  for (fn in available_generators()) {
    for (case in descriptor_failure_cases(GENERATORS[[fn]])) {
      dir <- withr::local_tempdir()
      paths <- case$make(dir)
      label <- paste(fn, case$label, sep = ": ")

      seen <- capture_generator_failure(fn, c(paths, list(module = NULL)))

      # The call failed, and failed the way this case is about: a green
      # assertion below must not be the silence of a call that succeeded, nor
      # of one that failed somewhere else.
      case$verify(seen$error, paths, label)

      expect_false(
        any(grepl("descriptor", seen$messages, ignore.case = TRUE)),
        info = label
      )
      expect_false(file.exists(paths$descriptor), info = label)

      # For the rollback case: the same descriptor path beside a target the
      # builder can open does get written and announced. Without this the case
      # would be indistinguishable from one that never reached the sidecar.
      if (isTRUE(case$control)) {
        ok <- capture_generator_messages(fn, list(
          file = file.path(dir, paste0("form", GENERATORS[[fn]])),
          descriptor = paths$descriptor,
          module = NULL
        ))
        expect_true(file.exists(paths$descriptor), info = label)
        expect_length(grep(paths$descriptor, ok, fixed = TRUE), 1L)
      }
    }
  }
})
