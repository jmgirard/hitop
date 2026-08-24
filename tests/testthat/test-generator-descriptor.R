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

skip_if_no_generators <- function() {
  skip_if_no_docx()
  testthat::skip_if_not_installed("zip")
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
  skip_if_no_generators()

  module <- hitop_module("hitopsr", scales = FOUR_STEMS)
  display <- display_of(FOUR_STEMS)
  items <- items_of(display)
  # The domain must not be able to empty silently: a zero-length `items` would
  # make the field comparisons below vacuous and this test still green.
  expect_true(length(items) > 0)

  for (fn in names(GENERATORS)) {
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
  skip_if_no_generators()

  full_items <- seq_len(405)
  expected <- score_hitopsr(sim_hitopsr, items = full_items, append = FALSE)

  for (fn in names(GENERATORS)) {
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
  skip_if_no_generators()

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

  for (label in names(calls)) {
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
  skip_if_no_generators()

  # Not a string, not NULL, and not length one: it fails the guard whichever
  # way the guard is written.
  bad <- list(list(1, 2), c("one.json", "two.json"), NA_character_)

  for (fn in names(GENERATORS)) {
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
  skip_if_no_generators()

  for (fn in names(GENERATORS)) {
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
  skip_if_no_generators()

  for (fn in names(GENERATORS)) {
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
