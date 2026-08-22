# Printed item numbering on HiTOP-SR Word forms (M46).
#
# A module form numbers its items 1..n down the page by default (D-036), and
# `randomize = TRUE` shuffles the printed order while keeping that numbering.
# The oracle throughout is `hitopsr_items` filtered by `Scale` -- never the
# module descriptor's own `items`, and never the generator's output read back
# as truth (IP2). `docx_item_rows()` (helper-generators.R) recovers the printed
# (number, text) pairs from the written file in document order.

m <- hitop_module(
  "hitopsr",
  c("Agoraphobia", "Appetite Loss", "Perfectionism", "Social Aloofness")
)

# The module's items, derived by filtering hitopsr_items$Scale, in ascending
# HSR order -- the independent expected set AC1 and AC2 are stated against.
expected_rows <- function(module) {
  rows <- hitopsr_items[hitopsr_items$Scale %in% module$scales, ]
  rows[order(rows$HSR), ]
}

# ---- T1: the row extractor, pinned to the committed full-instrument form ----

test_that("docx_item_rows() recovers the committed HiTOP-SR form's rows", {
  skip_if_no_docx()
  committed <- system.file("extdata", "hitopsr_US.docx", package = "hitop")
  skip_if(committed == "")

  rows <- docx_item_rows(committed)
  full <- hitopsr_items[order(hitopsr_items$HSR), ]

  expect_equal(nrow(rows), nrow(full))
  expect_equal(rows$number, as.character(full$HSR))
  expect_equal(rows$text, full$Text)
})

# ---- AC1: a module form numbers its items 1..n ------------------------------

test_that("a module Word form renumbers its items 1..n by default", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f, module = m))

  rows <- docx_item_rows(f)
  want <- expected_rows(m)

  expect_equal(rows$number, as.character(seq_len(m$nItems)))
  expect_equal(rows$text, want$Text)
})

# ---- AC2: the scoring page lists ranks, keeping the (R) markers -------------

test_that("the module scoring page lists each scale's items by their ranks", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f, module = m))

  printed <- docx_scoring_rows(f)
  want <- expected_rows(m)
  expect_setequal(printed$scale, m$scales)

  for (scale in m$scales) {
    rows <- want[want$Scale == scale, ]
    rank <- match(rows$HSR, want$HSR)
    label <- ifelse(rows$Reverse, paste0(rank, "(R)"), as.character(rank))
    expect_equal(
      printed$items[printed$scale == scale],
      paste(label, collapse = ", ")
    )
  }

  # At least one reverse-keyed item is in play, or the (R) clause is vacuous.
  expect_true(any(want$Reverse))
})

# ---- AC3: the full form is untouched, and renumber = FALSE opts out --------

test_that("a fresh default full-instrument form matches the committed one", {
  skip_if_no_docx()
  committed <- system.file("extdata", "hitopsr_US.docx", package = "hitop")
  skip_if(committed == "")

  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f))

  expect_equal(docx_item_rows(f), docx_item_rows(committed))
})

test_that("renumber = FALSE keeps a module form's original gapped numbers", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(
    generate_docx_hitopsr(file = f, module = m, renumber = FALSE)
  )

  rows <- docx_item_rows(f)
  want <- expected_rows(m)

  expect_equal(rows$number, as.character(want$HSR))
  expect_equal(rows$text, want$Text)
  # The gapped numbers are what makes this criterion falsifiable: an
  # implementation that renumbers unconditionally would print 1..n here.
  expect_false(identical(rows$number, as.character(seq_len(m$nItems))))

  printed <- docx_scoring_rows(f)
  for (scale in m$scales) {
    rows_s <- want[want$Scale == scale, ]
    label <- ifelse(
      rows_s$Reverse,
      paste0(rows_s$HSR, "(R)"),
      as.character(rows_s$HSR)
    )
    expect_equal(
      printed$items[printed$scale == scale],
      paste(label, collapse = ", ")
    )
  }
})
