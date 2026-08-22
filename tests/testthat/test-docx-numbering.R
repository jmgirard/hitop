# Printed item numbering on HiTOP-SR Word forms (M46).
#
# A module form numbers its items 1..n down the page by default (D-036), and
# `randomize = TRUE` shuffles the printed order while keeping that numbering.
# The oracle throughout is `hitopsr_items` filtered by `Scale` -- never the
# module descriptor's own `items`, and never the generator's output read back
# as truth (IP2). `docx_item_rows()` (helper-generators.R) recovers the printed
# (number, text) pairs from the written file in document order.

# Romantic Disinterest is in the module on purpose: HSR 310 is the ONLY
# reverse-keyed item in all 405, so without that scale every `(R)` assertion
# below would pass vacuously.
m <- hitop_module(
  "hitopsr",
  c(
    "Agoraphobia",
    "Appetite Loss",
    "Perfectionism",
    "Romantic Disinterest",
    "Social Aloofness"
  )
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

  # The pinned exemplar: HSR 310 is the module's 20th item, and it is the only
  # one carrying a marker.
  expect_equal(which(want$HSR == 310L), 20L)
  expect_equal(sum(want$Reverse), 1L)
  expect_equal(sum(grepl("(R)", printed$items, fixed = TRUE)), 1L)
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

  # Stated in full, not spot-checked: `m` opens with HSR 1 and 2, so the two
  # numberings agree for two rows and diverge only from the third.
  expect_equal(
    rows$number,
    as.character(c(
      1, 2, 42, 45, 55, 66, 84, 86, 109, 118, 144, 152, 187, 195, 202,
      216, 260, 278, 291, 310, 338, 355, 389
    ))
  )
  expect_equal(rows$number, as.character(want$HSR))
  expect_equal(rows$text, want$Text)

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
