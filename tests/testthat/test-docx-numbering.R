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
