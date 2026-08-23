# Printed item numbering on HiTOP-SR Word forms (M046).
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
  expect_equal(sum(grepl("[0-9]\\(R\\)", printed$items)), 1L)
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

# ---- AC4: randomize shuffles the printed order, keeping 1..n numbering ------

printed_texts <- function(...) {
  f <- withr::local_tempfile(fileext = ".docx", .local_envir = parent.frame())
  suppressMessages(generate_docx_hitopsr(file = f, ...))
  docx_item_rows(f)$text
}

test_that("randomize = TRUE keeps 1..n numbering and permutes the texts", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(1)
  suppressMessages(
    generate_docx_hitopsr(file = f, module = m, randomize = TRUE)
  )

  rows <- docx_item_rows(f)
  want <- expected_rows(m)

  expect_equal(rows$number, as.character(seq_len(m$nItems)))
  expect_setequal(rows$text, want$Text)
  expect_equal(sort(rows$text), sort(want$Text))
})

test_that("the shuffled order varies across seeds and repeats under one seed", {
  skip_if_no_docx()
  orders <- lapply(1:5, function(s) {
    withr::local_seed(s)
    printed_texts(module = m, randomize = TRUE)
  })
  expect_gte(length(unique(orders)), 2L)

  withr::local_seed(1)
  a <- printed_texts(module = m, randomize = TRUE)
  withr::local_seed(1)
  b <- printed_texts(module = m, randomize = TRUE)
  expect_identical(a, b)
})

test_that("randomize = TRUE with no module shuffles all 405 items", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(7)
  suppressMessages(generate_docx_hitopsr(file = f, randomize = TRUE))

  rows <- docx_item_rows(f)
  full <- hitopsr_items[order(hitopsr_items$HSR), ]

  expect_equal(rows$number, as.character(1:405))
  expect_equal(sort(rows$text), sort(full$Text))
  expect_false(identical(rows$text, full$Text))
})

# ---- AC5: the crosswalk, the item_order attribute, and the (R) marker ------

test_that("a shuffled form carries a crosswalk and an item_order attribute", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(3)
  out <- suppressMessages(
    generate_docx_hitopsr(file = f, module = m, randomize = TRUE)
  )

  order <- attr(out, "item_order")
  rows <- docx_item_rows(f)
  want <- expected_rows(m)

  # The attribute is the original HSR numbers in printed order: reading the
  # texts back through it must reproduce the page.
  expect_equal(sort(order), want$HSR)
  expect_equal(want$Text[match(order, want$HSR)], rows$text)

  # The printed crosswalk says the same thing as the attribute.
  cross <- docx_crosswalk_pairs(f)
  expect_equal(cross$new, seq_len(m$nItems))
  expect_equal(cross$original, order)

  # Each scale's printed numbers map back to exactly its original items.
  #
  # The printed numbers are READ OFF THE SCORING PAGE. Deriving them instead
  # as `match(want$HSR[want$Scale == scale], order)` reduces the assertion to
  # `order[match(v, order)] == v`, an identity that holds for any order at all
  # and constrains the generated document not at all -- the defect this
  # milestone's review returned on.
  scoring <- docx_scoring_rows(f)
  expect_setequal(scoring$scale, m$scales)
  for (scale in m$scales) {
    printed_nums <- as.integer(gsub(
      "\\(R\\)", "",
      strsplit(scoring$items[scoring$scale == scale], ", ")[[1]]
    ))
    expect_setequal(
      order[printed_nums],
      hitopsr_scales$itemNumbers[[which(hitopsr_scales$Scale == scale)]]
    )
    # And the row is sorted by printed number, not left in original order.
    expect_equal(printed_nums, sort(printed_nums))
  }

  # The (R) marker follows the SHUFFLED numbering, not the pre-shuffle rank.
  scoring <- docx_scoring_rows(f)
  marked <- grep("[0-9]+\\(R\\)", scoring$items, value = TRUE)
  expect_length(marked, 1L)
  expect_equal(
    as.integer(sub("^.*?([0-9]+)\\(R\\).*$", "\\1", marked)),
    match(310L, order)
  )
  expect_equal(sum(grepl("[0-9]\\(R\\)", scoring$items)), 1L)
})

test_that("the crosswalk does not depend on the scoring key being appended", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(3)
  out <- suppressMessages(generate_docx_hitopsr(
    file = f,
    module = m,
    randomize = TRUE,
    include_scoring = FALSE
  ))

  # `include_scoring = FALSE` asks for a participant-facing form with no key.
  # The crosswalk is not a key -- it names no scale and marks no reverse item
  # -- and a shuffled form is unscoreable without it, so it still prints.
  cross <- docx_crosswalk_pairs(f)
  expect_equal(nrow(cross), m$nItems)
  expect_equal(cross$original, attr(out, "item_order"))
  expect_equal(nrow(docx_scoring_rows(f)), 0L)
})

test_that("a shuffled full instrument prints no crosswalk", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(1)
  out <- suppressMessages(generate_docx_hitopsr(file = f, randomize = TRUE))

  # 405 pairs would be one dense paragraph on a participant-facing page, and
  # the IP1 sign-off covers module forms only; the caller reads the order off
  # the return value instead.
  expect_equal(nrow(docx_crosswalk_pairs(f)), 0L)
  expect_setequal(attr(out, "item_order"), hitopsr_items$HSR)
})

test_that("item_order is an integer vector", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  out <- suppressMessages(generate_docx_hitopsr(file = f))
  expect_identical(attr(out, "item_order"), 1:405)
})

test_that("an unshuffled form still reports its item_order and no crosswalk", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  out <- suppressMessages(generate_docx_hitopsr(file = f, module = m))

  expect_equal(attr(out, "item_order"), expected_rows(m)$HSR)
  expect_equal(nrow(docx_crosswalk_pairs(f)), 0L)
})

test_that("randomize with renumber = FALSE shuffles but keeps the numbers", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(4)
  out <- suppressMessages(generate_docx_hitopsr(
    file = f,
    module = m,
    randomize = TRUE,
    renumber = FALSE
  ))

  rows <- docx_item_rows(f)
  want <- expected_rows(m)

  # The printed numbers ARE the original ones, so they are their own
  # crosswalk and none is printed.
  expect_equal(as.integer(rows$number), attr(out, "item_order"))
  expect_setequal(as.integer(rows$number), want$HSR)
  expect_false(identical(as.integer(rows$number), want$HSR))
  expect_equal(nrow(docx_crosswalk_pairs(f)), 0L)
})

test_that("renumber and randomize reject non-flag values", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(generate_docx_hitopsr(file = f, renumber = "yes"), "renumber")
  expect_error(generate_docx_hitopsr(file = f, randomize = NA), "randomize")
  expect_error(generate_docx_hitopsr(file = f, renumber = c(TRUE, TRUE)),
               "renumber")
})

# ---- Subscale rows follow the same printed-order map -----------------------
#
# `include_subscales` cannot be combined with `module`, so this path is
# reachable only on the full instrument -- where renumbering is the identity
# and only `randomize` moves anything. The subscale rows are built from
# `hitopsr_subscales`, a different table from the one the scale rows come
# from, so they are the one place the printed-order map could be applied to
# one table and not the other.

test_that("shuffling remaps the subscale scoring rows too", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  withr::local_seed(11)
  out <- suppressMessages(generate_docx_hitopsr(
    file = f,
    include_subscales = TRUE,
    randomize = TRUE
  ))

  order <- attr(out, "item_order")
  printed <- docx_scoring_rows(f)
  sub_rows <- printed[grepl(" (Subscale)", printed$scale, fixed = TRUE), ]
  expect_gt(nrow(sub_rows), 0L)

  for (i in seq_len(nrow(sub_rows))) {
    name <- sub(" \\(Subscale\\)$", "", sub_rows$scale[i])
    want <- hitopsr_subscales$itemdata[[
      which(hitopsr_subscales$Subscale == name)
    ]]$HSR
    got <- as.integer(gsub("\\(R\\)", "", strsplit(sub_rows$items[i], ", ")[[1]]))
    # Each printed number is the position its original item landed in.
    expect_setequal(order[got], want)
    # And the row is sorted by printed number, not left in original order.
    expect_equal(got, sort(got))
  }
})
