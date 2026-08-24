# Oracle note: every expected value in this file is derived from the shipped
# tables, never read off the object under test. Item membership and the reverse
# key come from `hitopsr_items` -- the ITEM-level table -- rather than from
# `hitopsr_scales$itemNumbers`, which is the table `hitop_module()` itself
# reads; a divergence between the two therefore surfaces here instead of
# cancelling out. Scale display names and camelCase stems come from
# `available_scales("hitopsr")`, which is the shipped scale table.

av <- available_scales("hitopsr")

# The item numbers a set of display scale names covers, ascending. `hitopsr_items`
# is stored in HSR order, so the subset is already ascending.
expected_items <- function(display) {
  hitopsr_items$HSR[hitopsr_items$Scale %in% display]
}

expected_reverse <- function(display) {
  hitopsr_items$Reverse[hitopsr_items$Scale %in% display]
}

# The display names for a set of camelCase stems, in scale-table row order --
# the order `hitop_module()` returns them in.
expected_display <- function(stems) {
  av$Scale[sort(match(stems, av$camelCase))]
}

# A minimal descriptor written straight to JSON, bypassing write_module(): the
# only way to test what happens to a file the package did not write.
descriptor_file <- function(x, envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  jsonlite::write_json(x, f, auto_unbox = TRUE)
  f
}

# A well-formed descriptor for two scales, as a plain list to be mangled.
base_descriptor <- function() {
  display <- c("Agoraphobia", "Appetite Loss")
  list(
    format = "1.0",
    instrument = "hitopsr",
    scales = display,
    items = as.integer(expected_items(display)),
    nItems = length(expected_items(display))
  )
}


# AC1 ------------------------------------------------------------------------

test_that("write_module() records the package's own tables and read_module() returns the module unchanged", {
  # Every scale on its own, then all of them at once, then four non-adjacent
  # rows. The set is enumerated from the scale table, so a scale added to the
  # instrument is covered without editing this test.
  stem_sets <- c(
    lapply(seq_len(nrow(av)), function(i) av$camelCase[[i]]),
    list(av$camelCase),
    list(av$camelCase[c(3L, 17L, 41L, 68L)])
  )

  for (stems in stem_sets) {
    label <- paste(stems, collapse = ", ")
    m <- hitop_module("hitopsr", scales = stems)
    f <- withr::local_tempfile(fileext = ".json")
    write_module(m, f)

    parsed <- jsonlite::fromJSON(f, simplifyVector = TRUE)
    display <- expected_display(stems)

    expect_identical(parsed$instrument, "hitopsr", info = label)
    expect_identical(parsed$scales, display, info = label)
    expect_identical(
      as.integer(parsed$items),
      as.integer(expected_items(display)),
      info = label
    )
    expect_identical(read_module(f), m, info = label)
  }
})


# AC2 ------------------------------------------------------------------------

test_that("a hand-written descriptor reads back into the module the tables describe", {
  # Written by hand, by no package function; see its `_provenance` field.
  m <- read_module(test_path("fixtures", "module-handwritten.json"))
  display <- c("Agoraphobia", "Binge Eating")

  expect_identical(m$instrument, "hitopsr")
  expect_identical(m$scales, display)
  expect_identical(m$camelCase, av$camelCase[sort(match(display, av$Scale))])
  expect_identical(as.integer(m$items), as.integer(expected_items(display)))
  expect_identical(m$reverse, expected_reverse(display))
  expect_identical(as.integer(m$nItems), length(expected_items(display)))
})


# AC3 ------------------------------------------------------------------------

test_that("read_module() rejects a descriptor whose recorded items disagree with the package", {
  withr::local_options(cli.width = 10000)

  wrong_value <- base_descriptor()
  wrong_value$items[[1L]] <- 999L
  f1 <- descriptor_file(wrong_value)
  e1 <- expect_error(read_module(f1), class = "hitop_module_file_items_mismatch")
  expect_true(grepl(f1, conditionMessage(e1), fixed = TRUE))
  expect_true(grepl("items", conditionMessage(e1), fixed = TRUE))

  wrong_length <- base_descriptor()
  wrong_length$items <- wrong_length$items[-1L]
  wrong_length$nItems <- length(wrong_length$items)
  f2 <- descriptor_file(wrong_length)
  e2 <- expect_error(read_module(f2), class = "hitop_module_file_items_mismatch")
  expect_true(grepl(f2, conditionMessage(e2), fixed = TRUE))
  expect_true(grepl("items", conditionMessage(e2), fixed = TRUE))
})

test_that("read_module() rejects a descriptor whose nItems disagrees with its items", {
  withr::local_options(cli.width = 10000)

  d <- base_descriptor()
  d$nItems <- 99L
  f <- descriptor_file(d)
  e <- expect_error(read_module(f), class = "hitop_module_file_items_mismatch")
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
  expect_true(grepl("nItems", conditionMessage(e), fixed = TRUE))
})

test_that("where the recorded fields agree, the keying is the package's own", {
  f <- descriptor_file(base_descriptor())
  m <- read_module(f)
  reference <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))

  expect_identical(m$items, reference$items)
  expect_identical(m$reverse, reference$reverse)
})


# AC4 ------------------------------------------------------------------------

test_that("read_module() rejects a file that is not valid JSON", {
  withr::local_options(cli.width = 10000)

  f <- withr::local_tempfile(fileext = ".json")
  writeLines('{"format": "1.0", "instrument":', f)
  e <- expect_error(read_module(f), class = "hitop_module_file_invalid_json")
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
})

test_that("read_module() rejects a descriptor missing any one required field", {
  withr::local_options(cli.width = 10000)

  for (field in c("format", "instrument", "scales")) {
    d <- base_descriptor()
    d[[field]] <- NULL
    f <- descriptor_file(d)
    e <- expect_error(
      read_module(f),
      class = "hitop_module_file_missing_field",
      info = field
    )
    expect_true(grepl(f, conditionMessage(e), fixed = TRUE), info = field)
    expect_true(grepl(field, conditionMessage(e), fixed = TRUE), info = field)
  }
})

test_that("read_module() rejects an instrument the package does not support", {
  withr::local_options(cli.width = 10000)

  d <- base_descriptor()
  d$instrument <- "pid5"
  f <- descriptor_file(d)
  e <- expect_error(read_module(f), class = "hitop_module_file_unknown_scales")
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
  # The refusal the module builder itself raised is kept as the parent, so a
  # caller can still tell an unsupported instrument from an unknown scale.
  expect_s3_class(e$parent, "hitop_unsupported_instrument")
})

test_that("read_module() rejects a scale name the package does not recognize", {
  withr::local_options(cli.width = 10000)

  d <- base_descriptor()
  d$scales <- c("Agoraphobia", "Fear Of Statistics")
  f <- descriptor_file(d)
  e <- expect_error(read_module(f), class = "hitop_module_file_unknown_scales")
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
  expect_true(grepl("Fear Of Statistics", conditionMessage(e$parent), fixed = TRUE))
})


# AC5 ------------------------------------------------------------------------

test_that("write_module() stamps the format version this release writes", {
  m <- hitop_module("hitopsr", scales = "agoraphobia")
  f <- withr::local_tempfile(fileext = ".json")
  write_module(m, f)

  # Asserted against the literal, not against anything the package computed:
  # a change to the version string must break this test.
  expect_identical(jsonlite::fromJSON(f, simplifyVector = TRUE)$format, "1.0")
})

test_that("read_module() refuses a format newer than this release writes", {
  withr::local_options(cli.width = 10000)

  for (version in c("1.1", "2.0")) {
    d <- base_descriptor()
    d$format <- version
    f <- descriptor_file(d)
    e <- expect_error(
      read_module(f),
      class = "hitop_module_file_unsupported_format",
      info = version
    )
    expect_true(grepl(f, conditionMessage(e), fixed = TRUE), info = version)
  }
})

test_that("read_module() refuses a format that is not a version string", {
  withr::local_options(cli.width = 10000)

  d <- base_descriptor()
  d$format <- 42L
  f <- descriptor_file(d)
  e <- expect_error(read_module(f), class = "hitop_module_file_unsupported_format")
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
})


# AC6 ------------------------------------------------------------------------

test_that("read_module() returns a recorded itemOrder on the item_order attribute", {
  d <- base_descriptor()
  d$itemOrder <- rev(d$items)
  f <- descriptor_file(d)
  m <- read_module(f)

  # The attribute name is the one generate_docx_hitopsr() returns for a
  # shuffled form, so a printed order reaches scoring by one route.
  expect_identical(attr(m, "item_order"), as.integer(rev(d$items)))
  expect_identical(
    unclass(m)[c("instrument", "scales", "camelCase", "items", "reverse", "nItems")],
    unclass(hitop_module("hitopsr", scales = d$scales))[
      c("instrument", "scales", "camelCase", "items", "reverse", "nItems")
    ]
  )
})

test_that("read_module() rejects an itemOrder that is not a permutation of items", {
  withr::local_options(cli.width = 10000)

  substituted <- base_descriptor()
  substituted$itemOrder <- c(999L, rev(substituted$items)[-1L])
  f1 <- descriptor_file(substituted)
  e1 <- expect_error(read_module(f1), class = "hitop_module_file_bad_item_order")
  expect_true(grepl(f1, conditionMessage(e1), fixed = TRUE))

  short <- base_descriptor()
  short$itemOrder <- short$items[-1L]
  f2 <- descriptor_file(short)
  expect_error(read_module(f2), class = "hitop_module_file_bad_item_order")
})

test_that("write_module() writes no itemOrder, a module object carrying no printed order", {
  m <- hitop_module("hitopsr", scales = c("agoraphobia", "appetiteLoss"))
  f <- withr::local_tempfile(fileext = ".json")
  write_module(m, f)

  expect_false("itemOrder" %in% names(jsonlite::fromJSON(f, simplifyVector = TRUE)))
  expect_null(attr(read_module(f), "item_order"))
})


# AC7 ------------------------------------------------------------------------

test_that("a module read from a file scores and estimates reliability identically to the in-memory one", {
  stems <- av$camelCase[c(3L, 17L, 41L, 68L)]
  m <- hitop_module("hitopsr", scales = stems)
  f <- withr::local_tempfile(fileext = ".json")
  write_module(m, f)

  collected <- sim_hitopsr[sprintf("hsr_%d", m$items)]

  expect_identical(
    suppressMessages(
      score_hitopsr(collected, items = names(collected), module = read_module(f))
    ),
    suppressMessages(
      score_hitopsr(collected, items = names(collected), module = m)
    )
  )

  # `omega = FALSE` keeps {lavaan}, a Suggests, out of the picture: the module
  # is resolved upstream of either coefficient.
  expect_identical(
    suppressMessages(
      reliability_hitopsr(
        collected,
        items = names(collected),
        module = read_module(f),
        omega = FALSE
      )
    ),
    suppressMessages(
      reliability_hitopsr(
        collected,
        items = names(collected),
        module = m,
        omega = FALSE
      )
    )
  )
})


# Review findings ------------------------------------------------------------
#
# Each block below pins a defect the M054 review found in `read_module()` and
# `write_module()`. None of AC1-AC9 fenced these cases, so each is asserted
# here directly rather than folded into a criterion's block above.

raw_descriptor <- function(txt, envir = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  writeLines(txt, f)
  f
}

# A well-formed descriptor as raw JSON text, so its fields can be replaced by
# shapes `jsonlite::write_json()` would never emit.
raw_head <- function() {
  '{"format":"1.0","instrument":"hitopsr","scales":["Agoraphobia","Appetite Loss"]'
}

test_that("read_module() rejects a JSON document whose top level is not an object", {
  withr::local_options(cli.width = 10000)

  # An array of objects parses to a *named* list under jsonlite's data-frame
  # simplification, so it walked past the shape guard and read as a module.
  f1 <- raw_descriptor(
    '[{"format":"1.0","instrument":"hitopsr","scales":["Agoraphobia"]}]'
  )
  e1 <- expect_error(read_module(f1), class = "hitop_module_file_invalid_json")
  expect_true(grepl(f1, conditionMessage(e1), fixed = TRUE))

  f2 <- raw_descriptor("[1, 2, 3]")
  expect_error(read_module(f2), class = "hitop_module_file_invalid_json")
})

test_that("read_module() rejects a number field that is not an array of numbers", {
  withr::local_options(cli.width = 10000)

  items <- paste(as.integer(expected_items(c("Agoraphobia", "Appetite Loss"))),
                 collapse = ",")

  # Ragged and nested arrays: the first parses to a list, which `as.integer()`
  # refuses with a bare simpleError; the second parsed to a matrix, which it
  # silently flattened.
  ragged <- raw_descriptor(paste0(raw_head(), ',"items":[[1,2],[3]]}'))
  e1 <- expect_error(read_module(ragged),
                     class = "hitop_module_file_items_mismatch")
  expect_true(grepl(ragged, conditionMessage(e1), fixed = TRUE))
  expect_true(grepl("items", conditionMessage(e1), fixed = TRUE))

  nested <- raw_descriptor(paste0(raw_head(), ',"items":[[1,2],[3,4]]}'))
  expect_error(read_module(nested), class = "hitop_module_file_items_mismatch")

  alpha <- raw_descriptor(paste0(raw_head(), ',"items":["a","b"]}'))
  expect_error(read_module(alpha), class = "hitop_module_file_items_mismatch")

  order <- raw_descriptor(
    sprintf('%s,"items":[%s],"itemOrder":[[1,2],[3]]}', raw_head(), items)
  )
  e2 <- expect_error(read_module(order),
                     class = "hitop_module_file_bad_item_order")
  expect_true(grepl("itemOrder", conditionMessage(e2), fixed = TRUE))
})

test_that("read_module() aborts on a path with no file, naming it", {
  withr::local_options(cli.width = 10000)

  f <- file.path(withr::local_tempdir(), "absent.json")
  e <- expect_error(read_module(f), class = "hitop_module_file_missing")
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
})

test_that("read_module() compares recorded items as a set, but rejects a repeat", {
  withr::local_options(cli.width = 10000)

  # The format states no order for `items`, so a hand-written descriptor
  # listing them any way round is a descriptor, not a defect.
  reversed <- base_descriptor()
  reversed$items <- rev(reversed$items)
  expect_identical(
    read_module(descriptor_file(reversed)),
    hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
  )

  repeated <- base_descriptor()
  repeated$items[[1L]] <- repeated$items[[2L]]
  e <- expect_error(read_module(descriptor_file(repeated)),
                    class = "hitop_module_file_items_mismatch")
  expect_true(grepl("Recorded more than once", conditionMessage(e), fixed = TRUE))
})

test_that("the nItems refusal names the scales it checked against, not an absent field", {
  withr::local_options(cli.width = 10000)

  # With no `items` in the file, the message used to report a count as coming
  # from an `items` field the file does not carry.
  d <- base_descriptor()
  d$items <- NULL
  d$nItems <- 99L
  e <- expect_error(read_module(descriptor_file(d)),
                    class = "hitop_module_file_items_mismatch")
  expect_true(grepl("nItems", conditionMessage(e), fixed = TRUE))
  expect_true(grepl("scales", conditionMessage(e), fixed = TRUE))
  expect_false(grepl("items field", conditionMessage(e), fixed = TRUE))
})

test_that("read_module() refuses a format below the first version of the format", {
  withr::local_options(cli.width = 10000)

  d <- base_descriptor()
  d$format <- "0.5"
  e <- expect_error(read_module(descriptor_file(d)),
                    class = "hitop_module_file_unsupported_format")
  expect_true(grepl("0.5", conditionMessage(e), fixed = TRUE))
})

test_that("write_module() names the file when it cannot be written", {
  withr::local_options(cli.width = 10000)

  m <- hitop_module("hitopsr", scales = "agoraphobia")
  f <- file.path(withr::local_tempdir(), "no-such-dir", "m.json")
  e <- expect_error(write_module(m, f))
  # The abort is this function's own, naming the file; `writeLines()`'s bare
  # "cannot open the connection" survives only as the parent, which is where a
  # caller should have to look for it.
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
  expect_true(
    grepl("Cannot write the module descriptor", conditionMessage(e), fixed = TRUE)
  )

  # The passing control: the same module to a writable path returns the path.
  good <- withr::local_tempfile(fileext = ".json")
  expect_identical(write_module(m, good), good)
})
