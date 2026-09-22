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
    items = expected_items(display),
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
      parsed$items,
      expected_items(display),
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
  expect_identical(m$items, expected_items(display))
  expect_identical(m$reverse, expected_reverse(display))
  expect_identical(m$nItems, length(expected_items(display)))
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

test_that("write_module() accepts a valid item_order on a module whose items are doubles", {
  # A `hitop_module` saved to `.rds` before item numbers became integers. The
  # usability check compares the order against the module's items, so comparing
  # with identical() would refuse this module on storage type alone -- which is
  # what it did while the check read `identical(sort(as.integer(item_order)),
  # module$items)`.
  m <- hitop_module("hitopsr", scales = c("agoraphobia", "appetiteLoss"))
  m$items <- as.double(m$items)
  attr(m, "item_order") <- rev(m$items)
  f <- withr::local_tempfile(fileext = ".json")

  expect_no_error(write_module(m, f))
  expect_identical(
    jsonlite::fromJSON(f, simplifyVector = TRUE)$itemOrder,
    rev(expected_items(c("Agoraphobia", "Appetite Loss")))
  )
})

test_that("write_module() still refuses an item_order that is not a permutation", {
  withr::local_options(cli.width = 10000)
  m <- hitop_module("hitopsr", scales = c("agoraphobia", "appetiteLoss"))

  # Three ways to not be a permutation: a substituted number, a repeat, and a
  # short order. Each must abort whatever the module's storage type, so the
  # module here carries the doubles the test above admits.
  m$items <- as.double(m$items)
  for (order in list(
    c(999, rev(m$items)[-1L]),
    c(m$items[[1L]], m$items[-1L][-1L], m$items[[1L]]),
    m$items[-1L]
  )) {
    bad <- m
    attr(bad, "item_order") <- order
    f <- withr::local_tempfile(fileext = ".json")
    e <- expect_error(write_module(bad, f))
    expect_match(conditionMessage(e), "unusable", fixed = TRUE)
    expect_false(file.exists(f))
  }
})


# AC7 ------------------------------------------------------------------------

test_that("a module read from a file scores and estimates reliability identically to the in-memory one", {
  stems <- av$camelCase[c(3L, 17L, 41L, 68L)]
  m <- hitop_module("hitopsr", scales = stems)
  f <- withr::local_tempfile(fileext = ".json")
  write_module(m, f)

  collected <- sim_hitopsr[sprintf("hsr_%03d", m$items)]

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

# The number fields are type-checked value by value. The probes use Social
# Aloofness because it covers item 1, so a JSON `true` in place of that item
# coerces to the right number and a check after coercion cannot see it.
aloof_items <- function() {
  expected_items("Social Aloofness")
}

# A descriptor for Social Aloofness with the three number fields given as raw
# JSON text. A NULL field is left out of the file.
aloof_descriptor <- function(items = NULL, nItems = NULL, itemOrder = NULL,
                             envir = parent.frame()) {
  fields <- c(
    '"format":"1.0"',
    '"instrument":"hitopsr"',
    '"scales":["Social Aloofness"]',
    if (!is.null(items)) paste0('"items":', items),
    if (!is.null(nItems)) paste0('"nItems":', nItems),
    if (!is.null(itemOrder)) paste0('"itemOrder":', itemOrder)
  )
  f <- withr::local_tempfile(fileext = ".json", .local_envir = envir)
  writeLines(paste0("{", paste(fields, collapse = ","), "}"), f)
  f
}

# The module's items as a JSON array, with element `i` replaced by `value`.
aloof_array <- function(i = NULL, value = NULL, x = aloof_items()) {
  x <- as.character(x)
  if (!is.null(i)) x[[i]] <- value
  paste0("[", paste(x, collapse = ","), "]")
}

expect_unreadable <- function(f, field, class) {
  e <- expect_error(read_module(f), class = class)
  expect_true(grepl(f, conditionMessage(e), fixed = TRUE))
  expect_true(grepl(
    paste0("unreadable ", field), conditionMessage(e), fixed = TRUE
  ))
}

test_that("read_module() refuses a string, boolean or fraction as a whole number field", {
  withr::local_options(cli.width = 10000)
  n <- length(aloof_items())

  # A string holding the right count and a fraction that truncates to it both
  # read as valid before the fields were type-checked.
  whole <- list(
    items = c('"1"', "true", "1.5"),
    nItems = c(sprintf('"%d"', n), "true", sprintf("%d.4", n)),
    itemOrder = c('"1"', "true", "1.5")
  )
  mismatch <- "hitop_module_file_items_mismatch"
  classes <- list(
    items = mismatch, nItems = mismatch,
    itemOrder = "hitop_module_file_bad_item_order"
  )
  for (field in names(whole)) {
    for (value in whole[[field]]) {
      args <- list(items = aloof_array())
      args[[field]] <- value
      f <- do.call(aloof_descriptor, args)
      expect_unreadable(f, field, classes[[field]])
    }
  }
})

test_that("read_module() refuses a string, boolean, null or fraction inside a number array", {
  withr::local_options(cli.width = 10000)
  x <- aloof_items()

  # Each probe names the right items: the number as a string, `true` for
  # item 1, and the number plus 0.4. All three read as valid before the fields
  # were type-checked. A `null` element was already refused.
  element <- list(
    list(i = 2L, value = sprintf('"%d"', x[[2L]])),
    list(i = 1L, value = "true"),
    list(i = 2L, value = "null"),
    list(i = 2L, value = sprintf("%d.4", x[[2L]]))
  )
  for (probe in element) {
    bad <- aloof_array(probe$i, probe$value)
    expect_unreadable(
      aloof_descriptor(items = bad), "items",
      "hitop_module_file_items_mismatch"
    )
    expect_unreadable(
      aloof_descriptor(items = aloof_array(), itemOrder = bad), "itemOrder",
      "hitop_module_file_bad_item_order"
    )
  }
})

test_that("read_module() reads a JSON null number field as absent", {
  f <- aloof_descriptor(items = "null", nItems = "null", itemOrder = "null")
  m <- read_module(f)
  expect_identical(m, hitop_module("hitopsr", scales = "Social Aloofness"))
  expect_null(attr(m, "item_order"))
})

test_that("read_module() accepts whole numbers written as 2.0 or 3e0", {
  x <- aloof_items()
  n <- length(x)
  built <- hitop_module("hitopsr", scales = "Social Aloofness")

  for (form in c("%d.0", "%de0")) {
    written <- paste0("[", paste(sprintf(form, x), collapse = ","), "]")
    reversed <- paste0("[", paste(sprintf(form, rev(x)), collapse = ","), "]")
    f <- aloof_descriptor(
      items = written, nItems = sprintf(form, n), itemOrder = reversed
    )
    m <- read_module(f)
    expect_identical(attr(m, "item_order"), rev(x))
    attr(m, "item_order") <- NULL
    expect_identical(m, built)
  }
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

# Plant `mangle` in a two-scale module and assert that write_module() refuses
# it with a {cli} error before touching the path: first with nothing at the
# path, then over an existing file that must keep every byte. `field` is the
# field the refusal must name, or NULL for a module that cannot be rebuilt.
expect_write_refused <- function(mangle, field) {
  m <- hitop_module("hitopsr", scales = c("agoraphobia", "appetiteLoss"))
  bad <- mangle(m)

  absent <- withr::local_tempfile(fileext = ".json")
  e <- expect_error(write_module(bad, absent), class = "rlang_error")
  expect_false(file.exists(absent))
  if (is.null(field)) {
    expect_true(grepl("Cannot rebuild", conditionMessage(e), fixed = TRUE))
    expect_s3_class(e$parent, "rlang_error")
  } else {
    expect_true(grepl(
      paste0("Its ", field, " field"), conditionMessage(e), fixed = TRUE
    ))
  }

  existing <- withr::local_tempfile(fileext = ".json")
  write_module(m, existing)
  before <- readBin(existing, what = "raw", n = file.size(existing))
  expect_error(write_module(bad, existing), class = "rlang_error")
  expect_identical(
    readBin(existing, what = "raw", n = file.size(existing)), before
  )
}

test_that("write_module() refuses a module whose items are not the ones its scales cover", {
  withr::local_options(cli.width = 10000)

  location <- list(
    dropped = function(m) { m$items <- m$items[-1L]; m },
    added = function(m) { m$items <- c(m$items, 999L); m },
    swapped = function(m) { m$items[1:2] <- m$items[2:1]; m },
    substituted = function(m) { m$items[[1L]] <- 999L; m },
    repeated = function(m) { m$items[[2L]] <- m$items[[1L]]; m }
  )
  form <- list(
    na = function(m) { m$items[[1L]] <- NA_integer_; m },
    removed = function(m) { m$items <- NULL; m },
    list = function(m) { m$items <- as.list(m$items); m },
    character = function(m) { m$items <- as.character(m$items); m }
  )
  for (mangle in c(location, form)) {
    expect_write_refused(mangle, "items")
  }
})

test_that("write_module() refuses a module whose nItems is not the count its scales cover", {
  withr::local_options(cli.width = 10000)

  form <- list(
    removed = function(m) { m$nItems <- NULL; m },
    na = function(m) { m$nItems <- NA_integer_; m },
    fraction = function(m) { m$nItems <- m$nItems + 0.5; m },
    length_two = function(m) { m$nItems <- c(m$nItems, m$nItems); m },
    wrong = function(m) { m$nItems <- m$nItems + 1L; m }
  )
  for (mangle in form) {
    expect_write_refused(mangle, "nItems")
  }
})

test_that("write_module() refuses a module its scales cannot rebuild, with the rebuild's error as parent", {
  withr::local_options(cli.width = 10000)

  expect_write_refused(function(m) { m$scales <- "Not A Scale"; m }, NULL)
  expect_write_refused(function(m) { m$instrument <- "pid5"; m }, NULL)
})

test_that("write_module() writes a module whose items are doubles equal to the rebuild's", {
  # The passing control for the refusals above: a module saved before item
  # numbers became integers differs from the rebuild in storage type only.
  m <- hitop_module("hitopsr", scales = c("agoraphobia", "appetiteLoss"))
  m$items <- as.double(m$items)
  m$nItems <- as.double(m$nItems)
  f <- withr::local_tempfile(fileext = ".json")

  expect_no_error(write_module(m, f))
  expect_identical(
    jsonlite::fromJSON(f, simplifyVector = TRUE)$items,
    expected_items(c("Agoraphobia", "Appetite Loss"))
  )
})

test_that("a file written from hitop_subset() reads back as the hitop_module() build", {
  display <- c("Agoraphobia", "Appetite Loss")
  withCallingHandlers(
    old <- hitop_subset("hitopsr", scales = display),
    hitop_deprecated_subset = function(cnd) {
      rlang::cnd_muffle(cnd)
    }
  )
  expect_s3_class(old, "hitop_subset")
  f <- withr::local_tempfile(fileext = ".json")
  write_module(old, f)

  back <- read_module(f)
  expect_identical(back, hitop_module("hitopsr", scales = display))
  expect_identical(class(back), "hitop_module")
  expect_type(back$items, "integer")
})

test_that("a file written from a module with double items reads back as the hitop_module() build", {
  display <- c("Agoraphobia", "Appetite Loss")
  m <- hitop_module("hitopsr", scales = display)
  m$items <- as.double(m$items)
  f <- withr::local_tempfile(fileext = ".json")
  write_module(m, f)

  back <- read_module(f)
  expect_identical(back, hitop_module("hitopsr", scales = display))
  expect_type(back$items, "integer")
})

test_that("write_module() ends every line with LF and writes no CR byte", {
  # A text-mode connection writes CRLF on Windows. Off Windows this test cannot
  # go red, so the windows-latest CI job is its proof.
  m <- hitop_module("hitopsr", scales = c("agoraphobia", "appetiteLoss"))
  f <- withr::local_tempfile(fileext = ".json")
  write_module(m, f)

  bytes <- readBin(f, what = "raw", n = file.size(f))
  expect_gt(sum(bytes == as.raw(0x0A)), 0L)
  expect_identical(sum(bytes == as.raw(0x0D)), 0L)
})

test_that("write_module() refuses an empty path rather than discarding the file", {
  # `writeLines(json, con = "")` opens an anonymous connection and throws the
  # contents away, so an empty path used to return quietly having written
  # nothing. The refusal is what stops a caller believing a descriptor was
  # saved when none exists.
  module <- hitop_module("hitopsr", scales = "agoraphobia")
  expect_error(write_module(module, ""), class = "rlang_error")
  expect_match(
    conditionMessage(expect_error(write_module(module, ""))),
    "empty string",
    fixed = TRUE
  )
})
