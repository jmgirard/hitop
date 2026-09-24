# read_form_responses() reads the CSV files the hitop-form page saves.
#
# The page's own saved files are read from two places: the PID-5-SF, PID-5-BF,
# HiTOP-SR and HiTOP-BR files from fixtures/ (see fixtures/README.md), and the
# full PID-5 file and the shuffled module file from the package's installed
# examples, through `example_file()` (see inst/examples/README.md). The
# synthetic files below are written by `form_file()` in the shape the page
# writes: five lead columns, then item columns, one response row, CRLF row
# endings unless the test says otherwise. The result always carries three
# optional lead columns after `submitted`: `item_order` sixth,
# `prolific_study` seventh and `prolific_session` eighth, so the item columns
# of a result start at the ninth. A file may hold each of them anywhere after
# `submitted`, or not at all.

lead <- c("study", "participant", "instrument", "form_build", "submitted")
result_lead <- c(lead, "item_order", "prolific_study", "prolific_session")

# Write one response file as the page does. `items` is a named vector of
# responses (a name is the column, a value the response; NA writes an empty
# field). `item_order` is the text of an `item_order` cell, written sixth, or
# last when `order_last` is TRUE; NULL writes no such column. `prolific` is a
# named character vector of Prolific cells (`prolific_study`,
# `prolific_session`, either or both, in the order given), written directly
# after `item_order` when that is written sixth and after `submitted`
# otherwise, or after the item columns when `prolific_last` is TRUE; NULL
# writes no such column. Returns the path.
form_file <- function(dir, name, items, participant = "p001",
                      instrument = "hitopbr", eol = "\r\n",
                      study = "study", form_build = "2026-09-20",
                      submitted = "2026-09-20T21:20:36Z",
                      item_order = NULL, order_last = FALSE,
                      prolific = NULL, prolific_last = FALSE) {
  vals <- ifelse(is.na(items), "", as.character(items))
  cols <- names(items)
  if (!is.null(prolific)) {
    if (prolific_last) {
      cols <- c(cols, names(prolific))
      vals <- c(vals, unname(prolific))
    } else {
      cols <- c(names(prolific), cols)
      vals <- c(unname(prolific), vals)
    }
  }
  if (!is.null(item_order)) {
    if (order_last) {
      cols <- c(cols, "item_order")
      vals <- c(vals, item_order)
    } else {
      cols <- c("item_order", cols)
      vals <- c(item_order, vals)
    }
  }
  header <- paste(c(lead, cols), collapse = ",")
  row <- paste(c(study, participant, instrument, form_build, submitted, vals),
               collapse = ",")
  path <- file.path(dir, name)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(paste0(header, eol, row, eol)), con)
  path
}

two_items <- function(a = 4L, b = 1L) c(hitopbr_01 = a, hitopbr_02 = b)

fixture <- function(...) test_path("fixtures", ...)

# ---- AC1: shape, types and order ------------------------------------------

test_that("a directory reads every .csv, sorted, one row per file", {
  dir <- withr::local_tempdir()
  # Written out of sorted order on purpose: the rows must follow the sorted
  # paths, not the write order.
  form_file(dir, "b_p002.csv", two_items(2L, 3L), participant = "p002",
            submitted = "2026-09-21T09:02:11Z")
  form_file(dir, "a_p001.csv", two_items(4L, 1L), participant = "p001")
  writeLines("not a response file", file.path(dir, "notes.txt"))

  out <- read_form_responses(dir)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)
  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$participant, c("p001", "p002"))
  expect_identical(out$item_order, c(NA_character_, NA_character_))
  expect_type(out$study, "character")
  expect_type(out$participant, "character")
  expect_type(out$instrument, "character")
  expect_s3_class(out$form_build, "Date")
  expect_identical(out$form_build, as.Date(c("2026-09-20", "2026-09-20")))
  expect_s3_class(out$submitted, "POSIXct")
  expect_identical(attr(out$submitted, "tzone"), "UTC")
  expect_identical(
    out$submitted,
    as.POSIXct(c("2026-09-20 21:20:36", "2026-09-21 09:02:11"), tz = "UTC")
  )
  expect_type(out$hitopbr_01, "integer")
  expect_identical(out$hitopbr_01, c(4L, 2L))
  expect_identical(out$hitopbr_02, c(1L, 3L))
})

test_that("a vector of two files reads both, in sorted order", {
  dir <- withr::local_tempdir()
  f2 <- form_file(dir, "p002.csv", two_items(2L, 3L), participant = "p002")
  f1 <- form_file(dir, "p001.csv", two_items(4L, 1L), participant = "p001")

  out <- read_form_responses(c(f2, f1))

  expect_equal(nrow(out), 2L)
  expect_identical(out$participant, c("p001", "p002"))
  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
})

test_that("one file reads to one row", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items())

  out <- read_form_responses(f)

  expect_equal(nrow(out), 1L)
  expect_identical(out$participant, "p001")
  expect_identical(out$hitopbr_01, 4L)
})

test_that("paths sort in the C locale, so a capital letter sorts first", {
  dir <- withr::local_tempdir()
  # A locale-aware sort puts "alpha" before "Zed"; the C locale puts every
  # capital before every lower-case letter.
  form_file(dir, "alpha.csv", two_items(), participant = "alpha")
  form_file(dir, "Zed.csv", two_items(), participant = "Zed")

  out <- read_form_responses(dir)
  expect_identical(out$participant, c("Zed", "alpha"))
})

test_that("item columns keep the first file's order", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", c(hitopsr_233 = 4L, hitopsr_194 = 3L))
  form_file(dir, "p002.csv", c(hitopsr_233 = 1L, hitopsr_194 = 2L))

  out <- read_form_responses(dir)
  expect_identical(names(out)[-seq_len(8L)], c("hitopsr_233", "hitopsr_194"))
  expect_identical(out$hitopsr_233, c(4L, 1L))
})

test_that("a blank item reads as NA, and LF row endings read too", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", two_items(4L, NA), eol = "\n")

  out <- read_form_responses(dir)
  expect_identical(out$hitopbr_01, 4L)
  expect_identical(out$hitopbr_02, NA_integer_)
})

test_that("a UTF-8 byte-order mark before the header is ignored", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items())
  bytes <- readBin(f, "raw", file.size(f))
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), bytes), f)

  out <- read_form_responses(f)
  expect_identical(names(out)[1L], "study")
})

# ---- AC2: the two classed conditions ---------------------------------------

test_that("files whose item columns differ in name abort by class", {
  dir <- withr::local_tempdir()
  f1 <- form_file(dir, "p001.csv", c(hitopbr_01 = 4L, hitopbr_02 = 1L))
  f2 <- form_file(dir, "p002.csv", c(hitopbr_01 = 4L, hitopbr_03 = 1L))

  expect_error(
    read_form_responses(dir),
    class = "hitop_form_responses_mismatch"
  )
  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  msg <- conditionMessage(cnd)
  expect_match(msg, basename(f2), fixed = TRUE)
  expect_match(msg, "names", fixed = TRUE)
})

test_that("files whose item columns differ in count abort by class", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", c(hitopbr_01 = 4L, hitopbr_02 = 1L))
  f2 <- form_file(dir, "p002.csv", c(hitopbr_01 = 4L))

  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  expect_s3_class(cnd, "hitop_form_responses_mismatch")
  msg <- conditionMessage(cnd)
  expect_match(msg, basename(f2), fixed = TRUE)
  expect_match(msg, "count", fixed = TRUE)
})

test_that("files whose item columns differ in order abort by class", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", c(hitopbr_01 = 4L, hitopbr_02 = 1L))
  f2 <- form_file(dir, "p002.csv", c(hitopbr_02 = 1L, hitopbr_01 = 4L))

  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  expect_s3_class(cnd, "hitop_form_responses_mismatch")
  msg <- conditionMessage(cnd)
  expect_match(msg, basename(f2), fixed = TRUE)
  expect_match(msg, "order", fixed = TRUE)
})

test_that("the mismatch message names every differing file, not the first", {
  dir <- withr::local_tempdir()
  f1 <- form_file(dir, "p001.csv", c(hitopbr_01 = 4L, hitopbr_02 = 1L))
  form_file(dir, "p002.csv", c(hitopbr_01 = 4L, hitopbr_02 = 1L))
  f3 <- form_file(dir, "p003.csv", c(hitopbr_01 = 4L))
  f4 <- form_file(dir, "p004.csv", c(hitopbr_02 = 1L, hitopbr_01 = 4L))

  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  msg <- conditionMessage(cnd)
  expect_match(msg, basename(f3), fixed = TRUE)
  expect_match(msg, basename(f4), fixed = TRUE)
  expect_false(grepl("p002.csv", msg, fixed = TRUE))
})

test_that("the mismatch message gives each differing file its own reason", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", c(hitopbr_01 = 4L, hitopbr_02 = 1L))
  form_file(dir, "p003.csv", c(hitopbr_01 = 4L))
  form_file(dir, "p004.csv", c(hitopbr_02 = 1L, hitopbr_01 = 4L))

  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  # `body` holds the bullets one per element, before line wrapping.
  body <- cli::ansi_strip(cnd$body)
  line3 <- body[grepl("p003.csv", body, fixed = TRUE)]
  line4 <- body[grepl("p004.csv", body, fixed = TRUE)]
  expect_length(line3, 1L)
  expect_length(line4, 1L)
  expect_match(line3, "count", fixed = TRUE)
  expect_false(grepl("order", line3, fixed = TRUE))
  expect_match(line4, "order", fixed = TRUE)
  expect_false(grepl("count", line4, fixed = TRUE))
})

test_that("a directory holding no .csv aborts by class", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "notes.txt"))

  expect_error(read_form_responses(dir), class = "hitop_form_responses_none")

  empty <- withr::local_tempdir()
  expect_error(read_form_responses(empty),
               class = "hitop_form_responses_none")
})

test_that("a directory scan takes .CSV as well and skips a folder named .csv", {
  dir <- withr::local_tempdir()
  form_file(dir, "P001.CSV", two_items())
  dir.create(file.path(dir, "old.csv"))

  out <- read_form_responses(dir)
  expect_equal(nrow(out), 1L)
  expect_identical(out$participant, "p001")
})

test_that("a submitted stamp with fractional seconds reads, to the second", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(),
                 submitted = "2026-09-20T21:20:36.123Z")

  out <- read_form_responses(f)
  expect_s3_class(out$submitted, "POSIXct")
  expect_equal(as.numeric(out$submitted),
               as.numeric(as.POSIXct("2026-09-20 21:20:36", tz = "UTC")) + 0.123)
})

test_that("a file with no final row ending reads without a warning", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items())
  bytes <- readBin(f, "raw", file.size(f))
  writeBin(bytes[seq_len(length(bytes) - 2L)], f)

  expect_no_warning(out <- read_form_responses(f))
  expect_identical(out$hitopbr_02, 1L)
})

# ---- The optional `item_order` column --------------------------------------
#
# A page showing items in a random order writes a sixth lead column,
# `item_order`, holding the item numbers in the order shown, joined by single
# spaces. A store may append it after the item columns instead. The reader
# places it sixth either way, gives NA to rows from files without it, and
# leaves it out of the item-column comparison.

test_that("a file with item_order sixth reads it as a character sixth column", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L), item_order = "2 1")

  out <- read_form_responses(f)

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_type(out$item_order, "character")
  expect_identical(out$item_order, "2 1")
  expect_identical(out$hitopbr_01, 4L)
  expect_identical(out$hitopbr_02, 1L)
})

test_that("a file with item_order last, as a sheet appends it, reads the same", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L), item_order = "2 1",
                 order_last = TRUE)

  out <- read_form_responses(f)

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$item_order, "2 1")
  expect_identical(out$hitopbr_01, 4L)
  expect_identical(out$hitopbr_02, 1L)
})

test_that("a file without item_order reads it as NA", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L))

  out <- read_form_responses(f)

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$item_order, NA_character_)
})

test_that("a directory mixing files with and without item_order reads as one", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", two_items(4L, 1L), participant = "p001")
  form_file(dir, "p002.csv", two_items(2L, 3L), participant = "p002",
            item_order = "2 1")
  form_file(dir, "p003.csv", two_items(3L, 3L), participant = "p003",
            item_order = "1 2", order_last = TRUE)

  expect_no_error(out <- read_form_responses(dir))

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$participant, c("p001", "p002", "p003"))
  expect_identical(out$item_order, c(NA_character_, "2 1", "1 2"))
  expect_identical(out$hitopbr_01, c(4L, 2L, 3L))
  expect_identical(out$hitopbr_02, c(1L, 3L, 3L))
})

test_that("item_order does not enter the mismatch comparison, and both classes keep their triggers", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", two_items(), item_order = "2 1")
  f2 <- form_file(dir, "p002.csv", c(hitopbr_01 = 4L, hitopbr_03 = 1L),
                  item_order = "3 1")

  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  expect_s3_class(cnd, "hitop_form_responses_mismatch")
  expect_match(conditionMessage(cnd), basename(f2), fixed = TRUE)
  expect_match(conditionMessage(cnd), "names", fixed = TRUE)

  empty <- withr::local_tempdir()
  expect_error(read_form_responses(empty),
               class = "hitop_form_responses_none")
})

test_that("a multi-row file reads each row's item_order, a blank cell as NA", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "sheet.csv")
  writeLines(c(
    paste(c(lead, "hitopsr_233", "hitopsr_194", "item_order"), collapse = ","),
    "s,p1,hitopsr,2026-09-20,2026-09-20T21:20:36Z,4,1,194 233",
    "s,p2,hitopsr,2026-09-20,2026-09-20T21:20:36Z,2,3,",
    "s,p3,hitopsr,2026-09-20,2026-09-20T21:20:36Z,1,1,233 194"
  ), f)

  out <- read_form_responses(f)

  expect_identical(names(out), c(result_lead, "hitopsr_233", "hitopsr_194"))
  expect_identical(out$item_order, c("194 233", NA_character_, "233 194"))
  expect_identical(out$hitopsr_233, c(4L, 2L, 1L))
})

# A bad cell is an unclassed refusal naming the file and the response row.
expect_item_order_refused <- function(path, row = 1L) {
  cnd <- rlang::catch_cnd(read_form_responses(path), "error")
  expect_s3_class(cnd, "error")
  expect_false(inherits(cnd, "hitop_form_responses_mismatch"))
  expect_false(inherits(cnd, "hitop_form_responses_none"))
  msg <- conditionMessage(cnd)
  expect_match(msg, basename(path), fixed = TRUE)
  expect_match(msg, "item_order", fixed = TRUE)
  expect_match(msg, paste("row", row), fixed = TRUE)
  invisible(msg)
}

test_that("an item_order cell that is not the file's item numbers, each once, is refused", {
  dir <- withr::local_tempdir()
  cells <- c(
    missing = "1",
    repeated = "1 1",
    outside = "1 3",
    letter = "1 a",
    leading_zero = "01 2",
    double_space = "1  2",
    leading_space = " 1 2",
    trailing_space = "1 2 "
  )
  for (case in names(cells)) {
    f <- form_file(dir, paste0(case, ".csv"), two_items(),
                   item_order = cells[[case]])
    expect_item_order_refused(f)
  }
  # The control: the same items under a cell that lists them each once read.
  g <- form_file(dir, "good.csv", two_items(), item_order = "2 1")
  expect_identical(read_form_responses(g)$item_order, "2 1")
})

test_that("a blank item_order cell reads as NA, and a bad cell names its row", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "rows.csv")
  writeLines(c(
    paste(c(lead, "item_order", "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,,4,1",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,2 1,4,1",
    "s,p3,hitopbr,2026-09-20,2026-09-20T21:20:36Z,2 2,4,1"
  ), f)

  msg <- expect_item_order_refused(f, row = 3L)
  expect_false(grepl("row 1", msg, fixed = TRUE))
  expect_false(grepl("row 2", msg, fixed = TRUE))

  # Two bad rows name both, in the plural.
  h <- file.path(dir, "rows2.csv")
  writeLines(c(
    paste(c(lead, "item_order", "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,2 1,4,1",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,2 2,4,1",
    "s,p3,hitopbr,2026-09-20,2026-09-20T21:20:36Z,1,4,1"
  ), h)
  cnd <- rlang::catch_cnd(read_form_responses(h), "error")
  expect_match(conditionMessage(cnd), "rows 2 and 3", fixed = TRUE)

  g <- file.path(dir, "blank.csv")
  writeLines(c(
    paste(c(lead, "item_order", "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,,4,1"
  ), g)
  out <- read_form_responses(g)
  expect_identical(out$item_order, NA_character_)
  expect_identical(out$hitopbr_01, 4L)
})

# ---- The optional Prolific columns -----------------------------------------
#
# A page run for a study recruited through Prolific writes two lead columns,
# `prolific_study` and `prolific_session`, after `submitted` and after
# `item_order` when that column is present. A store may append them after the
# item columns instead. The reader places `prolific_study` seventh and
# `prolific_session` eighth either way, as character, with NA on a row from a
# file without the column and on a blank cell, and leaves both out of the
# item-column comparison.

prolific_pair <- c(prolific_study = "st01", prolific_session = "se01")

# The two files of every Prolific shape read to the same eight lead columns
# and the same items; only the two cells differ by shape.
expect_prolific <- function(out, study, session) {
  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_type(out$prolific_study, "character")
  expect_type(out$prolific_session, "character")
  expect_identical(out$prolific_study, study)
  expect_identical(out$prolific_session, session)
  expect_identical(out$hitopbr_01, 4L)
  expect_identical(out$hitopbr_02, 1L)
}

test_that("a file with prolific_study alone after submitted reads it seventh, prolific_session NA", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L),
                 prolific = prolific_pair["prolific_study"])
  # The column sits directly after `submitted` in the file.
  expect_identical(names(utils::read.csv(f))[6L], "prolific_study")

  out <- read_form_responses(f)
  expect_prolific(out, "st01", NA_character_)
  expect_identical(out$item_order, NA_character_)
})

test_that("a file with prolific_session alone after submitted reads it eighth, prolific_study NA", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L),
                 prolific = prolific_pair["prolific_session"])
  expect_identical(names(utils::read.csv(f))[6L], "prolific_session")

  out <- read_form_responses(f)
  expect_prolific(out, NA_character_, "se01")
  expect_identical(out$item_order, NA_character_)
})

test_that("the pair directly after submitted, with no item_order, reads seventh and eighth", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L), prolific = prolific_pair)
  expect_identical(names(utils::read.csv(f))[6:7],
                   c("prolific_study", "prolific_session"))

  out <- read_form_responses(f)
  expect_prolific(out, "st01", "se01")
  expect_identical(out$item_order, NA_character_)
})

test_that("the pair after item_order, as the page writes it under a random order, reads the same", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L), item_order = "2 1",
                 prolific = prolific_pair)
  expect_identical(names(utils::read.csv(f))[6:8],
                   c("item_order", "prolific_study", "prolific_session"))

  out <- read_form_responses(f)
  expect_prolific(out, "st01", "se01")
  expect_identical(out$item_order, "2 1")
})

test_that("the pair in reverse order in the file still reads study seventh and session eighth", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L),
                 prolific = rev(prolific_pair))
  expect_identical(names(utils::read.csv(f))[6:7],
                   c("prolific_session", "prolific_study"))

  out <- read_form_responses(f)
  expect_prolific(out, "st01", "se01")
})

test_that("the pair appended after the item columns, as a sheet does, reads the same", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L), prolific = prolific_pair,
                 prolific_last = TRUE)
  expect_identical(names(utils::read.csv(f))[8:9],
                   c("prolific_study", "prolific_session"))

  out <- read_form_responses(f)
  expect_prolific(out, "st01", "se01")
})

test_that("a file with neither Prolific column reads both as NA", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(4L, 1L))
  expect_false(any(c("prolific_study", "prolific_session") %in%
                     names(utils::read.csv(f))))

  out <- read_form_responses(f)
  expect_prolific(out, NA_character_, NA_character_)
})

test_that("a two-row store download reads each row's cells, a blank cell as NA", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "sheet.csv")
  writeLines(c(
    paste(c(lead, "prolific_study", "prolific_session", "hitopbr_01", "hitopbr_02"),
          collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,st01,se01,4,1",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,st01,,2,3"
  ), f)

  out <- read_form_responses(f)

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$prolific_study, c("st01", "st01"))
  expect_identical(out$prolific_session, c("se01", NA_character_))
  expect_identical(out$hitopbr_01, c(4L, 2L))

  # Three rows with `prolific_study` alone, one cell blank: the absent
  # column fills NA down every row, and the blank cell is NA in the other.
  g <- file.path(dir, "sheet2.csv")
  writeLines(c(
    paste(c(lead, "prolific_study", "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,st01,4,1",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,,2,3",
    "s,p3,hitopbr,2026-09-20,2026-09-20T21:20:36Z,st03,1,1"
  ), g)
  out <- read_form_responses(g)
  expect_identical(out$prolific_study, c("st01", NA_character_, "st03"))
  expect_identical(out$prolific_session, rep(NA_character_, 3L))
  expect_identical(out$hitopbr_02, c(1L, 3L, 1L))
})

test_that("a directory mixing a file with the pair and a file without reads as one, with no condition", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", two_items(4L, 1L), participant = "p001")
  form_file(dir, "p002.csv", two_items(2L, 3L), participant = "p002",
            prolific = prolific_pair)

  expect_no_condition(out <- read_form_responses(dir))

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$participant, c("p001", "p002"))
  expect_identical(out$prolific_study, c(NA_character_, "st01"))
  expect_identical(out$prolific_session, c(NA_character_, "se01"))
  expect_identical(out$hitopbr_01, c(4L, 2L))
})

test_that("a directory mixing the pair after item_order and the pair after the items reads as one, with no condition", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", two_items(4L, 1L), participant = "p001",
            item_order = "2 1", prolific = prolific_pair)
  form_file(dir, "p002.csv", two_items(2L, 3L), participant = "p002",
            item_order = "1 2", order_last = TRUE,
            prolific = c(prolific_study = "st02", prolific_session = "se02"),
            prolific_last = TRUE)

  expect_no_condition(out <- read_form_responses(dir))

  expect_identical(names(out), c(result_lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$item_order, c("2 1", "1 2"))
  expect_identical(out$prolific_study, c("st01", "st02"))
  expect_identical(out$prolific_session, c("se01", "se02"))
  expect_identical(out$hitopbr_02, c(1L, 3L))
})

test_that("files carrying the pair still refuse differing items by class, and the none class keeps its trigger", {
  dir <- withr::local_tempdir()
  form_file(dir, "p001.csv", two_items(), prolific = prolific_pair)
  f2 <- form_file(dir, "p002.csv", c(hitopbr_01 = 4L, hitopbr_03 = 1L),
                  prolific = prolific_pair)

  # The same Prolific cells on both files: the refusal is about the items.
  # That the pair itself never enters the comparison is shown by the two
  # mixed-directory tests above.
  cnd <- rlang::catch_cnd(read_form_responses(dir),
                          "hitop_form_responses_mismatch")
  expect_s3_class(cnd, "hitop_form_responses_mismatch")
  expect_match(conditionMessage(cnd), basename(f2), fixed = TRUE)
  expect_match(conditionMessage(cnd), "names", fixed = TRUE)
  expect_false(grepl("prolific", conditionMessage(cnd), fixed = TRUE))

  empty <- withr::local_tempdir()
  expect_error(read_form_responses(empty),
               class = "hitop_form_responses_none")
})

# ---- Plain refusals: not a response file, bad argument ---------------------

test_that("a missing path is an error naming it", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items())
  nowhere <- file.path(dir, "nowhere.csv")

  cnd <- rlang::catch_cnd(read_form_responses(c(f, nowhere)), "error")
  expect_false(inherits(cnd, "hitop_form_responses_mismatch"))
  expect_false(inherits(cnd, "hitop_form_responses_none"))
  expect_match(conditionMessage(cnd), "nowhere.csv", fixed = TRUE)
})

test_that("a non-character or NA path is an error", {
  expect_error(read_form_responses(1), "path")
  expect_error(read_form_responses(NA_character_), "path")
  expect_error(read_form_responses(character(0)), "path")
})

test_that("a CSV with other lead columns is refused by name", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "other.csv")
  writeLines(c("id,hitopbr_01", "p1,4"), f)

  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_false(inherits(cnd, "hitop_form_responses_mismatch"))
  expect_match(conditionMessage(cnd), "other.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "not a hitop-form response file",
               fixed = TRUE)
})

test_that("a header-only file is refused by name", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "empty.csv")
  writeLines(paste(c(lead, "hitopbr_01"), collapse = ","), f)

  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_false(inherits(cnd, "hitop_form_responses_mismatch"))
  expect_false(inherits(cnd, "hitop_form_responses_none"))
  expect_match(conditionMessage(cnd), "empty.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "no response row", fixed = TRUE)
})

test_that("a bad item value on a later row is refused by column", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "bad2.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,1",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,yes"
  ), f)

  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_match(conditionMessage(cnd), "bad2.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "hitopbr_02", fixed = TRUE)
  expect_match(conditionMessage(cnd), "whole number", fixed = TRUE)

  g <- file.path(dir, "stamp2.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4",
    "s,p2,hitopbr,2026-09-20,20/09/2026,3"
  ), g)
  cnd <- rlang::catch_cnd(read_form_responses(g), "error")
  expect_match(conditionMessage(cnd), "stamp2.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "submitted", fixed = TRUE)
  expect_match(conditionMessage(cnd), "20/09/2026", fixed = TRUE)

  h <- file.path(dir, "date2.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4",
    "s,p2,hitopbr,20/09/2026,2026-09-20T21:20:36Z,3"
  ), h)
  cnd <- rlang::catch_cnd(read_form_responses(h), "error")
  expect_match(conditionMessage(cnd), "date2.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "form_build", fixed = TRUE)
  expect_match(conditionMessage(cnd), "20/09/2026", fixed = TRUE)

  w <- file.path(dir, "wide2.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,99999999999"
  ), w)
  expect_no_warning(cnd <- rlang::catch_cnd(read_form_responses(w), "error"))
  expect_match(conditionMessage(cnd), "wide2.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "hitopbr_01", fixed = TRUE)
  expect_match(conditionMessage(cnd), "integer range", fixed = TRUE)
})

# ---- Multi-row files: a store's export -------------------------------------
#
# A Google Sheet or a Supabase table exports one file holding every
# participant's row. The files below are written from the HiTOP-BR fixture's
# one row, repeated with `participant` changed, so the expected values are the
# fixture's own.

# The fixture's header and data row, as text.
hitopbr_lines <- function() readLines(fixture("responses-hitopbr.csv"))

# The fixture's data row with `participant` (the second field) replaced.
with_participant <- function(row, participant) {
  fields <- strsplit(row, ",", fixed = TRUE)[[1]]
  fields[2] <- participant
  paste(fields, collapse = ",")
}

# Write `lines` to `path` with the given row ending, with or without one
# after the last line.
write_rows <- function(path, lines, eol = "\r\n", final = TRUE) {
  text <- paste(lines, collapse = eol)
  if (final) text <- paste0(text, eol)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(text), con)
  path
}

test_that("a file holding two response rows reads as two rows, in file order", {
  dir <- withr::local_tempdir()
  src <- hitopbr_lines()
  f <- write_rows(file.path(dir, "sheet.csv"), c(
    src[1],
    with_participant(src[2], "p002"),
    with_participant(src[2], "p001")
  ))

  out <- read_form_responses(f)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2L)
  expect_identical(out$participant, c("p002", "p001"))
  expect_identical(names(out)[-seq_len(8L)], sprintf("hitopbr_%02d", 1:45))
  expect_s3_class(out$form_build, "Date")
  expect_s3_class(out$submitted, "POSIXct")
  expect_identical(attr(out$submitted, "tzone"), "UTC")
  expect_true(all(vapply(out[-seq_len(8L)], is.integer, logical(1L))))
  expect_identical(out$hitopbr_01, c(4L, 4L))
  expect_identical(out$hitopbr_02, c(3L, 3L))
})

test_that("a two-row file with LF endings and no final newline reads the same", {
  dir <- withr::local_tempdir()
  src <- hitopbr_lines()
  f <- write_rows(file.path(dir, "sheet.csv"), c(
    src[1],
    with_participant(src[2], "p002"),
    with_participant(src[2], "p001")
  ), eol = "\n", final = FALSE)

  expect_no_warning(out <- read_form_responses(f))
  expect_equal(nrow(out), 2L)
  expect_identical(out$participant, c("p002", "p001"))
  expect_identical(out$hitopbr_45, c(4L, 4L))
})

test_that("a directory of a one-row and a two-row file yields three rows in path then file order", {
  dir <- withr::local_tempdir()
  src <- hitopbr_lines()
  # The two-row file sorts first by path, so its rows come first, in its
  # own order; the one-row file's row follows.
  write_rows(file.path(dir, "a_sheet.csv"), c(
    src[1],
    with_participant(src[2], "p003"),
    with_participant(src[2], "p001")
  ))
  write_rows(file.path(dir, "b_single.csv"), c(
    src[1],
    with_participant(src[2], "p002")
  ))

  out <- read_form_responses(dir)

  expect_equal(nrow(out), 3L)
  expect_identical(out$participant, c("p003", "p001", "p002"))
  expect_identical(out$hitopbr_01, c(4L, 4L, 4L))
})

test_that("an item value that is not a whole number is refused by column", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "bad.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,yes"
  ), f)

  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_match(conditionMessage(cnd), "hitopbr_02", fixed = TRUE)
  expect_match(conditionMessage(cnd), "whole number", fixed = TRUE)
})

test_that("a column that appears twice is refused by name", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "dup.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01", "hitopbr_01"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,3"
  ), f)

  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_match(conditionMessage(cnd), "dup.csv", fixed = TRUE)
  expect_match(conditionMessage(cnd), "hitopbr_01", fixed = TRUE)
  expect_match(conditionMessage(cnd), "more than once", fixed = TRUE)
})

test_that("an item value outside the integer range is refused by column", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "wide.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01", "hitopbr_02"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,99999999999"
  ), f)

  expect_no_warning(cnd <- rlang::catch_cnd(read_form_responses(f), "error"))
  expect_match(conditionMessage(cnd), "hitopbr_02", fixed = TRUE)
  expect_match(conditionMessage(cnd), "integer range", fixed = TRUE)
})

test_that("a date with a trailing fragment is refused, not truncated", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(), form_build = "2026-09-20T99")
  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_match(conditionMessage(cnd), "form_build", fixed = TRUE)
  expect_match(conditionMessage(cnd), "does not parse", fixed = TRUE)
})

test_that("a date or time stamp that does not parse is refused by field", {
  dir <- withr::local_tempdir()
  f <- form_file(dir, "p001.csv", two_items(), form_build = "20/09/2026")
  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_match(conditionMessage(cnd), "form_build", fixed = TRUE)

  g <- form_file(dir, "p002.csv", two_items(),
                 submitted = "2026-09-20 21:20:36")
  cnd <- rlang::catch_cnd(read_form_responses(g), "error")
  expect_match(conditionMessage(cnd), "submitted", fixed = TRUE)
})

# ---- AC3: the page's own files round-trip through the scoring functions ---
#
# Oracle note: the expected values come from the fixture's responses and the
# shipped keying tables, never from the scoring engine. For the module fixture
# they are hand-computed literals (worked below); for the two full forms they
# are recomputed here from `*_items$Reverse` and `*_scales$itemNumbers`.

# Mean of the reverse-keyed responses of each scale, from the tables alone.
# `responses` is one row of item responses named by instrument number
# (`1`, `2`, ...); `items` the instrument's `*_items` table with its number
# column named `number`; `scales` the `*_scales` table.
table_means <- function(responses, items, scales, srange = c(1, 4)) {
  keyed <- responses
  rev <- items$number[items$Reverse]
  keyed[as.character(rev)] <- srange[1] + srange[2] - keyed[as.character(rev)]
  vapply(scales$itemNumbers, function(n) {
    mean(keyed[as.character(n)])
  }, numeric(1))
}

test_that("the full HiTOP-BR file scores to the table-derived means", {
  data <- read_form_responses(fixture("responses-hitopbr.csv"))
  item_cols <- names(data)[-seq_len(8L)]
  expect_identical(item_cols, sprintf("hitopbr_%02d", 1:45))
  expect_identical(data$instrument, "hitopbr")

  scored <- score_hitopbr(data, items = item_cols, append = FALSE)

  responses <- stats::setNames(as.numeric(data[1, item_cols]), 1:45)
  items <- hitopbr_items
  items$number <- items$HBR
  expected <- table_means(responses, items, hitopbr_scales)
  names(expected) <- paste0("hbr_", hitopbr_scales$camelCase)

  expect_identical(names(scored), names(expected))
  expect_equal(unlist(scored[1, ]), expected)
})

test_that("the full HiTOP-SR file scores to the table-derived means", {
  data <- read_form_responses(fixture("responses-hitopsr.csv"))
  item_cols <- names(data)[-seq_len(8L)]
  expect_identical(item_cols, sprintf("hitopsr_%03d", 1:405))
  expect_identical(data$instrument, "hitopsr")

  scored <- score_hitopsr(data, items = item_cols, append = FALSE)

  responses <- stats::setNames(as.numeric(data[1, item_cols]), 1:405)
  items <- hitopsr_items
  items$number <- items$HSR
  expected <- table_means(responses, items, hitopsr_scales)
  names(expected) <- paste0("hsr_", hitopsr_scales$camelCase)

  expect_identical(names(scored), names(expected))
  expect_equal(unlist(scored[1, ]), expected)
})

# ---- Store exports: a Google Sheet download and a Supabase export ---------
#
# Each file holds every participant's row (see inst/examples/README.md and
# fixtures/README.md). The expected means are recomputed per row from the
# file's own text and the shipped tables, as above.

# Every data row of a file as a named character vector, keyed by the header.
store_rows <- function(path) {
  lines <- readLines(path, warn = FALSE)
  header <- strsplit(lines[1], ",", fixed = TRUE)[[1]]
  lapply(lines[-1], function(l) {
    stats::setNames(strsplit(l, ",", fixed = TRUE)[[1]], header)
  })
}

expect_hitopbr_export <- function(path, participants) {
  data <- read_form_responses(path)
  item_cols <- names(data)[-seq_len(8L)]

  expect_equal(nrow(data), length(participants))
  expect_type(data$participant, "character")
  expect_identical(data$participant, participants)
  expect_identical(data$instrument, rep("hitopbr", length(participants)))
  expect_s3_class(data$submitted, "POSIXct")
  expect_identical(attr(data$submitted, "tzone"), "UTC")
  expect_false(anyNA(data$submitted))
  expect_identical(item_cols, sprintf("hitopbr_%02d", 1:45))
  expect_true(all(vapply(data[item_cols], is.integer, logical(1L))))

  scored <- score_hitopbr(data, items = item_cols, append = FALSE)

  rows <- store_rows(path)
  items <- hitopbr_items
  items$number <- items$HBR
  for (i in seq_along(rows)) {
    responses <- stats::setNames(as.numeric(rows[[i]][item_cols]), 1:45)
    expected <- table_means(responses, items, hitopbr_scales)
    names(expected) <- paste0("hbr_", hitopbr_scales$camelCase)
    expect_identical(names(scored), names(expected))
    expect_equal(unlist(scored[i, ]), expected, info = paste("row", i))
  }
  invisible(data)
}

test_that("the Google Sheet download reads two rows and scores to the table-derived means", {
  data <- expect_hitopbr_export(example_file("responses-sheet-hitopbr.csv"),
                                c("=1+1", "007"))
  expect_identical(
    data$submitted,
    as.POSIXct(c("2026-09-23 19:33:25", "2026-09-23 19:33:30"), tz = "UTC")
  )
})

test_that("the Supabase export reads two rows and scores to the table-derived means", {
  data <- expect_hitopbr_export(fixture("supabase-hitopbr.csv"),
                                c("p001", "p002"))
  expect_identical(
    data$submitted,
    as.POSIXct(c("2026-09-23 21:01:51", "2026-09-23 21:03:49"), tz = "UTC")
  )
})

# ---- A file saved under the page's random order ----------------------------
#
# The page keeps the item columns in the instrument's order and writes the
# order shown in `item_order` (see fixtures/README.md). The file reads and
# scores as one without the column does; `item_order` comes back as the
# file's own text.

test_that("the shuffled HiTOP-BR file reads item_order as written and scores to the table-derived means", {
  path <- fixture("responses-hitopbr-shuffled.csv")
  data <- expect_hitopbr_export(path, "p001")

  cell <- store_rows(path)[[1]][["item_order"]]
  expect_identical(data$item_order, cell)
  expect_match(cell, "^[1-9][0-9]*( [1-9][0-9]*)*$")
  shown <- as.integer(strsplit(cell, " ", fixed = TRUE)[[1]])
  expect_setequal(shown, 1:45)
  expect_length(shown, 45L)
  # The capture was a rearrangement, not the instrument order.
  expect_false(identical(shown, 1:45))

  # The page answered by its fixed pattern at each shown position: the
  # option at index (position * 7) mod 4 of the four options worth 1 to 4,
  # so 4, 3, 2, 1 repeating down the shown order. The value of item n is
  # therefore the pattern at n's position in `item_order`, stated here from
  # the pattern and independently of the reader.
  position <- match(1:45, shown)
  expect_identical(
    unname(unlist(data[1, sprintf("hitopbr_%02d", 1:45)])),
    (position * 7L) %% 4L + 1L
  )
})

# The shuffled module fixture: Agoraphobia and Distress-Dysphoria, 21 items,
# saved in the descriptor's itemOrder with the page's fixed answer pattern
# 4, 3, 2, 1 repeating down the columns. Neither scale has a reverse item.
#
#   column  item  scale                response
#   1       233   Distress-Dysphoria   4
#   2       194   Distress-Dysphoria   3
#   3       170   Distress-Dysphoria   2
#   4        64   Distress-Dysphoria   1
#   5       365   Distress-Dysphoria   4
#   6        11   Distress-Dysphoria   3
#   7        20   Distress-Dysphoria   2
#   8       300   Distress-Dysphoria   1
#   9       109   Agoraphobia          4
#   10      118   Agoraphobia          3
#   11      260   Agoraphobia          2
#   12      394   Distress-Dysphoria   1
#   13      224   Distress-Dysphoria   4
#   14      291   Agoraphobia          3
#   15      304   Distress-Dysphoria   2
#   16      367   Distress-Dysphoria   1
#   17      343   Distress-Dysphoria   4
#   18       66   Agoraphobia          3
#   19      100   Distress-Dysphoria   2
#   20      386   Distress-Dysphoria   1
#   21      380   Distress-Dysphoria   4
#
#   Agoraphobia (5 items): 4 + 3 + 2 + 3 + 3 = 15; 15 / 5 = 3
#   Distress-Dysphoria (16 items):
#     4 + 3 + 2 + 1 + 4 + 3 + 2 + 1 + 1 + 4 + 2 + 1 + 4 + 2 + 1 + 4 = 39;
#     39 / 16 = 2.4375
test_that("the shuffled module file scores through its descriptor", {
  data <- read_form_responses(example_file("responses-module-shuffled.csv"))
  item_cols <- names(data)[-seq_len(8L)]
  expect_length(item_cols, 21L)
  expect_identical(item_cols[1:3], c("hitopsr_233", "hitopsr_194", "hitopsr_170"))

  module <- read_module(example_file("module-shuffled.json"))
  # Item columns by position: under `layout = "printed"` the names carry
  # numbers in printed order, and naming them would trip the ascending-order
  # heuristic that positions skip (see ?score_hitopsr).
  scored <- score_hitopsr(
    data,
    items = match(item_cols, names(data)),
    module = module,
    layout = "printed",
    append = FALSE
  )

  expect_identical(names(scored), c("hsr_agoraphobia", "hsr_distressDysphoria"))
  expect_equal(scored$hsr_agoraphobia, 3)
  expect_equal(scored$hsr_distressDysphoria, 2.4375)
})

test_that("the shuffled module file scored in instrument order would differ", {
  # The control for the test above: the same columns scored as if they were
  # in instrument order give a different Distress-Dysphoria mean, so the
  # `layout = "printed"` remap is doing the work the test credits it with.
  data <- read_form_responses(example_file("responses-module-shuffled.csv"))
  item_cols <- names(data)[-seq_len(8L)]
  module <- read_module(example_file("module-shuffled.json"))

  scored <- suppressWarnings(score_hitopsr(
    data, items = item_cols, module = module, append = FALSE
  ))
  expect_false(isTRUE(all.equal(scored$hsr_distressDysphoria, 2.4375)))
})

# ---- The three PID-5 forms round-trip through score_pid5() ----------------
#
# Oracle note: as above, the expected means come from the fixture's responses
# and the shipped tables. The PID-5 options run 0 to 3, so the reversal is
# 3 - x. An item reverses where `pid_items$Reverse` is TRUE on the row whose
# version column holds its number. No row with an SF or BF number is
# reversed, so the reversal changes expected values on the full form only
# (8 of its 25 facets with this fixture). FULL and SF domains are the mean of their
# three primary facets (`pid_domains$facetStems`); the BF domains and total
# are rows of `pid_scales$BF`, averaged from their items like any scale.

pid5_expected <- function(responses, version) {
  items <- pid_items[!is.na(pid_items[[version]]), ]
  items$number <- items[[version]]
  scales <- pid_scales[[version]]
  expected <- table_means(responses, items, scales, srange = c(0, 3))
  names(expected) <- scales$camelCase
  if (version %in% c("FULL", "SF")) {
    domains <- vapply(pid_domains$facetStems, function(f) {
      mean(expected[f])
    }, numeric(1))
    expected <- c(expected, stats::setNames(domains, pid_domains$camelCase))
  }
  stats::setNames(expected, paste0("pid_", names(expected)))
}

# The file's own text for each item, read without the package's reader.
raw_item_text <- function(path) {
  lines <- readLines(path)
  header <- strsplit(lines[1], ",", fixed = TRUE)[[1]]
  values <- strsplit(lines[2], ",", fixed = TRUE)[[1]]
  stats::setNames(values, header)[-seq_len(5L)]
}

# The full-form file is the installed example the PID-5 vignette reads; the
# other two stay test fixtures.
pid5_cases <- list(
  list(version = "FULL", path = example_file("responses-pid5.csv"),
       names = sprintf("pid5_%03d", 1:220)),
  list(version = "SF", path = fixture("responses-pid5sf.csv"),
       names = sprintf("pid5sf_%03d", 1:100)),
  list(version = "BF", path = fixture("responses-pid5bf.csv"),
       names = sprintf("pid5bf_%02d", 1:25))
)

for (case in pid5_cases) {
  test_that(paste("the PID-5", case$version, "file scores to the table-derived means"), {
    path <- case$path
    data <- read_form_responses(path)
    expect_identical(nrow(data), 1L)
    item_cols <- names(data)[-seq_len(8L)]
    expect_identical(item_cols, case$names)
    expect_true(all(vapply(data[item_cols], is.integer, logical(1))))

    # A 0 in the file is read as 0, not as missing.
    raw <- raw_item_text(path)
    expect_identical(names(raw), item_cols)
    zeros <- names(raw)[raw == "0"]
    expect_gt(length(zeros), 0L)
    expect_identical(unname(unlist(data[1, zeros])), rep(0L, length(zeros)))
    # Every item the reader returns matches the file's own text.
    expect_identical(unname(unlist(data[1, item_cols])), as.integer(unname(raw)))

    scored <- score_pid5(data, items = item_cols, version = case$version,
                         append = FALSE)

    # The expected means start from the file's text, not the reader's output.
    n <- length(item_cols)
    responses <- stats::setNames(as.numeric(raw), seq_len(n))
    expected <- pid5_expected(responses, case$version)

    expect_identical(names(scored), names(expected))
    expect_equal(unlist(scored[1, ]), expected)
  })
}

# ---- The installed examples the vignette and article read -----------------
#
# vignette("pid5_scoring") and the HiTOP-SR modules article read these through
# system.file("examples", ...). Under devtools::test() that resolves to the
# source inst/examples/, so this checks the files and their shape; whether they
# reach an installed package is checked by running the vignette code against
# one.

test_that("the example files read through system.file()", {
  pid5 <- system.file("examples", "responses-pid5.csv", package = "hitop")
  expect_true(nzchar(pid5))
  data <- read_form_responses(pid5)
  expect_identical(nrow(data), 1L)
  expect_identical(names(data)[-seq_len(8L)], sprintf("pid5_%03d", 1:220))

  module <- system.file("examples", "responses-module-shuffled.csv",
                        package = "hitop")
  expect_true(nzchar(module))
  expect_identical(ncol(read_form_responses(module)), 8L + 21L)

  descriptor <- system.file("examples", "module-shuffled.json",
                            package = "hitop")
  expect_true(nzchar(descriptor))
  expect_true(is_module(read_module(descriptor)))
})
