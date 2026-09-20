# read_form_responses() reads the CSV files the hitop-form page saves.
#
# The three committed fixtures under fixtures/ are files the page saved (see
# fixtures/README.md). The synthetic files below are written by `form_file()`
# in the shape the page writes: five lead columns, then item columns, one
# response row, CRLF row endings unless the test says otherwise.

lead <- c("study", "participant", "instrument", "form_build", "submitted")

# Write one response file as the page does. `items` is a named vector of
# responses (a name is the column, a value the response; NA writes an empty
# field). Returns the path.
form_file <- function(dir, name, items, participant = "p001",
                      instrument = "hitopbr", eol = "\r\n",
                      study = "study", form_build = "2026-09-20",
                      submitted = "2026-09-20T21:20:36Z") {
  vals <- ifelse(is.na(items), "", as.character(items))
  header <- paste(c(lead, names(items)), collapse = ",")
  row <- paste(c(study, participant, instrument, form_build, submitted, vals),
               collapse = ",")
  path <- file.path(dir, name)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(paste0(header, eol, row, eol)), con)
  path
}

two_items <- function(a = 4L, b = 1L) c(hitopbr_01 = a, hitopbr_02 = b)

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
  expect_identical(names(out), c(lead, "hitopbr_01", "hitopbr_02"))
  expect_identical(out$participant, c("p001", "p002"))
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
  expect_identical(names(out), c(lead, "hitopbr_01", "hitopbr_02"))
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
  expect_identical(names(out)[-seq_len(5L)], c("hitopsr_233", "hitopsr_194"))
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

test_that("a directory holding no .csv aborts by class", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "notes.txt"))

  expect_error(read_form_responses(dir), class = "hitop_form_responses_none")

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

test_that("a file with two response rows is refused by name", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "two.csv")
  writeLines(c(
    paste(c(lead, "hitopbr_01"), collapse = ","),
    "s,p1,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4",
    "s,p2,hitopbr,2026-09-20,2026-09-20T21:20:36Z,3"
  ), f)

  cnd <- rlang::catch_cnd(read_form_responses(f), "error")
  expect_match(conditionMessage(cnd), "2 response rows", fixed = TRUE)
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

fixture <- function(...) test_path("fixtures", ...)

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
  item_cols <- names(data)[-seq_len(5L)]
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
  item_cols <- names(data)[-seq_len(5L)]
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
  data <- read_form_responses(fixture("responses-module-shuffled.csv"))
  item_cols <- names(data)[-seq_len(5L)]
  expect_length(item_cols, 21L)
  expect_identical(item_cols[1:3], c("hitopsr_233", "hitopsr_194", "hitopsr_170"))

  module <- read_module(fixture("module-shuffled.json"))
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
  data <- read_form_responses(fixture("responses-module-shuffled.csv"))
  item_cols <- names(data)[-seq_len(5L)]
  module <- read_module(fixture("module-shuffled.json"))

  scored <- suppressWarnings(score_hitopsr(
    data, items = item_cols, module = module, append = FALSE
  ))
  expect_false(isTRUE(all.equal(scored$hsr_distressDysphoria, 2.4375)))
})
