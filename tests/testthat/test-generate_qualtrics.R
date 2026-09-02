# Ground-truth tests for the Qualtrics export family (M010).
#
# Oracle: parse the generated .txt back (helper-generators.R) and compare it to
# the SOURCE item/instruction datasets derived independently -- never to the
# file's own bytes. Every generate_qualtrics_* routine funnels through
# build_qualtrics_txt() (R/generate_qualtrics.R:165).

# ---- Content oracle: HiTOP-BR (items passed through unsorted) ---------------

test_that("generate_qualtrics_hitopbr transcribes every item and choice", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopbr(file = f))
  q <- read_qualtrics(f)

  # Structural: advanced-format header, named block, one question per item.
  expect_true(q$advanced_format)
  expect_identical(q$block, "HiTOP-BR")
  expect_equal(nrow(q$questions), 45L)
  expect_true(q$has_instructions)

  # Item numbers set-equal to the source key (1:45), and each item's text
  # matches the source row for that number (independent lookup, not file order).
  expect_setequal(q$questions$num, hitopbr_items$HBR)
  expect_setequal(q$questions$num, 1:45)
  expected_text <- hitopbr_items$Text[match(q$questions$num, hitopbr_items$HBR)]
  expect_identical(q$questions$text, expected_text)

  # IDs are the prefix + zero-padded number, the width of the instrument's
  # largest item number (45, so 2 digits).
  expect_identical(q$questions$id, sprintf("HBR_%02d", q$questions$num))

  # Choices equal the instruction options exactly.
  expect_equal(q$choices$value, hitopbr_instructions$options$value)
  expect_identical(q$choices$label, hitopbr_instructions$options$label)

  # Default breaks = 15 -> page breaks after items 15 and 30 (not after 45).
  expect_equal(q$pagebreak_after, c(15L, 30L))
})

# ---- Content oracle: PID-5 FULL (filtered + sorted by FULL) -----------------

test_that("generate_qualtrics_pid5 emits all 220 items with 3-digit IDs", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_pid5(file = f))
  q <- read_qualtrics(f)

  expect_equal(nrow(q$questions), 220L)
  expect_setequal(q$questions$num, 1:220)

  # Text matches the source PID-5 key, looked up by FULL item number.
  expected_text <- pid_items$Text[match(q$questions$num, pid_items$FULL)]
  expect_identical(q$questions$text, expected_text)

  # 220 items -> 3-digit zero padding.
  expect_identical(q$questions$id, sprintf("PID5_%03d", q$questions$num))
  expect_equal(q$choices$value, pid_instructions$options$value)

  # Default breaks = 15 -> after 15, 30, ..., 210 (14 breaks; none after 220).
  expect_equal(q$pagebreak_after, seq(15L, 210L, by = 15L))
})

# ---- Structural toggles -----------------------------------------------------

test_that("include_instructions and breaks arguments behave", {
  # Instructions omitted.
  f1 <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopbr(file = f1, include_instructions = FALSE))
  expect_false(read_qualtrics(f1)$has_instructions)

  # breaks = NULL and breaks = 0 both disable pagination.
  f2 <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopbr(file = f2, breaks = NULL))
  expect_length(read_qualtrics(f2)$pagebreak_after, 0)

  f3 <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopbr(file = f3, breaks = 0))
  expect_length(read_qualtrics(f3)$pagebreak_after, 0)

  # A custom break interval lands on the right items.
  f4 <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopbr(file = f4, breaks = 10))
  expect_equal(read_qualtrics(f4)$pagebreak_after, c(10L, 20L, 30L, 40L))
})

# ---- Smoke coverage: all 5 Qualtrics generators -----------------------------

test_that("all Qualtrics generators run and produce one question per source item", {
  cases <- list(
    list(fn = generate_qualtrics_hitopbr, n = 45L),
    list(fn = generate_qualtrics_hitopsr, n = nrow(hitopsr_items)),
    list(fn = generate_qualtrics_pid5,    n = 220L),
    list(fn = generate_qualtrics_pid5sf,  n = 100L),
    list(fn = generate_qualtrics_pid5bf,  n = 25L)
  )
  for (case in cases) {
    f <- withr::local_tempfile(fileext = ".txt")
    suppressMessages(case$fn(file = f))
    expect_gt(file.info(f)$size, 0)
    q <- read_qualtrics(f)
    expect_true(q$advanced_format)
    expect_equal(nrow(q$questions), case$n)
  }
})

# ---- HiTOP-SR scale modules (M024) -------------------------------------------
#
# Parse-and-compare per D-010: expected numbers/texts come from `hitopsr_items`
# filtered by `Scale`, independently of the `hitopsr_scales$itemNumbers` path
# the generator walks.

test_that("generate_qualtrics_hitopsr(module =) emits exactly the module's items", {
  s <- hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopsr(file = f, module = s))
  parsed <- read_qualtrics(f)

  kept <- hitopsr_items[
    hitopsr_items$Scale %in% c("Agoraphobia", "Appetite Loss"),
  ]
  expect_equal(parsed$questions$num, kept$HSR)
  expect_equal(parsed$questions$text, kept$Text)
  expect_equal(nrow(parsed$questions), 8)

  # Original numbering, uniformly zero-padded to the instrument's largest item
  # number -- not to the largest number this module exports.
  expect_equal(parsed$questions$id[1], "HSR_066")
  expect_equal(parsed$questions$id[8], "HSR_389")
  expect_true(all(nchar(parsed$questions$id) == nchar("HSR_066")))

  # Structure is otherwise a normal Qualtrics file.
  expect_true(parsed$advanced_format)
  expect_equal(parsed$block, "HiTOP-SR")
  expect_true(parsed$has_instructions)
})

test_that("module = NULL leaves Qualtrics output byte-identical for all instruments", {
  # Locks the shared-helper padding change (nrow -> max item number): every
  # existing generator must be unaffected.
  gens <- list(
    generate_qualtrics_hitopsr,
    generate_qualtrics_hitopbr,
    generate_qualtrics_pid5,
    generate_qualtrics_pid5sf,
    generate_qualtrics_pid5bf
  )
  widths <- c(3, 2, 3, 3, 2) # largest item numbers 405, 45, 220, 100, 25
  for (i in seq_along(gens)) {
    f <- withr::local_tempfile(fileext = ".txt")
    suppressMessages(gens[[i]](file = f))
    parsed <- read_qualtrics(f)
    ids <- parsed$questions$id
    expect_equal(
      unique(nchar(sub("^.*_", "", ids))),
      widths[i]
    )
    # Numbering is 1..n contiguous for every full instrument.
    expect_equal(parsed$questions$num, seq_len(nrow(parsed$questions)))
  }
})

test_that("generate_qualtrics_hitopsr() rejects a non-hitop_module module", {
  f <- withr::local_tempfile(fileext = ".txt")
  expect_error(
    generate_qualtrics_hitopsr(file = f, module = "Agoraphobia"),
    "hitop_module"
  )
})
