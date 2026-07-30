# Ground-truth tests for the DOCX export family (M10).
#
# A .docx is a ZIP of XML; we unzip word/document.xml (helper-generators.R) and
# assert (a) it is non-empty, (b) it contains the instrument's own text, and
# (c) the papersize branch sets the right <w:pgSz>. officer/flextable are
# Imports but the skip guard keeps local runs graceful when they are absent.

# skip_if_no_docx() lives in helper-generators.R.

# First source Text made only of plain ASCII (no XML-escaped chars), so a
# fixed-string search of document.xml is reliable.
first_clean_text <- function(v) {
  hit <- v[grepl("^[A-Za-z0-9 .,]+$", v)]
  hit[1]
}

# ---- Detailed: HiTOP-BR (valid docx, content, page-size branch, errors) -----

test_that("generate_docx_hitopbr writes a valid docx with item text", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopbr(file = f))
  expect_gt(file.info(f)$size, 0)

  xml <- read_docx_xml(f)
  expect_gt(nchar(xml), 0)
  expect_true(grepl(first_clean_text(hitopbr_items$Text), xml, fixed = TRUE))
})

test_that("papersize selects letter vs A4 page dimensions", {
  skip_if_no_docx()
  fus <- withr::local_tempfile(fileext = ".docx")
  fa4 <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopbr(file = fus, papersize = "us"))
  suppressMessages(generate_docx_hitopbr(file = fa4, papersize = "a4"))

  us <- docx_page_size(read_docx_xml(fus))
  a4 <- docx_page_size(read_docx_xml(fa4))

  # US Letter is exact: 8.5 x 11 in = 12240 x 15840 twips.
  expect_equal(unname(us), c(12240L, 15840L))
  # A4 differs and lands within a few twips of ISO A4 (210 x 297 mm).
  expect_false(isTRUE(all.equal(unname(a4), unname(us))))
  expect_lt(abs(a4[["width"]] - 11906L), 12)
  expect_lt(abs(a4[["height"]] - 16838L), 12)
})

test_that("an invalid papersize is rejected by match.arg", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    suppressMessages(generate_docx_hitopbr(file = f, papersize = "legal")),
    "should be one of"
  )
})

# ---- Smoke coverage: all 6 DOCX generators ----------------------------------

test_that("all DOCX generators produce a non-empty document with their own text", {
  skip_if_no_docx()
  cases <- list(
    list(fn = generate_docx_hitopbr, text = first_clean_text(hitopbr_items$Text)),
    list(fn = generate_docx_hitopsr, text = first_clean_text(hitopsr_items$Text)),
    list(fn = generate_docx_pid5,    text = first_clean_text(pid_items$Text[!is.na(pid_items$FULL)])),
    list(fn = generate_docx_pid5sf,  text = first_clean_text(pid_items$Text[!is.na(pid_items$SF)])),
    list(fn = generate_docx_pid5bf,  text = first_clean_text(pid_items$Text[!is.na(pid_items$BF)])),
    # HSUM DOCX is a curated overview, not an item table; assert a stable phrase.
    list(fn = generate_docx_hitophsum, text = "In what forms did you use nicotine")
  )
  for (case in cases) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(case$fn(file = f))
    expect_gt(file.info(f)$size, 0)
    xml <- read_docx_xml(f)
    expect_gt(nchar(xml), 0)
    expect_true(grepl(case$text, xml, fixed = TRUE))
  }
})

# ---- HSUM overview: wording synced to the August 2024 sheet ------------------
#
# Expected strings hand-transcribed from the "revised SUD module-August 2024"
# sheet (see tests/testthat/test-keying-hitophsum.R and cairn/SOURCES.md).

test_that("the HSUM overview uses the corrected sheet wording", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitophsum(file = f))
  xml <- read_docx_xml(f)

  # Substance label (sheet row 20): "Street opioids", not "Heroin/opiates".
  expect_true(grepl("Street opioids (heroin, opium, fentanyl, etc.)", xml, fixed = TRUE))
  expect_false(grepl("Heroin/opiates", xml, fixed = TRUE))

  # WITH01 (sheet row 138): two words.
  expect_true(grepl("Goose bumps", xml, fixed = TRUE))
  expect_false(grepl("Goosebumps", xml, fixed = TRUE))

  # Other-drugs SUD01 carries the typo repair ("urge to use", not the
  # sheet's alcohol-carryover "urge to drink").
  expect_true(grepl(
    "Reminders of [substance] gave me a strong urge to use [substance].",
    xml,
    fixed = TRUE
  ))
  expect_false(grepl(
    "Reminders of [substance] gave me a strong urge to drink.",
    xml,
    fixed = TRUE
  ))
})

# ---- HiTOP-SR scale subsets (M24) -------------------------------------------
#
# Parse-and-compare per D-010: the expected item numbers and texts are derived
# from `hitopsr_items` (filtered by `Scale`), independently of the
# `hitopsr_scales$itemNumbers` path the generator itself walks.

test_that("generate_docx_hitopsr(subset =) emits exactly the subset's items", {
  skip_if_no_docx()
  s <- hitop_subset("hitopsr", c("Agoraphobia", "Appetite Loss"))
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f, subset = s))
  xml <- read_docx_xml(f)

  kept <- hitopsr_items[
    hitopsr_items$Scale %in% c("Agoraphobia", "Appetite Loss"),
  ]
  expect_equal(kept$HSR, c(66, 109, 118, 144, 202, 260, 291, 389))

  # Every kept item appears, numbered with its ORIGINAL HSR number.
  for (i in seq_len(nrow(kept))) {
    expect_true(
      grepl(paste0(kept$HSR[i], ".  ", kept$Text[i]), xml, fixed = TRUE)
    )
  }

  # No item from any other scale leaks in.
  dropped <- hitopsr_items[!hitopsr_items$HSR %in% kept$HSR, ]
  for (txt in utils::head(dropped$Text, 25)) {
    expect_false(grepl(txt, xml, fixed = TRUE))
  }

  # Renumbering did NOT happen: item 66 is first, and there is no "1.  " row.
  expect_false(grepl(paste0("1.  ", kept$Text[1]), xml, fixed = TRUE))
})

test_that("the subset DOCX scoring table lists only the subset's scales", {
  skip_if_no_docx()
  s <- hitop_subset("hitopsr", c("Agoraphobia", "Romantic Disinterest"))
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f, subset = s))
  xml <- read_docx_xml(f)

  # Scoring rows carry original numbers, and HSR 310 keeps its (R) marker.
  expect_true(grepl("66, 109, 118, 260, 291", xml, fixed = TRUE))
  expect_true(grepl("42, 152, 187, 310(R), 338", xml, fixed = TRUE))

  # Scales outside the subset are absent from the scoring table.
  expect_false(grepl("Antisocial Behavior", xml, fixed = TRUE))
  expect_false(grepl("Appetite Loss", xml, fixed = TRUE))
})

test_that("generate_docx_hitopsr() rejects subset + include_subscales", {
  s <- hitop_subset("hitopsr", "Agoraphobia")
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    generate_docx_hitopsr(file = f, subset = s, include_subscales = TRUE),
    "cannot be combined"
  )
  # Truthy non-TRUE values must not slip past the guard: the code that adds
  # the subscale rows tests plain truthiness, so the guard must too.
  for (truthy in list(1, 1L)) {
    expect_error(
      generate_docx_hitopsr(file = f, subset = s, include_subscales = truthy),
      "cannot be combined"
    )
  }
  # A value R cannot read as a logical errors either way, never silently.
  expect_error(
    generate_docx_hitopsr(file = f, subset = s, include_subscales = "yes")
  )
})

test_that("generate_docx_hitopsr() rejects a non-hitop_subset subset", {
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(
    generate_docx_hitopsr(file = f, subset = c("Agoraphobia")),
    "hitop_subset"
  )
})
