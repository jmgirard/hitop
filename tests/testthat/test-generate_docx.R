# Ground-truth tests for the DOCX export family (M10).
#
# A .docx is a ZIP of XML; we unzip word/document.xml (helper-generators.R) and
# assert (a) it is non-empty, (b) it contains the instrument's own text, and
# (c) the papersize branch sets the right <w:pgSz>. officer/flextable are
# Imports but the skip guard keeps local runs graceful when they are absent.

skip_if_no_docx <- function() {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
}

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
