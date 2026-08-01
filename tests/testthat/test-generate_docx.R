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

test_that("the BF scoring table carries the Total row keyed to pid_scales", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bf(file = f))
  xml <- read_docx_xml(f)

  # The Total row's item cell is derived from the keying table, never hardcoded,
  # so this fails if pid_scales and the printed form ever drift apart (IP2).
  bf <- pid_scales[["BF"]]
  total_items <- paste(bf$itemNumbers[["total"]], collapse = ", ")
  expect_true(grepl("Total", xml, fixed = TRUE))
  expect_true(grepl(total_items, xml, fixed = TRUE))

  # It really is all 25 items in ascending order. Asserted against a literal:
  # comparing the vector to its own sort() is satisfied by any vector, and
  # length-25 alone does not pin which items.
  expect_equal(bf$itemNumbers[["total"]], as.numeric(1:25))

  # And no item on the printed form carries a reverse mark, because no BF item
  # is reverse-keyed. Checked against the DOCX itself -- the earlier version of
  # this assertion searched `total_items`, a string the test builds by joining
  # a numeric vector, which can never contain "(R)" whatever the form says.
  # The pattern is digit-then-mark: the form's own scoring instruction reads
  # "Reverse-scored items are indicated with (R).", so a bare "(R)" search
  # matches every BF form ever generated and asserts nothing.
  expect_false(any(pid_items$Reverse[!is.na(pid_items$BF)]))
  expect_false(grepl("[0-9]\\(R\\)", xml))

  # The five domain rows are still printed alongside it.
  for (stem in setdiff(bf$camelCase, "total")) {
    items <- paste(bf$itemNumbers[[stem]], collapse = ", ")
    expect_true(grepl(items, xml, fixed = TRUE))
  }
})

test_that("include_scoring = FALSE still omits the BF scoring table entirely", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bf(file = f, include_scoring = FALSE))
  xml <- read_docx_xml(f)
  # Adding the Total row must not leak the scoring table into the no-scoring form.
  total_items <- paste(pid_scales[["BF"]]$itemNumbers[["total"]], collapse = ", ")
  expect_false(grepl(total_items, xml, fixed = TRUE))
})

# ---- Response-option legend line breaking (M36) -----------------------------
#
# The legend is participant-facing content under IP1: this milestone changes
# only where it breaks, never what it says (D-028), so these tests assert the
# line structure AND recover the pairs to prove the wording is untouched. The
# oracle for "unchanged" is `*_instructions$options` and the committed SR/BR
# forms -- never the PID forms this milestone rebuilds, which would be the
# code's own output asserted as truth (IP2).

test_that("the PID legend prints two options per line, in printed order", {
  skip_if_no_docx()
  for (gen in list(generate_docx_pid5, generate_docx_pid5sf, generate_docx_pid5bf)) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(gen(file = f))
    lines <- docx_legend_lines(f)

    # Exactly two legend lines. The scoring table's header row is "Scale"/
    # "Items" and cannot match the extractor's "<value> = " anchor, so this
    # counts the items table's header alone.
    expect_length(lines, 2L)

    opts <- pid_instructions$options
    expect_equal(docx_legend_pairs(lines[[1]])$value, as.character(opts$value[1:2]))
    expect_equal(docx_legend_pairs(lines[[2]])$value, as.character(opts$value[3:4]))
  }
})

test_that("splitting the PID legend changes no wording and adds no character", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5(file = f))
  lines <- docx_legend_lines(f)
  got <- docx_legend_pairs(lines)
  opts <- pid_instructions$options

  # Every pair survives the split, in printed order, value and label intact.
  expect_equal(got$value, as.character(opts$value))
  expect_equal(got$label, opts$label)

  # The separator is consumed by the break, not carried to a line end, and the
  # split adds nothing: rejoining the lines reproduces the one-line legend.
  expect_false(any(grepl("•\\s*$", lines)))
  expect_equal(
    paste(lines, collapse = " • "),
    paste(opts$value, opts$label, sep = " = ", collapse = " • ")
  )
})

test_that("the HiTOP-SR and HiTOP-BR legends still print on one line", {
  skip_if_no_docx()
  for (gen in list(generate_docx_hitopsr, generate_docx_hitopbr)) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(gen(file = f))
    lines <- docx_legend_lines(f)
    expect_length(lines, 1L)
    expect_equal(nrow(docx_legend_pairs(lines)), 4L)
  }
})

test_that("the default legend matches the committed forms this milestone leaves alone", {
  skip_if_no_docx()
  # The SR/BR artifacts are not rebuilt by M36, so their committed bytes are an
  # external record of the pre-change single-line legend -- the oracle for the
  # claim that `make_items_table()`'s default is unchanged.
  committed <- list(
    hitopsr = system.file("extdata", "hitopsr_US.docx", package = "hitop"),
    hitopbr = system.file("extdata", "hitopbr_US.docx", package = "hitop")
  )
  fresh <- list(hitopsr = generate_docx_hitopsr, hitopbr = generate_docx_hitopbr)

  for (nm in names(committed)) {
    skip_if(committed[[nm]] == "")
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(fresh[[nm]](file = f))
    expect_equal(docx_legend_lines(f), docx_legend_lines(committed[[nm]]))
  }
})

test_that("opts_per_line defaults to the option count, not a hardcoded four", {
  skip_if_no_docx()
  # A three-option table must still print on ONE line by default; a hardcoded 4
  # would pass every four-option check above while wrapping this one wrongly.
  opts3 <- data.frame(value = 1:3, label = c("Never", "Sometimes", "Always"))
  items <- data.frame(Number = 1:3, Text = c("a", "b", "c"))

  one <- make_items_table(items, "Number", opts3, 7, 10, "Times New Roman")
  f1 <- withr::local_tempfile(fileext = ".docx")
  print(flextable::body_add_flextable(officer::read_docx(), one), target = f1)
  expect_length(docx_legend_lines(f1), 1L)

  two <- make_items_table(items, "Number", opts3, 7, 10, "Times New Roman", opts_per_line = 2)
  f2 <- withr::local_tempfile(fileext = ".docx")
  print(flextable::body_add_flextable(officer::read_docx(), two), target = f2)
  # 3 options at 2 per line is 2 + 1, and the short final line carries no
  # dangling separator.
  expect_length(docx_legend_lines(f2), 2L)
  expect_equal(nrow(docx_legend_pairs(docx_legend_lines(f2)[[2]])), 1L)
})

test_that("a one-item table builds without error", {
  skip_if_no_docx()
  # Regression: even-row shading used seq(2, n, by = 2), which counts backwards
  # at n = 1 and aborts -- reachable through a single-item subset form.
  one_item <- data.frame(Number = 1L, Text = "a")
  expect_no_error(
    make_items_table(one_item, "Number", hitopsr_instructions$options, 7, 10, "Times New Roman")
  )
})
