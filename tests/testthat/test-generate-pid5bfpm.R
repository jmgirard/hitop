# Parse-back tests for the PID5BF+M Word, Qualtrics and REDCap generators.
#
# D-010 style: each generated file is parsed back and compared with the source
# tables, `pid_items` (the BFPM column, the item text), `pid_instructions`,
# `pid_scales$BFPM` and `pid_bfpm_domains`, never with the generator's own
# output. The BF+M forms reuse the APA PID-5 instructions and 0 to 3 labels by
# maintainer sign-off (cairn/SOURCES.md).

# The 36 BF+M items in BF+M order, looked up from `pid_items` by number.
bfpm_expected <- function() {
  n <- 1:36
  data.frame(
    number = n,
    text = pid_items$Text[match(n, pid_items$BFPM)],
    stringsAsFactors = FALSE
  )
}

# All <w:t> runs of a .docx, in document order.
docx_runs <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  unescape_xml(gsub("<[^>]+>", "", runs))
}

# ---- Word (AC1) --------------------------------------------------------------

test_that("the BF+M Word form prints exactly the 36 BF+M items in BF+M order", {
  skip_if_no_docx()
  expected <- bfpm_expected()
  # Independent fact: the form has 36 items, numbered 1 to 36 without gaps.
  expect_identical(sort(pid_items$BFPM[!is.na(pid_items$BFPM)]), 1:36)
  expect_false(anyNA(expected$text))

  for (paper in c("us", "a4")) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(generate_docx_pid5bfpm(file = f, papersize = paper))
    # The papersize argument reaches the page: US Letter is 12240 x 15840
    # twips, and A4 lands within a few twips of 11906 x 16838.
    size <- docx_page_size(read_docx_xml(f))
    want <- if (paper == "us") c(12240L, 15840L) else c(11906L, 16838L)
    expect_true(all(abs(unname(size) - want) < 12), info = paper)

    rows <- docx_item_rows(f)
    expect_equal(as.integer(rows$number), expected$number, info = paper)
    expect_identical(rows$text, expected$text, info = paper)

    # Each item row is followed by its four response values, 0 to 3.
    runs <- docx_runs(f)
    item_at <- which(grepl("^[0-9]+\\.  ", runs))
    expect_length(item_at, 36L)
    for (i in item_at) {
      expect_identical(
        runs[i + 1:4],
        as.character(pid_instructions$options$value),
        info = paste(paper, runs[i])
      )
    }
  }
})

test_that("the BF+M Word form prints the stored PID-5 instructions and legend", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bfpm(file = f))
  runs <- docx_runs(f)

  expect_true(pid_instructions$start %in% runs)

  got <- docx_legend_pairs(docx_legend_lines(f))
  expect_equal(got$value, as.character(pid_instructions$options$value))
  expect_identical(got$label, pid_instructions$options$label)
})

test_that("the BF+M scoring page prints the facets, item pairs and domain map", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bfpm(file = f))

  # Facet table: one row per BF+M facet with its two item numbers. No BF+M
  # item is reverse-keyed, so no "(R)" mark appears.
  bfpm <- pid_scales$BFPM
  expected_facets <- data.frame(
    scale = bfpm$Facet,
    items = vapply(bfpm$itemNumbers, paste, character(1), collapse = ", "),
    stringsAsFactors = FALSE
  )
  printed <- docx_scoring_rows(f)
  expect_equal(nrow(printed), 18L)
  expect_setequal(printed$scale, expected_facets$scale)
  expect_identical(
    printed$items,
    expected_facets$items[match(printed$scale, expected_facets$scale)]
  )
  expect_false(any(pid_items$Reverse[!is.na(pid_items$BFPM)]))
  expect_false(any(grepl("(R)", printed$items, fixed = TRUE)))

  # Domain table: the six domains in `pid_bfpm_domains` order, each with its
  # three facets, named through the facet stems and `pid_scales$BFPM`.
  domains <- docx_domain_rows(f)
  expect_identical(domains$domain, pid_bfpm_domains$Domain)
  expected_members <- vapply(
    pid_bfpm_domains$facetStems,
    function(stems) {
      paste(bfpm$Facet[match(stems, bfpm$camelCase)], collapse = ", ")
    },
    character(1)
  )
  expect_identical(domains$facets, unname(expected_members))

  # The instruction line states the item-mean scale: facets average their
  # items and domains average their three facet scores.
  runs <- docx_runs(f)
  msg <- runs[grepl("^Average the responses", runs)]
  expect_length(msg, 1L)
  expect_match(msg, "Average the responses for the following item numbers", fixed = TRUE)
  expect_match(msg, "average the three facet scores", fixed = TRUE)
  # No BF+M item is reverse-keyed, so the line says so rather than pointing
  # to "(R)" marks the form never prints.
  expect_match(msg, "No items are reverse-scored.", fixed = TRUE)
  expect_no_match(msg, "(R)", fixed = TRUE)
})

test_that("include_scoring = FALSE drops both BF+M scoring tables", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bfpm(file = f, include_scoring = FALSE))
  expect_equal(nrow(docx_scoring_rows(f)), 0L)
  expect_equal(nrow(docx_domain_rows(f)), 0L)
  expect_equal(nrow(docx_item_rows(f)), 36L)
})

test_that("the domain table does not leak into the other PID-5 forms", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bf(file = f))
  expect_equal(nrow(docx_domain_rows(f)), 0L)
  expect_gt(nrow(docx_scoring_rows(f)), 0L)
})

# ---- Qualtrics (AC2) ---------------------------------------------------------

test_that("the BF+M Qualtrics file holds the 36 BF+M items in BF+M order", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_pid5bfpm(file = f))
  q <- read_qualtrics(f)
  expected <- bfpm_expected()

  expect_true(q$advanced_format)
  expect_identical(q$block, "PID5BF+M")
  expect_equal(q$questions$num, expected$number)
  expect_identical(q$questions$text, expected$text)
  expect_identical(q$questions$id, sprintf("PID5BFPM_%02d", 1:36))

  # Every question carries the same four choices: count each (value, label)
  # pair across the file.
  opts <- pid_instructions$options
  for (k in seq_len(nrow(opts))) {
    at <- which(q$lines == sprintf("[[Choice:%d]]", opts$value[k]))
    expect_length(at, 36L)
    expect_true(all(q$lines[at + 1L] == opts$label[k]))
  }
  expect_equal(q$choices$value, opts$value)
  expect_identical(q$choices$label, opts$label)

  # The instructions block carries the stored PID-5 text.
  ins <- which(q$lines == "[[ID:start_instructions]]")
  expect_length(ins, 1L)
  expect_identical(q$lines[ins + 1L], pid_instructions$start)
})

# ---- REDCap (AC2) ------------------------------------------------------------

test_that("the BF+M REDCap dictionary holds the 36 BF+M items in BF+M order", {
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_pid5bfpm(file = f))
  r <- read_redcap_csv(f)
  expected <- bfpm_expected()

  expect_equal(nrow(r), 37L) # 36 items + instruction row
  expect_identical(r[["Field Type"]][1], "descriptive")
  expect_identical(r[["Field Label"]][1], pid_instructions$start)

  items <- r[-1, ]
  expect_identical(items[["Variable / Field Name"]], sprintf("pid5bfpm_%02d", 1:36))
  expect_identical(items[["Field Label"]], expected$text)
  expect_true(all(items[["Field Type"]] == "radio"))

  opts <- pid_instructions$options
  choices <- paste(opts$value, opts$label, sep = ", ", collapse = " | ")
  expect_true(all(items[["Choices, Calculations, OR Slider Labels"]] == choices))

  # The 36 field names score without error when passed to
  # score_pid5(version = "BFPM") as `items`.
  df <- as.data.frame(
    matrix(0L, nrow = 1L, ncol = 36L, dimnames = list(NULL, items[["Variable / Field Name"]]))
  )
  expect_no_error(score_pid5(df, items = names(df), version = "BFPM", append = FALSE))
})
