# Parse-back tests for the PID-5 Informant Form Word, Qualtrics and REDCap
# generators (M160).
#
# D-010 style: each generated file is parsed back and compared with the source
# tables, `pid_items` (the IRF column and the informant wording `TextIRF`),
# the stored `pid_irf_instructions`, and the key's Facet Table and reverse
# list typed in helper-fixtures.R, never with the generator's own output.

# The 218 informant items in IRF order, looked up from `pid_items` by number.
irf_expected <- function() {
  n <- 1:218
  data.frame(
    number = n,
    text = pid_items$TextIRF[match(n, pid_items$IRF)],
    stringsAsFactors = FALSE
  )
}

# The instruction text the forms print: the opening paragraph, the rating
# prompt and the stem each item completes, as stored.
irf_instruction_text <- function() {
  paste(
    pid_irf_instructions$start,
    pid_irf_instructions$prompt,
    pid_irf_instructions$stem
  )
}

# All <w:t> runs of a .docx, in document order.
irf_docx_runs <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  unescape_xml(gsub("<[^>]+>", "", runs))
}

# ---- Word (AC1) --------------------------------------------------------------

test_that("the IRF Word form prints exactly the 218 informant items in IRF order", {
  skip_if_no_docx()
  expected <- irf_expected()
  # Precondition on the source table: `pid_items$IRF` numbers 218 items, 1 to
  # 218 without gaps.
  expect_identical(sort(pid_items$IRF[!is.na(pid_items$IRF)]), 1:218)
  expect_false(anyNA(expected$text))

  for (paper in c("us", "a4")) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(generate_docx_pid5irf(file = f, papersize = paper))
    size <- docx_page_size(read_docx_xml(f))
    want <- if (paper == "us") c(12240L, 15840L) else c(11906L, 16838L)
    expect_true(all(abs(unname(size) - want) < 12), info = paper)

    rows <- docx_item_rows(f)
    expect_equal(as.integer(rows$number), expected$number, info = paper)
    expect_identical(rows$text, expected$text, info = paper)
    # No self-report wording appears as an item.
    expect_false(any(rows$text %in% pid_items$Text), info = paper)

    # Each item row is followed by its four response values, 0 to 3.
    runs <- irf_docx_runs(f)
    item_at <- which(grepl("^[0-9]+\\.  ", runs))
    expect_length(item_at, 218L)
    for (i in item_at) {
      expect_identical(
        runs[i + 1:4],
        as.character(pid_irf_instructions$options$value),
        info = paste(paper, runs[i])
      )
    }
  }
})

test_that("the IRF Word form prints the stored informant instructions, legend and APA notice", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5irf(file = f))
  runs <- irf_docx_runs(f)

  expect_true(irf_instruction_text() %in% runs)
  expect_false(pid_instructions$start %in% runs)

  got <- docx_legend_pairs(docx_legend_lines(f))
  expect_equal(got$value, as.character(pid_irf_instructions$options$value))
  expect_identical(got$label, pid_irf_instructions$options$label)

  footer <- read_docx_footer(f)
  expect_true(grepl(pid_irf_instructions$notice, footer, fixed = TRUE))
  expect_false(grepl("Hierarchical Taxonomy of Psychopathology Society", footer, fixed = TRUE))
})

test_that("the IRF scoring page lists each facet's informant items and R marks", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5irf(file = f))

  # Expected rows from the key's Facet Table and R marks, typed in
  # helper-fixtures.R: each facet's IRF items in ascending order, "(R)" on
  # the 14 reverse-scored items.
  expected <- vapply(
    irf_facets,
    function(i) {
      i <- sort(i)
      paste(ifelse(i %in% irf_reverse, paste0(i, "(R)"), i), collapse = ", ")
    },
    character(1)
  )
  printed <- docx_scoring_rows(f)
  expect_equal(nrow(printed), 25L)
  expect_setequal(printed$scale, names(irf_facets))
  expect_identical(printed$items, unname(expected[printed$scale]))
  # Not the self-report numbers: Anxiousness has 8 informant items, not 9.
  expect_identical(
    printed$items[printed$scale == "Anxiousness"],
    "79, 93, 95, 108, 109, 129, 140, 173"
  )
  expect_equal(sum(lengths(regmatches(printed$items, gregexpr("(R)", printed$items, fixed = TRUE)))), 14L)

  runs <- irf_docx_runs(f)
  msg <- runs[grepl("^Average the responses", runs)]
  expect_length(msg, 1L)
  expect_match(msg, "Reverse-scored items are indicated with (R).", fixed = TRUE)
})

test_that("include_scoring = FALSE drops the IRF scoring table", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5irf(file = f, include_scoring = FALSE))
  expect_equal(nrow(docx_scoring_rows(f)), 0L)
  expect_equal(nrow(docx_item_rows(f)), 218L)
})

test_that("the other PID-5 forms keep the Society footer", {
  skip_if_no_docx()
  for (gen in list(
    generate_docx_pid5,
    generate_docx_pid5sf,
    generate_docx_pid5bf,
    generate_docx_pid5bfpm
  )) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(gen(file = f))
    footer <- read_docx_footer(f)
    expect_true(grepl("Hierarchical Taxonomy of Psychopathology Society", footer, fixed = TRUE))
    expect_false(grepl("American Psychiatric Association", footer, fixed = TRUE))
  }
})

# ---- Qualtrics (AC2) ---------------------------------------------------------

test_that("the IRF Qualtrics file holds the 218 informant items in IRF order", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_pid5irf(file = f))
  q <- read_qualtrics(f)
  expected <- irf_expected()

  expect_true(q$advanced_format)
  expect_identical(q$block, "PID-5-IRF")
  expect_equal(q$questions$num, expected$number)
  expect_identical(q$questions$text, expected$text)
  expect_identical(q$questions$id, sprintf("PID5IRF_%03d", 1:218))

  opts <- pid_irf_instructions$options
  for (k in seq_len(nrow(opts))) {
    at <- which(q$lines == sprintf("[[Choice:%d]]", opts$value[k]))
    expect_length(at, 218L)
    expect_true(all(q$lines[at + 1L] == opts$label[k]))
  }
  expect_equal(q$choices$value, opts$value)
  expect_identical(q$choices$label, opts$label)

  ins <- which(q$lines == "[[ID:start_instructions]]")
  expect_length(ins, 1L)
  expect_identical(q$lines[ins + 1L], irf_instruction_text())
})

# ---- REDCap (AC2) ------------------------------------------------------------

test_that("the IRF REDCap dictionary holds the 218 informant items in IRF order", {
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_pid5irf(file = f))
  r <- read_redcap_csv(f)
  expected <- irf_expected()

  expect_equal(nrow(r), 219L) # 218 items + instruction row
  expect_identical(r[["Field Type"]][1], "descriptive")
  expect_identical(r[["Field Label"]][1], irf_instruction_text())

  items <- r[-1, ]
  expect_identical(items[["Variable / Field Name"]], sprintf("pid5irf_%03d", 1:218))
  expect_identical(items[["Field Label"]], expected$text)
  expect_true(all(items[["Field Type"]] == "radio"))

  opts <- pid_irf_instructions$options
  choices <- paste(opts$value, opts$label, sep = ", ", collapse = " | ")
  expect_true(all(items[["Choices, Calculations, OR Slider Labels"]] == choices))

  # The 218 field names are the ones rename_pid5_items() and label_pid5()
  # use, and they score with score_pid5(version = "IRF").
  legacy <- as.data.frame(
    matrix(0L, nrow = 1L, ncol = 218L, dimnames = list(NULL, paste0("pid_", 1:218)))
  )
  expect_identical(
    names(rename_pid5_items(legacy, version = "IRF")),
    items[["Variable / Field Name"]]
  )
  df <- as.data.frame(
    matrix(0L, nrow = 1L, ncol = 218L, dimnames = list(NULL, items[["Variable / Field Name"]]))
  )
  expect_no_error(score_pid5(df, items = names(df), version = "IRF", append = FALSE))
  labeled <- label_pid5(df, target = "items", version = "IRF")
  expect_identical(attr(labeled$pid5irf_001, "label"), expected$text[1])
})
