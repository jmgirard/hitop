# Parse-back tests for the PID-5 child form (ages 11 to 17) Word, Qualtrics
# and REDCap generators (M161).
#
# D-010 style: each generated file is parsed back and compared with the source
# tables (`pid_items`, `pid_scales`), the stored `pid_child_instructions`, and
# text typed from the APA child forms (apa2013pid5child.pdf and
# apa2013pid5bfchild.pdf), never with the generator's own output.

# The child forms' items are the adult version's, in the same order
# (cairn/references/apa2013pid5child.md), looked up from `pid_items` by number.
child_expected <- function(version) {
  n <- seq_len(max(pid_items[[version]], na.rm = TRUE))
  data.frame(
    number = n,
    text = pid_items$Text[match(n, pid_items[[version]])],
    stringsAsFactors = FALSE
  )
}

# Typed from the first item page of each child form (PDF p. 2), without the
# label ("Instructions to the child receiving care:" or "Instructions:") and
# with ASCII apostrophes and quotes. The full form quotes "right" and "wrong";
# the brief form does not.
child_start_typed <- list(
  FULL = paste(
    "This is a list of things different people might say about themselves.",
    "We are interested in how you would describe yourself. There are no",
    "\"right\" or \"wrong\" answers. So you can describe yourself as honestly",
    "as possible, we will keep your responses confidential. We'd like you to",
    "take your time and read each statement carefully, selecting the response",
    "that best describes you."
  ),
  BF = paste(
    "This is a list of things different people might say about themselves.",
    "We are interested in how you would describe yourself. There are no right",
    "or wrong answers. So you can describe yourself as honestly as possible,",
    "we will keep your responses confidential. We'd like you to take your time",
    "and read each statement carefully, selecting the response that best",
    "describes you."
  )
)

# Typed from the foot of each child form's item pages. The full form prints
# "All rights reserved", the brief form "All Rights Reserved".
child_notice_typed <- list(
  FULL = paste(
    "Krueger RF, Derringer J, Markon KE, Watson D, Skodol AE. Copyright © 2013",
    "American Psychiatric Association. All rights reserved. This material can",
    "be reproduced without permission by researchers and by clinicians for use",
    "with their patients."
  ),
  BF = paste(
    "Krueger RF, Derringer J, Markon KE, Watson D, Skodol AE. Copyright © 2013",
    "American Psychiatric Association. All Rights Reserved. This material can",
    "be reproduced without permission by researchers and by clinicians for use",
    "with their patients."
  )
)

# Typed from the column heads of both child forms.
child_labels_typed <- c(
  "Very False or Often False",
  "Sometimes or Somewhat False",
  "Sometimes or Somewhat True",
  "Very True or Often True"
)

child_docx_runs <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  unescape_xml(gsub("<[^>]+>", "", runs))
}

child_gens <- list(
  FULL = list(
    docx = generate_docx_pid5child,
    qualtrics = generate_qualtrics_pid5child,
    redcap = generate_redcap_pid5child,
    title = "PID-5 (Full), Child Age 11\u201317",
    block = "PID-5 Child",
    qid = sprintf("PID5_%03d", 1:220),
    field = sprintf("pid5_%03d", 1:220),
    form_name = "pid5child_questionnaire"
  ),
  BF = list(
    docx = generate_docx_pid5bfchild,
    qualtrics = generate_qualtrics_pid5bfchild,
    redcap = generate_redcap_pid5bfchild,
    title = "PID-5-BF, Child Age 11\u201317",
    block = "PID-5-BF Child",
    qid = sprintf("PID5BF_%02d", 1:25),
    field = sprintf("pid5bf_%02d", 1:25),
    form_name = "pid5bfchild_questionnaire"
  )
)

test_that("the stored child instructions are the APA child forms' text", {
  for (v in c("FULL", "BF")) {
    instr <- pid_child_instructions[[v]]
    expect_identical(instr$start, child_start_typed[[v]], info = v)
    expect_identical(instr$notice, child_notice_typed[[v]], info = v)
    expect_identical(instr$options$label, child_labels_typed, info = v)
    expect_identical(instr$options$value, 0:3, info = v)
  }
  # The full child form's paragraph is not the adult one; the brief child
  # form's paragraph is.
  expect_false(identical(pid_child_instructions$FULL$start, pid_instructions$start))
  expect_identical(pid_child_instructions$BF$start, pid_instructions$start)
})

# ---- Word --------------------------------------------------------------------

test_that("the child Word forms print exactly the adult items in order, on both paper sizes", {
  skip_if_no_docx()
  for (v in c("FULL", "BF")) {
    g <- child_gens[[v]]
    expected <- child_expected(v)
    expect_false(anyNA(expected$text))
    for (paper in c("us", "a4")) {
      f <- withr::local_tempfile(fileext = ".docx")
      suppressMessages(g$docx(file = f, papersize = paper))
      info <- paste(v, paper)
      size <- docx_page_size(read_docx_xml(f))
      want <- if (paper == "us") c(12240L, 15840L) else c(11906L, 16838L)
      expect_true(all(abs(unname(size) - want) < 12), info = info)

      rows <- docx_item_rows(f)
      expect_equal(as.integer(rows$number), expected$number, info = info)
      expect_identical(rows$text, expected$text, info = info)

      runs <- child_docx_runs(f)
      expect_true(child_start_typed[[v]] %in% runs, info = info)
      # Two response options per legend line, as the other PID-5 forms print
      # them (D-028), so the 4 options take 2 lines.
      expect_length(docx_legend_lines(f), 2L)
      got <- docx_legend_pairs(docx_legend_lines(f))
      expect_equal(got$value, as.character(0:3), info = info)
      expect_identical(got$label, child_labels_typed, info = info)

      footer <- read_docx_footer(f)
      expect_true(grepl(child_notice_typed[[v]], footer, fixed = TRUE), info = info)
      expect_false(
        grepl("Hierarchical Taxonomy of Psychopathology Society", footer, fixed = TRUE),
        info = info
      )
      expect_identical(docx_header_title(f), g$title, info = info)
    }
  }
})

test_that("the child scoring pages list each scale's items and R marks from the keying tables", {
  skip_if_no_docx()
  for (v in c("FULL", "BF")) {
    g <- child_gens[[v]]
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(g$docx(file = f))
    printed <- docx_scoring_rows(f)

    # Expected rows built from `pid_scales` and `pid_items$Reverse`, not from
    # any generator's output: each scale's items in ascending order, with
    # "(R)" on the reverse-keyed ones.
    scales <- pid_scales[[v]]
    scale_col <- if (v == "BF") "Domain" else "Facet"
    reversed <- pid_items[[v]][!is.na(pid_items[[v]]) & pid_items$Reverse]
    expected <- vapply(scales$itemNumbers, function(i) {
      i <- sort(i)
      paste(ifelse(i %in% reversed, paste0(i, "(R)"), i), collapse = ", ")
    }, character(1))
    names(expected) <- scales[[scale_col]]

    expect_equal(nrow(printed), if (v == "FULL") 25L else 6L, info = v)
    expect_setequal(printed$scale, names(expected))
    expect_identical(printed$items, unname(expected[printed$scale]), info = v)
  }
  # Anchors typed from the child keys: the full form's Facet Table (p. 8)
  # and the brief form's Domain Scoring table (p. 3).
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5child(file = f))
  printed <- docx_scoring_rows(f)
  expect_identical(
    printed$items[printed$scale == "Anhedonia"],
    "1, 23, 26, 30(R), 124, 155(R), 157, 189"
  )
  expect_identical(
    printed$items[printed$scale == "Suspiciousness"],
    "2, 103, 117, 131(R), 133, 177(R), 190"
  )
  b <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_pid5bfchild(file = b))
  printed <- docx_scoring_rows(b)
  expect_identical(printed$items[printed$scale == "Disinhibition"], "1, 2, 3, 5, 6")
})

test_that("include_scoring = FALSE drops the child scoring tables", {
  skip_if_no_docx()
  for (v in c("FULL", "BF")) {
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(child_gens[[v]]$docx(file = f, include_scoring = FALSE))
    expect_equal(nrow(docx_scoring_rows(f)), 0L, info = v)
    expect_equal(nrow(docx_item_rows(f)), nrow(child_expected(v)), info = v)
  }
})

# ---- Qualtrics ---------------------------------------------------------------

test_that("the child Qualtrics files hold the adult items under the adult IDs", {
  for (v in c("FULL", "BF")) {
    g <- child_gens[[v]]
    f <- withr::local_tempfile(fileext = ".txt")
    suppressMessages(g$qualtrics(file = f))
    q <- read_qualtrics(f)
    expected <- child_expected(v)

    expect_true(q$advanced_format, info = v)
    expect_identical(q$block, g$block, info = v)
    expect_equal(q$questions$num, expected$number, info = v)
    expect_identical(q$questions$text, expected$text, info = v)
    expect_identical(q$questions$id, g$qid, info = v)

    for (k in 1:4) {
      at <- which(q$lines == sprintf("[[Choice:%d]]", k - 1L))
      expect_length(at, nrow(expected))
      expect_true(all(q$lines[at + 1L] == child_labels_typed[k]), info = paste(v, k))
    }
    ins <- which(q$lines == "[[ID:start_instructions]]")
    expect_length(ins, 1L)
    expect_identical(q$lines[ins + 1L], child_start_typed[[v]], info = v)
  }
})

# ---- REDCap ------------------------------------------------------------------

test_that("the child REDCap dictionaries hold the adult items under the adult field names", {
  for (v in c("FULL", "BF")) {
    g <- child_gens[[v]]
    f <- withr::local_tempfile(fileext = ".zip")
    suppressMessages(g$redcap(file = f))
    r <- read_redcap_csv(f)
    expected <- child_expected(v)

    expect_equal(nrow(r), nrow(expected) + 1L, info = v)
    expect_identical(r[["Field Type"]][1], "descriptive", info = v)
    expect_identical(r[["Field Label"]][1], child_start_typed[[v]], info = v)
    expect_true(all(r[["Form Name"]] == g$form_name), info = v)

    items <- r[-1, ]
    expect_identical(items[["Variable / Field Name"]], g$field, info = v)
    expect_identical(items[["Field Label"]], expected$text, info = v)
    expect_true(all(items[["Field Type"]] == "radio"), info = v)
    choices <- paste(0:3, child_labels_typed, sep = ", ", collapse = " | ")
    expect_true(all(items[["Choices, Calculations, OR Slider Labels"]] == choices), info = v)

    # The field names are the adult form's: rename_pid5_items() writes them,
    # score_pid5() scores them and label_pid5() labels them, with no rename.
    n <- nrow(expected)
    legacy <- as.data.frame(
      matrix(0L, nrow = 1L, ncol = n, dimnames = list(NULL, paste0("pid_", seq_len(n))))
    )
    expect_identical(
      names(rename_pid5_items(legacy, version = v)),
      items[["Variable / Field Name"]],
      info = v
    )
    # Scores checked against values worked by hand from the child keys. Full
    # form, every item answered 1: Anhedonia's 8 items include 2 reversed
    # (30R, 155R, Facet Table p. 8), which score 2, so (6 * 1 + 2 * 2) / 8 =
    # 1.25. Brief form, item 8 answered 3 and the rest 0: item 8 is one of
    # Negative Affect's 5 items (8, 9, 10, 11, 15, p. 3), so 3 / 5 = 0.6, and
    # Detachment (4, 13, 14, 16, 18) is 0.
    if (v == "FULL") {
      df <- as.data.frame(
        matrix(1L, nrow = 1L, ncol = n, dimnames = list(NULL, g$field))
      )
      scored <- score_pid5(df, items = g$field, version = v, append = FALSE)
      expect_equal(scored$pid_anhedonia, 1.25)
    } else {
      df <- as.data.frame(
        matrix(0L, nrow = 1L, ncol = n, dimnames = list(NULL, g$field))
      )
      df$pid5bf_08 <- 3L
      scored <- score_pid5(df, items = g$field, version = v, append = FALSE)
      expect_equal(scored$pid_negativeAffectivity, 0.6)
      expect_equal(scored$pid_detachment, 0)
    }
    labeled <- label_pid5(df, target = "items", version = v)
    expect_identical(attr(labeled[[g$field[1]]], "label"), expected$text[1], info = v)
  }
})

# ---- Non-default arguments ---------------------------------------------------

test_that("the child generators pass their non-default arguments through", {
  skip_if_no_docx()
  for (v in c("FULL", "BF")) {
    g <- child_gens[[v]]
    n <- nrow(child_expected(v))

    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(g$docx(
      file = f, papersize = "a4", title = "My child form",
      include_scoring = FALSE, font_size = 12, font_family = "Arial"
    ))
    expect_identical(docx_header_title(f), "My child form", info = v)
    expect_equal(nrow(docx_scoring_rows(f)), 0L, info = v)
    xml <- read_docx_xml(f)
    expect_match(xml, 'w:ascii="Arial"', fixed = TRUE, info = v)
    expect_false(grepl("Times New Roman", xml, fixed = TRUE), info = v)
    expect_match(xml, '<w:sz w:val="24"', fixed = TRUE, info = v)

    q <- withr::local_tempfile(fileext = ".txt")
    suppressMessages(g$qualtrics(
      file = q, block_name = "Kids", id_prefix = "KID",
      include_instructions = FALSE, breaks = NULL
    ))
    parsed <- read_qualtrics(q)
    expect_identical(parsed$block, "Kids", info = v)
    expect_true(all(startsWith(parsed$questions$id, "KID_")), info = v)
    expect_false(any(parsed$lines == "[[ID:start_instructions]]"), info = v)
    expect_false(any(parsed$lines == "[[PageBreak]]"), info = v)

    r <- withr::local_tempfile(fileext = ".zip")
    suppressMessages(g$redcap(
      file = r, form_name = "kids_form", required = FALSE, breaks = NULL
    ))
    dd <- read_redcap_csv(r)
    expect_true(all(dd[["Form Name"]] == "kids_form"), info = v)
    expect_true(all(dd[["Required Field?"]][-1] == "n"), info = v)
    expect_true(all(dd[["Section Header"]] == ""), info = v)
    expect_equal(nrow(dd), n + 1L, info = v)
  }
})
