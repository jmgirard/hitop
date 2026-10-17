# The HiTOP-DAT Word form and REDCap dictionary, parsed back and compared to
# the source tables (`hitopdat_items`, `hitopdat_choices` and the internal
# `hitopdat_instructions`), never to a generated file. The expected values are
# built here from those tables.

dat_items <- function() hitopdat_items[order(hitopdat_items$Item), ]

# The (value, label) pairs of one answer set, in table order.
dat_set <- function(set) {
  ch <- hitopdat_choices[hitopdat_choices$Choice_Set == set, ]
  data.frame(value = as.character(ch$Value), label = ch$Label)
}

# The six instruction texts in battery order; the CAPE has none.
dat_instructions <- function() {
  measures <- unique(dat_items()$Measure)
  text <- unname(hitopdat_instructions$start[measures])
  data.frame(measure = measures[!is.na(text)], text = text[!is.na(text)])
}

ITEM_ROW <- "^([0-9]+)\\.  (.*)$"

for (ps in c("us", "a4")) {
  test_that(paste("the", ps, "Word form prints every item with its own answer values"), {
    skip_if_no_docx()
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(generate_docx_hitopdat(file = f, papersize = ps))
    blocks <- docx_blocks(f)
    tables <- Filter(function(b) b$type == "tbl", blocks)
    rows <- unlist(lapply(tables, `[[`, "rows"), recursive = FALSE)
    item_rows <- Filter(function(r) grepl(ITEM_ROW, r[1]), rows)

    items <- dat_items()
    # Exactly the 382 items, in battery order, with their numbers and text.
    expect_length(item_rows, nrow(items))
    first <- vapply(item_rows, `[`, character(1), 1)
    expect_equal(as.integer(sub(ITEM_ROW, "\\1", first)), items$Item)
    expect_equal(sub(ITEM_ROW, "\\2", first), items$Text)
    # Each row's option cells are its own answer set's values.
    for (k in seq_along(item_rows)) {
      expect_equal(
        item_rows[[k]][-1],
        dat_set(items$Choice_Set[k])$value,
        info = paste("item", items$Item[k])
      )
    }
  })

  test_that(paste("the", ps, "Word form's legends give each table's answer set"), {
    skip_if_no_docx()
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(generate_docx_hitopdat(file = f, papersize = ps))
    tables <- Filter(function(b) b$type == "tbl", docx_blocks(f))
    expect_gt(length(tables), 0)
    items <- dat_items()
    legend_sets <- list()
    for (tbl in tables) {
      firsts <- vapply(tbl$rows, `[`, character(1), 1)
      numbers <- as.integer(sub(ITEM_ROW, "\\1", grep(ITEM_ROW, firsts, value = TRUE)))
      sets <- unique(items$Choice_Set[match(numbers, items$Item)])
      # A table holds one answer set, so one legend can describe it.
      expect_length(sets, 1)
      legend <- grep("^[0-9]+ = ", firsts, value = TRUE)
      got <- docx_legend_pairs(legend)
      want <- dat_set(sets)
      expect_equal(got$value, want$value, info = sets)
      expect_equal(got$label, want$label, info = sets)
      measure <- unique(items$Measure[match(numbers, items$Item)])
      legend_sets[[measure]] <- union(legend_sets[[measure]], sets)
    }
    # Every measure's legends together give every answer set it uses.
    for (m in unique(items$Measure)) {
      expect_setequal(legend_sets[[m]], unique(items$Choice_Set[items$Measure == m]))
    }
  })

  test_that(paste("the", ps, "Word form prints each instruction once, before its measure"), {
    skip_if_no_docx()
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(generate_docx_hitopdat(file = f, papersize = ps))
    blocks <- docx_blocks(f)
    is_text <- vapply(blocks, function(b) b$type == "p" && nzchar(b$text), logical(1))
    want <- dat_instructions()
    # Exactly the six texts, line breaks included, in battery order.
    expect_equal(vapply(blocks[is_text], `[[`, character(1), "text"), want$text)
    # The next block after each is a table opening on its measure's first item.
    items <- dat_items()
    for (k in seq_len(nrow(want))) {
      at <- which(is_text)[k]
      nxt <- blocks[[at + 1L]]
      expect_equal(nxt$type, "tbl", info = want$measure[k])
      firsts <- vapply(nxt$rows, `[`, character(1), 1)
      first_item <- as.integer(sub(ITEM_ROW, "\\1", grep(ITEM_ROW, firsts, value = TRUE)[1]))
      expect_equal(
        first_item,
        min(items$Item[items$Measure == want$measure[k]]),
        info = want$measure[k]
      )
    }
  })
}

test_that("the DUDIT instruction keeps its list lines as line breaks", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopdat(file = f))
  xml <- read_docx_xml(f)
  # The control: the text holds "\n", and the form writes one Word line
  # break for each, not the characters themselves. No other instruction has
  # a "\n", so the document's line breaks are the DUDIT's alone.
  dudit <- hitopdat_instructions$start[["DUDIT"]]
  n_lines <- lengths(regmatches(dudit, gregexpr("\n", dudit, fixed = TRUE)))
  expect_gt(n_lines, 0)
  breaks <- gregexpr('<w:br w:type="textWrapping"/>', xml, fixed = TRUE)[[1]]
  expect_equal(sum(breaks > 0), n_lines)
})

# Which legends print on one line and which split two options per line. The
# rule is a layout choice, stated here as the line count each set's legend
# takes: at the default 10-point font the five-option sets with long labels
# split, and the shorter sets do not; at 16 points every set splits.
legend_lines <- function(file) {
  tables <- Filter(function(b) b$type == "tbl", docx_blocks(file))
  items <- dat_items()
  out <- list()
  for (tbl in tables) {
    firsts <- vapply(tbl$rows, `[`, character(1), 1)
    number <- as.integer(sub(ITEM_ROW, "\\1", grep(ITEM_ROW, firsts, value = TRUE)[1]))
    out[[items$Choice_Set[items$Item == number]]] <- sum(grepl("^[0-9]+ = ", firsts))
  }
  unlist(out)
}

test_that("a legend splits two options per line only when it is long", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopdat(file = f))
  got <- legend_lines(f)
  one_line <- c("whodas", "audit_drinks", "audit_harm", "dudit_times", "dudit_harm", "cape", "phq15")
  split <- c("idas", "audit_freq", "audit_often", "dudit_freq", "dudit_often", "catpd")
  expect_setequal(names(got), c(one_line, split))
  expect_true(all(got[one_line] == 1), info = paste(one_line, got[one_line], collapse = " "))
  # Five options at two per line take three lines.
  expect_true(all(got[split] == 3), info = paste(split, got[split], collapse = " "))

  f16 <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopdat(file = f16, font_size = 16))
  got16 <- legend_lines(f16)
  n_opts <- table(hitopdat_choices$Choice_Set)
  expect_equal(unname(got16), unname(as.vector(ceiling(n_opts[names(got16)] / 2))))
})

for (ps in c("us", "a4")) test_that(paste("the", ps, "Word footer names the source of each of the seven measures"), {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopdat(file = f, papersize = ps))
  footer <- read_docx_footer(f)
  # Stated here from the revised manual's "Measures and reference data"
  # headings (cairn/SOURCES.md, "HiTOP-DAT"), not read from the package: the
  # WHODAS without the "& others, 1988" its heading prints (DAT-16).
  sources <- c(
    "WHODAS (World Health Organization)",
    "IDAS-II (Watson et al., 2012)",
    "AUDIT (Saunders et al., 1993)",
    "DUDIT (Berman et al., 2007)",
    "CAPE (Stefanis et al., 2002)",
    "CAT-PD (Simms et al., 2011)",
    "PHQ-15 (Kroenke et al., 2002)"
  )
  for (s in sources) expect_match(footer, s, fixed = TRUE)
  expect_match(footer, "Copyright", fixed = TRUE)
  expect_no_match(footer, "Hierarchical Taxonomy of Psychopathology Society", fixed = TRUE)
})

test_that("the Word form has no scoring page", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopdat(file = f))
  xml <- read_docx_xml(f)
  expect_no_match(xml, "Scoring Instructions", fixed = TRUE)
  expect_no_match(xml, '<w:br w:type="page"/>', fixed = TRUE)
  expect_equal(nrow(docx_scoring_rows(f)), 0)
})

test_that("the REDCap dictionary holds each item with its own answer set", {
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopdat(file = f))
  dd <- read_redcap_csv(f)
  items <- dat_items()
  fields <- dd[dd[["Field Type"]] == "radio", ]
  # Exactly hdat_001 to hdat_382, in battery order, with each item's text.
  expect_equal(fields[["Variable / Field Name"]], sprintf("hdat_%03d", 1:382))
  expect_equal(fields[["Field Label"]], items$Text)
  # Each choices string read back into its (value, label) pairs. REDCap
  # splits a pair at its first comma, so a label holding one survives.
  for (k in seq_len(nrow(fields))) {
    pairs <- strsplit(fields[["Choices, Calculations, OR Slider Labels"]][k], " | ", fixed = TRUE)[[1]]
    got <- data.frame(
      value = sub("^([^,]*), (.*)$", "\\1", pairs),
      label = sub("^([^,]*), (.*)$", "\\2", pairs)
    )
    expect_equal(got, dat_set(items$Choice_Set[k]), info = fields[k, 1])
  }
  # The dictionary holds the items and the instructions and nothing else.
  expect_setequal(unique(dd[["Field Type"]]), c("radio", "descriptive"))
})

test_that("the REDCap dictionary puts each instruction before its measure's first item", {
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopdat(file = f))
  dd <- read_redcap_csv(f)
  want <- dat_instructions()
  desc <- which(dd[["Field Type"]] == "descriptive")
  expect_equal(dd[["Field Label"]][desc], want$text)
  # Named for the measure, as the help page says.
  expect_equal(
    dd[["Variable / Field Name"]][desc],
    c(
      "hdat_whodas_instructions", "hdat_idas_ii_instructions",
      "hdat_audit_instructions", "hdat_dudit_instructions",
      "hdat_cat_pd_instructions", "hdat_phq_15_instructions"
    )
  )
  items <- dat_items()
  for (k in seq_len(nrow(want))) {
    first <- min(items$Item[items$Measure == want$measure[k]])
    expect_equal(
      dd[["Variable / Field Name"]][desc[k] + 1L],
      sprintf("hdat_%03d", first),
      info = want$measure[k]
    )
  }
})

test_that("each REDCap measure after the first starts a page, and breaks count from it", {
  items <- dat_items()
  # The field that opens each measure: its instruction, or its first item for
  # the CAPE.
  opener <- function(dd, measure) {
    first <- sprintf("hdat_%03d", min(items$Item[items$Measure == measure]))
    at <- match(first, dd[["Variable / Field Name"]])
    if (dd[["Field Type"]][at - 1L] == "descriptive") at - 1L else at
  }
  measures <- unique(items$Measure)

  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopdat(file = f, breaks = 0))
  dd <- read_redcap_csv(f)
  opens <- vapply(measures, opener, integer(1), dd = dd)
  # With no item breaks, the only page breaks are the measures' openings.
  expect_equal(which(nzchar(dd[["Section Header"]])), unname(opens[-1]))
  expect_equal(dd[["Section Header"]][opens[-1]], rep("<br>", length(measures) - 1L))

  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopdat(file = f, breaks = 10))
  dd <- read_redcap_csv(f)
  # Within a measure, a break falls on its 11th, 21st, ... item.
  want <- unlist(lapply(measures, function(m) {
    n <- items$Item[items$Measure == m]
    sprintf("hdat_%03d", n[seq_along(n) %% 10 == 1 & seq_along(n) > 1])
  }))
  item_breaks <- dd[["Variable / Field Name"]][
    nzchar(dd[["Section Header"]]) & dd[["Field Type"]] == "radio"
  ]
  # The CAPE's first item opens its measure and is the one item-row break
  # that is a measure's opening rather than an item break.
  cape_first <- sprintf("hdat_%03d", min(items$Item[items$Measure == "CAPE"]))
  expect_setequal(item_breaks, c(want, cape_first))

  # An item break restates its measure's instruction, so a later page does
  # not show items with no stem; the CAPE, with none, breaks with "<br>".
  instruction <- hitopdat_instructions$start
  for (field in setdiff(item_breaks, cape_first)) {
    measure <- items$Measure[items$Item == as.integer(sub("^hdat_", "", field))]
    want_header <- if (is.na(instruction[[measure]])) "<br>" else unname(instruction[[measure]])
    expect_equal(
      dd[["Section Header"]][dd[["Variable / Field Name"]] == field],
      want_header,
      info = field
    )
  }
  expect_equal(dd[["Section Header"]][dd[["Variable / Field Name"]] == cape_first], "<br>")
})

# ---- Qualtrics import file ---------------------------------------------------

test_that("the Qualtrics file holds each item with its own answer set and recode values", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopdat(file = f))
  expect_identical(readLines(f, n = 1L), "[[AdvancedFormat]]")
  qs <- read_qualtrics_questions(f)
  mc <- Filter(function(q) q$type == "MC", qs)
  items <- dat_items()
  # Exactly hdat_001 to hdat_382, in battery order, with each item's text.
  expect_identical(vapply(mc, `[[`, "", "id"), sprintf("hdat_%03d", 1:382))
  expect_identical(vapply(mc, `[[`, "", "text"), items$Text)
  # Each item's [[Choice:value]] values and labels are its answer set's, in
  # the set's order: the [[Choice:<value>]] tags the help page names.
  for (k in seq_along(mc)) {
    expect_identical(mc[[k]]$choices, dat_set(items$Choice_Set[k]), info = mc[[k]]$id)
  }
  # Every question is an item (MC) or an instruction (DB).
  expect_setequal(vapply(qs, `[[`, "", "type"), c("MC", "DB"))
})

test_that("the Qualtrics file puts each instruction once, before its measure's first item", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopdat(file = f))
  qs <- read_qualtrics_questions(f)
  want <- dat_instructions()
  db <- which(vapply(qs, `[[`, "", "type") == "DB")
  # The six texts, each once, in battery order; the DUDIT's keeps its lines.
  expect_identical(vapply(qs[db], `[[`, "", "text"), want$text)
  # Named for the measure, as the help page says.
  expect_identical(
    vapply(qs[db], `[[`, "", "id"),
    c(
      "hdat_whodas_instructions", "hdat_idas_ii_instructions",
      "hdat_audit_instructions", "hdat_dudit_instructions",
      "hdat_cat_pd_instructions", "hdat_phq_15_instructions"
    )
  )
  items <- dat_items()
  for (k in seq_len(nrow(want))) {
    first <- min(items$Item[items$Measure == want$measure[k]])
    expect_identical(qs[[db[k] + 1L]]$id, sprintf("hdat_%03d", first), info = want$measure[k])
  }

  # Without instructions the file holds the items alone.
  g <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopdat(file = g, include_instructions = FALSE))
  expect_identical(unique(vapply(read_qualtrics_questions(g), `[[`, "", "type")), "MC")
})

test_that("each Qualtrics measure after the first starts a page, and breaks count from it", {
  items <- dat_items()
  # The item each page break comes before: the next item question after it.
  break_before <- function(file) {
    lines <- readLines(file, warn = FALSE)
    ids <- grep("^\\[\\[ID:hdat_[0-9]+\\]\\]$", lines)
    vapply(which(lines == "[[PageBreak]]"), function(p) {
      as.integer(sub("^\\[\\[ID:hdat_0*([0-9]+)\\]\\]$", "\\1", lines[min(ids[ids > p])]))
    }, integer(1))
  }
  firsts <- vapply(unique(items$Measure), function(m) min(items$Item[items$Measure == m]), integer(1))

  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopdat(file = f, breaks = 0))
  expect_identical(break_before(f), unname(firsts[-1]))

  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopdat(file = f, breaks = 10))
  # Within a measure, a break falls before its 11th, 21st, ... item.
  within <- unlist(lapply(unique(items$Measure), function(m) {
    n <- items$Item[items$Measure == m]
    n[seq_along(n) %% 10 == 1 & seq_along(n) > 1]
  }))
  expect_identical(break_before(f), sort(c(within, unname(firsts[-1]))))
})

# ---- Refusals (AC4) ---------------------------------------------------------
#
# Each formal of the three generators maps to a bad value and the words its
# check prints. The map is checked against formals(), so an argument added
# later fails here until it has a refusal.
DAT_REFUSALS <- list(
  generate_docx_hitopdat = list(
    file = list(1, "`file` argument must be a single string"),
    papersize = list("legal", "should be one of"),
    title = list(c("a", "b"), "`title` argument must be a single string"),
    font_size = list("10", "`font_size` argument must be a single number greater than 0"),
    font_family = list(NA, "`font_family` argument must be a single string")
  ),
  generate_redcap_hitopdat = list(
    file = list(c("a.zip", "b.zip"), "`file` argument must be a single string"),
    form_name = list(1, "`form_name` argument must be a single string"),
    required = list("yes", "`required` argument must be `TRUE` or `FALSE`"),
    breaks = list(1.5, "`breaks` argument must be a single whole number or NULL")
  ),
  generate_qualtrics_hitopdat = list(
    file = list(c("a.txt", "b.txt"), "`file` argument must be a single string"),
    block_name = list(1, "`block_name` argument must be a single string or NULL"),
    id_prefix = list(NA_character_, "`id_prefix` argument must be a single string"),
    include_instructions = list(NA, "`include_instructions` argument must be `TRUE` or `FALSE`"),
    breaks = list(-1, "`breaks` argument is out of range")
  )
)

for (fn_name in names(DAT_REFUSALS)) {
  test_that(paste(fn_name, "refuses a bad value of each argument"), {
    if (startsWith(fn_name, "generate_docx")) skip_if_no_docx()
    fn <- getExportedValue("hitop", fn_name)
    refusals <- DAT_REFUSALS[[fn_name]]
    expect_setequal(names(refusals), names(formals(fn)))
    ext <- switch(
      sub("^generate_([a-z]+)_.*$", "\\1", fn_name),
      docx = ".docx", redcap = ".zip", qualtrics = ".txt"
    )
    for (arg in names(refusals)) {
      out <- withr::local_tempfile(fileext = ext)
      call_args <- list(file = out)
      call_args[arg] <- list(refusals[[arg]][[1]])
      err <- expect_error(suppressMessages(do.call(fn, call_args)), info = arg)
      expect_match(
        cli::ansi_strip(conditionMessage(err)),
        refusals[[arg]][[2]],
        fixed = TRUE,
        info = arg
      )
      # Refused before anything was written.
      expect_false(file.exists(out), info = arg)
    }
  })
}

test_that("font_size refuses zero, a negative, two values and NA, and takes 10.5", {
  skip_if_no_docx()
  for (bad in list(0, -1, c(10, 12), NA_real_, Inf)) {
    expect_error(
      generate_docx_hitopdat(file = withr::local_tempfile(fileext = ".docx"), font_size = bad),
      "must be a single number greater than 0"
    )
  }
  # The control: a size that is not whole is a size Word prints.
  expect_no_error(suppressMessages(
    generate_docx_hitopdat(file = withr::local_tempfile(fileext = ".docx"), font_size = 10.5)
  ))
})

test_that("breaks refuses a negative count and takes 0 and NULL", {
  expect_error(
    generate_redcap_hitopdat(file = withr::local_tempfile(fileext = ".zip"), breaks = -1),
    "out of range"
  )
  for (ok in list(0, NULL)) {
    expect_no_error(suppressMessages(generate_redcap_hitopdat(
      file = withr::local_tempfile(fileext = ".zip"),
      breaks = ok
    )))
  }
})

test_that("the Qualtrics generator refuses a breaks that is not whole and takes 0 and NULL", {
  expect_error(
    generate_qualtrics_hitopdat(file = withr::local_tempfile(fileext = ".txt"), breaks = 1.5),
    "must be a single whole number or NULL"
  )
  for (ok in list(0, NULL)) {
    expect_no_error(suppressMessages(generate_qualtrics_hitopdat(
      file = withr::local_tempfile(fileext = ".txt"),
      breaks = ok
    )))
  }
})

# ---- The other instruments' forms are unchanged -----------------------------
#
# The shared builders gained per-item answer sets and per-measure sections for
# the HiTOP-DAT. A no-regression lock (D-054), not an oracle: each other
# instrument's fresh Word form and REDCap dictionary parse the same as the
# committed download file of the same name. The scoring table is compared as
# a set of (scale, items) rows: make_scoring_table() sorts the scales with
# order(), and testthat runs under C collation, which places "p-Factor" after
# the capitalized names where the build that wrote the committed files did
# not. Every other block is compared exactly, in order.
form_parse <- function(file) {
  blocks <- docx_blocks(file)
  is_scoring <- vapply(
    blocks,
    function(b) b$type == "tbl" && identical(b$rows[[1]][1], "Scale"),
    logical(1)
  )
  scoring <- docx_scoring_rows(file)
  scoring <- scoring[order(scoring$scale, scoring$items), ]
  rownames(scoring) <- NULL
  list(blocks = blocks[!is_scoring], scoring = scoring)
}

test_that("every other instrument's fresh Word form parses as its committed file", {
  skip_if_no_docx()
  extdata <- system.file("extdata", package = "hitop")
  m <- latest_manifest()
  m <- m[m$format %in% c("docx_us", "docx_a4") & m$instrument != "HiTOP-DAT", ]
  expect_gt(nrow(m), 0)
  gens <- c(
    hitopsr = "generate_docx_hitopsr", hitopbr = "generate_docx_hitopbr",
    hitophsum = "generate_docx_hitophsum", pid5 = "generate_docx_pid5",
    pid5sf = "generate_docx_pid5sf", pid5bf = "generate_docx_pid5bf",
    pid5bfpm = "generate_docx_pid5bfpm", pid5irf = "generate_docx_pid5irf",
    pid5child = "generate_docx_pid5child", pid5bfchild = "generate_docx_pid5bfchild"
  )
  stems <- sub("_(US|A4)\\.docx$", "", m$file)
  expect_setequal(unique(stems), names(gens))
  for (i in seq_len(nrow(m))) {
    f <- withr::local_tempfile(fileext = ".docx")
    ps <- tolower(sub("^.*_(US|A4)\\.docx$", "\\1", m$file[i]))
    suppressMessages(getExportedValue("hitop", gens[[stems[i]]])(file = f, papersize = ps))
    expect_identical(
      form_parse(f),
      form_parse(file.path(extdata, m$file[i])),
      info = m$file[i]
    )
    # docx_blocks() reads a line break and a literal "\n" alike, so the line
    # breaks are counted too: a form whose text gained a "\n" would show here.
    breaks <- function(file) {
      sum(gregexpr('<w:br w:type="textWrapping"/>', read_docx_xml(file), fixed = TRUE)[[1]] > 0)
    }
    expect_equal(breaks(f), breaks(file.path(extdata, m$file[i])), info = m$file[i])
  }
})

test_that("every other instrument's fresh REDCap dictionary equals its committed file", {
  extdata <- system.file("extdata", package = "hitop")
  m <- latest_manifest()
  m <- m[m$format == "redcap" & m$instrument != "HiTOP-DAT", ]
  expect_gt(nrow(m), 0)
  for (i in seq_len(nrow(m))) {
    stem <- sub("_redcap\\.zip$", "", m$file[i])
    f <- withr::local_tempfile(fileext = ".zip")
    suppressMessages(getExportedValue("hitop", paste0("generate_redcap_", stem))(file = f))
    expect_identical(
      read_redcap_csv(f),
      read_redcap_csv(file.path(extdata, m$file[i])),
      info = m$file[i]
    )
  }
})

# The other instruments' Qualtrics files are flat text, so their lock is a
# byte comparison, in test-response-value-no-move.R.
