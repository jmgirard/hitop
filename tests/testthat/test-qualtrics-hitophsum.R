# Ground-truth verification of the committed HiTOP-HSUM Qualtrics QSF
# (inst/extdata/hitophsum_qualtrics.qsf) against the keying tables (M19).
#
# Oracle: parse the QSF's JSON and compare every question's text, choices,
# and display logic to expectations derived independently from
# `hitophsum_items`/`hitophsum_choices` (which trace to the Aug 2024 source
# sheet; cairn/SOURCES.md) — never from the QSF itself. The deliberate text
# adaptations mirrored in qsf_expected_text() are documented in
# devel/qualtrics_hitophsum.R (the maintainer script that builds the survey)
# and cairn/SOURCES.md.

# base R's %||% needs 4.4; the package floor is 4.1 (DESIGN.md).
`%||%` <- function(x, y) if (is.null(x)) y else x

have_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)

qsf_questions <- NULL
if (have_jsonlite) {
  qsf_path <- system.file(
    "extdata", "hitophsum_qualtrics.qsf",
    package = "hitop"
  )
  qsf <- jsonlite::fromJSON(qsf_path, simplifyVector = FALSE)
  qsf_questions <- Filter(
    function(e) identical(e$Element, "SQ"),
    qsf$SurveyElements
  )
}

qsf_tags <- function() {
  vapply(
    qsf_questions,
    function(e) e$Payload$DataExportTag %||% NA_character_,
    character(1)
  )
}

# Payload of the first question carrying a tag (first occurrence keeps the
# helpers informative, not erroring, while duplicates exist).
qsf_payload <- function(tag) {
  qsf_questions[[which(qsf_tags() == tag)[1]]]$Payload
}

# The QID Qualtrics assigned to a tag, read from the file itself (QIDs are
# structural, not content, so resolving them from the QSF is not
# self-reference).
qsf_qid <- function(tag) {
  p <- qsf_payload(tag)
  p$QuestionID
}

# Expected QSF question text: hitophsum_items$Text plus the two documented
# adaptations, applied in the script's order.
qsf_expected_text <- function(text, oth_piped) {
  # 1. Withdrawal-style items ("stem: symptom") bold the symptom.
  if (grepl(":\\s*\\S", text)) {
    text <- sub("(.*?:\\s*)(.*)", "\\1<br><br><b>\\2</b>", text)
  }
  # 2. The "other" substance name is piped from the specify field.
  if (grepl("other specified substances", text, fixed = TRUE)) {
    text <- gsub("other specified substances", oth_piped, text, fixed = TRUE)
  }
  text
}

# Flatten a DisplayLogic tree into its Expression nodes.
qsf_logic_exprs <- function(dl) {
  out <- list()
  walk <- function(x) {
    if (is.list(x)) {
      if (identical(x$Type, "Expression")) {
        out[[length(out) + 1]] <<- x
      } else {
        for (v in x) walk(v)
      }
    }
  }
  walk(dl)
  out
}

# Expected gate expansion, mirroring the REDCap rules on the Qualtrics side:
# discrete lists and comparisons become Selected-choice enumerations (99 =
# Prefer not to say is never enumerated, so PNTS cannot satisfy a gate);
# count>1 becomes a SelectedChoicesCount comparison.
expected_gate_values <- function(gate_val, parent_values) {
  gate_val <- trimws(gate_val)
  if (grepl("^(>=|<=|>|<|!=)", gate_val)) {
    op <- regmatches(gate_val, regexec("^(>=|<=|>|<|!=)", gate_val))[[1]][1]
    threshold <- as.numeric(trimws(sub(op, "", gate_val, fixed = TRUE)))
    matched <- switch(
      op,
      ">=" = parent_values[parent_values >= threshold],
      "<=" = parent_values[parent_values <= threshold],
      ">" = parent_values[parent_values > threshold],
      "<" = parent_values[parent_values < threshold],
      "!=" = parent_values[parent_values != threshold]
    )
    as.character(matched[!is.na(matched) & matched != 99])
  } else {
    vals <- trimws(strsplit(gate_val, "[,|]")[[1]])
    vals[vals != ""]
  }
}

# ---- Structure: one question per item, unique tags, right types -------------

test_that("the QSF has exactly one question per hitophsum_items row", {
  skip_if_not(have_jsonlite)
  tags <- qsf_tags()
  expect_false(anyDuplicated(tags) > 0)
  expect_setequal(tags, c("hitophsum_instructions", hitophsum_items$Variable))
  expect_length(tags, nrow(hitophsum_items) + 1L)
})

test_that("QSF question types and selectors match Field_Type", {
  skip_if_not(have_jsonlite)
  inst <- qsf_payload("hitophsum_instructions")
  expect_identical(inst$QuestionType, "DB")
  expect_identical(inst$QuestionText, hitophsum_instructions$start[1])

  expected_sel <- c(
    radio = "SAVR",
    checkbox = "MAVR",
    dropdown = "DL",
    text = "SL"
  )
  for (i in seq_len(nrow(hitophsum_items))) {
    ft <- tolower(hitophsum_items$Field_Type[i])
    p <- qsf_payload(hitophsum_items$Variable[i])
    expect_identical(
      p$QuestionType,
      if (ft == "text") "TE" else "MC",
      label = paste(hitophsum_items$Variable[i], "type")
    )
    expect_identical(
      p$Selector,
      unname(expected_sel[ft]),
      label = paste(hitophsum_items$Variable[i], "selector")
    )
  }
})

# ---- Content fidelity: item text ---------------------------------------------

test_that("QSF question text matches hitophsum_items modulo documented adaptations", {
  skip_if_not(have_jsonlite)
  oth_piped <- sprintf(
    "other specified substances (${q://%s/ChoiceTextEntryValue})",
    qsf_qid("hsum_oth_txt")
  )
  for (i in seq_len(nrow(hitophsum_items))) {
    var <- hitophsum_items$Variable[i]
    expect_identical(
      qsf_payload(var)$QuestionText,
      qsf_expected_text(hitophsum_items$Text[i], oth_piped),
      label = var
    )
  }
})

# ---- Content fidelity: choices ------------------------------------------------

test_that("QSF choices match each item's Choice_Set values and labels", {
  skip_if_not(have_jsonlite)
  mc <- hitophsum_items[
    tolower(hitophsum_items$Field_Type) %in% c("radio", "checkbox", "dropdown"),
  ]
  for (i in seq_len(nrow(mc))) {
    var <- mc$Variable[i]
    cs <- hitophsum_choices[hitophsum_choices$Choice_Set == mc$Choice_Set[i], ]
    p <- qsf_payload(var)
    expect_setequal(names(p$Choices), as.character(cs$Value))
    actual_labels <- vapply(p$Choices, function(x) x$Display, character(1))
    expect_identical(
      unname(actual_labels[as.character(cs$Value)]),
      cs$Label,
      label = paste(var, "labels")
    )
  }
})

# ---- Logic fidelity: gates ----------------------------------------------------

test_that("QSF display logic expands Gate_Variable/Gate_Value correctly", {
  skip_if_not(have_jsonlite)
  gated <- hitophsum_items[
    !is.na(hitophsum_items$Gate_Variable) & !is.na(hitophsum_items$Gate_Value),
  ]
  for (i in seq_len(nrow(gated))) {
    var <- gated$Variable[i]
    gate_var <- gated$Gate_Variable[i]
    gate_val <- trimws(gated$Gate_Value[i])
    gate_qid <- qsf_qid(gate_var)
    exprs <- qsf_logic_exprs(qsf_payload(var)$DisplayLogic)
    expect_gt(length(exprs), 0, label = paste(var, "has display logic"))

    # Every condition must reference the gate question, nothing else.
    expect_true(
      all(vapply(exprs, function(e) identical(e$QuestionID, gate_qid), logical(1))),
      label = paste(var, "gates on", gate_var)
    )

    if (gate_val == "count>1") {
      expect_length(exprs, 1L)
      expect_identical(exprs[[1]]$Operator, "GreaterThan", label = var)
      expect_identical(
        exprs[[1]]$ChoiceLocator,
        paste0("q://", gate_qid, "/SelectedChoicesCount"),
        label = var
      )
      expect_identical(exprs[[1]]$RightOperand, "1", label = var)
    } else {
      parent_idx <- match(gate_var, hitophsum_items$Variable)
      parent_cs <- hitophsum_items$Choice_Set[parent_idx]
      parent_values <- hitophsum_choices$Value[
        hitophsum_choices$Choice_Set == parent_cs
      ]
      expected_vals <- expected_gate_values(gate_val, parent_values)
      actual_vals <- vapply(
        exprs,
        function(e) sub(".*/SelectableChoice/", "", e$ChoiceLocator),
        character(1)
      )
      expect_true(
        all(vapply(exprs, function(e) identical(e$Operator, "Selected"), logical(1))),
        label = paste(var, "Selected operators")
      )
      expect_setequal(actual_vals, expected_vals)
    }
  }
})

test_that("no QSF display-logic condition selects choice 99 (PNTS)", {
  skip_if_not(have_jsonlite)
  for (e in qsf_questions) {
    dl <- e$Payload$DisplayLogic
    if (is.null(dl)) next
    locs <- vapply(
      qsf_logic_exprs(dl),
      function(x) x$ChoiceLocator %||% "",
      character(1)
    )
    expect_false(
      any(grepl("/SelectableChoice/99$", locs)),
      label = paste(e$Payload$DataExportTag %||% "?", "admits PNTS")
    )
  }
})
