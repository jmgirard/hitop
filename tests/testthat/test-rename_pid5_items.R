# Helpers shared by the tests below -------------------------------------------

# The rows of `pid_items` belonging to one PID-5 form, in that form's item
# order, plus the facts the criteria are written against.
pid5_form <- function(version) {
  rows <- pid_items[!is.na(pid_items[[version]]), ]
  rows <- rows[order(rows[[version]]), ]
  list(
    numbers = rows[[version]],
    text = rows$Text,
    max_n = max(rows[[version]]),
    prefix = switch(version, FULL = "pid5_", SF = "pid5sf_", BF = "pid5bf_")
  )
}

# AC1 -------------------------------------------------------------------------

test_that("method = 'text' renames every item of each form to its padded name", {
  for (version in c("FULL", "SF", "BF")) {
    form <- pid5_form(version)
    cols <- paste0("q", seq_along(form$numbers), "_raw")
    df <- as.data.frame(
      matrix(
        0,
        nrow = 2,
        ncol = length(cols),
        dimnames = list(NULL, cols)
      )
    )

    res <- rename_pid5_items(
      df,
      version = version,
      method = "text",
      item_cols = cols,
      item_text = form$text
    )

    expect_identical(
      colnames(res),
      item_names(form$prefix, form$numbers, max_n = form$max_n),
      info = version
    )
  }

  # The sweep above is only meaningful at the documented widths.
  expect_identical(
    vapply(c("FULL", "SF", "BF"), \(v) length(pid5_form(v)$numbers), integer(1)),
    c(FULL = 220L, SF = 100L, BF = 25L)
  )
})

# AC2 -------------------------------------------------------------------------

test_that("method = 'number' renames item-numbered columns and reports the rest", {
  for (version in c("FULL", "SF", "BF")) {
    form <- pid5_form(version)

    # One column per item of the form, spelled with an unpadded number.
    item_cols <- paste0("pid_", form$numbers)

    # Columns matching `<from_prefix><digits>` whose number names no item of
    # this form: above the form's largest, the number zero, and (SF and BF) a
    # number that names a FULL item this form does not carry.
    unnamed_numbers <- c(form$max_n + 1L, 0L)
    if (version != "FULL") {
      unnamed_numbers <- c(unnamed_numbers, if (version == "SF") 150L else 100L)
    }
    unnamed_cols <- paste0("pid_", unnamed_numbers)

    # Columns not matching `<from_prefix><digits>` at all.
    non_matching <- c(
      item_names(form$prefix, form$numbers[[1]], max_n = form$max_n),
      "pid_total",
      "study_pid_7",
      "PID_7"
    )

    cols <- c(item_cols, unnamed_cols, non_matching)
    df <- as.data.frame(
      matrix(0, nrow = 2, ncol = length(cols), dimnames = list(NULL, cols))
    )

    caught <- NULL
    res <- withCallingHandlers(
      rename_pid5_items(df, version = version, method = "number"),
      hitop_unmatched_items = function(w) {
        caught <<- w
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )

    expect_identical(
      colnames(res)[seq_along(item_cols)],
      item_names(form$prefix, form$numbers, max_n = form$max_n),
      info = version
    )
    expect_identical(
      colnames(res)[-seq_along(item_cols)],
      c(unnamed_cols, non_matching),
      info = version
    )

    # The report names exactly the matching-but-unnamed columns: its bullets
    # are compared as a set, so a report that also named a renamed item column
    # or a non-matching one fails here.
    expect_s3_class(caught, "hitop_unmatched_items")
    lines <- strsplit(conditionMessage(caught), "\n", fixed = TRUE)[[1]]
    # Drop the header line and the bullet glyph cli prefixes each entry with.
    bullets <- sub("^\\S+ ", "", lines[-1])
    expect_setequal(bullets, unnamed_cols)
    expect_length(bullets, length(unnamed_cols))
  }
})

# AC3 -------------------------------------------------------------------------

test_that("method = 'text' reports unmatchable item text under its own class", {
  for (version in c("FULL", "SF", "BF")) {
    form <- pid5_form(version)

    # Two matching entries, then each member of the unmatchable family: a
    # wholly foreign string, and the text of a `pid_items` row this form does
    # not carry.
    # The full form carries every row, so it has no off-form text to plant;
    # the short and brief forms do.
    off_form <- pid_items$Text[is.na(pid_items[[version]])]
    expect_identical(length(off_form) == 0L, version == "FULL")
    unmatchable <- c("A sentence from no instrument at all.", off_form[1])
    unmatchable <- unmatchable[!is.na(unmatchable)]

    # Positive control: an entry differing from a real item's text only in
    # surrounding whitespace is matched, the comparison running under
    # `trimws()`. Without it the family above could be vacuous.
    padded <- paste0("  ", form$text[[2]], "  ")

    item_text <- c(form$text[[1]], padded, unmatchable)
    cols <- paste0("q", seq_along(item_text))
    df <- as.data.frame(
      matrix(0, nrow = 2, ncol = length(cols), dimnames = list(NULL, cols))
    )

    caught <- NULL
    res <- withCallingHandlers(
      rename_pid5_items(
        df,
        version = version,
        method = "text",
        item_cols = cols,
        item_text = item_text
      ),
      hitop_unmatched_items = function(w) {
        caught <<- w
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )

    expect_s3_class(caught, "hitop_unmatched_items")
    reported <- conditionMessage(caught)
    for (entry in unmatchable) {
      expect_match(reported, entry, fixed = TRUE, info = paste(version, entry))
    }

    expected <- c(
      item_names(form$prefix, form$numbers[1:2], max_n = form$max_n),
      cols[-(1:2)]
    )
    expect_identical(colnames(res), expected, info = version)
  }
})

# AC4 -------------------------------------------------------------------------

test_that("prefix and from_prefix default per version", {
  # Literal expectations, not re-derived by calling the function.
  expected <- list(
    FULL = c("pid5_001", "pid5_002", "pid5_220"),
    SF = c("pid5sf_001", "pid5sf_002", "pid5sf_100"),
    BF = c("pid5bf_01", "pid5bf_02", "pid5bf_25")
  )

  for (version in names(expected)) {
    form <- pid5_form(version)
    picks <- c(1L, 2L, form$max_n)

    # method = "text": no `prefix` given.
    text_cols <- c("a", "b", "c")
    df_text <- as.data.frame(
      matrix(0, nrow = 2, ncol = 3, dimnames = list(NULL, text_cols))
    )
    res_text <- suppressWarnings(rename_pid5_items(
      df_text,
      version = version,
      method = "text",
      item_cols = text_cols,
      item_text = form$text[picks]
    ))
    expect_identical(colnames(res_text), expected[[version]], info = version)

    # method = "number": neither `prefix` nor `from_prefix` given, over
    # columns spelled `pid_<number>`. Renames nothing unless `from_prefix`
    # defaults to "pid_".
    num_cols <- paste0("pid_", picks)
    df_num <- as.data.frame(
      matrix(0, nrow = 2, ncol = 3, dimnames = list(NULL, num_cols))
    )
    res_num <- suppressWarnings(
      rename_pid5_items(df_num, version = version, method = "number")
    )
    expect_identical(colnames(res_num), expected[[version]], info = version)
  }
})

# Argument validation ---------------------------------------------------------

test_that("rename_pid5_items validates its arguments", {
  df <- data.frame(pid_1 = 1:2)

  expect_error(rename_pid5_items(df, version = "XL"), "arg")
  expect_error(
    rename_pid5_items(df, version = "FULL", method = "text"),
    "item_cols"
  )
  expect_error(
    rename_pid5_items(
      df,
      version = "FULL",
      method = "text",
      item_cols = "pid_1",
      item_text = c("a", "b")
    ),
    "same length"
  )
  expect_error(
    rename_pid5_items(
      df,
      version = "FULL",
      method = "text",
      item_cols = "absent",
      item_text = "a"
    ),
    "not found"
  )
  expect_error(
    rename_pid5_items(df, version = "FULL", from_prefix = 1),
    "from_prefix"
  )
  expect_error(
    rename_pid5_items(df, version = "FULL", prefix = 1),
    "prefix"
  )

  # `version` is resolved case-insensitively, as `score_pid5()` resolves it.
  expect_identical(
    colnames(suppressWarnings(rename_pid5_items(df, version = "full"))),
    "pid5_001"
  )
})

test_that("rename_pid5_items warns when fewer than all items are renamed", {
  df <- data.frame(pid_1 = 1:2, pid_2 = 1:2)
  warns <- capture_warnings(rename_pid5_items(df, version = "FULL"))
  expect_match(warns, "2 out of 220 PID-5 items", all = FALSE)

  # Silent when every item of the form is renamed.
  form <- pid5_form("BF")
  full_bf <- as.data.frame(matrix(
    0,
    nrow = 1,
    ncol = length(form$numbers),
    dimnames = list(NULL, paste0("pid_", form$numbers))
  ))
  expect_silent(rename_pid5_items(full_bf, version = "BF"))
})
