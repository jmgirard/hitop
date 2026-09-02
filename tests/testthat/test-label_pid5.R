# Tests for label_pid5() (milestone M083): item text and scale display names
# attached as `label` attributes, as label_hitopsr() and label_hitopbr() do for
# the two HiTOP instruments.

# The stem score_pid5() writes scale columns under, and the item stems the
# shipped datasets and the online exports carry. Written out literally here so
# the expectations do not re-derive the defaults from the function under test.
item_stem <- c(FULL = "pid5_", SF = "pid5sf_", BF = "pid5bf_")
item_width <- c(FULL = 3L, SF = 3L, BF = 2L)

sim_for <- function(version) {
  switch(version, FULL = sim_pid5, SF = sim_pid5sf, BF = sim_pid5bf)
}

# The display name this package holds for a scale stem, read straight from the
# shipped tables rather than from anything label_pid5() computes.
expected_scale_label <- function(stem, version) {
  tbl <- pid_scales[[version]]
  name_col <- if (version == "BF") "Domain" else "Facet"
  hit <- match(stem, tbl$camelCase)
  if (!is.na(hit)) return(tbl[[name_col]][hit])
  hit <- match(stem, pid_domains$camelCase)
  if (!is.na(hit)) return(pid_domains$Domain[hit])
  NA_character_
}

# ---- AC1: every item column carries its own prompt text ---------------------

test_that("label_pid5(target = 'items') attaches each item's text at the default prefix", {
  for (version in c("FULL", "SF", "BF")) {
    labeled <- label_pid5(sim_for(version), target = "items", version = version)
    stem <- item_stem[[version]]

    for (nm in names(labeled)) {
      number <- as.integer(sub(paste0("^", stem), "", nm))
      expect_identical(
        attr(labeled[[nm]], "label"),
        pid_items$Text[match(number, pid_items[[version]])],
        info = paste(version, nm)
      )
    }
    # The sweep above is worthless if the frame is empty of item columns.
    expect_equal(ncol(labeled), sum(!is.na(pid_items[[version]])))
  }
})

# ---- AC2: every scale column score_pid5() writes carries a display name -----

test_that("label_pid5(target = 'scales') labels every column score_pid5() writes", {
  for (version in c("FULL", "SF", "BF")) {
    data <- sim_for(version)
    scored <- score_pid5(
      data,
      items = names(data),
      version = version,
      append = FALSE
    )
    labeled <- label_pid5(scored, target = "scales", version = version)

    expect_gt(ncol(labeled), 0)
    for (nm in names(labeled)) {
      stem <- sub("^pid_", "", nm)
      expected <- expected_scale_label(stem, version)
      expect_false(is.na(expected), info = paste(version, nm))
      expect_identical(
        attr(labeled[[nm]], "label"),
        expected,
        info = paste(version, nm)
      )
    }
  }
})

# ---- AC3: prefix = NULL resolves per version and target ---------------------

test_that("the default prefix labels exactly the expected columns, per version and target", {
  # Item frames: the expected names are written out, and each frame carries the
  # other two forms' stems plus the pre-rename `pid_` stem as decoys.
  item_cases <- list(
    list(
      version = "FULL",
      cols = c("pid5_001", "pid5_220", "pid5sf_001", "pid5bf_01", "pid_001"),
      expected = c("pid5_001", "pid5_220")
    ),
    list(
      version = "SF",
      cols = c("pid5sf_001", "pid5sf_100", "pid5_001", "pid5bf_01", "pid_001"),
      expected = c("pid5sf_001", "pid5sf_100")
    ),
    list(
      version = "BF",
      cols = c("pid5bf_01", "pid5bf_25", "pid5_001", "pid5sf_001", "pid_01"),
      expected = c("pid5bf_01", "pid5bf_25")
    )
  )

  for (case in item_cases) {
    df <- as.data.frame(matrix(0L, nrow = 1, ncol = length(case$cols)))
    names(df) <- case$cols
    labeled <- label_pid5(df, target = "items", version = case$version)
    labelled_cols <- names(df)[
      !vapply(labeled, function(x) is.null(attr(x, "label")), logical(1))
    ]
    expect_identical(labelled_cols, case$expected, info = case$version)
  }

  # Scale frames: all three forms write scale columns under `pid_`, but only
  # the named form's own scales are labelled -- `pid_total` is a BF scale only,
  # and `pid_anhedonia` is a facet the BF form does not score.
  scale_cases <- list(
    list(
      version = "FULL",
      cols = c("pid_anhedonia", "pid_psychoticism", "pid_total", "pid5_anhedonia"),
      expected = c("pid_anhedonia", "pid_psychoticism")
    ),
    list(
      version = "SF",
      cols = c("pid_anhedonia", "pid_psychoticism", "pid_total", "pid5_anhedonia"),
      expected = c("pid_anhedonia", "pid_psychoticism")
    ),
    list(
      version = "BF",
      cols = c("pid_psychoticism", "pid_total", "pid_anhedonia", "pid5_total"),
      expected = c("pid_psychoticism", "pid_total")
    )
  )

  for (case in scale_cases) {
    df <- as.data.frame(matrix(0, nrow = 1, ncol = length(case$cols)))
    names(df) <- case$cols
    labeled <- label_pid5(df, target = "scales", version = case$version)
    labelled_cols <- names(df)[
      !vapply(labeled, function(x) is.null(attr(x, "label")), logical(1))
    ]
    expect_identical(labelled_cols, case$expected, info = case$version)
  }
})

# ---- AC4: mis-padded item columns are reported, not silently skipped --------

test_that("label_pid5(target = 'items') names mis-padded item columns in a warning", {
  for (version in c("FULL", "SF", "BF")) {
    data <- sim_for(version)
    stem <- item_stem[[version]]
    width <- item_width[[version]]
    numbers <- pid_items[[version]][!is.na(pid_items[[version]])]
    probes <- c(min(numbers), max(numbers))  # first and last item of the form
    correct <- sprintf(paste0(stem, "%0", width, "d"), probes)

    # Every mis-padding unpadded_item_cols() admits: narrower than the form's
    # width, and one digit wider.
    for (w in setdiff(seq_len(width + 1L), width)) {
      spelled <- sprintf(paste0(stem, "%0", w, "d"), probes)
      mispadded <- spelled[spelled != correct]
      if (length(mispadded) == 0) next

      df <- data
      names(df)[match(correct[spelled != correct], names(df))] <- mispadded

      cnd <- expect_warning(
        labeled <- label_pid5(df, target = "items", version = version),
        class = "hitop_unpadded_items"
      )
      msg <- cli::ansi_strip(conditionMessage(cnd))
      for (nm in mispadded) {
        expect_true(
          grepl(nm, msg, fixed = TRUE),
          info = paste(version, w, nm)
        )
        expect_null(attr(labeled[[nm]], "label"), info = paste(version, w, nm))
      }
      # Correctly padded neighbours in the same frame are still labelled.
      still_padded <- setdiff(names(labeled), mispadded)[1]
      expect_false(is.null(attr(labeled[[still_padded]], "label")))
    }

    # Control: the shipped frame is padded to the form's width throughout, so
    # the report must stay silent on it.
    expect_no_warning(label_pid5(data, target = "items", version = version))
  }
})

test_that("label_pid5() warns and returns data unchanged when nothing matches", {
  df <- data.frame(foo = c(1, 2), bar = c(3, 4))
  expect_warning(
    res <- label_pid5(df, target = "items", version = "FULL"),
    "No columns matched"
  )
  expect_identical(res, df)
  expect_warning(
    res2 <- label_pid5(df, target = "scales", version = "BF"),
    "No columns matched"
  )
  expect_identical(res2, df)
})

test_that("label_pid5() matches `version` case-insensitively and validates its arguments", {
  df <- data.frame(pid5bf_01 = 1L)
  expect_identical(
    attr(label_pid5(df, target = "items", version = "bf")$pid5bf_01, "label"),
    pid_items$Text[match(1L, pid_items$BF)]
  )
  expect_error(label_pid5(df, target = "items", version = "XX"))
  expect_error(label_pid5(df, target = "nonsense", version = "BF"))
  expect_error(label_pid5(df, target = "items", version = "BF", prefix = 1))
  expect_error(label_pid5("not a data frame", target = "items", version = "BF"))
})

# ---- AC1: the padding report fires even when no column matched --------------

test_that("label_pid5(target = 'items') reports mis-padded columns when nothing matched", {
  # Five columns per form, each spelled at a width the form does not use, so
  # `match()` finds nothing and the helper takes its no-match path. Five is the
  # most cli lists before truncating, so every name stays readable.
  probes <- list(
    FULL = paste0("pid5_", 1:5),
    SF = paste0("pid5sf_", 1:5),
    BF = paste0("pid5bf_", 1:5)
  )
  for (version in names(probes)) {
    cols <- probes[[version]]
    df <- frame_of_cols(cols)

    caught <- collect_warnings(
      label_pid5(df, target = "items", version = version)
    )
    expect_length(caught$warnings, 2L)
    if (length(caught$warnings) != 2L) next
    expect_match(warning_text(caught, 1L), "No columns matched", info = version)
    expect_s3_class(caught$warnings[[2]], "hitop_unpadded_items")

    msg <- warning_text(caught, 2L)
    for (nm in cols) {
      expect_true(grepl(nm, msg, fixed = TRUE), info = paste(version, nm))
    }
    # Nothing matched, so nothing is labelled and the frame comes back as it went in.
    expect_identical(caught$value, df, info = version)
  }
})

# ---- AC2/AC3: two kinds of unlabelled column, two sentences -----------------

# A number no item of the form carries, at the form's own padding width.
out_of_range_probe <- c(FULL = "pid5_221", SF = "pid5sf_101", BF = "pid5bf_26")

test_that("label_pid5(target = 'items') sorts mis-padded and out-of-range columns into their own sentences", {
  for (version in c("FULL", "SF", "BF")) {
    stem <- item_stem[[version]]
    width <- item_width[[version]]
    keep <- sprintf(paste0(stem, "%0", width, "d"), 1L)   # a correctly padded neighbour
    mis <- paste0(stem, "3")                              # item 3, one digit wide
    oor <- out_of_range_probe[[version]]
    hint <- sprintf(paste0(stem, "%0", width, "d"), 3L)

    caught <- collect_warnings(
      label_pid5(frame_of_cols(c(keep, mis, oor)), target = "items", version = version)
    )
    expect_length(caught$warnings, 1L)
    if (length(caught$warnings) != 1L) next
    expect_s3_class(caught$warnings[[1]], "hitop_unpadded_items")

    msg <- squashed_warning(caught)
    mis_head <- sentence_pos(msg, "not zero-padded to")
    oor_head <- sentence_pos(msg, "outside the range 1 to")
    expect_true(mis_head > 0, info = version)
    expect_true(oor_head > mis_head, info = version)
    # Each name lands in its own sentence: the mis-padded one before the
    # out-of-range sentence opens, the out-of-range one after.
    expect_true(sentence_pos(msg, mis) > mis_head && sentence_pos(msg, mis) < oor_head, info = version)
    expect_true(sentence_pos(msg, oor) > oor_head, info = version)
    # The hint names a column the form really carries, never the out-of-range one.
    expect_true(grepl(paste0("expected as `", hint, "`"), msg, fixed = TRUE), info = version)
    expect_false(grepl(paste0("expected as `", oor, "`"), msg, fixed = TRUE), info = version)
    # The neighbour that is spelled correctly is still labelled.
    expect_false(is.null(attr(caught$value[[keep]], "label")), info = version)
  }
})

test_that("label_pid5(target = 'items') calls a wrongly padded out-of-range number out of range", {
  # `pid5_0221` is both: four digits where the form uses three, and past item
  # 220. Reported as out of range, since a padding hint would name `pid5_221`,
  # which the form does not carry either.
  caught <- collect_warnings(
    label_pid5(frame_of_cols(c("pid5_001", "pid5_0221")), target = "items", version = "FULL")
  )
  expect_length(caught$warnings, 1L)
  msg <- squashed_warning(caught)
  expect_true(grepl("outside the range 1 to 220", msg, fixed = TRUE))
  expect_false(grepl("not zero-padded", msg, fixed = TRUE))
  expect_false(grepl("expected as", msg, fixed = TRUE))
})

test_that("label_pid5(target = 'items') pluralizes each sentence by its own column count", {
  # Width 3 (FULL) and width 2 (BF) -- the two the PID-5 helpers pass -- each at
  # one and at two reported columns per sentence.
  cases <- list(
    list(version = "FULL", stem = "pid5_", keep = "pid5_001", instrument = "PID-5",
         width = 3L, mis = c("pid5_3", "pid5_4"), oor = c("pid5_221", "pid5_222")),
    list(version = "BF", stem = "pid5bf_", keep = "pid5bf_01", instrument = "PID-5-BF",
         width = 2L, mis = c("pid5bf_3", "pid5bf_4"), oor = c("pid5bf_26", "pid5bf_27"))
  )
  for (case in cases) {
    for (n in 1:2) {
      caught <- collect_warnings(label_pid5(
        frame_of_cols(c(case$keep, case$mis[seq_len(n)], case$oor[seq_len(n)])),
        target = "items",
        version = case$version
      ))
      msg <- squashed_warning(caught)
      subject <- if (n == 1L) "1 column is" else "2 columns are"
      tail_clause <- if (n == 1L) "so it was not labelled" else "so they were not labelled"
      number <- if (n == 1L) "its number is" else "their numbers are"
      expect_true(
        grepl(
          paste0(subject, " named like ", case$instrument, " items but not zero-padded to ",
                 case$width, " digits, ", tail_clause),
          msg, fixed = TRUE
        ),
        info = paste(case$version, n, "mis-padded")
      )
      expect_true(
        grepl(
          paste0(subject, " named like ", case$instrument, " items but ", number,
                 " outside the range 1 to "),
          msg, fixed = TRUE
        ),
        info = paste(case$version, n, "out of range")
      )
      # The out-of-range sentence's closing clause, read from that sentence
      # alone rather than from anywhere in the two-sentence message.
      oor_sentence <- sub("^.*outside the range", "", msg)
      expect_true(
        grepl(tail_clause, oor_sentence, fixed = TRUE),
        info = paste(case$version, n, "out-of-range tail")
      )
    }
  }
})
