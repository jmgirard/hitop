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
