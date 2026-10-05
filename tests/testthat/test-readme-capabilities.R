# README.Rmd's "What the package covers" table states what the package does
# for each instrument. Each cell is checked here against the package itself, so
# a new instrument, scoring function, tutorial or form fails this file until
# the table follows. The source README is not in the built tarball, so under
# R CMD check every test here skips; they run under devtools::test().

readme_path <- function() testthat::test_path("..", "..", "README.Rmd")

readme_columns <- c(
  "Instrument", "Items", "Scoring", "Reliability", "Tutorial", "Forms"
)

# Row to `version` argument (`?score_pid5`; the child forms score as their
# adult forms). The HiTOP rows take no version.
readme_versions <- c(
  "PID-5" = "FULL", "PID-5-SF" = "SF", "PID-5-BF" = "BF",
  "PID5BF+M" = "BFPM", "PID-5-IRF" = "IRF", "PID-5-FFBF" = "FFBF",
  "PID-5 Child" = "FULL", "PID-5-BF Child" = "BF"
)

readme_hitop <- c("HiTOP-SR" = "sr", "HiTOP-BR" = "br", "HiTOP-HSUM" = "hsum")

readme_rows <- c(names(readme_hitop), names(readme_versions))

# The table as a character matrix, one row per instrument, cells trimmed.
readme_table <- function() {
  lines <- readLines(readme_path(), encoding = "UTF-8")
  start <- which(lines == "## What the package covers")
  if (length(start) != 1L) {
    return(NULL)
  }
  rest <- lines[seq.int(start + 1L, length(lines))]
  first <- which(startsWith(rest, "|"))[1]
  if (is.na(first)) {
    return(NULL)
  }
  rest <- rest[first:length(rest)]
  block <- rest[seq_len(match(FALSE, startsWith(rest, "|"), nomatch = length(rest) + 1L) - 1L)]
  cells <- lapply(block, function(l) {
    l <- sub("^\\|", "", sub("\\|\\s*$", "", l))
    trimws(strsplit(l, "|", fixed = TRUE)[[1]])
  })
  header <- cells[[1]]
  body <- cells[-(1:2)]
  out <- do.call(rbind, body)
  colnames(out) <- header
  out
}

# The instrument's row. A missing or repeated row gives NA cells, so each check
# fails with the instrument's name in its label rather than erroring.
readme_row <- function(tab, instrument) {
  hit <- which(tab[, "Instrument"] == instrument)
  if (length(hit) != 1L) {
    return(stats::setNames(rep(NA_character_, ncol(tab)), colnames(tab)))
  }
  tab[hit, , drop = TRUE]
}

# Expected item count, from the instrument's keying table.
readme_items <- function(instrument) {
  if (instrument %in% names(readme_hitop)) {
    tbl <- switch(
      readme_hitop[[instrument]],
      sr = hitopsr_items, br = hitopbr_items, hsum = hitophsum_items
    )
    return(nrow(tbl))
  }
  version <- readme_versions[[instrument]]
  if (version == "FFBF") nrow(pid_ffbf_items) else sum(!is.na(pid_items[[version]]))
}

readme_stem <- function(instrument) {
  if (instrument %in% names(readme_hitop)) {
    paste0("hitop", readme_hitop[[instrument]])
  } else {
    "pid5"
  }
}

# The exported scoring or reliability function for a row, or NULL when the
# package exports none.
readme_function <- function(instrument, family) {
  name <- paste0(family, "_", readme_stem(instrument))
  if (name %in% getNamespaceExports("hitop")) getExportedValue("hitop", name) else NULL
}

# Calls `fn` on one row of in-range responses with the row's version. An error
# or a warning propagates and fails the test.
readme_call <- function(fn, instrument) {
  n <- readme_items(instrument)
  if (instrument %in% names(readme_hitop)) {
    d <- as.data.frame(matrix(rep_len(1:4, n), nrow = 1))
    expect_no_warning(fn(d, items = seq_len(n)))
  } else {
    d <- as.data.frame(matrix(rep_len(0:3, n), nrow = 1))
    expect_no_warning(fn(d, items = seq_len(n), version = readme_versions[[instrument]]))
  }
}

# Top-level vignettes holding a scoring call for the row.
readme_tutorials <- function(instrument) {
  dir <- testthat::test_path("..", "..", "vignettes")
  files <- list.files(dir, pattern = "\\.Rmd$", full.names = TRUE)
  needle <- if (instrument %in% c("HiTOP-SR", "HiTOP-BR")) {
    paste0("score_hitop", readme_hitop[[instrument]], "(")
  } else if (instrument %in% names(readme_versions)) {
    sprintf('version = "%s"', readme_versions[[instrument]])
  } else {
    return(character())
  }
  hits <- vapply(files, function(f) {
    any(grepl(needle, readLines(f, encoding = "UTF-8"), fixed = TRUE))
  }, logical(1))
  stems <- sort(tools::file_path_sans_ext(basename(files[hits])))
  if (length(stems) == 0L) {
    return(character())
  }
  sprintf("https://jmgirard.github.io/hitop/articles/%s.html", stems)
}

# The link targets in a cell, as written.
readme_links <- function(cell) {
  urls <- regmatches(cell, gregexpr("\\]\\(([^)]*)\\)", cell))[[1]]
  sort(gsub("^\\]\\(|\\)$", "", urls))
}

# Formats `hitop_artifacts` holds for the instrument, in README spelling. A
# format with no README spelling keeps its own name, so the check fails until
# the README names it.
readme_forms <- function(instrument) {
  formats <- unique(hitop_artifacts$format[hitop_artifacts$instrument == instrument])
  labels <- c(
    docx_us = "Word", docx_a4 = "Word", qualtrics = "Qualtrics",
    redcap = "REDCap", json = "JSON"
  )
  spelled <- as.character(formats)
  known <- spelled %in% names(labels)
  spelled[known] <- labels[spelled[known]]
  sort(unique(unname(spelled)))
}

readme_list <- function(cell) {
  if (!nzchar(cell)) character() else sort(trimws(strsplit(cell, ",", fixed = TRUE)[[1]]))
}

skip_without_readme <- function() {
  testthat::skip_if_not(file.exists(readme_path()), "README.Rmd is not present")
}

test_that("the README table has the planned columns and one row per instrument", {
  skip_without_readme()
  tab <- readme_table()
  if (is.null(tab)) {
    return(fail("no table under `## What the package covers`"))
  }
  expect_identical(colnames(tab), readme_columns)
  expect_setequal(tab[, "Instrument"], readme_rows)
  expect_identical(anyDuplicated(tab[, "Instrument"]), 0L)
})

test_that("the README rows cover every instrument, scoring function and version", {
  skip_without_readme()
  # Every instrument with downloads has a row.
  expect_true(
    all(unique(hitop_artifacts$instrument) %in% readme_rows),
    label = "every hitop_artifacts instrument has a README row"
  )
  # Every exported scoring or reliability function belongs to a row.
  exported <- grep("^(score|reliability)_", getNamespaceExports("hitop"), value = TRUE)
  stems <- unique(sub("^(score|reliability)_", "", exported))
  row_stems <- unique(vapply(readme_rows, readme_stem, character(1)))
  expect_true(
    all(stems %in% row_stems),
    label = "every exported score_*() and reliability_*() has a README row"
  )
  # Every PID-5 version the scoring function takes has a row.
  expect_true(
    all(eval(formals(score_pid5)$version) %in% readme_versions),
    label = "every score_pid5() version has a README row"
  )
})

test_that("each Items cell is the instrument's item count", {
  skip_without_readme()
  tab <- readme_table()
  if (is.null(tab)) {
    return(fail("no table under `## What the package covers`"))
  }
  for (inst in readme_rows) {
    expect_identical(
      readme_row(tab, inst)[["Items"]], as.character(readme_items(inst)),
      label = paste(inst, "Items")
    )
  }
})

test_that("each Scoring and Reliability cell matches the package's functions", {
  skip_without_readme()
  tab <- readme_table()
  if (is.null(tab)) {
    return(fail("no table under `## What the package covers`"))
  }
  for (family in c("score", "reliability")) {
    column <- if (family == "score") "Scoring" else "Reliability"
    for (inst in readme_rows) {
      fn <- readme_function(inst, family)
      if (!is.null(fn)) readme_call(fn, inst)
      expect_identical(
        readme_row(tab, inst)[[column]], if (is.null(fn)) "" else "Yes",
        label = paste(inst, column)
      )
    }
  }
})

test_that("each Tutorial cell links every vignette that scores the instrument", {
  skip_without_readme()
  tab <- readme_table()
  if (is.null(tab)) {
    return(fail("no table under `## What the package covers`"))
  }
  for (inst in readme_rows) {
    expect_identical(
      readme_links(readme_row(tab, inst)[["Tutorial"]]), readme_tutorials(inst),
      label = paste(inst, "Tutorial")
    )
  }
})

test_that("each Forms cell lists the formats hitop_artifacts holds", {
  skip_without_readme()
  tab <- readme_table()
  if (is.null(tab)) {
    return(fail("no table under `## What the package covers`"))
  }
  for (inst in readme_rows) {
    expect_identical(
      readme_list(readme_row(tab, inst)[["Forms"]]), readme_forms(inst),
      label = paste(inst, "Forms")
    )
  }
})
