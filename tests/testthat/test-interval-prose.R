# Every surface that shows a HiTOP-SR score interval says what its reference
# group is (M041, AC6).
#
# `interval_hitopsr()` compares a score against a development sample, not
# against a community norm, and the difference changes what a reader may
# conclude from an interval. Four surfaces carry that statement -- the help
# page, the changelog entry, the pkgdown reference index, and the scoring
# vignette -- and this file asserts the statement on all four as text, plus that
# the sample size each names is the one documented for `hitopsr_devstats`.
#
# Read from the source tree: none of `man/`, `NEWS.md`, `_pkgdown.yml` or
# `vignettes/` is installed as-is, so under `R CMD check` these skip. They run
# under `devtools::test()`, which is where the artifacts exist.

root <- function() testthat::test_path("..", "..")

# The text of one artifact, whitespace runs collapsed so a phrase that Rd or
# YAML wrapped across a line still matches, and so the assertions survive the
# source being reflowed.
artifact_text <- function(relative) {
  path <- file.path(root(), relative)
  skip_if(!file.exists(path), paste(relative, "not available"))
  text <- paste(readLines(path, warn = FALSE), collapse = " ")
  ## An empty or unreadable artifact would make every assertion below vacuous.
  expect_gt(nchar(text), 200L)
  gsub("[[:space:]]+", " ", text)
}

# The one section of each artifact that is about this function, so a phrase
# occurring elsewhere in a long file cannot satisfy the assertion. Each cut is
# anchored on text the artifact itself owns.
#
# BOTH ends are asserted found. A terminator that matches nothing would silently
# widen the cut to the whole remainder of the file, and the assertions below
# would then be answered by text that is not about this function at all -- which
# is what a missing terminator did here until the M041 review caught it.
between <- function(text, from, to) {
  start <- regexpr(from, text, fixed = TRUE)
  expect_gt(start, 0L)
  rest <- substring(text, nchar(from) + start)
  stop_at <- regexpr(to, rest, fixed = TRUE)
  expect_gt(stop_at, 0L)
  substring(rest, 1L, stop_at - 1L)
}

# Every "N = <number>" the passage states.
sample_sizes <- function(passage) {
  hits <- regmatches(passage, gregexpr("N = [0-9,]+", passage))[[1]]
  as.integer(gsub("[^0-9]", "", hits))
}

surfaces <- function() {
  list(
    ## The Rd's own bold heading opens the passage and the next bold heading
    ## closes it. The cut drops the anchor, so every phrase below is asserted
    ## against the paragraph rather than against the words that located it.
    "help page" = between(
      artifact_text("man/interval_hitopsr.Rd"),
      "\\strong{The reference group is a development sample.}",
      "\\strong{Two limitations worth stating.}"
    ),
    ## Terminated on the start of whatever the next changelog entry is, rather
    ## than on that entry's wording, so inserting an entry between the two does
    ## not silently widen this cut.
    "changelog" = between(
      artifact_text("NEWS.md"),
      "* **`interval_hitopsr()` puts a confidence interval",
      "* **"
    ),
    "pkgdown reference index" = between(
      artifact_text("_pkgdown.yml"),
      "- title: Score Intervals",
      "- title: Plots"
    ),
    "scoring vignette" = between(
      artifact_text("vignettes/hitopsr_scoring.Rmd"),
      "### What the reference group is",
      "## Scoring Only Some Scales"
    )
  )
}

test_that("every surface showing an interval names the development sample and denies it is a norm", {
  for (nm in names(surfaces())) {
    passage <- surfaces()[[nm]]
    expect_gt(nchar(passage), 100L)
    expect_match(passage, "Development Sample 2", fixed = TRUE, info = nm)
    expect_match(passage, "development sample", info = nm)
    expect_match(passage, "not a community norm", fixed = TRUE, info = nm)
  }
})

test_that("the sample size every surface names is the one documented for hitopsr_devstats", {
  ## The documented figure, read from the dataset's own help page rather than
  ## typed here, so a corrected N has one place to change.
  documented <- sample_sizes(between(
    artifact_text("man/hitopsr_devstats.Rd"),
    "Development Sample 2",
    "Every statistic"
  ))
  expect_length(documented, 1L)
  expect_identical(documented, 780L)

  for (nm in names(surfaces())) {
    found <- sample_sizes(surfaces()[[nm]])
    expect_gte(length(found), 1L)
    expect_true(all(found == documented), info = nm)
  }
})

test_that("the wording guard notices a surface that stops saying it", {
  ## The guard is a text match, so it is worth showing it can miss nothing: a
  ## passage with the phrase removed, and one naming a different N, both fail
  ## the assertions above.
  passage <- surfaces()[["changelog"]]
  stripped <- gsub("not a community norm", "a community norm", passage,
                   fixed = TRUE)
  expect_false(grepl("not a community norm", stripped, fixed = TRUE))
  renumbered <- gsub("N = 780", "N = 1082", passage, fixed = TRUE)
  expect_false(all(sample_sizes(renumbered) == 780L))
})
