# Every surface that shows a HiTOP-BR score interval says what its reference
# group is (M068, AC6).
#
# `interval_hitopbr()` compares a score against a development sample, not
# against a community norm, and the difference changes what a reader may
# conclude from an interval. Four surfaces carry that statement -- the function's
# help page, the dataset's help page, the changelog entry, and the scoring
# vignette -- and this file asserts the statement on all four as text, plus that
# the sample size each names is the one documented for `hitopbr_devstats`.
#
# The shape is the one tests/testthat/test-interval-prose.R uses for the
# HiTOP-SR surfaces, including its both-ends-anchored cut.
#
# Read from the source tree: none of `man/`, `NEWS.md` or `vignettes/` is
# installed as-is, so under `R CMD check` these skip. They run under
# `devtools::test()`, which is where the artifacts exist.

root <- function() testthat::test_path("..", "..")

# The text of one artifact, whitespace runs collapsed so a phrase that Rd or
# Markdown wrapped across a line still matches, and so the assertions survive
# the source being reflowed.
br_artifact_text <- function(relative) {
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
# would then be answered by text that is not about this function at all.
br_between <- function(text, from, to) {
  start <- regexpr(from, text, fixed = TRUE)
  expect_gt(start, 0L)
  rest <- substring(text, nchar(from) + start)
  stop_at <- regexpr(to, rest, fixed = TRUE)
  expect_gt(stop_at, 0L)
  substring(rest, 1L, stop_at - 1L)
}

# Every "N = <number>" the passage states.
br_sample_sizes <- function(passage) {
  hits <- regmatches(passage, gregexpr("N = [0-9,]+", passage))[[1]]
  as.integer(gsub("[^0-9]", "", hits))
}

br_surfaces <- function() {
  list(
    ## The Rd's own bold heading opens the passage and the next bold heading
    ## closes it. The cut drops the anchor, so every phrase below is asserted
    ## against the paragraph rather than against the words that located it.
    "help page" = br_between(
      br_artifact_text("man/interval_hitopbr.Rd"),
      "\\strong{The reference group is a development sample.}",
      "\\strong{Two limitations worth stating.}"
    ),
    ## The dataset's own help page, cut from the sentence that names the
    ## reference group to the sentence that turns to the cells.
    "dataset help page" = br_between(
      br_artifact_text("man/hitopbr_devstats.Rd"),
      "The reference group is",
      "Every statistic"
    ),
    ## Terminated on the start of whatever the next changelog entry is, rather
    ## than on that entry's wording, so inserting an entry between the two does
    ## not silently widen this cut.
    "changelog" = br_between(
      br_artifact_text("NEWS.md"),
      "* **`hitopbr_devstats` ships the HiTOP-BR",
      "## "
    ),
    "scoring vignette" = br_between(
      br_artifact_text("vignettes/hitopbr_scoring.Rmd"),
      "### What the reference group is",
      "## Scale Reliability"
    )
  )
}

test_that("every surface showing a HiTOP-BR interval names the development sample and denies it is a norm", {
  for (nm in names(br_surfaces())) {
    passage <- br_surfaces()[[nm]]
    expect_gt(nchar(passage), 100L)
    expect_match(passage, "Development Sample 2", fixed = TRUE, info = nm)
    expect_match(passage, "development sample", info = nm)
    expect_match(passage, "not a community norm", fixed = TRUE, info = nm)
  }
})

test_that("the sample size every HiTOP-BR surface names is the one documented for hitopbr_devstats", {
  ## The documented figure, read from the dataset's own help page rather than
  ## typed here, so a corrected N has one place to change.
  documented <- br_sample_sizes(br_surfaces()[["dataset help page"]])
  expect_length(documented, 1L)
  expect_identical(documented, 780L)

  for (nm in names(br_surfaces())) {
    found <- br_sample_sizes(br_surfaces()[[nm]])
    expect_gte(length(found), 1L)
    expect_true(all(found == documented), info = nm)
  }
})

test_that("the wording guard notices a surface that stops saying it", {
  ## The guard is a text match, so it is worth showing it can miss nothing: a
  ## passage with the phrase removed, and one naming a different N, both fail
  ## the assertions above.
  passage <- br_surfaces()[["changelog"]]
  stripped <- gsub("not a community norm", "a community norm", passage,
                   fixed = TRUE)
  expect_false(grepl("not a community norm", stripped, fixed = TRUE))
  renumbered <- gsub("N = 780", "N = 1082", passage, fixed = TRUE)
  expect_false(all(br_sample_sizes(renumbered) == 780L))

  ## And that a terminator matching nothing is caught rather than silently
  ## widening the cut to the rest of the file.
  expect_error(
    br_between(
      br_artifact_text("vignettes/hitopbr_scoring.Rmd"),
      "### What the reference group is",
      "## A Heading This Vignette Does Not Have"
    ),
    class = "expectation_failure"
  )
})
