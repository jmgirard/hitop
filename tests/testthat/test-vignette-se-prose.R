# Locks the "Simple Standard Errors" prose in the three scoring vignettes that
# demonstrate `calc_se` against what the scoring engine actually computes.
#
# The behavior these sentences describe is pinned elsewhere: `calc_sem()` drops
# NA before dividing (test-util.R), and a FULL/SF domain SE derives from the 3
# facet scores rather than from the domain's items (test-score_pid5.R). This
# file guards the description, which previously said every scale's SE was the SD
# of "its corresponding items" over the square root of "its number of items" and
# offered the SEs as convertible into confidence intervals.

se_section <- function(vignette) {
  # Source-checkout only: vignettes/ is not installed.
  vdir <- testthat::test_path("..", "..", "vignettes")
  skip_if(!dir.exists(vdir), "vignettes/ not available")

  path <- file.path(vdir, vignette)
  expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # The section body: from its heading to the next top-level heading.
  m <- regmatches(
    text,
    regexpr("(?s)## Simple Standard Errors\n.*?(?=\n## )", text, perl = TRUE)
  )
  expect_length(m, 1L)
  m
}

vignettes_with_se <- c(
  "pid5sf_scoring.Rmd",
  "hitopsr_scoring.Rmd",
  "hitopbr_scoring.Rmd"
)

test_that("the SE sections divide by the count of answered items", {
  for (v in vignettes_with_se) {
    section <- se_section(v)

    # The divisor is how many items the respondent answered, not how many the
    # scale has -- calc_sem() drops NA before dividing.
    expect_match(section, "items the respondent actually answered", info = v)
    expect_match(section, "square root of how many of those items", info = v)
    expect_false(
      grepl("square root of its number of items", section, fixed = TRUE),
      info = v
    )
  }
})

test_that("the SE sections do not offer the SEs as confidence intervals", {
  for (v in vignettes_with_se) {
    section <- se_section(v)

    expect_false(
      grepl("converted into confidence intervals", section, fixed = TRUE),
      info = v
    )
    expect_match(
      section,
      "does not give a confidence interval for a respondent's true score",
      info = v
    )
  }
})

test_that("the PID-5 SF section derives domain SEs from the 3 facet scores", {
  section <- se_section("pid5sf_scoring.Rmd")

  # score_pid5() computes a FULL/SF domain SE over its 3 primary facet scores,
  # never over the domain's 15 items.
  expect_match(section, "three contributing facet scores")
  expect_match(section, "square root of 3", fixed = TRUE)

  # And an SE is masked wherever its scale score is NA (mask_se_na = TRUE).
  expect_match(section, "`NA` wherever its scale score is `NA`", fixed = TRUE)
})

# ---- The deprecation, across every vignette that mentions the argument ------

# The domain is discovered, not listed: AC4 of the deprecation milestone binds
# every file under vignettes/ that mentions `calc_se`, so a new vignette that
# mentions it is caught here rather than quietly exempted.
vignettes_mentioning_se <- function() {
  vdir <- testthat::test_path("..", "..", "vignettes")
  skip_if(!dir.exists(vdir), "vignettes/ not available")

  files <- list.files(vdir, pattern = "[.]Rmd$", recursive = TRUE)
  hits <- Filter(
    function(f) any(grepl("calc_se", readLines(file.path(vdir, f), warn = FALSE))),
    files
  )
  # A silently empty domain would make every assertion below vacuous.
  expect_gte(length(hits), 4L)
  hits
}

# What each file must send its reader to. A file the map does not know fails
# rather than passing by default: adding a vignette that mentions the argument
# is a decision about where its readers go, not an omission.
se_replacement <- c(
  "pid5sf_scoring.Rmd" = "no interval function for the PID-5",
  "hitopsr_scoring.Rmd" = "interval_hitopsr()",
  "hitopbr_scoring.Rmd" = "interval_hitopbr()",
  "articles/modules-hitopsr.Rmd" = "interval_hitopsr()"
)

test_that("every vignette mentioning calc_se says it is deprecated", {
  for (v in vignettes_mentioning_se()) {
    text <- paste(
      readLines(testthat::test_path("..", "..", "vignettes", v), warn = FALSE),
      collapse = " "
    )
    expect_match(text, "deprecated", ignore.case = TRUE, info = v)
    expect_match(text, "removed in a future release", info = v)
  }
})

test_that("every vignette mentioning calc_se names its instrument's replacement", {
  for (v in vignettes_mentioning_se()) {
    expect_true(v %in% names(se_replacement), info = v)
    text <- paste(
      readLines(testthat::test_path("..", "..", "vignettes", v), warn = FALSE),
      collapse = " "
    )
    expect_match(text, se_replacement[[v]], fixed = TRUE, info = v)
  }
})
