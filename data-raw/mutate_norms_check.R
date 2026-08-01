## Mutation check for tests/testthat/test-norms.R (M25, AC4)
#
# A test suite that passes proves nothing about what it would catch. This
# script applies, one at a time, each corruption test-norms.R is meant to
# detect, swaps the mutated dataset into data/pid_norms.rda, re-runs that test
# file, and reports which tests failed. The pristine dataset is restored on
# exit -- including on error -- and the restore is checked by hash.
#
# Usage:  Rscript data-raw/mutate_norms_check.R

stopifnot(requireNamespace("devtools", quietly = TRUE))

rda <- "data/pid_norms.rda"
stopifnot(file.exists(rda))

## Row selectors used by the mutations below.
row_of <- function(x, version, scale, tscore = NULL, raw = NULL) {
  hit <- x$version == version & x$scale == scale
  if (!is.null(tscore)) hit <- hit & !is.na(x$tscore) & x$tscore == tscore
  if (!is.null(raw)) hit <- hit & x$raw == raw
  i <- which(hit)
  stopifnot(length(i) == 1L)
  i
}

mutations <- list(
  list(
    ac = "AC4(a)",
    desc = "FULL negativeAffectivity raw at T = 60 raised by 0.02",
    f = function(x) {
      i <- row_of(x, "FULL", "negativeAffectivity", tscore = 60)
      x$raw[[i]] <- x$raw[[i]] + 0.02
      x
    }
  ),
  list(
    ac = "AC4(a)",
    desc = "SF psychoticism raw at T = 30 (on the zero floor) raised to 0.02",
    f = function(x) {
      i <- row_of(x, "SF", "psychoticism", tscore = 30)
      stopifnot(x$raw[[i]] == 0)
      x$raw[[i]] <- 0.02
      x
    }
  ),
  list(
    ac = "AC4(b)",
    desc = "BF detachment percentile at T = 70 dropped below its predecessor",
    f = function(x) {
      i <- row_of(x, "BF", "detachment", tscore = 70)
      prev <- row_of(x, "BF", "detachment", tscore = 69)
      x$percentile[[i]] <- x$percentile[[prev]] - 0.01
      x
    }
  ),
  list(
    ac = "AC4(c)",
    desc = "FULL INC percentile at a score of 5 set to 1.4",
    f = function(x) {
      i <- row_of(x, "FULL", "INC", raw = 5)
      x$percentile[[i]] <- 1.4
      x
    }
  ),
  list(
    ac = "AC4(d)",
    desc = "FULL PRD percentile at a score of 21 nudged 0.894 -> 0.895",
    f = function(x) {
      i <- row_of(x, "FULL", "PRD", raw = 21)
      stopifnot(x$percentile[[i]] == 0.894)
      x$percentile[[i]] <- 0.895
      x
    }
  ),
  list(
    ac = "AC4(d)",
    desc = "SF antagonism percentile at T = 48 reverted to the pre-correction 0.58",
    f = function(x) {
      i <- row_of(x, "SF", "antagonism", tscore = 48)
      stopifnot(x$percentile[[i]] == 0.52)
      x$percentile[[i]] <- 0.58
      x
    }
  ),
  ## M33's facet mutations. The first is the defect this dataset actually had
  ## once (a whole column off by one row), which no structural invariant can
  ## see: it leaves the raws on a line and the percentiles monotone.
  list(
    ac = "M33 AC6",
    desc = "FULL hostility raw column displaced down one T row",
    f = function(x) {
      i <- which(x$version == "FULL" & x$scale == "hostility")
      i <- i[order(x$tscore[i])]
      x$raw[i] <- c(x$raw[[i[[1]]]], utils::head(x$raw[i], -1))
      x
    }
  ),
  list(
    ac = "M33 AC5",
    desc = "SF perseveration raw at T = 55 pushed 0.02 off its column's line",
    f = function(x) {
      i <- row_of(x, "SF", "perseveration", tscore = 55)
      x$raw[[i]] <- x$raw[[i]] + 0.02
      x
    }
  ),
  ## A percentile column displaced on its own is the thinnest case in the
  ## dataset: it stays monotone, and `raw` -- which every other test reads -- is
  ## untouched, so only an anchor whose percentile happens to step at that T can
  ## see it. Kept here to measure the gap rather than to assert it is closed.
  list(
    ac = "M33 AC6",
    desc = "SF withdrawal percentile column displaced down one T row (raw untouched)",
    f = function(x) {
      i <- which(x$version == "SF" & x$scale == "withdrawal")
      i <- i[order(x$tscore[i])]
      x$percentile[i] <- c(x$percentile[[i[[1]]]], utils::head(x$percentile[i], -1))
      x
    }
  ),
  list(
    ac = "M33 AC6",
    desc = "FULL anhedonia percentile column displaced down one T row (raw untouched)",
    f = function(x) {
      i <- which(x$version == "FULL" & x$scale == "anhedonia")
      i <- i[order(x$tscore[i])]
      x$percentile[i] <- c(x$percentile[[i[[1]]]], utils::head(x$percentile[i], -1))
      x
    }
  ),
  list(
    ac = "M33 AC4",
    desc = "SF anxiousness ceiling run truncated -- its 12 rows at 4.00 cut to 1",
    f = function(x) {
      i <- which(x$version == "SF" & x$scale == "anxiousness" & x$raw == 4)
      stopifnot(length(i) == 12L)
      x[-utils::tail(i, -1), ]
    }
  )
)

failing_tests <- function() {
  res <- as.data.frame(devtools::test(filter = "norms", reporter = "silent"))
  hit <- res$failed > 0 | res$error
  unique(res$test[hit])
}

## The mutations run inside a function so that on.exit() defers to the end of
## the run: at top level it would fire at the end of its own statement, leaving
## a mutated dataset behind.
run_mutations <- function() {
  pristine <- tempfile(fileext = ".rda")
  invisible(file.copy(rda, pristine, overwrite = TRUE))
  before <- tools::md5sum(rda)[[1]]
  on.exit({
    invisible(file.copy(pristine, rda, overwrite = TRUE))
    after <- tools::md5sum(rda)[[1]]
    cat("\nrestored ", rda, ": md5 ", after,
        if (identical(before, after)) " (unchanged)" else " (MISMATCH)", "\n",
        sep = "")
  }, add = TRUE)

  ## The unmutated suite must be green, or nothing below means anything.
  cat("baseline (no mutation):\n")
  base_fail <- failing_tests()
  if (length(base_fail)) {
    stop(
      "test-norms.R is not green before mutation: ",
      paste(base_fail, collapse = "; ")
    )
  }
  cat("  all tests pass\n\n")

  for (m in mutations) {
    load(pristine)                     # pristine pid_norms
    pid_norms <- m$f(pid_norms)
    save(pid_norms, file = rda, compress = "bzip2", version = 2)

    failed <- failing_tests()
    cat(m$ac, " ", m$desc, "\n", sep = "")
    if (length(failed)) {
      cat("  CAUGHT by: ", paste(failed, collapse = "; "), "\n\n", sep = "")
    } else {
      cat("  NOT CAUGHT -- no test failed\n\n")
    }
  }
}

run_mutations()
