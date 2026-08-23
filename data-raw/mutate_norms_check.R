## Mutation check for tests/testthat/test-norms.R (M025, AC4)
#
# A test suite that passes proves nothing about what it would catch. This
# script applies, one at a time, each corruption test-norms.R is meant to
# detect, swaps the mutated dataset into data/pid_norms.rda, re-runs that test
# file, and reports which tests failed. The pristine dataset is restored on
# exit -- including on error -- and the restore is checked by hash.
#
# The corruptions themselves live in data-raw/norms_mutations.R, shared with
# data-raw/mutate_norms_book_check.R, which runs the same list against the
# book comparison instead of against this suite.
#
# Usage:  Rscript data-raw/mutate_norms_check.R

stopifnot(requireNamespace("devtools", quietly = TRUE))

source("data-raw/norms_mutations.R")

rda <- "data/pid_norms.rda"

failing_tests <- function() {
  res <- as.data.frame(devtools::test(filter = "norms", reporter = "silent"))
  hit <- res$failed > 0 | res$error
  unique(res$test[hit])
}

## The mutations run inside a function so that on.exit() defers to the end of
## the run: at top level it would fire at the end of its own statement, leaving
## a mutated dataset behind.
run_mutations <- function(pristine) {
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

  for (m in norms_mutations) {
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

with_pristine_norms(rda, run_mutations)
