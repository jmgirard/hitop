## Mutation check for the book comparison in verify_norms_against_book.R (M035, AC4)
#
# data-raw/mutate_norms_check.R runs the seeded corruptions of `pid_norms`
# against tests/testthat/test-norms.R. This script runs the same list against
# the other layer: the cell-by-cell comparison of the shipped dataset with the
# book's own tables. Each corruption is swapped into data/pid_norms.rda, the
# verification script is run as a subprocess, and the run counts as CAUGHT only
# if that script's assembly comparison reports the mutation and exits non-zero.
# The pristine dataset is restored on exit -- including on error -- and the
# restore is checked by hash.
#
# The four cases M034 seeded are the ones this milestone's AC4 names -- the SF
# withdrawal and FULL anhedonia percentile-column displacements and the two
# column swaps -- but an exhaustive cell-by-cell diff should see every
# corruption in the list, so the script asserts all of them and stops if any
# goes unreported.
#
# Needs the gitignored shelf epub that verify_norms_against_book.R reads, so
# like that script this is a maintainer tool and cannot run in CI. Each run
# re-parses the book, so the whole sweep takes a few minutes.
#
# Usage:  Rscript data-raw/mutate_norms_book_check.R

source("data-raw/norms_mutations.R")

rda <- "data/pid_norms.rda"
verify <- "data-raw/verify_norms_against_book.R"
stopifnot(file.exists(verify))

## Run the verification script and report what its assembly comparison saw.
## The CSV comparison in that script reads the data-raw CSVs, which no mutation
## here touches, so attributing a failure to the assembly layer means reading
## its own three counts rather than merely the exit status.
book_check <- function() {
  out <- suppressWarnings(system2(
    "Rscript", verify, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  counts <- grep("rows in pid_norms only:", out, value = TRUE)
  n <- if (length(counts)) {
    as.integer(regmatches(counts[[1]], gregexpr("[0-9]+", counts[[1]]))[[1]])
  } else {
    integer(0)
  }
  list(
    stopped = !is.null(status) && status != 0L,
    counts = n,
    reported = length(n) == 3L && sum(n) > 0L,
    lines = grep("^  - assembly:", out, value = TRUE)
  )
}

run_mutations <- function(pristine) {
  ## The unmutated dataset must pass, or nothing below means anything.
  cat("baseline (no mutation):\n")
  base <- book_check()
  if (base$stopped || base$reported) {
    stop("the book comparison does not pass before mutation")
  }
  cat("  the shipped pid_norms matches the book\n\n")

  missed <- character(0)
  for (m in norms_mutations) {
    load(pristine)                     # pristine pid_norms
    pid_norms <- m$f(pid_norms)
    save(pid_norms, file = rda, compress = "bzip2", version = 2)

    res <- book_check()
    cat(m$ac, " ", m$desc, "\n", sep = "")
    if (res$stopped && res$reported) {
      cat("  CAUGHT -- pid_norms-only rows ", res$counts[[1]],
          ", book-only rows ", res$counts[[2]],
          ", differing values ", res$counts[[3]], "\n", sep = "")
      cat("    e.g. ", trimws(res$lines[[1]]), "\n\n", sep = "")
    } else {
      missed <- c(missed, m$id)
      cat("  NOT CAUGHT -- the book comparison did not report it\n\n")
    }
  }

  if (length(missed)) {
    stop(length(missed), " mutation(s) unreported by the book comparison: ",
         paste(missed, collapse = ", "), call. = FALSE)
  }
  cat("every seeded corruption was reported by the book comparison.\n")
}

with_pristine_norms(rda, run_mutations)
