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
# Each run gets exactly one of three verdicts, because a run that never reached
# the comparison says nothing about what the comparison would have seen and must
# not be read as a miss:
#
#   CAUGHT      the comparison ran, reported the mutation, and the script exited
#               non-zero
#   NOT CAUGHT  the comparison ran and reported nothing
#   ERRORED     the comparison did not run -- the book extraction, the CSV layer
#               or the script's own setup stopped first
#
# The three are told apart by the comparison's own count line rather than by the
# exit status, which a crash and a catch share. A sweep carrying any errored run
# exits non-zero naming those runs separately from the misses.
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
##
## `ran` is what separates a crash from a miss. The count line is printed once,
## by the assembly comparison, after it has finished diffing; a run that stops
## earlier -- a missing shelf epub, a parse failure, a mutation the assembly step
## itself cannot load -- never prints it. So the count line's presence, not the
## exit status, says whether there is a verdict to read at all.
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
    ran = length(n) == 3L,
    reported = length(n) == 3L && sum(n) > 0L,
    tail = utils::tail(out, 3L),
    lines = grep("^  - assembly:", out, value = TRUE)
  )
}

## The three-way verdict. "errored" is decided first and on its own evidence, so
## a crashed run can never be filed as a miss.
book_verdict <- function(res) {
  if (!res$ran) {
    "errored"
  } else if (res$stopped && res$reported) {
    "caught"
  } else {
    "not caught"
  }
}

run_mutations <- function(pristine) {
  ## The unmutated dataset must pass, or nothing below means anything.
  cat("baseline (no mutation):\n")
  base <- book_check()
  if (!base$ran) {
    cat("  the run did not reach the comparison; its last lines were:\n")
    for (l in base$tail) cat("    ", l, "\n", sep = "")
    stop("the book comparison errored before mutation", call. = FALSE)
  }
  if (base$stopped || base$reported) {
    stop("the book comparison does not pass before mutation")
  }
  cat("  the shipped pid_norms matches the book\n\n")

  missed <- character(0)
  errored <- character(0)
  for (m in norms_mutations) {
    load(pristine)                     # pristine pid_norms
    pid_norms <- m$f(pid_norms)
    save(pid_norms, file = rda, compress = "bzip2", version = 2)

    res <- book_check()
    cat(m$ac, " ", m$desc, "\n", sep = "")
    switch(
      book_verdict(res),
      caught = {
        cat("  CAUGHT -- pid_norms-only rows ", res$counts[[1]],
            ", book-only rows ", res$counts[[2]],
            ", differing values ", res$counts[[3]], "\n", sep = "")
        cat("    e.g. ", trimws(res$lines[[1]]), "\n\n", sep = "")
      },
      `not caught` = {
        missed <- c(missed, m$id)
        cat("  NOT CAUGHT -- the comparison ran and reported nothing\n\n")
      },
      errored = {
        errored <- c(errored, m$id)
        cat("  ERRORED -- the run never reached the comparison, so it says\n",
            "    nothing about what the comparison would have seen. Last lines:\n",
            sep = "")
        for (l in res$tail) cat("      ", l, "\n", sep = "")
        cat("\n")
      }
    )
  }

  if (length(errored) || length(missed)) {
    stop(
      if (length(errored)) {
        paste0(length(errored), " mutation(s) errored before the comparison ran: ",
               paste(errored, collapse = ", "),
               if (length(missed)) "; " else "")
      },
      if (length(missed)) {
        paste0(length(missed), " mutation(s) unreported by the book comparison: ",
               paste(missed, collapse = ", "))
      },
      call. = FALSE
    )
  }
  cat("every seeded corruption was reported by the book comparison.\n")
}

with_pristine_norms(rda, run_mutations)
