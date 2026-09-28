# Compare read_form_responses() on a base ref and on the working tree (M139).
#
# M139 changes the reader so that a file may hold item columns of two or more
# instruments, and returns `form_build` as character. This script checks that
# nothing else changed for the files the base ref's own tests read.
#
# Run from the package root: Rscript data-raw/compare_form_reader.R [ref]
# The ref defaults to `main`.
#
# 1. It loads the working tree with pkgload, and the base ref's
#    R/read_form_responses.R into an environment whose parent is the loaded
#    namespace, so the base reader runs with its own internals.
# 2. It extracts the base ref's tests/testthat/ with `git archive` and runs
#    test-read_form_responses.R from it, with read_form_responses() replaced
#    by a wrapper. The wrapper copies each input (a directory or a vector of
#    files) to its own folder, keeps a bad argument as it is, and stands a
#    missing path in that folder in for a missing file. It records the
#    running test's name and then calls the base reader, so the base tests
#    run as they did.
# 3. It reads each copied input with both readers. Two results must hold
#    identical columns, except that the working tree's `form_build` must
#    equal format() of the base's. Two refusals must hold the same condition
#    class and message. The inputs of the tests in `replaced` below may
#    differ, because M139 replaces their refusal. The script prints the
#    working tree's message for each of them.
#
# It exits 1 when any other input differs, or when no input was captured.

args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) >= 1L) args[[1L]] else "main"

# The base tests whose refusal M139 replaces: item columns of two stems were
# refused as such, and are now read as two groups, so the `instrument` cell
# of one stem is the fault.
replaced <- c(
  "item columns of two stems are refused naming the stems",
  "a two-stem file with an item_order of 1 1 is refused for the stems, not the cell"
)

suppressMessages(pkgload::load_all(".", quiet = TRUE))
ns <- asNamespace("hitop")

git <- function(...) {
  out <- system2("git", c(...), stdout = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) stop("git ", paste(c(...), collapse = " "), " failed")
  out
}

base_env <- new.env(parent = ns)
eval(parse(text = git("show", paste0(ref, ":R/read_form_responses.R"))), base_env)
base_reader <- base_env$read_form_responses

work <- tempfile("compare-form-reader")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)
tarball <- file.path(work, "tests.tar")
invisible(git("archive", "--format=tar", paste0("--output=", tarball), ref, "tests/testthat"))
utils::untar(tarball, exdir = work)
capture_root <- file.path(work, "captured")
dir.create(capture_root)

# The name of the test_that() block running now, or NA outside one.
current_test <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    if (identical(sys.function(i), testthat::test_that)) {
      return(get("desc", envir = sys.frame(i)))
    }
  }
  NA_character_
}

captures <- list()

# Copy `path` to a folder of its own and return the copy's path, or `path`
# itself when it is not a readable character vector.
snapshot <- function(path) {
  d <- file.path(capture_root, sprintf("%04d", length(captures) + 1L))
  dir.create(d)
  if (!is.character(path) || length(path) < 1L || anyNA(path)) {
    return(path)
  }
  if (length(path) == 1L && dir.exists(path)) {
    file.copy(path, d, recursive = TRUE)
    return(file.path(d, basename(path)))
  }
  vapply(seq_along(path), function(i) {
    sub <- file.path(d, i)
    dir.create(sub)
    if (file.exists(path[[i]])) {
      file.copy(path[[i]], sub, recursive = TRUE)
    }
    file.path(sub, basename(path[[i]]))
  }, character(1L))
}

wrapper <- function(path) {
  copy <- snapshot(path)
  captures[[length(captures) + 1L]] <<- list(test = current_test(), path = copy)
  base_reader(path)
}
for (where in list(ns, as.environment("package:hitop"))) {
  unlockBinding("read_form_responses", where)
  assign("read_form_responses", wrapper, envir = where)
}
results <- as.data.frame(testthat::test_file(
  file.path(work, "tests", "testthat", "test-read_form_responses.R"),
  reporter = "silent", package = "hitop", load_package = "none"
))
failed <- results$test[results$failed > 0L | results$error]
if (length(failed) > 0L) {
  cat("Base tests that failed under the wrapper:\n")
  cat(paste(" -", failed), sep = "\n")
}

# Reload the working tree, so the wrapper is gone and its reader is current.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
branch_reader <- asNamespace("hitop")$read_form_responses

outcome <- function(reader, path) {
  tryCatch(list(value = reader(path)), error = function(e) list(error = e))
}

same <- function(base, branch) {
  if (!is.null(base$error) || !is.null(branch$error)) {
    return(!is.null(base$error) && !is.null(branch$error) &&
             identical(class(base$error), class(branch$error)) &&
             identical(conditionMessage(base$error), conditionMessage(branch$error)))
  }
  b <- base$value
  w <- branch$value
  if (!identical(names(b), names(w))) {
    return(FALSE)
  }
  others <- setdiff(names(b), "form_build")
  identical(w$form_build, format(b$form_build)) && identical(w[others], b[others])
}

n_results <- 0L
n_refusals <- 0L
unexpected <- character(0)
cat(sprintf("Base ref %s: %d base tests ran, %d inputs captured.\n",
            ref, length(unique(results$test)), length(captures)))
for (cap in captures) {
  base <- outcome(base_reader, cap$path)
  branch <- outcome(branch_reader, cap$path)
  if (is.null(base$error)) n_results <- n_results + 1L else n_refusals <- n_refusals + 1L
  if (same(base, branch)) {
    next
  }
  if (cap$test %in% replaced) {
    msg <- if (is.null(branch$error)) "(reads)" else conditionMessage(branch$error)
    cat(sprintf("\nReplaced refusal, test \"%s\". Working tree:\n%s\n",
                cap$test, cli::ansi_strip(msg)))
  } else {
    unexpected <- c(unexpected, cap$test)
  }
}
cat(sprintf("\n%d inputs read to a result on the base, %d to a refusal.\n",
            n_results, n_refusals))
if (length(captures) == 0L || length(unexpected) > 0L) {
  cat("Inputs that differ outside the replaced refusals:\n")
  cat(paste(" -", unique(unexpected)), sep = "\n")
  quit(status = 1L)
}
cat("Every other input reads the same on both.\n")
