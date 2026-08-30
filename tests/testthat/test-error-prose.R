# Both refusals are documented where a caller reads them (AC4).
#
# The help page of every export that appends its output states the
# output-column collision, and the three exports taking a variable-length
# selection also state the empty-selection refusal. `NEWS.md` records both.
#
# The domain is the same getNamespaceExports() sweep the guards are tested by,
# and each swept name is resolved to an Rd file through the package's alias
# index rather than by assuming the file is named after the function -- an
# unresolved name fails here rather than dropping quietly out of the domain.
#
# Read from the source tree: neither `man/` nor `NEWS.md` is installed in this
# form, so these skip under `R CMD check` and run under devtools::test(), which
# is where the artifacts exist.

root <- function() testthat::test_path("..", "..")

prose_exports <- function() {
  ns <- asNamespace("hitop")
  nms <- sort(getNamespaceExports("hitop"))
  keep <- vapply(
    nms,
    function(n) {
      obj <- get(n, envir = ns)
      is.function(obj) && "append" %in% names(formals(obj))
    },
    logical(1)
  )
  nms[keep]
}

# name -> Rd path, built from every \alias{} in man/. Rd files are not always
# named after the function they document (a family shares one page), so the
# alias index is the resolution the help system itself uses.
alias_index <- function() {
  files <- list.files(file.path(root(), "man"), pattern = "[.]Rd$", full.names = TRUE)
  skip_if(length(files) == 0, "man/ not available")
  idx <- list()
  for (f in files) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    aliases <- regmatches(text, gregexpr("\\\\alias\\{[^}]+\\}", text))[[1]]
    for (a in aliases) {
      idx[[gsub("^\\\\alias\\{|\\}$", "", a)]] <- f
    }
  }
  idx
}

artifact_text <- function(path) {
  skip_if(!file.exists(path), paste(basename(path), "not available"))
  text <- paste(readLines(path, warn = FALSE), collapse = " ")
  # An empty or unreadable artifact would make every assertion below vacuous.
  expect_gt(nchar(text), 200L)
  gsub("[[:space:]]+", " ", text)
}

# The Errors passage of one Rd, cut between anchors the Rd itself owns and with
# the opening anchor dropped, so every phrase below is asserted against the
# passage rather than against the words that located it.
#
# BOTH ends are asserted found. A terminator matching nothing would silently
# widen the cut to the rest of the file, and a phrase occurring anywhere later
# on the page would then satisfy the assertion (the M041/M046 trap).
between <- function(text, from, to, info) {
  start <- regexpr(from, text, fixed = TRUE)
  expect_true(start > 0L, info = paste(info, "opening anchor"))
  rest <- substring(text, nchar(from) + start)
  stop_at <- regexpr(to, rest, fixed = TRUE)
  expect_true(stop_at > 0L, info = paste(info, "closing anchor"))
  passage <- substring(rest, 1L, stop_at - 1L)
  expect_true(nchar(passage) > 50L, info = paste(info, "passage length"))
  passage
}

# The closing anchor per export. `\examples{` is the next top-level macro on
# seven of the eight pages; on norm_pid5 the Errors passage is followed by a
# further bold heading, which is the tighter cut.
errors_terminator <- function() {
  c(
    interval_hitopbr = "\\examples{",
    interval_hitopsr = "\\examples{",
    norm_pid5 = "\\strong{Response coding.}",
    rank_scales = "\\examples{",
    score_hitopbr = "\\examples{",
    score_hitopsr = "\\examples{",
    score_pid5 = "\\examples{",
    validity_pid5 = "\\examples{"
  )
}

# Rendered, markup-free fragments. Neither contains an Rd macro, so a reflow or
# a change of markup around them does not move the assertion.
COLLISION_PHRASE <-
  "would also produce is an error rather than an overwrite or a duplicated column"
EMPTY_PHRASE <-
  "argument that names no columns is an error, reported ahead of the"

# The two condition classes, named on the pages that signal them so a caller can
# find the name to catch without reading the release notes.
COLLISION_CLASS <- "hitop_append_collision"
EMPTY_CLASS <- "hitop_empty_selection"

# The four exports taking a variable-length column selection.
selection_exports <- c("interval_hitopbr", "interval_hitopsr", "norm_pid5",
                       "rank_scales")

test_that("the sweep matches the terminator table and resolves through the aliases", {
  exports <- prose_exports()
  expect_true(length(exports) > 0)
  expect_setequal(exports, names(errors_terminator()))

  idx <- alias_index()
  expect_true(length(idx) > 0)
  # An export with no alias fails here rather than being skipped over.
  expect_setequal(intersect(exports, names(idx)), exports)
  expect_true(all(selection_exports %in% exports))
})

test_that("every appending export states the collision refusal on its help page", {
  exports <- prose_exports()
  idx <- alias_index()
  terminators <- errors_terminator()

  for (nm in exports) {
    rd <- idx[[nm]]
    expect_false(is.null(rd), info = nm)
    passage <- between(
      artifact_text(rd),
      "\\strong{Errors.}",
      terminators[[nm]],
      info = nm
    )
    expect_match(passage, COLLISION_PHRASE, fixed = TRUE, info = nm)
    expect_match(passage, COLLISION_CLASS, fixed = TRUE, info = nm)
  }
})

test_that("the three selection exports state the empty-selection refusal", {
  idx <- alias_index()
  terminators <- errors_terminator()

  for (nm in selection_exports) {
    passage <- between(
      artifact_text(idx[[nm]]),
      "\\strong{Errors.}",
      terminators[[nm]],
      info = nm
    )
    expect_match(passage, EMPTY_PHRASE, fixed = TRUE, info = nm)
    # The argument the caller wrote, named in the passage that promises it.
    arg <- if (nm == "rank_scales") "scales" else "scores"
    expect_match(passage, arg, fixed = TRUE, info = nm)
    expect_match(passage, EMPTY_CLASS, fixed = TRUE, info = nm)
  }
})

test_that("the four non-selection exports do not promise an empty-selection refusal", {
  # A page must not claim a refusal its function does not make: score_pid5() and
  # the other three take a fixed-length `items`, which validate_items() already
  # rejects at the wrong length.
  idx <- alias_index()
  terminators <- errors_terminator()
  others <- setdiff(prose_exports(), selection_exports)
  expect_true(length(others) > 0)

  for (nm in others) {
    passage <- between(
      artifact_text(idx[[nm]]),
      "\\strong{Errors.}",
      terminators[[nm]],
      info = nm
    )
    expect_no_match(passage, EMPTY_PHRASE, fixed = TRUE, info = nm)
    expect_no_match(passage, EMPTY_CLASS, fixed = TRUE, info = nm)
  }
})

test_that("NEWS.md records both refusals under the current version", {
  desc <- file.path(root(), "DESCRIPTION")
  # Guarded like every other source-tree read in this file: under R CMD check
  # the source tree is absent, and an unguarded read errors rather than skips.
  skip_if(!file.exists(desc), "DESCRIPTION not available")
  version <- as.character(read.dcf(desc, fields = "Version")[[1]])
  news <- artifact_text(file.path(root(), "NEWS.md"))
  heading <- paste("# hitop", version)
  # Cut at the current version's heading and stop at the next one, so an entry
  # under an older release cannot satisfy the assertion.
  start <- regexpr(heading, news, fixed = TRUE)
  expect_gt(start, 0L)
  rest <- substring(news, nchar(heading) + start)
  stop_at <- regexpr("# hitop ", rest, fixed = TRUE)
  section <- if (stop_at > 0L) substring(rest, 1L, stop_at - 1L) else rest
  expect_gt(nchar(section), 200L)

  expect_match(section, "collid", info = "collision entry")
  expect_match(section, "hitop_append_collision", fixed = TRUE)
  expect_match(section, "hitop_empty_selection", fixed = TRUE)
})
