# Every function the package exports is either demonstrated or linked in the
# shipped vignettes and articles, or is deprecated.
#
# The gap this closes is silent: adding an `@export` grows the reference index
# and the pkgdown site without touching a single vignette, so a function can
# ship fully documented and never once be shown in use. The sweep below reads
# `NAMESPACE` and every `.Rmd` under `vignettes/` and names each export that
# neither appears as a call in an evaluated chunk nor links to its own
# reference page.
#
# Three arms deliberately do NOT count as coverage:
#   * a name in prose or in a comment -- mentioning `rank_scales()` in a
#     sentence is not a demonstration;
#   * a call inside an `eval = FALSE` chunk -- the reader never sees output;
#   * a non-deprecated export mentioned nowhere at all.
# The classifier is exercised against fixtures for each arm below, so its
# green over the real vignettes is a result and not an assumption.

# ---- The classifier -------------------------------------------------------

# Split one .Rmd's lines into evaluated `{r}` chunk bodies and everything else
# (prose, YAML, and raw `{=html}` blocks). A chunk is dropped from the chunk
# side when it is not evaluated, whether that is declared in the chunk header
# (`{r, eval = FALSE}`) or as a body option (`#| eval: false`).
split_rmd <- function(lines) {
  opens <- grep("^[ \t]*`{3,}[ \t]*\\{[rR][ ,}]", lines)
  closes <- grep("^[ \t]*`{3,}[ \t]*$", lines)

  chunk <- character(0)
  is_text <- rep(TRUE, length(lines))

  for (s in opens) {
    e <- closes[closes > s]
    if (length(e) == 0L) next
    e <- e[[1L]]
    # The whole fenced block leaves the text side either way: a chunk's code is
    # not prose, and a skipped chunk must not slip back in as a prose mention.
    is_text[s:e] <- FALSE
    if (e <= s + 1L) next

    body <- lines[(s + 1L):(e - 1L)]
    header_off <- grepl("eval[ \t]*=[ \t]*FALSE", lines[[s]])
    body_off <- any(grepl("^[ \t]*#\\|[ \t]*eval:[ \t]*(false|FALSE)", body))
    if (header_off || body_off) next

    chunk <- c(chunk, body)
  }

  # Comments and chunk-option lines are prose that happens to sit inside a
  # chunk; a name only mentioned there was never called.
  chunk <- sub("#.*$", "", chunk)

  list(chunk = chunk, all = lines)
}

# Does `name` appear as a call -- `name(` -- rather than as a bare mention?
called_in <- function(name, lines) {
  pattern <- paste0("(^|[^A-Za-z0-9._])", gsub(".", "[.]", name, fixed = TRUE), "[ \t]*\\(")
  any(grepl(pattern, lines))
}

# Does any line link the export's own pkgdown reference page?
linked_in <- function(name, lines) {
  pattern <- paste0("reference/", gsub(".", "[.]", name, fixed = TRUE), "[.]html")
  any(grepl(pattern, lines))
}

# The exports left uncovered. `deprecated` names the exports exempt because
# they are on their way out; `files` is a list of character vectors, one per
# .Rmd, each the file's lines.
uncovered_exports <- function(exports, files, deprecated = character(0)) {
  parts <- lapply(files, split_rmd)
  chunk_lines <- unlist(lapply(parts, `[[`, "chunk"), use.names = FALSE)
  all_lines <- unlist(lapply(parts, `[[`, "all"), use.names = FALSE)

  covered <- vapply(
    exports,
    function(e) {
      e %in% deprecated ||
        called_in(e, chunk_lines) ||
        linked_in(e, all_lines)
    },
    logical(1)
  )
  sort(exports[!covered])
}

# ---- The classifier is exercised on each arm ------------------------------

# One fixture file per arm, plus the shapes that must stay silent. `covered`
# is called in a live chunk and `linked` only appears as an href, so a change
# that broke either arm's positive case would show up as a false report here
# rather than as a quiet pass.
fixture <- c(
  "---",
  "title: Fixture",
  "---",
  "",
  "Prose calling mentioned() by name, which is not a demonstration.",
  "",
  "```{r}",
  "covered(data, items = 1:10)",
  "# commented_out(x) is a comment, not a call",
  "```",
  "",
  "```{r, eval = FALSE}",
  "header_off(x)",
  "```",
  "",
  "```{r}",
  "#| eval: false",
  "body_off(x)",
  "```",
  "",
  "```{=html}",
  "<a href=\"../reference/linked.html\">Linked</a>",
  "```"
)

fixture_exports <- c(
  "covered", "linked", "mentioned", "commented_out",
  "header_off", "body_off", "absent", "gone"
)

test_that("a call in an evaluated chunk and a reference link both count", {
  # The two arms that must stay silent, checked by name rather than by count.
  out <- uncovered_exports(fixture_exports, list(fixture))
  expect_false("covered" %in% out)
  expect_false("linked" %in% out)
})

test_that("a name in prose or in a comment is not a demonstration", {
  out <- uncovered_exports(fixture_exports, list(fixture))
  expect_true("mentioned" %in% out)
  expect_true("commented_out" %in% out)
})

test_that("a call in an unevaluated chunk is not a demonstration", {
  out <- uncovered_exports(fixture_exports, list(fixture))
  # Both spellings of the option: the chunk header and the `#|` body line.
  expect_true("header_off" %in% out)
  expect_true("body_off" %in% out)
})

test_that("an export appearing nowhere is reported, unless it is deprecated", {
  out <- uncovered_exports(fixture_exports, list(fixture))
  expect_true("absent" %in% out)

  exempt <- uncovered_exports(fixture_exports, list(fixture), deprecated = "gone")
  expect_false("gone" %in% exempt)
  # Exempting one export exempts only that one.
  expect_true("absent" %in% exempt)
})

test_that("the whole fixture classifies to exactly the uncovered names", {
  expect_identical(
    uncovered_exports(fixture_exports, list(fixture)),
    c("absent", "body_off", "commented_out", "gone", "header_off", "mentioned")
  )
})

# ---- The sweep over the package's own exports and vignettes ---------------

test_that("every export is demonstrated or linked in a vignette", {
  # Source-checkout only: vignettes/ is not installed.
  vdir <- testthat::test_path("..", "..", "vignettes")
  skip_if(!dir.exists(vdir), "vignettes/ not available")
  nspath <- testthat::test_path("..", "..", "NAMESPACE")
  skip_if(!file.exists(nspath), "NAMESPACE not available")

  ns_lines <- readLines(nspath, warn = FALSE)
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns_lines, value = TRUE))
  exports <- gsub("[`\"]", "", exports)

  paths <- list.files(vdir, pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE)

  # Neither enumeration may silently empty: a broken glob or a moved NAMESPACE
  # would otherwise make every assertion below vacuously true.
  expect_gte(length(exports), 30L)
  expect_gte(length(paths), 10L)
  # And the file list must reach both directories, not just the top level.
  expect_true(any(grepl("/articles/", paths)))

  # Exempt: a function whose own body calls one of R/util.R's deprecate_*()
  # helpers is on its way out, and demonstrating it would teach its use.
  deprecated <- Filter(
    function(e) {
      obj <- get0(e, envir = asNamespace("hitop"))
      is.function(obj) && any(grepl("deprecate_[A-Za-z_]+\\(", deparse(body(obj))))
    },
    exports
  )
  # The exemption is a live arm, not dead code.
  expect_true("hitop_subset" %in% deprecated)

  files <- lapply(paths, readLines, warn = FALSE)
  expect_identical(uncovered_exports(exports, files, deprecated), character(0))
})
