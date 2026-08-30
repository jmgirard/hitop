# Locks the module documentation in `vignettes/articles/modules-hitopsr.Rmd` and
# the three HiTOP-SR generator help pages against what the generators do.
#
# Four statements had drifted out of step with the code and one had never been
# written. The behavior itself is pinned elsewhere -- the module Word header in
# test-docx-title.R, the printed-number crosswalk in test-docx-numbering.R, and
# the descriptor sidecar and its console message in test-generator-descriptor.R.
# This file guards the descriptions:
#
#   * a module Word form is headed "HiTOP-SR Module (v1.0)" (R/generate_docx.R,
#     `title <- if (is.null(module))` ...), which the article never said;
#   * the crosswalk is printed only for a RENUMBERED module form, so
#     `renumber = FALSE` gets none -- the article promised one unconditionally;
#   * `collected[order(item_order)]` puts columns back in instrument order only
#     when they are in the form's PRINTED order to begin with, which neither the
#     article nor `?generate_docx_hitopsr` said;
#   * all three generators announce the descriptor's path on the console, which
#     no `descriptor` help text mentioned;
#   * `include_subscales = TRUE` with `module` is an error, which the article
#     never mentioned.

# --- slicing helpers ------------------------------------------------------
#
# Both cut boundaries are asserted found. A regexpr() that matches nothing
# returns -1, which widens a substring cut to the whole file -- a guard that
# then asserts a phrase present passes on text from somewhere else entirely.

squash <- function(x) gsub("[[:space:]]+", " ", x)

article_section <- function(heading) {
  # Source-checkout only: vignettes/ is not installed.
  vdir <- testthat::test_path("..", "..", "vignettes")
  skip_if(!dir.exists(vdir), "vignettes/ not available")

  path <- file.path(vdir, "articles", "modules-hitopsr.Rmd")
  expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_gt(nchar(text), 0L)

  open_at <- regexpr(paste0("(?m)^## ", heading, "$"), text, perl = TRUE)
  expect_false(open_at == -1L, label = paste0("heading '## ", heading, "' found"))
  if (open_at == -1L) return(NA_character_)

  rest <- substring(text, open_at)
  close_at <- regexpr("(?m)^## ", substring(rest, 4L), perl = TRUE)
  # The last section of the file has no following heading; that is legal, and
  # is the one case where the cut legitimately runs to the end.
  body <- if (close_at == -1L) rest else substring(rest, 1L, close_at + 2L)
  expect_gt(nchar(body), 0L)
  squash(body)
}

rd_item <- function(rd, param) {
  # Source-checkout only: man/ is not installed as .Rd.
  mdir <- testthat::test_path("..", "..", "man")
  skip_if(!dir.exists(mdir), "man/ not available")

  path <- file.path(mdir, rd)
  expect_true(file.exists(path))
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_gt(nchar(text), 0L)

  open_at <- regexpr(paste0("(?s)\\\\item\\{", param, "\\}"), text, perl = TRUE)
  expect_false(open_at == -1L, label = paste0("\\item{", param, "} found in ", rd))
  if (open_at == -1L) return(NA_character_)

  rest <- substring(text, open_at)
  close_at <- regexpr("(?m)^\\\\item\\{", substring(rest, 2L), perl = TRUE)
  expect_false(close_at == -1L, label = paste0("item after ", param, " found in ", rd))
  if (close_at == -1L) return(NA_character_)

  # Rd wraps the roxygen prose, so a phrase can straddle a newline.
  squash(substring(rest, 1L, close_at))
}

generators <- c(
  "generate_docx_hitopsr.Rd",
  "generate_qualtrics_hitopsr.Rd",
  "generate_redcap_hitopsr.Rd"
)

# --- the header a module Word form carries --------------------------------

test_that("the article names both Word headers and the title override", {
  section <- article_section("Generating the Instrument")

  expect_match(section, "HiTOP-SR Module (v1.0)", fixed = TRUE)
  expect_match(section, "HiTOP-SR (v1.0)", fixed = TRUE)
  expect_match(section, "`title =`", fixed = TRUE)
})

# --- the crosswalk is printed for one case only ---------------------------

test_that("the article says a renumber = FALSE form prints no crosswalk", {
  section <- article_section("Generating the Instrument")

  expect_match(section, "crosswalk", fixed = TRUE)
  expect_match(section, "renumber = FALSE", fixed = TRUE)
  # The sentence the correction replaced promised the crosswalk to any module
  # form; nothing may claim that again.
  expect_false(
    grepl("and a module form also carries a crosswalk", section, fixed = TRUE)
  )
})

# --- the reorder recipe's precondition ------------------------------------

test_that("both article recipes state the collected columns are in printed order", {
  recipe_sections <- c("Generating the Instrument", "Saving the Module Beside the Form")

  for (heading in recipe_sections) {
    section <- article_section(heading)
    # A failed slice returns NA; using it below would abort the whole block and
    # hide the other heading's result.
    if (is.na(section)) next

    # The full recipe shape, not a bare `order(` -- the section also holds an
    # unrelated `scale_menu[order(...)]` sort, which a looser anchor accepts.
    expect_match(section, "collected[order(", fixed = TRUE, info = heading)
    expect_match(section, "the order the form printed", fixed = TRUE, info = heading)
    expect_match(section, "item 1", fixed = TRUE, info = heading)
  }
})

test_that("the randomize help text states the recipe's precondition", {
  param <- rd_item("generate_docx_hitopsr.Rd", "randomize")
  expect_false(is.na(param))
  if (is.na(param)) return(invisible(NULL))

  expect_match(param, "order the form printed", fixed = TRUE)
  expect_match(param, "item 1", fixed = TRUE)
  expect_match(param, "already in instrument order", fixed = TRUE)
})

# --- the descriptor write is announced ------------------------------------

test_that("every generator's descriptor help text names the console message", {
  for (rd in generators) {
    param <- rd_item(rd, "descriptor")
    expect_false(is.na(param), info = rd)
    if (is.na(param)) next

    expect_match(param, "announced on the console", fixed = TRUE, info = rd)
  }
})

# --- the refused combination ----------------------------------------------

test_that("the article names the include_subscales and module conflict", {
  section <- article_section("Generating the Instrument")

  expect_match(section, "include_subscales = TRUE", fixed = TRUE)
  expect_match(section, "refuses", fixed = TRUE)
})
