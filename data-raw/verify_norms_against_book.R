# Verify data-raw/norms_*.csv against the published source (M25, AC1; M33)
#
# Independent transcription of the PID-5 normative tables, extracted
# deterministically from the book's own table markup and diffed cell by cell
# against the committed CSVs. The extraction path never reads the CSVs; the
# comparison happens only after both sides exist, so every reported
# discrepancy is a fact about the CSVs rather than about a transcriber.
#
# The seven M25 tables were transcribed by hand, so for those this is a
# transcription check. The two facet tables M33 adds carry 3,550 cells each and
# are machine-extracted by data-raw/extract_facet_norms.R, so for those this is
# a cross-check of two *independent reshapings* of the same markup: the
# extractor walks rows sequentially carrying each banner's facet names forward,
# while the block reader below ignores banners when assembling rows -- it cuts
# the numeric rows at their T-score restarts -- and recovers the facet names
# from the banners only afterwards, then compares column by *name* rather than
# by position. What neither can catch is a defect already present in the book's
# own markup; the hand-read spot values in tests/testthat/test-norms.R are the
# layer that reads the rendered page.
#
# Source (gitignored shelf, not distributed):
#   cairn/references/sources/markon2024.epub
#   Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
#   Understanding the Personality Inventory for DSM-5 (PID-5).
#   American Psychiatric Association Publishing. ISBN 9781615375127.
#
# Requires xml2 and readr installed locally. Like the other data-raw scripts
# these are maintainer tools, not package dependencies (M18 lesson).
#
# Usage:  Rscript data-raw/verify_norms_against_book.R

stopifnot(requireNamespace("xml2", quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))

epub_path <- "cairn/references/sources/markon2024.epub"
if (!file.exists(epub_path)) {
  stop("Source not on the shelf: ", epub_path, call. = FALSE)
}

## ---- extract -------------------------------------------------------------
## Read every <table> in the Appendix as a character matrix, plus the page
## number in force where each table starts (the epub carries pagebreak
## anchors, so table anchors are exact rather than estimated).

exdir <- tempfile("markon2024-")
utils::unzip(epub_path, exdir = exdir)
appendix <- file.path(exdir, "OEBPS", "xhtml", "20_Appendix.xhtml")
stopifnot(file.exists(appendix))

doc <- xml2::read_html(appendix)

cell_text <- function(node) {
  txt <- xml2::xml_text(node)
  txt <- gsub(" ", " ", txt)          # nbsp
  txt <- gsub("[–−]", "-", txt)  # en-dash / minus used as hyphen
  trimws(gsub("[[:space:]]+", " ", txt))
}

table_rows <- function(tbl) {
  lapply(xml2::xml_find_all(tbl, ".//tr"), function(tr) {
    vapply(xml2::xml_find_all(tr, "./td|./th"), cell_text, character(1))
  })
}

tables <- xml2::xml_find_all(doc, "//table")

## Page in force at each table: the last pagebreak anchor preceding it.
page_anchors <- xml2::xml_find_all(doc, "//*[@id[starts-with(., 'page')]]")
anchor_pages <- sub("^page[_-]?", "", xml2::xml_attr(page_anchors, "id"))
node_order <- function(nodes) vapply(nodes, function(n) length(xml2::xml_find_all(n, "preceding::*")), integer(1))
anchor_pos <- node_order(page_anchors)
table_pos <- node_order(tables)
page_for_table <- vapply(table_pos, function(p) {
  before <- which(anchor_pos <= p)
  if (!length(before)) NA_character_ else anchor_pages[[max(before)]]
}, character(1))

## ---- shape ---------------------------------------------------------------
## A data row is one whose first cell is a number. Everything else -- the
## scale-name banner and the sub-header rows repeated at page breaks -- is
## dropped by that test alone, never by position.

is_num <- function(x) grepl("^-?[0-9]+(\\.[0-9]+)?$", x)

data_matrix <- function(tbl, ncol_expected) {
  rows <- table_rows(tbl)
  rows <- rows[vapply(rows, function(r) length(r) == ncol_expected, logical(1))]
  rows <- rows[vapply(rows, function(r) length(r) > 0 && is_num(r[[1]]), logical(1))]
  if (!length(rows)) return(matrix(character(0), nrow = 0, ncol = ncol_expected))
  matrix(unlist(rows), nrow = length(rows), byrow = TRUE)
}

## ---- facet blocks ---------------------------------------------------------
## A-6 and A-8 each print all 25 facets inside one <table>, as five blocks of
## five facets stacked vertically: a banner row of width 6 (a blank cell then
## five facet names), a repeated sub-header, then 71 data rows of width 11
## (T, then five Raw/Percentile pairs). Rows are assembled without reference to
## the banners -- the numeric rows are cut wherever T restarts -- and the block
## order is only then paired with the banner order, so a mis-split shows up as
## a block whose T column is not 30-100 rather than as silently relabelled data.

facet_banner_names <- function(tbl, width = 6L) {
  rows <- table_rows(tbl)
  banners <- Filter(function(r) length(r) == width && !is_num(r[[1]]), rows)
  lapply(banners, function(r) as.character(r[-1]))
}

facet_data_blocks <- function(tbl, width = 11L) {
  rows <- table_rows(tbl)
  rows <- Filter(function(r) length(r) == width && is_num(r[[1]]), rows)
  tvals <- vapply(rows, function(r) as.numeric(r[[1]]), numeric(1))
  ## A new block starts at the first row and wherever T stops increasing.
  starts <- c(TRUE, diff(tvals) <= 0)
  split(rows, cumsum(starts))
}

## The book's facet columns, as a named list of one 71 x 2 numeric matrix per
## facet label. Column identity comes from the banner, never from a position.
facet_book_columns <- function(tbl) {
  names_by_block <- facet_banner_names(tbl)
  blocks <- facet_data_blocks(tbl)
  stopifnot(length(names_by_block) == length(blocks))
  out <- list()
  for (b in seq_along(blocks)) {
    m <- matrix(as.numeric(unlist(blocks[[b]])), nrow = length(blocks[[b]]),
                byrow = TRUE)
    stopifnot(identical(as.integer(m[, 1]), 30:100))
    for (j in seq_along(names_by_block[[b]])) {
      out[[names_by_block[[b]][[j]]]] <- m[, c(2L * j, 2L * j + 1L), drop = FALSE]
    }
  }
  out
}

## The seven tables M25 ships plus the two facet tables M33 adds, in book order,
## with their CSV counterparts. Appendix table A-n is tables[[n]].
spec <- list(
  list(tbl = 1L,  ncol = 2L,  csv = "data-raw/norms_pid5_vrin.csv",       label = "A-1  inconsistency, FULL (INC)"),
  list(tbl = 2L,  ncol = 2L,  csv = "data-raw/norms_pid5sf_vrin.csv",     label = "A-2  inconsistency, SF (INCS)"),
  list(tbl = 3L,  ncol = 2L,  csv = "data-raw/norms_pid5_ors.csv",        label = "A-3  over-reporting (ORS)"),
  list(tbl = 4L,  ncol = 2L,  csv = "data-raw/norms_pid5_pimrd.csv",      label = "A-4  PID-5-PRD (PRD)"),
  list(tbl = 5L,  ncol = 11L, csv = "data-raw/norms_pid5_domains.csv",    label = "A-5  SRF domain scales"),
  list(tbl = 6L,  mode = "facet", csv = "data-raw/norms_pid5_facets.csv", label = "A-6  SRF facet scales"),
  list(tbl = 7L,  ncol = 11L, csv = "data-raw/norms_pid5sf_domains.csv",  label = "A-7  SF domain scales"),
  list(tbl = 8L,  mode = "facet", csv = "data-raw/norms_pid5sf_facets.csv", label = "A-8  SF facet scales"),
  list(tbl = 9L,  ncol = 13L, csv = "data-raw/norms_pid5bf_domains.csv",  label = "A-9  BF total + domain scales")
)

## ---- compare -------------------------------------------------------------

discrepancies <- list()
cat("PID-5 norms: book vs committed CSVs\n")
cat("source: ", epub_path, "\n\n", sep = "")

## One facet table: compare each book column to the CSV column carrying that
## facet's label. A label present on one side only, or a value that differs, is
## a discrepancy; the CSV's column order is never assumed.
compare_facets <- function(s, tbl, page) {
  book <- facet_book_columns(tbl)
  csv <- as.data.frame(readr::read_csv(s$csv, show_col_types = FALSE),
                       check.names = FALSE)
  out <- character(0)

  cat(sprintf("%-38s p. %-5s book %3d x %2d   csv %3d x %2d\n",
              s$label, page, nrow(book[[1]]), 1L + 2L * length(book),
              nrow(csv), ncol(csv)))

  if (!identical(as.integer(csv[[1]]), 30:100)) {
    return(sprintf("%s: CSV T column is not 30-100", s$label))
  }
  wanted <- as.vector(rbind(paste0(names(book), "_Raw"), paste0(names(book), "_Ptl")))
  missing <- setdiff(wanted, names(csv))
  extra <- setdiff(names(csv)[-1], wanted)
  if (length(missing) || length(extra)) {
    return(c(
      if (length(missing)) sprintf("%s: CSV is missing column %s", s$label, missing),
      if (length(extra)) sprintf("%s: CSV has unexpected column %s", s$label, extra)
    ))
  }

  for (facet in names(book)) {
    for (k in seq_len(2L)) {
      col <- paste0(facet, c("_Raw", "_Ptl")[[k]])
      b <- book[[facet]][, k]
      cv <- as.numeric(csv[[col]])
      bad <- which(b != cv)
      for (r in bad) {
        out <- c(out, sprintf(
          "%s: T = %d, column %s -- book %s, csv %s",
          s$label, csv[[1]][[r]], col, format(b[[r]]), format(cv[[r]])))
      }
    }
  }
  out
}

for (s in spec) {
  page <- page_for_table[[s$tbl]]

  if (identical(s$mode, "facet")) {
    found <- compare_facets(s, tables[[s$tbl]], page)
    for (d in found) discrepancies[[length(discrepancies) + 1L]] <- d
    next
  }

  book <- data_matrix(tables[[s$tbl]], s$ncol)
  csv <- as.data.frame(readr::read_csv(s$csv, show_col_types = FALSE))

  cat(sprintf("%-38s p. %-5s book %3d x %2d   csv %3d x %2d\n",
              s$label, page, nrow(book), ncol(book), nrow(csv), ncol(csv)))

  if (nrow(book) != nrow(csv) || ncol(book) != ncol(csv)) {
    discrepancies[[length(discrepancies) + 1L]] <- sprintf(
      "%s: DIMENSIONS book %dx%d vs csv %dx%d",
      s$label, nrow(book), ncol(book), nrow(csv), ncol(csv))
    next
  }

  bookn <- matrix(suppressWarnings(as.numeric(book)), nrow = nrow(book))
  csvn <- as.matrix(csv)
  storage.mode(csvn) <- "double"

  bad <- which(!(is.na(bookn) & is.na(csvn)) & (is.na(bookn) | is.na(csvn) | bookn != csvn),
               arr.ind = TRUE)
  if (nrow(bad)) {
    for (i in seq_len(nrow(bad))) {
      r <- bad[i, "row"]; k <- bad[i, "col"]
      discrepancies[[length(discrepancies) + 1L]] <- sprintf(
        "%s: row %d (%s=%s), column %s -- book %s, csv %s",
        s$label, r, names(csv)[[1]], csv[r, 1], names(csv)[[k]],
        book[r, k], format(csv[r, k]))
    }
  }
}

cat("\n")
if (!length(discrepancies)) {
  cat("RESULT: every cell of all ", length(spec),
      " tables matches the book.\n", sep = "")
} else {
  cat("RESULT: ", length(discrepancies), " discrepancy/discrepancies\n\n", sep = "")
  for (d in discrepancies) cat("  - ", d, "\n", sep = "")
  ## A non-zero exit, so a caller running this as a gate cannot mistake a
  ## printed discrepancy list for a pass.
  stop(length(discrepancies), " discrepancy/discrepancies against the book",
       call. = FALSE)
}
