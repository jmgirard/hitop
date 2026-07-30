# Verify data-raw/norms_*.csv against the published source (M25, AC1)
#
# Independent transcription of the PID-5 normative tables, extracted
# deterministically from the book's own table markup and diffed cell by cell
# against the committed CSVs. The extraction path never reads the CSVs; the
# comparison happens only after both sides exist, so every reported
# discrepancy is a fact about the CSVs rather than about a transcriber.
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

## The seven tables M25 ships, in book order, with their CSV counterparts.
## Appendix table A-n is tables[[n]].
spec <- list(
  list(tbl = 1L,  ncol = 2L,  csv = "data-raw/norms_pid5_vrin.csv",       label = "A-1  SRF VRIN (= INC)"),
  list(tbl = 2L,  ncol = 2L,  csv = "data-raw/norms_pid5sf_vrin.csv",     label = "A-2  100-item VRIN (= INC-S)"),
  list(tbl = 3L,  ncol = 2L,  csv = "data-raw/norms_pid5_ors.csv",        label = "A-3  ORS"),
  list(tbl = 4L,  ncol = 2L,  csv = "data-raw/norms_pid5_pimrd.csv",      label = "A-4  PIM-RD (= PRD)"),
  list(tbl = 5L,  ncol = 11L, csv = "data-raw/norms_pid5_domains.csv",    label = "A-5  SRF domain scales"),
  list(tbl = 7L,  ncol = 11L, csv = "data-raw/norms_pid5sf_domains.csv",  label = "A-7  SF domain scales"),
  list(tbl = 9L,  ncol = 13L, csv = "data-raw/norms_pid5bf_domains.csv",  label = "A-9  BF total + domain scales")
)

## ---- compare -------------------------------------------------------------

discrepancies <- list()
cat("PID-5 norms: book vs committed CSVs\n")
cat("source: ", epub_path, "\n\n", sep = "")

for (s in spec) {
  book <- data_matrix(tables[[s$tbl]], s$ncol)
  csv <- as.data.frame(readr::read_csv(s$csv, show_col_types = FALSE))
  page <- page_for_table[[s$tbl]]

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
  cat("RESULT: every cell of all seven tables matches the book.\n")
} else {
  cat("RESULT: ", length(discrepancies), " discrepancy/discrepancies\n\n", sep = "")
  for (d in discrepancies) cat("  - ", d, "\n", sep = "")
}
