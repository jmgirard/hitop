## Extract the PID-5 facet normative tables (A-6, A-8) from the book (M33)
#
# Writes data-raw/norms_pid5_facets.csv and data-raw/norms_pid5sf_facets.csv
# from the book's own table markup. The domain and validity CSVs were small
# enough to transcribe by hand (M25); these two carry 3,550 cells each side of
# the raw/percentile pair, so they are machine-extracted here instead.
#
# That makes the check in data-raw/verify_norms_against_book.R a *cross-check of
# two independent reshapings*, not a transcription check: this script walks the
# rows sequentially, carrying the facet names forward from each banner row,
# while the verifier ignores banners for row assembly and instead cuts the
# numeric rows at their T-score restarts, recovering names from the banners
# afterwards. A reshaping bug would have to occur identically in both to pass.
# The layer that catches a displaced column outright is the hand-read spot
# values in tests/testthat/test-norms.R, read off the rendered page.
#
# Requires xml2 and readr installed locally; maintainer tools, not package
# dependencies (M18 lesson).
#
# Usage:  Rscript data-raw/extract_facet_norms.R

stopifnot(requireNamespace("xml2", quietly = TRUE))
stopifnot(requireNamespace("readr", quietly = TRUE))

epub_path <- "cairn/references/sources/markon2024.epub"
if (!file.exists(epub_path)) {
  stop("Source not on the shelf: ", epub_path, call. = FALSE)
}

exdir <- tempfile("markon2024-")
utils::unzip(epub_path, exdir = exdir)
appendix <- file.path(exdir, "OEBPS", "xhtml", "20_Appendix.xhtml")
stopifnot(file.exists(appendix))

doc <- xml2::read_html(appendix)

cell_text <- function(node) {
  txt <- xml2::xml_text(node)
  txt <- gsub(" ", " ", txt)               # nbsp
  txt <- gsub("[–−]", "-", txt)       # en-dash / minus used as hyphen
  trimws(gsub("[[:space:]]+", " ", txt))
}

table_rows <- function(tbl) {
  lapply(xml2::xml_find_all(tbl, ".//tr"), function(tr) {
    vapply(xml2::xml_find_all(tr, "./td|./th"), cell_text, character(1))
  })
}

is_num <- function(x) grepl("^-?[0-9]+(\\.[0-9]+)?$", x)

## Sequential walk. A width-6 row is a block banner: a blank leading cell then
## five facet names. A width-11 row whose first cell is a number is a data row
## belonging to whichever banner was seen last (T, then five Raw/Percentile
## pairs in banner order). Every other row -- the repeated column sub-headers,
## the loose page-number rows -- fails both tests and is dropped by that test
## alone, never by position.
facet_blocks <- function(tbl, n_facets_per_block = 5L) {
  rows <- table_rows(tbl)
  current <- NULL
  blocks <- list()
  for (r in rows) {
    if (length(r) == n_facets_per_block + 1L && !is_num(r[[1]])) {
      current <- as.character(r[-1])
      stopifnot(all(nzchar(current)))
      blocks[[length(blocks) + 1L]] <- list(facets = current, rows = list())
      next
    }
    if (length(r) == 2L * n_facets_per_block + 1L && is_num(r[[1]])) {
      stopifnot(!is.null(current))
      k <- length(blocks)
      blocks[[k]]$rows[[length(blocks[[k]]$rows) + 1L]] <- r
    }
  }
  blocks
}

## One block -> a data frame of T plus `<Facet>_Raw` / `<Facet>_Ptl` columns.
block_frame <- function(block) {
  m <- matrix(
    unlist(block$rows),
    nrow = length(block$rows),
    byrow = TRUE
  )
  num <- matrix(as.numeric(m), nrow = nrow(m))
  stopifnot(!anyNA(num))
  out <- list(T = as.integer(num[, 1]))
  for (j in seq_along(block$facets)) {
    out[[paste0(block$facets[[j]], "_Raw")]] <- num[, 2L * j]
    out[[paste0(block$facets[[j]], "_Ptl")]] <- num[, 2L * j + 1L]
  }
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

extract_facet_table <- function(tbl) {
  blocks <- facet_blocks(tbl)
  stopifnot(length(blocks) == 5L)
  frames <- lapply(blocks, block_frame)
  ## Every block must carry the same T column, printed 30-100, so the blocks
  ## can be joined side by side by position rather than by a merge.
  for (f in frames) {
    stopifnot(identical(f$T, 30:100))
  }
  out <- cbind(frames[[1]], do.call(cbind, lapply(frames[-1], function(f) f[-1])))
  stopifnot(ncol(out) == 51L, nrow(out) == 71L)
  ## Facet columns are ordered as the book prints them, block by block.
  out
}

tables <- xml2::xml_find_all(doc, "//table")

spec <- list(
  list(tbl = 6L, csv = "data-raw/norms_pid5_facets.csv",   label = "A-6  SRF facet scales"),
  list(tbl = 8L, csv = "data-raw/norms_pid5sf_facets.csv", label = "A-8  SF facet scales")
)

for (s in spec) {
  out <- extract_facet_table(tables[[s$tbl]])
  readr::write_csv(out, s$csv)
  cat(sprintf("%-26s -> %-36s %d x %d\n", s$label, s$csv, nrow(out), ncol(out)))
}
