# Verify the PID-5 norms against the published source (M25, AC1; M33; M35)
#
# Independent transcription of the PID-5 normative tables, extracted
# deterministically from the book's own table markup and diffed cell by cell
# against the committed CSVs and against the shipped `pid_norms`. The
# extraction path never reads either; the comparisons happen only after both
# sides exist, so every reported discrepancy is a fact about the package's data
# rather than about a transcriber.
#
# The seven M25 tables were transcribed by hand, so for those this is a
# transcription check. The two facet tables M33 adds carry 3,550 cells each and
# are machine-extracted by data-raw/extract_facet_norms.R, so for those this is
# a cross-check of two *independent reshapings* of the same markup: the
# extractor walks rows sequentially carrying each banner's facet names forward,
# while the block reader below ignores banners when assembling rows -- it cuts
# the numeric rows at their T-score restarts -- and recovers the facet names
# from the banners only afterwards, then compares column by *name* rather than
# by position. The third comparison M35 adds reads the same tables into
# `pid_norms`'s long format and diffs the shipped dataset itself, so
# data-raw/norms_pid5.R's assembly of the CSVs is covered too.
#
# Which layer covers what:
#
#   * This script is exhaustive -- every printed cell, in both directions --
#     but it is markup-based and maintainer-run: it needs the gitignored shelf
#     epub below, so it never runs in CI, and it cannot see a defect already
#     present in the book's own markup.
#   * The hand-read spot values in tests/testthat/test-norms.R are the only
#     layer that reads the rendered page, and so the only one that can catch
#     such a markup defect. Being anchors, they are not exhaustive.
#   * CI therefore sees those anchors and the structural invariants only. An
#     exhaustive check of the shipped data against the book happens exactly
#     when a maintainer runs this script.
#
# data-raw/mutate_norms_book_check.R is the evidence that the comparisons here
# catch what they are meant to: it seeds each known corruption of `pid_norms`
# in turn and requires this script to report it.
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
    m <- matrix(suppressWarnings(as.numeric(unlist(blocks[[b]]))),
                nrow = length(blocks[[b]]), byrow = TRUE)
    ## A cell the book prints as something non-numeric would arrive here as NA
    ## and compare equal to nothing; fail loudly rather than let it through.
    stopifnot(!anyNA(m), identical(as.integer(m[, 1]), 30:100))
    for (j in seq_along(names_by_block[[b]])) {
      out[[names_by_block[[b]][[j]]]] <- m[, c(2L * j, 2L * j + 1L), drop = FALSE]
    }
  }
  out
}

## The seven tables M25 ships plus the two facet tables M33 adds, in book order,
## with their CSV counterparts and the `pid_norms` (version, scale) coordinates
## the assembly comparison below reads them into. Appendix table A-n is
## tables[[n]]; `caption` is asserted against the table's own <caption>, so a
## re-flowed epub that renumbered the tables fails here rather than silently
## comparing A-6 against the A-8 CSV.
spec <- list(
  list(tbl = 1L,  ncol = 2L,  csv = "data-raw/norms_pid5_vrin.csv",       label = "A-1  inconsistency, FULL (INC)",
       caption = "Table A-1.", mode = "validity", version = "FULL", scale = "INC"),
  list(tbl = 2L,  ncol = 2L,  csv = "data-raw/norms_pid5sf_vrin.csv",     label = "A-2  inconsistency, SF (INCS)",
       caption = "Table A-2.", mode = "validity", version = "SF", scale = "INCS"),
  list(tbl = 3L,  ncol = 2L,  csv = "data-raw/norms_pid5_ors.csv",        label = "A-3  over-reporting (ORS)",
       caption = "Table A-3.", mode = "validity", version = "FULL", scale = "ORS"),
  list(tbl = 4L,  ncol = 2L,  csv = "data-raw/norms_pid5_pimrd.csv",      label = "A-4  PID-5-PRD (PRD)",
       caption = "Table A-4.", mode = "validity", version = "FULL", scale = "PRD"),
  list(tbl = 5L,  ncol = 11L, csv = "data-raw/norms_pid5_domains.csv",    label = "A-5  SRF domain scales",
       caption = "Table A-5.", mode = "domain", version = "FULL"),
  list(tbl = 6L,  mode = "facet", csv = "data-raw/norms_pid5_facets.csv", label = "A-6  SRF facet scales",
       caption = "Table A-6.", version = "FULL"),
  list(tbl = 7L,  ncol = 11L, csv = "data-raw/norms_pid5sf_domains.csv",  label = "A-7  SF domain scales",
       caption = "Table A-7.", mode = "domain", version = "SF"),
  list(tbl = 8L,  mode = "facet", csv = "data-raw/norms_pid5sf_facets.csv", label = "A-8  SF facet scales",
       caption = "Table A-8.", version = "SF"),
  list(tbl = 9L,  ncol = 13L, csv = "data-raw/norms_pid5bf_domains.csv",  label = "A-9  BF total + domain scales",
       caption = "Table A-9.", mode = "domain", version = "BF")
)

for (s in spec) {
  cap <- xml2::xml_find_first(tables[[s$tbl]], "./caption")
  stopifnot(!is.na(cap), startsWith(cell_text(cap), s$caption))
}

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
      cv <- suppressWarnings(as.numeric(csv[[col]]))
      ## NA-aware, like the non-facet path below: `b != cv` is NA wherever
      ## either side failed to parse, and which() drops NA, so a CSV cell typed
      ## `1.7O` would otherwise be reported as matching the book.
      bad <- which(is.na(b) | is.na(cv) | b != cv)
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

## ---- assembly: the shipped pid_norms vs the book --------------------------
## The comparison above stops at the CSVs, so data-raw/norms_pid5.R's reshaping
## of them into the long-format `pid_norms` sat between two layers with nothing
## exhaustive behind it. This third comparison closes that: it reads the book's
## tables into the same long format and diffs the shipped dataset against them
## row by row, keyed by (version, scale) plus T score -- or, on the four
## validity scales the book prints without one, raw score.
##
## The book's own wording is spelled out here rather than read from
## data-raw/norms_pid5.R, and the package-side names come from pid_domains /
## pid_scales. So the two crosswalks are written independently and must agree:
## a facet the build script mislabels is compared against the right column here
## and reported, where sharing one map would compare it against itself.

load("data/pid_domains.rda")
load("data/pid_scales.rda")

## Domain banners as the book prints them ("Negative affect"), against the
## package's own domain labels. The BF's "Total" column has no domain entry --
## it is the whole-form score, whose `pid_norms` scale is "total".
domain_banner <- c(
  "Negative affect" = "Negative affectivity",
  "Detachment"      = "Detachment",
  "Antagonism"      = "Antagonism",
  "Disinhibition"   = "Disinhibition",
  "Psychoticism"    = "Psychoticism"
)
stopifnot(all(domain_banner %in% pid_domains$Domain))

domain_stem <- function(banner) {
  vapply(banner, function(b) {
    if (identical(b, "Total")) return("total")
    lbl <- domain_banner[[b]]
    pid_domains$camelCase[[match(lbl, pid_domains$Domain)]]
  }, character(1), USE.NAMES = FALSE)
}

## The facet banners differ from the package's `Facet` labels only in case and
## in spelling out "&", so they are matched by a normalizing rule rather than by
## a second retyped table -- no strings to drift, and a caption matching no facet
## (or two) stops the script.
norm_label <- function(x) tolower(gsub("&", "and", x, fixed = TRUE))

facet_stem <- function(banner, version) {
  scales <- pid_scales[[version]]
  i <- match(norm_label(banner), norm_label(scales$Facet))
  stopifnot(!anyNA(i), !anyDuplicated(i))
  scales$camelCase[i]
}

## One spec entry -> the rows it contributes to `pid_norms`, in that object's
## own column order. `tscore` is NA on the validity tables, which print a raw
## score instead; every other table's first column is the T score.
book_long <- function(s) {
  tbl <- tables[[s$tbl]]

  if (identical(s$mode, "facet")) {
    cols <- facet_book_columns(tbl)
    stem <- facet_stem(names(cols), s$version)
    return(do.call(rbind, lapply(seq_along(cols), function(j) data.frame(
      version = s$version, scale = stem[[j]], tscore = 30:100,
      raw = cols[[j]][, 1], percentile = cols[[j]][, 2],
      stringsAsFactors = FALSE))))
  }

  m <- matrix(suppressWarnings(as.numeric(data_matrix(tbl, s$ncol))),
              ncol = s$ncol)
  stopifnot(nrow(m) > 0, !anyNA(m))

  if (identical(s$mode, "validity")) {
    return(data.frame(
      version = s$version, scale = s$scale, tscore = NA_integer_,
      raw = m[, 1], percentile = m[, 2], stringsAsFactors = FALSE))
  }

  banner <- facet_banner_names(tbl, width = 1L + (s$ncol - 1L) %/% 2L)
  stopifnot(length(banner) == 1L)
  stem <- domain_stem(banner[[1]])
  do.call(rbind, lapply(seq_along(stem), function(j) data.frame(
    version = s$version, scale = stem[[j]], tscore = as.integer(m[, 1]),
    raw = m[, 2L * j], percentile = m[, 2L * j + 1L],
    stringsAsFactors = FALSE)))
}

book <- do.call(rbind, lapply(spec, book_long))
load("data/pid_norms.rda")
ship <- as.data.frame(pid_norms)

## The key is a name, never a row position: (version, scale) plus whichever of
## T score or raw score the table is indexed by.
norm_key <- function(d) paste(d$version, d$scale,
                              ifelse(is.na(d$tscore),
                                     paste0("raw=", d$raw), paste0("T=", d$tscore)),
                              sep = "|")
kb <- norm_key(book)
ks <- norm_key(ship)
stopifnot(!anyDuplicated(kb), !anyDuplicated(ks))

cat("\nassembly: shipped pid_norms vs the book\n")
cat(sprintf("book %d rows over %d (version, scale) columns   pid_norms %d rows over %d\n",
            nrow(book), nrow(unique(book[c("version", "scale")])),
            nrow(ship), nrow(unique(ship[c("version", "scale")]))))

## Category 1 and 2: rows on one side only. Reported apart, because a shipped
## row the book never printed and a printed row the package dropped are
## different defects.
extra_rows <- ship[!ks %in% kb, , drop = FALSE]
missing_rows <- book[!kb %in% ks, , drop = FALSE]

## Category 3: rows on both sides whose values differ. NA-aware in the strict
## direction -- a cell that failed to parse on either side is reported, never
## passed over as equal (`NA != NA` is NA, which which() would drop).
i <- match(kb, ks)
paired <- !is.na(i)
differs <- function(a, b) is.na(a) | is.na(b) | a != b
value_bad <- which(paired &
                     (differs(book$raw, ship$raw[i]) |
                        differs(book$percentile, ship$percentile[i])))

## Per-column comparison counts, so a reader can see that every one of the
## dataset's columns was compared and that the counts sum to its row count.
pairs <- unique(rbind(book[c("version", "scale")], ship[c("version", "scale")]))
pairs <- pairs[order(match(pairs$version, c("FULL", "SF", "BF")), pairs$scale), ]
counted <- 0L
line <- character(0)
for (r in seq_len(nrow(pairs))) {
  v <- pairs$version[[r]]; sc <- pairs$scale[[r]]
  n <- sum(paired & book$version == v & book$scale == sc)
  counted <- counted + n
  line <- c(line, sprintf("%-4s %-24s %3d", v, sc, n))
  if (length(line) == 3L || r == nrow(pairs)) {
    cat("  ", paste(line, collapse = "  "), "\n", sep = "")
    line <- character(0)
  }
}
cat(sprintf("  compared %d rows over %d columns (pid_norms has %d rows)\n",
            counted, nrow(pairs), nrow(ship)))

assembly <- c(
  if (nrow(extra_rows)) sprintf(
    "assembly: pid_norms has a row the book does not print -- %s %s %s",
    extra_rows$version, extra_rows$scale,
    ifelse(is.na(extra_rows$tscore), paste0("raw = ", extra_rows$raw),
           paste0("T = ", extra_rows$tscore))),
  if (nrow(missing_rows)) sprintf(
    "assembly: the book prints a row pid_norms does not carry -- %s %s %s",
    missing_rows$version, missing_rows$scale,
    ifelse(is.na(missing_rows$tscore), paste0("raw = ", missing_rows$raw),
           paste0("T = ", missing_rows$tscore))),
  if (length(value_bad)) sprintf(
    "assembly: %s %s %s -- book raw %s ptl %s, pid_norms raw %s ptl %s",
    book$version[value_bad], book$scale[value_bad],
    ifelse(is.na(book$tscore[value_bad]), paste0("raw = ", book$raw[value_bad]),
           paste0("T = ", book$tscore[value_bad])),
    format(book$raw[value_bad]), format(book$percentile[value_bad]),
    format(ship$raw[i[value_bad]]), format(ship$percentile[i[value_bad]]))
)

cat(sprintf("  rows in pid_norms only: %d   rows in the book only: %d   values differing: %d\n",
            nrow(extra_rows), nrow(missing_rows), length(value_bad)))

cat("\n")
n_bad <- length(discrepancies) + length(assembly)
if (!n_bad) {
  cat("RESULT: every cell of all ", length(spec),
      " tables matches the book, in the CSVs and in the shipped pid_norms.\n",
      sep = "")
} else {
  cat("RESULT: ", n_bad, " discrepancy/discrepancies\n\n", sep = "")
  for (d in c(discrepancies, assembly)) cat("  - ", d, "\n", sep = "")
  ## A non-zero exit, so a caller running this as a gate cannot mistake a
  ## printed discrepancy list for a pass.
  stop(n_bad, " discrepancy/discrepancies against the book", call. = FALSE)
}
