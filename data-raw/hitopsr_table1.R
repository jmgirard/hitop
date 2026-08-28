# Table 1 of the HiTOP-SR introduction paper, extracted from the shelf PDF.
#
# Sourced by data-raw/verify_hitopsr_scale_name.R (which compares two committed
# scale names against their Table 1 cells) and by data-raw/verify_hitopsr_names.R
# (which reconciles every label the table prints against the two shipped name
# tables). It lives in its own file so neither verifier owns the extraction: the
# extraction never sees a committed name, which is what keeps both comparisons
# from being circular (IP2).
#
# Maintainer-run, never CI: it needs the gitignored source shelf and pdftotext.

hitopsr_source_pdf <- "cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf"

## The document D-042 admits as this package's source for HiTOP-SR scale names,
## pinned by content rather than by filename -- the shelf is gitignored, so a
## future run could otherwise be reading a different manuscript at this path.
hitopsr_source_sha256 <-
  "1c211219b7fe13f8ed172f9210c152a642a9be77d790e08d795843c25da8e425"

# Check the preconditions and return the shelf copy's sha256. Stops rather than
# reports: a missing shelf, a missing pdftotext and a substituted document are
# all "this run proves nothing", and a printed report must never be mistakable
# for a pass.
hitopsr_source_check <- function(pdf = hitopsr_source_pdf) {
  if (!file.exists(pdf)) {
    stop("source not on the shelf: ", pdf, call. = FALSE)
  }
  if (nchar(Sys.which("pdftotext")) == 0L) {
    stop("pdftotext not found; install poppler", call. = FALSE)
  }
  sha <- sub(
    " .*$", "",
    system2("shasum", c("-a", "256", shQuote(pdf)), stdout = TRUE)
  )
  if (!identical(sha, hitopsr_source_sha256)) {
    stop("shelf PDF is not the document D-042 admits\n",
         "  expected: ", hitopsr_source_sha256, "\n  found:    ", sha,
         call. = FALSE)
  }
  sha
}

# The rows of Table 1, as a data frame of label / page / shape.
#
# Three things make the extraction more than a line split.
#
#   1. The proof is stamped with a rotated "For Peer Review" watermark, which
#      pdftotext interleaves into the text as the fragments Fo / rP / ee / rR /
#      ev / iew -- sometimes standing alone in a column gap ("Fo    3"), which
#      would otherwise read as a row labelled "Fo", and sometimes glued to a
#      number ("0.86rR", "ee0.67"). They are removed before anything else.
#
#   2. Rows come in two shapes. Most extract with the label and its numeric
#      cells on one line ("Appearance Focus   5   0.81 ..."); where the
#      watermark splits the layout, a run of labels extracts on lines of their
#      own with the numeric cells following on separate lines ("Non-suicidal
#      Self-injury"). Both shapes are kept; the shape is reported so a caller
#      can show that it exercised each.
#
#   3. The manuscript numbers its own lines in the left margin, and the table
#      is bracketed by running heads, the caption, the column header, the Note
#      and a footnote. Those are stripped by position and by pattern, never by
#      matching a scale name.
#
# The table's own section headers ("Somatoform Scales", "Superspectra and
# Spectra Scales", ...) are separated out rather than dropped, so a caller can
# see that the section structure was recognized instead of silently eaten.
hitopsr_table1_rows <- function(pdf = hitopsr_source_pdf) {
  txt <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
  pages <- split(txt, cumsum(grepl("\f", txt)))
  has <- function(p, re) any(grepl(re, p))

  first <- which(vapply(pages, has, logical(1), "^ *[0-9]* *Table 1\\."))[1]
  after <- which(vapply(pages, has, logical(1), "^ *[0-9]* *Table 2\\."))[1]
  if (is.na(first) || is.na(after) || after <= first) {
    stop("could not locate Table 1's page range in the source", call. = FALSE)
  }
  idx <- seq(first, after - 1L)

  page_no <- rep(idx, vapply(pages[idx], length, integer(1)))
  line <- gsub("\f", "", unlist(pages[idx], use.names = FALSE))

  ## 1. The watermark.
  wm <- "Fo|rP|ee|rR|ev|iew"
  line <- gsub(paste0("(?<=[0-9])(", wm, ")"), "", line, perl = TRUE)
  line <- gsub(paste0("(", wm, ")(?=[0-9])"), "", line, perl = TRUE)
  line <- gsub(paste0("(?<![A-Za-z])(", wm, ")(?![A-Za-z])"), "", line, perl = TRUE)

  ## 3a. The manuscript's left-margin line numbers, and blank lines.
  line <- sub("^ *[0-9]{1,2}(?= {2,}| *$)", "", line, perl = TRUE)
  line <- sub("[ \t]+$", "", line)
  keep <- nzchar(trimws(line))
  line <- line[keep]
  page_no <- page_no[keep]

  ## 3b. The table body ends at the Note, which is followed by the dagger
  ## footnote. Everything after it is prose about the table, not a row.
  note <- grep("^ *Note\\.", line)
  if (length(note)) {
    line <- line[seq_len(note[1] - 1L)]
    page_no <- page_no[seq_len(note[1] - 1L)]
  }

  ## 3c. Running heads, the caption and its runover, and the column header.
  furniture <- grepl(paste0(
    "manuscriptcentral|^ *Assessment *$|Page [0-9]+ of [0-9]+|",
    "Assessing the HiTOP Model|^ *Table 1\\.|",
    "Superspectra and Spectra Scales in|^ *Scale / Subscale"
  ), line)
  line <- line[!furniture]
  page_no <- page_no[!furniture]

  ## A row must carry a letter; a cells-only continuation line does not.
  lettered <- grepl("[A-Za-z]", line)
  line <- line[lettered]
  page_no <- page_no[lettered]

  ## 2. Split the label from the numeric cells. A cell block starts at the
  ## first run of two or more spaces followed by a number, a minus sign or the
  ## Range column's opening bracket; no scale label contains one.
  label <- trimws(sub("(?<= {2})-?[0-9\\[].*$", "", line, perl = TRUE))
  shape <- ifelse(label == trimws(line), "label-only", "with-cells")

  out <- data.frame(
    label = label,
    page = page_no,
    shape = shape,
    section = grepl("\\bScales\\b", label),
    stringsAsFactors = FALSE
  )

  ## The domain this whole family quantifies over is the extracted table. A
  ## pdftotext or pagination change could empty it or drop a shape silently,
  ## and every caller's report would then pass by having nothing to compare.
  if (nrow(out) == 0L) {
    stop("Table 1 extracted no rows", call. = FALSE)
  }
  if (!all(c("label-only", "with-cells") %in% out$shape)) {
    stop("Table 1 extracted only one row shape (", 
         paste(unique(out$shape), collapse = ", "),
         "); the extraction no longer exercises both", call. = FALSE)
  }
  if (!any(out$section)) {
    stop("Table 1 extracted no section headers", call. = FALSE)
  }
  out
}
