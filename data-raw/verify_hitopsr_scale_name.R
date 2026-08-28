# Verify the HiTOP-SR NSSI scale name against the published source (M058, AC1)
#
# The HiTOP-SR introduction paper is the naming authority for a HiTOP-SR scale
# (M058's Decisions; D-018's per-content-type rule). This script reads that
# document from the gitignored source shelf and does two things the milestone
# file cannot do for itself:
#
#   1. Inventories every rendering of the scale name the paper prints, with the
#      page of each. The paper disagrees with itself -- its tables and its prose
#      capitalize differently -- and IP1 requires such a discrepancy to stay
#      visible rather than be silently resolved, so the inventory is regenerated
#      here rather than transcribed into cairn/SOURCES.md by hand. The OQ entry
#      in that file quotes this script's output.
#
#   2. Compares the string committed in data-raw/hitopsr_items.csv character for
#      character against the cell in Table 1. The comparison reads the source at
#      run time; it never compares one transcription against another, which is
#      why neither the milestone file nor SOURCES.md is an input here.
#
# Maintainer-run, never CI: it needs both the shelf PDF (gitignored) and
# pdftotext. Exits non-zero on any mismatch, so a printed report cannot be
# mistaken for a pass.

pdf <- "cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf"

## The document this milestone was decided against, pinned by content rather
## than by filename -- the shelf is gitignored, so a future run could otherwise
## be reading a different manuscript under the same path.
expected_sha256 <-
  "1c211219b7fe13f8ed172f9210c152a642a9be77d790e08d795843c25da8e425"

## The name as committed. Read from the keying CSV, never typed here: typing it
## would make this script a second transcription and the comparison circular.
items <- read.csv("data-raw/hitopsr_items.csv", stringsAsFactors = FALSE)
committed <- unique(items$Scale[grepl("hurt myself|cut myself|bit myself", items$Text)])

stopifnot(
  "the NSSI items no longer identify a single scale" = length(committed) == 1
)

if (!file.exists(pdf)) {
  stop("source not on the shelf: ", pdf, call. = FALSE)
}
if (nchar(Sys.which("pdftotext")) == 0L) {
  stop("pdftotext not found; install poppler", call. = FALSE)
}

sha <- sub(" .*$", "", system2("shasum", c("-a", "256", shQuote(pdf)), stdout = TRUE))
if (!identical(sha, expected_sha256)) {
  stop("shelf PDF is not the document M058 was decided against\n",
       "  expected: ", expected_sha256, "\n  found:    ", sha, call. = FALSE)
}

txt <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
pages <- split(txt, cumsum(grepl("\f", txt)))

## Per page, collapsed to one string. The proof interleaves the manuscript's
## line numbers into the text, so a name broken across a typeset line arrives as
## "Self-" + a line number + the remainder; that noise is removed, ordinary
## column wraps are not (they are reported separately below).
flat <- vapply(pages, function(p) {
  x <- gsub("\f", " ", paste(p, collapse = " "))
  x <- gsub("[ \t]+", " ", x)
  gsub("Self-( ?[0-9]+ ?)* ?", "Self-", x)
}, character(1))

pattern <- "Non-?[Ss]uicidal Self-?[Ii]njur(y|ies)?"

found <- do.call(rbind, lapply(seq_along(flat), function(i) {
  m <- regmatches(flat[[i]], gregexpr(pattern, flat[[i]]))[[1]]
  if (length(m)) data.frame(page = i, variant = m, stringsAsFactors = FALSE)
}))

## A cell that wraps mid-name inside a table puts the two halves either side of
## a neighbouring column's text, so the pattern above cannot span it. Those
## pages are reported rather than counted, so the inventory never silently
## undercounts.
wrapped <- setdiff(
  which(grepl("Non-?[Ss]uicidal Self-", flat) & grepl("injur", flat, ignore.case = TRUE)),
  unique(found$page)
)

cat("Source: ", pdf, "\n", sep = "")
cat("sha256: ", sha, " (matches)\n\n", sep = "")

cat("Renderings printed by the paper, by page:\n")
tab <- table(found$variant)
for (v in names(tab)) {
  pp <- sort(unique(found$page[found$variant == v]))
  cat(sprintf("  %-26s %d occurrence%s on page%s %s\n",
              v, tab[[v]], if (tab[[v]] == 1) "" else "s",
              if (length(pp) == 1) "" else "s", paste(pp, collapse = ", ")))
}
if (length(wrapped)) {
  cat(sprintf("  %-26s page%s %s (name wraps across a table cell; read by eye)\n",
              "[wrapped occurrence]", if (length(wrapped) == 1) "" else "s",
              paste(wrapped, collapse = ", ")))
}

## Table 1's name column extracts to lines carrying nothing else, so the cell is
## the whole trimmed line -- and a line consisting solely of the name is the
## table-cell signature, since the paper's prose always continues the sentence
## on the same line. The scan is over every page rather than over pages chosen
## by the committed string, which would make the comparison circular.
## A trailing parenthetical is admitted into the cell rather than excluded from
## the match: AC1 stops on a glossed cell rather than stripping the gloss, and a
## cell regex that refused to match one would report "found 0" instead, leaving
## that branch unreachable.
cell_pattern <- paste0("^", pattern, "( *\\([^)]*\\))?$")
cells <- unique(unlist(lapply(pages, function(p) {
  lines <- trimws(p)
  lines[grepl(cell_pattern, lines)]
})))

cat("\nCell read from the paper's tables: ",
    if (length(cells)) paste(sprintf("%s", cells), collapse = " / ") else "(none found)",
    "\nString committed in data-raw/hitopsr_items.csv: ", committed, "\n", sep = "")

bad <- character(0)
if (length(cells) != 1L) {
  bad <- c(bad, sprintf("expected one distinct table cell, found %d", length(cells)))
} else {
  if (grepl("[()]", cells)) {
    bad <- c(bad, sprintf(
      "the table cell now carries a parenthetical (%s); M058's AC1 stops here rather than stripping it",
      cells))
  }
  if (!identical(cells, committed)) {
    bad <- c(bad, sprintf("committed %s does not match the source cell %s",
                          encodeString(committed, quote = '"'),
                          encodeString(cells, quote = '"')))
  }
}

cat("\n")
if (!length(bad)) {
  cat("RESULT: the committed scale name matches the source cell character for character.\n")
} else {
  cat("RESULT: ", length(bad), " discrepancy/discrepancies\n\n", sep = "")
  for (b in bad) cat("  - ", b, "\n", sep = "")
  stop(length(bad), " discrepancy/discrepancies against the source", call. = FALSE)
}
