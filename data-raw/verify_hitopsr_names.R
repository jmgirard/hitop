# Reconcile every name Table 1 prints against the two shipped HiTOP-SR name
# tables (M059, AC4)
#
# data-raw/verify_hitopsr_scale_name.R checks the two names this package has
# deliberately adopted from the introduction paper. This script asks the
# complementary question: does any OTHER label in Table 1 disagree with what
# the package ships? It was the M041 plan gate's reconciliation that found the
# `Body Focus` / `Appearance Focus` divergence M059 fixes, and running it as a
# script rather than by hand is what lets the next source revision be checked
# the same way.
#
# Nothing here is hardcoded from the milestone file. The label side comes from
# data-raw/hitopsr_table1.R, which reads Table 1 without seeing a committed
# name; the expected count comes from the paper's own prose, read at run time;
# the package side comes from the built tables. Maintainer-run, never CI: it
# needs the gitignored shelf and pdftotext. Exits non-zero on any departure
# from the reconciled state, so a printed report cannot be mistaken for a pass.

source("data-raw/hitopsr_table1.R")
devtools::load_all(quiet = TRUE)

sha <- hitopsr_source_check()
rows <- hitopsr_table1_rows()

txt <- system2(
  "pdftotext",
  c("-layout", shQuote(hitopsr_source_pdf), "-"),
  stdout = TRUE
)
pages <- split(txt, cumsum(grepl("\f", txt)))
flat <- vapply(pages, function(p) {
  gsub("[ \t]+", " ", gsub("\f", " ", paste(p, collapse = " ")))
}, character(1))

bad <- character(0)
note <- function(...) bad <<- c(bad, paste0(...))

cat("Source: ", hitopsr_source_pdf, "\n", sep = "")
cat("sha256: ", sha, " (matches)\n\n", sep = "")

# ---- The table's own structure ----------------------------------------------
#
# Reported by name, not by count alone: a count would be satisfied by a
# partition that put the wrong rows on either side of it.

headers <- rows$label[rows$section]
cat("Section headers Table 1 prints (", length(headers), "):\n", sep = "")
for (h in headers) cat("  ", h, "\n", sep = "")

block_header <- "Superspectra and Spectra Scales"
at <- match(block_header, rows$label)
if (is.na(at)) {
  stop("Table 1 no longer carries a '", block_header, "' section", call. = FALSE)
}
in_block <- seq_len(nrow(rows)) > at & !rows$section
block <- rows$label[in_block]
labels <- rows$label[!rows$section & !in_block]

cat("\nSuperspectra and Spectra block (", length(block),
    " HiTOP-BR scales, outside both HiTOP-SR tables):\n", sep = "")
for (b in block) cat("  ", b, "\n", sep = "")

## The block is excluded from the reconciliation below, so nothing downstream
## would notice a section boundary that swapped one of its members for a
## primary scale while leaving both counts intact. Its membership is therefore
## pinned here, by name, to what M059 measured. These are HiTOP-BR names and
## this milestone does not reconcile them against hitopbr_scales -- Table 1
## prints `p-factor` where that table ships `p-Factor`, a divergence the
## ROADMAP carries as a candidate rather than a rename folded into M059.
expected_block <- c(
  "Externalizing", "p-factor", "Internalizing", "Somatoform",
  "Detachment", "Thought Disorder", "Disinhibition", "Antagonism"
)
if (!identical(block, expected_block)) {
  note("the Superspectra and Spectra block reads ",
       paste(encodeString(block, quote = '"'), collapse = ", "),
       "; M059 measured ",
       paste(encodeString(expected_block, quote = '"'), collapse = ", "))
}
if (length(headers) != 13L) {
  note("Table 1 has ", length(headers),
       " section headers where M059 measured 13")
}

# ---- The count, read from the paper's own prose -----------------------------
#
# Not a constant transcribed from the milestone file: the figure is grepped out
# of the manuscript at run time, so a source revision that renumbers the
# instrument moves the expectation with it instead of failing as a mismatch.

split_counts <- regmatches(
  flat,
  gregexpr("([0-9]+) primary scales and ([0-9]+) subscales", flat)
)
total_counts <- regmatches(
  flat,
  gregexpr("([0-9]+) primary scales and subscales", flat)
)
stated <- unique(c(
  vapply(unlist(split_counts), function(x) {
    n <- as.integer(regmatches(x, gregexpr("[0-9]+", x))[[1]])
    sum(n)
  }, numeric(1)),
  as.numeric(sub(" .*$", "", unlist(total_counts)))
))
where <- c(
  which(lengths(split_counts) > 0),
  which(lengths(total_counts) > 0)
)
cat("\nCount the paper's prose states, on page(s) ",
    paste(sort(unique(where)), collapse = ", "), ": ",
    paste(stated, collapse = " / "), "\n", sep = "")
cat("Labels extracted outside the block: ", length(labels), "\n", sep = "")

if (length(stated) != 1L) {
  note("the paper states more than one scale count (",
       paste(stated, collapse = ", "), "); the reconciliation has no oracle")
} else if (!identical(as.integer(stated), length(labels))) {
  note("extracted ", length(labels), " labels outside the block where the ",
       "paper's prose states ", stated)
}

# ---- The reconciliation -----------------------------------------------------

shipped <- c(hitopsr_scales$Scale, hitopsr_subscales$Subscale)
only_source <- sort(setdiff(labels, shipped))
only_package <- sort(setdiff(shipped, labels))

cat("\nIn Table 1, not in the shipped tables (", length(only_source), "):\n",
    sep = "")
for (x in only_source) cat("  ", x, "\n", sep = "")
cat("In the shipped tables, not in Table 1 (", length(only_package), "):\n",
    sep = "")
for (x in only_package) cat("  ", x, "\n", sep = "")

## The reconciled state, pinned to what M059 measured. A member on either side
## is either an extraction defect -- fixed in data-raw/hitopsr_table1.R under
## the milestone that finds it -- or a real divergence between the naming
## authority and what this package ships, which IP1 keeps visible as an OQ-n
## entry in cairn/SOURCES.md and the ROADMAP tracks as a candidate row. Neither
## disposal is available to a run: the run fails, and a person adjudicates.
expected_source <- "Manic Energy†"
expected_package <- "Manic Energy"
show <- function(x) {
  if (!length(x)) "none" else paste(encodeString(x, quote = '"'), collapse = ", ")
}
if (!identical(only_source, expected_source)) {
  note("source-only names are ", show(only_source), "; M059 reconciled them to ",
       show(expected_source),
       ", the same name carrying Table 1's footnote dagger")
}
if (!identical(only_package, expected_package)) {
  note("package-only names are ", show(only_package),
       "; M059 reconciled them to ", show(expected_package))
}

# ---- Positive controls on the extraction ------------------------------------
#
# The label set is the domain every claim above quantifies over, and three ways
# it could be wrong leave the counts intact. Each control is checked against
# something outside the extractor's own output.

## (a) The watermark is really there to be stripped. Without this, a pdftotext
## that stopped emitting the fragments would leave the stripping untested and
## every report unchanged.
fragments <- hitopsr_table1_watermark_fragments
raw_pages <- unlist(pages[unique(rows$page)], use.names = FALSE)
present <- fragments[vapply(fragments, function(f) {
  any(grepl(paste0("(?<![A-Za-z])", f, "(?![A-Za-z])|[0-9]", f, "|", f, "[0-9]"),
            raw_pages, perl = TRUE))
}, logical(1))]
cat("\nWatermark fragments present in the unstripped table pages: ",
    if (length(present)) paste(present, collapse = ", ") else "(none)", "\n",
    sep = "")
if (!length(present)) {
  note("no watermark fragment appears in the unstripped table pages, so the ",
       "stripping this extraction depends on was not exercised")
}

## (b) ...and none of it survived as a label. A stray fragment standing in a
## column gap extracts as a row of its own.
survivors <- intersect(c(labels, block, headers), fragments)
if (length(survivors)) {
  note("watermark fragments survived as labels: ",
       paste(survivors, collapse = ", "))
}

## (c) A label's final character survives. The M041 plan gate's hand
## reconciliation hit seven apparent mismatches that were pdftotext truncating
## a final character under the watermark, so a label whose last character is a
## footnote dagger is the probe for that family.
if (!any(grepl("†$", c(labels, block)))) {
  note("no extracted label ends in a footnote dagger; the truncation the M041 ",
       "reconciliation hit would now pass unnoticed")
}

## (d) Both row shapes reached the label set, not merely the table.
shapes <- unique(rows$shape[!rows$section])
cat("Row shapes among the extracted labels: ",
    paste(sort(shapes), collapse = ", "), "\n", sep = "")
if (!all(c("label-only", "with-cells") %in% shapes)) {
  note("labels extracted in only the ", paste(shapes, collapse = ", "),
       " shape; one branch of the extraction is untested")
}

## (e) The watermark strip removes the fragments and nothing else. Each
## fragment spells an ordinary letter run, so a strip that is not bounded by
## `(?<![A-Za-z])` and `(?![A-Za-z])` edits any word carrying one: before M071
## the digit-anchored calls rewrote `committee1` to `committ1` and `free1` to
## `fr1`. Checked on synthetic text rather than the proof, because the proof
## prints no such label -- which is why the defect survived every run over it.
##
## The probes are assembled from the fragment vector rather than spelled out,
## so a source whose watermark changes moves them with it, and so this file
## states the fragments exactly once.
inside <- c(paste0("a", fragments, "1"), paste0("1", fragments, "a"))
kept <- hitopsr_table1_strip_watermark(inside)
if (!identical(kept, inside)) {
  note("the watermark strip edits a word carrying a fragment inside it: ",
       paste(sprintf("%s -> %s", inside[kept != inside], kept[kept != inside]),
             collapse = ", "))
}
## ...and the passing half, so the control cannot be green by stripping nothing
## at all. These are the three shapes the proof prints: a fragment after a
## number, before one, and standing in a column gap.
glued <- c(paste0("0.86", fragments), paste0(fragments, "0.67"),
           paste0(fragments, "   3"))
want <- rep(c("0.86", "0.67", "   3"), each = length(fragments))
got <- hitopsr_table1_strip_watermark(glued)
if (!identical(got, want)) {
  note("the watermark strip no longer removes the fragments the proof prints: ",
       paste(sprintf("%s -> %s", glued[got != want], got[got != want]),
             collapse = ", "))
}
cat("Watermark strip on synthetic text: ", length(inside),
    " words carrying a fragment left unchanged, ", length(glued),
    " watermarked cells stripped\n", sep = "")

## (f) A section header is recognized because it is one of Table 1's thirteen
## pinned headers, not because its label carries the word `Scales`. That older
## test misclassified in both directions and the reconciliation above absorbed
## each in silence: a header a revision renames without the word joined the
## label set, and a scale whose printed name carried the word left it. The
## control renames one of each in the `-layout` dump the extraction reads.
hitopsr_table1_rename_label <- function(txt, from, to) {
  group <- cumsum(grepl("\f", txt))
  on_table <- which(group %in% (hitopsr_table1_pages() - 1L) &
                      grepl(from, txt, fixed = TRUE))
  if (length(on_table) != 1L) {
    stop("the control's label ", encodeString(from, quote = '"'), " matched ",
         length(on_table), " of Table 1's lines, not one", call. = FALSE)
  }
  txt[on_table] <- sub(from, to, txt[on_table], fixed = TRUE)
  txt
}

raw_txt <- system2(
  "pdftotext", c("-layout", shQuote(hitopsr_source_pdf), "-"), stdout = TRUE
)
## The passing half: the same dump, handed in rather than read, is the same
## table -- so a mutation's effect below is the mutation's, not the seam's.
if (!identical(hitopsr_table1_rows(txt = raw_txt), rows)) {
  note("handing the -layout dump in returns a different table than reading ",
       "it, so the header controls below prove nothing")
}

## A header renamed to carry no `Scales`. Before M071 it read as a scale and
## joined the reconciliation's label set; now the run stops naming it.
renamed <- hitopsr_table1_rename_label(raw_txt, "Antisocial Scales",
                                       "Antisocial")
control <- try(hitopsr_table1_rows(txt = renamed), silent = TRUE)
if (!inherits(control, "try-error")) {
  note("a section header renamed to carry no 'Scales' left ",
       sum(control$section), " headers and ", sum(!control$section),
       " labels without stopping")
} else {
  msg <- conditionMessage(attr(control, "condition"))
  ## Which failure: the pinned-header report naming the renamed one, not the
  ## empty-table or single-shape guard that stops in the same place.
  if (!grepl("section headers not found among the extracted labels", msg,
             fixed = TRUE) ||
        !grepl("Antisocial Scales", msg, fixed = TRUE)) {
    note("renaming a section header stopped the extraction with ",
         encodeString(msg, quote = '"'),
         ", which is not the pinned-header report naming it")
  }
}

## ...and the other direction: a scale whose printed name carries the word.
## Before M071 it read as a header and dropped out of the reconciliation.
carrying <- hitopsr_table1_rename_label(raw_txt, "Trichotillomania",
                                        "Trichotillomania Scales")
control2 <- hitopsr_table1_rows(txt = carrying)
if (sum(control2$section) != 13L ||
      !"Trichotillomania Scales" %in% control2$label[!control2$section]) {
  note("a scale printed as 'Trichotillomania Scales' was read as a section ",
       "header, so it would leave the reconciliation without a word")
}
cat("Section headers on a mutated dump: renaming 'Antisocial Scales' stops ",
    "the run; a scale named 'Trichotillomania Scales' stays a scale\n", sep = "")

cat("\n")
if (!length(bad)) {
  cat("RESULT: Table 1 reconciles with the shipped name tables.\n")
} else {
  cat("RESULT: ", length(bad), " departure(s) from the reconciled state\n\n",
      sep = "")
  for (b in bad) cat("  - ", b, "\n", sep = "")
  stop(length(bad), " departure(s) from the reconciled state", call. = FALSE)
}
