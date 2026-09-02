# Verify hitopbr_devstats against Table 1 of the HiTOP-SR introduction paper
# (M068, AC2)
#
# The committed transcription in data-raw/hitopbr_table1.R was read by eye from
# the table pages rendered at 200 dpi. This script is the independent
# extraction: it reads every word's page coordinates and rebuilds the table by
# banding rows on the vertical axis and columns on the horizontal one. Neither
# route sees the other -- the transcription file sources no extractor and the
# extractor knows no committed value -- so the diff below is a check and not a
# restatement (IP2). The extraction itself lives in data-raw/hitopsr_table1.R,
# shared with the HiTOP-SR verifier and the two name verifiers, so no verifier
# owns the source side; its `hitopsr_table1_cells()` already returns the eight
# HiTOP-BR rows as `block == "superspectra"`.
#
# Five comparisons, in this order:
#
#   1. The two files' shelf pins -- so a transcription of one document can never
#      be diffed against an extraction of another.
#   2. Table 1's 8 Superspectra and Spectra rows against
#      data-raw/hitopbr_table1.R -- the transcription check.
#   3. The same rows against the shipped `hitopbr_devstats` -- so
#      data-raw/hitopbr_devstats.R's name map, join and column derivation are
#      covered too, not only the typing.
#   4. Table 1's printed `# Items` against the item counts `hitopbr_scales`
#      derives from `hitopbr_items` -- the keying side of the same cell.
#   5. Table 1's Range column against the response coding `interval_hitopbr()`
#      converts on -- the one number that function hardcodes and that nothing
#      else traces back to the source.
#
# What this script cannot do: it needs the gitignored source shelf and
# pdftotext, so it never runs in CI. The CI-runnable half of AC2 is in
# tests/testthat/test-interval_hitopbr.R, which asserts every shipped `nItems`
# against `lengths(hitopbr_scales$itemNumbers)` -- an oracle no transcription
# error could survive without also having moved an item count.
#
# Proven able to fail, 2026-08-30, one planted defect at a time against that
# day's committed data, with a clean control before and after. The eight plants
# vary in form as well as in location, and reach all five comparisons: a changed
# transcribed cell (Detachment mean 2.13 -> 2.14), a removed transcribed row
# (Somatoform), a relabelled transcribed row (`p-factor` -> `p factor`, which
# breaks the join rather than a cell), a nudged shipped reliability (Antagonism
# 0.82 -> 0.83), a relabelled shipped `reliabilityType` (Internalizing alpha ->
# omega), a shipped `nItems` moved off its printed cell (Detachment 5 -> 6), the
# keying side moved instead (item 36 put back into `detachment`, which is what
# comparison 4 alone reports), and a substituted shelf pin. Each was named in
# the report and exited 1 -- the pin plant stopping before any comparison ran,
# which is what it is there to do -- and neither control reported anything.
#
# Source (gitignored shelf, not distributed):
#   cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf
#
# Run from the package root:  Rscript data-raw/verify_hitopbr_devstats.R

source("data-raw/hitopsr_table1.R")
source("data-raw/hitopbr_table1.R")

discrepancies <- character(0)

## ---- 1. the two files read the same document -------------------------------

cat("Source: ", hitopbr_source_pdf, "\n", sep = "")
if (!identical(hitopbr_source_pdf, hitopsr_source_pdf)) {
  discrepancies <- c(discrepancies, sprintf(
    "pin: the transcription reads %s, the extraction reads %s",
    hitopbr_source_pdf, hitopsr_source_pdf))
}
if (!identical(hitopbr_source_sha256, hitopsr_source_sha256)) {
  discrepancies <- c(discrepancies, sprintf(
    "pin: the transcription pins sha256 %s, the extraction pins %s",
    hitopbr_source_sha256, hitopsr_source_sha256))
}
## A pin mismatch makes every comparison below meaningless, so stop rather than
## collect: a printed discrepancy list must never be a diff of two documents.
if (length(discrepancies)) {
  for (d in discrepancies) cat("  - ", d, "\n", sep = "")
  stop("the transcription and the extraction do not pin the same document",
       call. = FALSE)
}
sha <- hitopsr_source_check()
cat("sha256: ", sha, " (matches, and both files pin it)\n\n", sep = "")

## ---- the source side -------------------------------------------------------

cells <- hitopsr_table1_cells()
table1 <- cells[cells$block == "superspectra", ]

cat("Table 1 pages: ", paste(unique(cells$page), collapse = ", "), "\n", sep = "")
cat("Data rows extracted: ", nrow(cells),
    " (", sum(cells$block == "primary"), " primary scales and subscales, ",
    nrow(table1), " Superspectra and Spectra)\n\n", sep = "")

## The domain this whole script quantifies over. `hitopsr_table1_cells()` stops
## on an empty or short table, but its own row-count guards are stated for the
## table as a whole; the block this script reads is the one it must not lose.
if (nrow(table1) != 8L) {
  stop("Table 1's Superspectra and Spectra block extracted ", nrow(table1),
       " rows, expected 8", call. = FALSE)
}

## ---- the package side ------------------------------------------------------

load("data/hitopbr_devstats.rda")
load("data/hitopbr_scales.rda")

## The one Table 1 label that does not match a package name character for
## character, resolved the way data-raw/hitopbr_devstats.R resolves it. Applied
## to the extracted labels only, so a change of rendering shows up as an
## unmatched row rather than being absorbed.
to_package_name <- function(x) sub("^p-factor$", "p-Factor", x)

## ---- 2. Table 1 against the transcription ----------------------------------

shipped_cols <- c("nItems", "alpha", "mean", "sd", "rangeLo", "rangeHi")

only_source <- setdiff(table1$label, hitopbr_table1$label)
only_file <- setdiff(hitopbr_table1$label, table1$label)
if (length(only_source)) {
  discrepancies <- c(discrepancies, sprintf(
    "transcription: Table 1 prints a row the transcription does not carry -- %s",
    only_source))
}
if (length(only_file)) {
  discrepancies <- c(discrepancies, sprintf(
    "transcription: the transcription carries a row Table 1 does not print -- %s",
    only_file))
}

i <- match(hitopbr_table1$label, table1$label)
paired <- !is.na(i)
n_cells <- 0L
for (col in shipped_cols) {
  a <- hitopbr_table1[[col]][paired]
  b <- table1[[col]][i[paired]]
  n_cells <- n_cells + length(a)
  bad <- which(abs(a - b) > 1e-9)
  if (length(bad)) {
    discrepancies <- c(discrepancies, sprintf(
      "transcription: %s %s -- Table 1 prints %s, the transcription carries %s",
      hitopbr_table1$label[paired][bad], col,
      format(b[bad]), format(a[bad])))
  }
}
cat("2. transcription vs Table 1: ", n_cells, " cells compared over ",
    sum(paired), " rows\n", sep = "")

## ---- 3. Table 1 against the shipped object ---------------------------------

source_name <- to_package_name(table1$label)
only_source2 <- setdiff(source_name, hitopbr_devstats$Scale)
only_ship <- setdiff(hitopbr_devstats$Scale, source_name)
if (length(only_source2)) {
  discrepancies <- c(discrepancies, sprintf(
    "rda: Table 1 prints a scale hitopbr_devstats does not carry -- %s",
    only_source2))
}
if (length(only_ship)) {
  discrepancies <- c(discrepancies, sprintf(
    "rda: hitopbr_devstats carries a scale Table 1 does not print -- %s",
    only_ship))
}

j <- match(hitopbr_devstats$Scale, source_name)
paired2 <- !is.na(j)
ship_cols <- c(nItems = "nItems", reliability = "alpha",
               mean = "mean", sd = "sd")
n_rda_cells <- 0L
for (k in seq_along(ship_cols)) {
  a <- hitopbr_devstats[[names(ship_cols)[k]]][paired2]
  b <- table1[[ship_cols[[k]]]][j[paired2]]
  n_rda_cells <- n_rda_cells + length(a)
  bad <- which(abs(a - b) > 1e-9)
  if (length(bad)) {
    discrepancies <- c(discrepancies, sprintf(
      "rda: %s %s -- Table 1 prints %s, hitopbr_devstats carries %s",
      hitopbr_devstats$Scale[paired2][bad], names(ship_cols)[k],
      format(b[bad]), format(a[bad])))
  }
}
cat("3. hitopbr_devstats vs Table 1: ", n_rda_cells, " cells compared over ",
    sum(paired2), " rows\n", sep = "")

## Every reliability the package ships must be labelled for what Table 1's
## column is. A row labelled anything else did not come from this table.
wrong_type <- hitopbr_devstats$reliabilityType != "alpha"
if (any(wrong_type)) {
  discrepancies <- c(discrepancies, sprintf(
    "rda: %s carries reliabilityType %s, but Table 1's column is alpha",
    hitopbr_devstats$Scale[wrong_type],
    hitopbr_devstats$reliabilityType[wrong_type]))
}

## ---- 4. Table 1's `# Items` against the keying tables ----------------------

## The set AC5 quantifies over: the rows where the printed count and the count
## `hitopbr_items` yields disagree. It is reported here rather than pre-counted
## anywhere, so a row that starts disagreeing is named by this run.
keyed <- lengths(hitopbr_scales$itemNumbers)[
  match(hitopbr_devstats$camelCase, hitopbr_scales$camelCase)
]
if (anyNA(keyed)) {
  discrepancies <- c(discrepancies, sprintf(
    "items: hitopbr_devstats carries a stem hitopbr_scales does not -- %s",
    hitopbr_devstats$camelCase[is.na(keyed)]))
}
printed <- table1$nItems[j[paired2]]
disputed <- which(!is.na(keyed[paired2]) & printed != keyed[paired2])
if (length(disputed)) {
  discrepancies <- c(discrepancies, sprintf(
    "items: %s -- Table 1 prints %s items, hitopbr_items yields %s",
    hitopbr_devstats$Scale[paired2][disputed],
    format(printed[disputed]), format(keyed[paired2][disputed])))
}
cat("4. `# Items` vs the keying tables: ", sum(paired2), " rows compared, ",
    length(disputed), " disagreeing\n", sep = "")

## ---- 5. Table 1's Range against the coding the package converts on ---------

## `interval_hitopbr()` refuses to convert a score computed on any coding other
## than c(1, 4), and that constant is written in the wrapper, not read from the
## data. Table 1 is where it comes from: every Range cell in this block opens at
## 1.0 and the widest close at 4.0, so the printed range is the source's own
## statement of the coding the mean and SD belong to.
ref_srange <- c(1, 4)
observed <- c(min(table1$rangeLo), max(table1$rangeHi))
if (!isTRUE(all.equal(observed, ref_srange))) {
  discrepancies <- c(discrepancies, sprintf(
    "coding: Table 1's Range column spans [%s, %s], but interval_hitopbr() converts on c(%s, %s)",
    format(observed[1]), format(observed[2]),
    format(ref_srange[1]), format(ref_srange[2])))
}
cat("5. Range vs the conversion coding: Table 1's block spans [",
    format(observed[1]), ", ", format(observed[2]), "]\n\n", sep = "")

## ---- result ----------------------------------------------------------------

if (!length(discrepancies)) {
  cat("RESULT: every shipped cell matches Table 1, in the transcription and in ",
      "the built hitopbr_devstats.\n", sep = "")
} else {
  cat("RESULT: ", length(discrepancies), " discrepancy/discrepancies\n\n",
      sep = "")
  for (d in discrepancies) cat("  - ", d, "\n", sep = "")
  ## A non-zero exit, so a caller running this as a gate cannot mistake a
  ## printed discrepancy list for a pass.
  stop(length(discrepancies), " discrepancy/discrepancies against Table 1",
       call. = FALSE)
}
