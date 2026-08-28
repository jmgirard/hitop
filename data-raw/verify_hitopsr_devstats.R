# Verify hitopsr_devstats against Table 1 of the HiTOP-SR introduction paper
# (M041, AC2)
#
# The committed CSV is a transcription: the three table pages were rendered at
# 200 dpi and read. This script is the independent extraction: it reads every
# word's page coordinates and rebuilds the table by banding rows on the vertical
# axis and columns on the horizontal one. Neither route sees the other, so the
# diff below is a check and not a restatement (IP2). The extraction itself lives
# in data-raw/hitopsr_table1.R, shared with the two name verifiers, so no
# verifier owns the source side.
#
# Three comparisons, in this order:
#
#   1. Table 1's 93 primary-scale and subscale rows against
#      data-raw/hitopsr_devstats.csv -- the transcription check.
#   2. The same rows against the shipped `hitopsr_devstats` -- so
#      data-raw/hitopsr_devstats.R's name map, join and column derivation are
#      covered too, not only the typing.
#   3. Table 1's own label indentation against the shipped `type` column -- the
#      one thing in the shipped table that Table 1 states and the CSV does not
#      carry.
#
# What this script cannot do: it needs the gitignored source shelf and
# pdftotext, so it never runs in CI. The CI-runnable half of AC2 is in
# tests/testthat/test-interval_hitopsr.R, which asserts every shipped `nItems`
# against `hitopsr_scales`/`hitopsr_subscales` -- an oracle no transcription
# error could survive without also having moved a row.
#
# Proven able to fail, 2026-08-28, one planted defect at a time against that
# day's committed data, with a clean control before and after: a changed CSV cell
# (Bodily Distress alpha 0.85 -> 0.86), a removed CSV row (Purging), a nudged
# shipped reliability (Appearance Focus 0.81 -> 0.82), a flipped shipped `type`
# (Agoraphobia), a relabelled `reliabilityType` (Binge Eating alpha -> omega),
# and a dropped shipped row (Antisocial Behavior). Each was reported by name and
# exited 1; neither control reported anything.
#
# Source (gitignored shelf, not distributed):
#   cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf
#
# Run from the package root:  Rscript data-raw/verify_hitopsr_devstats.R

source("data-raw/hitopsr_table1.R")

cat("Source: ", hitopsr_source_pdf, "\n", sep = "")
sha <- hitopsr_source_check()
cat("sha256: ", sha, " (matches)\n\n", sep = "")

## ---- the source side -------------------------------------------------------

cells <- hitopsr_table1_cells()
table1 <- cells[cells$block == "primary", ]
table1$isSubscale <- hitopsr_table1_is_subscale(cells)[cells$block == "primary"]

cat("Table 1 pages: ", paste(unique(cells$page), collapse = ", "), "\n", sep = "")
cat("Data rows extracted: ", nrow(cells),
    " (", nrow(table1), " primary scales and subscales, ",
    sum(cells$block == "superspectra"), " Superspectra and Spectra)\n", sep = "")
cat("Label positions found: ",
    paste(sort(unique(round(table1$indent, 1))), collapse = ", "),
    " (the outer marks a scale, the inner a subscale)\n\n", sep = "")

## ---- the package side ------------------------------------------------------

csv <- read.csv(
  "data-raw/hitopsr_devstats.csv",
  colClasses = c("character", "integer", "numeric", "numeric", "numeric"),
  encoding = "UTF-8"
)
load("data/hitopsr_devstats.rda")

## The one Table 1 label that does not match a package name character for
## character, resolved the way data-raw/hitopsr_devstats.R resolves it. Applied
## to the extracted labels only, so a change of marker shows up as an unmatched
## row rather than being absorbed.
strip_marker <- function(x) sub("†$", "", x)

## ---- 1. Table 1 against the CSV --------------------------------------------

shipped_cols <- c("nItems", "alpha", "mean", "sd")
discrepancies <- character(0)

only_source <- setdiff(table1$label, csv$label)
only_csv <- setdiff(csv$label, table1$label)
if (length(only_source)) {
  discrepancies <- c(discrepancies, sprintf(
    "csv: Table 1 prints a row the CSV does not carry -- %s", only_source))
}
if (length(only_csv)) {
  discrepancies <- c(discrepancies, sprintf(
    "csv: the CSV carries a row Table 1 does not print -- %s", only_csv))
}

i <- match(csv$label, table1$label)
paired <- !is.na(i)
n_csv_cells <- 0L
for (col in shipped_cols) {
  a <- csv[[col]][paired]
  b <- table1[[col]][i[paired]]
  n_csv_cells <- n_csv_cells + length(a)
  bad <- which(abs(a - b) > 1e-9)
  if (length(bad)) {
    discrepancies <- c(discrepancies, sprintf(
      "csv: %s %s -- Table 1 prints %s, the CSV carries %s",
      csv$label[paired][bad], col, format(b[bad]), format(a[bad])))
  }
}
cat("1. CSV vs Table 1: ", n_csv_cells, " cells compared over ",
    sum(paired), " rows\n", sep = "")

## ---- 2. Table 1 against the shipped object ---------------------------------

source_name <- strip_marker(table1$label)
only_source2 <- setdiff(source_name, hitopsr_devstats$scale)
only_ship <- setdiff(hitopsr_devstats$scale, source_name)
if (length(only_source2)) {
  discrepancies <- c(discrepancies, sprintf(
    "rda: Table 1 prints a scale hitopsr_devstats does not carry -- %s",
    only_source2))
}
if (length(only_ship)) {
  discrepancies <- c(discrepancies, sprintf(
    "rda: hitopsr_devstats carries a scale Table 1 does not print -- %s",
    only_ship))
}

j <- match(hitopsr_devstats$scale, source_name)
paired2 <- !is.na(j)
ship_cols <- c(nItems = "nItems", reliability = "alpha",
               mean = "mean", sd = "sd")
n_rda_cells <- 0L
for (k in seq_along(ship_cols)) {
  a <- hitopsr_devstats[[names(ship_cols)[k]]][paired2]
  b <- table1[[ship_cols[[k]]]][j[paired2]]
  n_rda_cells <- n_rda_cells + length(a)
  bad <- which(abs(a - b) > 1e-9)
  if (length(bad)) {
    discrepancies <- c(discrepancies, sprintf(
      "rda: %s %s -- Table 1 prints %s, hitopsr_devstats carries %s",
      hitopsr_devstats$scale[paired2][bad], names(ship_cols)[k],
      format(b[bad]), format(a[bad])))
  }
}
cat("2. hitopsr_devstats vs Table 1: ", n_rda_cells, " cells compared over ",
    sum(paired2), " rows\n", sep = "")

## Every reliability the package ships must be labelled for what Table 1's
## column is. A row labelled anything else did not come from this table.
wrong_type <- hitopsr_devstats$scale[hitopsr_devstats$reliabilityType != "alpha"]
if (length(wrong_type)) {
  discrepancies <- c(discrepancies, sprintf(
    "rda: %s carries reliabilityType %s, but Table 1's column is alpha",
    wrong_type,
    hitopsr_devstats$reliabilityType[hitopsr_devstats$reliabilityType != "alpha"]
  ))
}

## ---- 3. Table 1's indentation against the shipped `type` -------------------

want <- ifelse(table1$isSubscale[j[paired2]], "subscale", "scale")
got <- hitopsr_devstats$type[paired2]
bad <- which(want != got)
if (length(bad)) {
  discrepancies <- c(discrepancies, sprintf(
    "type: %s -- Table 1 indents it as a %s, hitopsr_devstats calls it a %s",
    hitopsr_devstats$scale[paired2][bad], want[bad], got[bad]))
}
cat("3. type vs Table 1's indentation: ", sum(paired2), " rows compared (",
    sum(want == "subscale"), " indented)\n\n", sep = "")

## ---- result ----------------------------------------------------------------

if (!length(discrepancies)) {
  cat("RESULT: every shipped cell matches Table 1, in the CSV and in the ",
      "built hitopsr_devstats.\n", sep = "")
} else {
  cat("RESULT: ", length(discrepancies), " discrepancy/discrepancies\n\n",
      sep = "")
  for (d in discrepancies) cat("  - ", d, "\n", sep = "")
  ## A non-zero exit, so a caller running this as a gate cannot mistake a
  ## printed discrepancy list for a pass.
  stop(length(discrepancies), " discrepancy/discrepancies against Table 1",
       call. = FALSE)
}
