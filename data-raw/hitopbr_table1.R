# The Superspectra and Spectra block of Table 1 of the HiTOP-SR introduction
# paper, transcribed by eye.
#
# This file is the *transcription* side of the HiTOP-BR development statistics,
# the counterpart of data-raw/hitopsr_devstats.csv for the HiTOP-SR block. The
# eight rows below were read from shelf pages 49-51 rendered at 200 dpi, the
# same route M041 used for the 93 primary-scale rows, and the two disputed cells
# were re-read from a 400 dpi crop of the block.
#
# It deliberately does **not** source data-raw/hitopsr_table1.R, which extracts
# the same rows from the words' page coordinates. Keeping the two routes apart is
# what makes data-raw/verify_hitopbr_devstats.R's diff a check rather than a
# restatement (IP2): that verifier reads the extraction from that file and the
# transcription from this one, and neither file has seen the other's numbers.
#
# Maintainer-run, never CI. It needs nothing but itself; the verifier that reads
# it needs the gitignored source shelf and pdftotext.

## The document the eight rows below were read from, pinned by content rather
## than by filename -- the shelf is gitignored, so a future run could otherwise
## be reading a different manuscript at this path. The extractor pins the same
## document under its own name; verify_hitopbr_devstats.R asserts the two pins
## are equal, so a transcription of one document can never be diffed against an
## extraction of another.
hitopbr_source_pdf <- "cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf"
hitopbr_source_sha256 <-
  "1c211219b7fe13f8ed172f9210c152a642a9be77d790e08d795843c25da8e425"

## Table 1's last section, "Superspectra and Spectra Scales" (shelf p. 51), in
## printed order. `label` is the label as printed, including the lowercase f of
## `p-factor`, which `hitopbr_scales$Scale` renders `p-Factor`; the name map that
## resolves that pair lives in data-raw/hitopbr_devstats.R, so this file states
## only what the page shows.
##
## The columns transcribed are the four the package ships and the two the Range
## column prints. Table 1's `Skewness` and `Kurtosis` cells are read on the page
## but are neither shipped nor transcribed: nothing in the package traces to
## them, so a transcribed copy here would be a value no check ever reads.
hitopbr_table1 <- data.frame(
  label = c(
    "Externalizing", "p-factor", "Internalizing", "Somatoform",
    "Detachment", "Thought Disorder", "Disinhibition", "Antagonism"
  ),
  nItems = c(10L, 12L, 8L, 8L, 5L, 6L, 9L, 9L),
  alpha = c(0.83, 0.86, 0.90, 0.88, 0.86, 0.85, 0.86, 0.82),
  mean = c(1.54, 1.68, 1.85, 1.82, 2.13, 1.26, 1.65, 1.42),
  sd = c(0.49, 0.55, 0.77, 0.71, 0.88, 0.46, 0.60, 0.45),
  rangeLo = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
  rangeHi = c(3.7, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 3.4),
  stringsAsFactors = FALSE
)
