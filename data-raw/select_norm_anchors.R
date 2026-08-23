## Choose the second spot-value anchor for each T-scored column (M034, T1)
#
# tests/testthat/test-norms.R anchors every (version, scale) column of
# `pid_norms` to values hand-read off the book's rendered pages. One anchor per
# column leaves two gaps that data-raw/mutate_norms_check.R measures as NOT
# CAUGHT: a percentile column displaced down one row with `raw` untouched, and a
# swap of two columns whose single anchors happen to read alike. This script
# picks the second anchor's T score for each column that has only one.
#
# Two properties are required of the choice, and this script is where they are
# established; test-norms.R then asserts them independently of it.
#
#   (a) Step placement. The chosen T must be one where that column's percentile
#       differs from the percentile one T lower. Under a downward displacement
#       every row takes its predecessor's value, so an anchor on a percentile
#       plateau reads the same before and after and witnesses nothing. Only the
#       downward direction is closed here: catching an upward displacement needs
#       a T differing from the row *above*, and 14 of the 66 columns have no
#       interior T differing from both neighbours -- their percentiles step only
#       every third row. That remainder is a ROADMAP candidate.
#
#   (b) Pairwise distinctness. No two columns may agree on both `raw` and
#       `percentile` at every T where either is anchored, or neither can witness
#       a swap of the two.
#
# What this script chooses is *where to read*, never *what the value is*: the
# values themselves are hand-read off the rendered page and are the only ground
# truth in that test file (IP2). Selecting an informative measurement point from
# the shipped data is not certifying the shipped data -- if `pid_norms` were
# already displaced, these T values would be the corrupted column's step
# locations, which is a real bound on what the anchors buy and why
# data-raw/verify_norms_against_book.R exists alongside them.
#
# Candidate T scores are tried in the fixed `preference` order below rather than
# taking each column's lowest eligible T. The anchors are read by eye off the
# rendered appendix, and a shared T lets a whole band of columns be read as one
# row scan across the page -- the same way M033 read all 50 of its anchors at
# T = 65 -- instead of hunting a different row in each of 63 columns. Five T
# scores in two bands cover every column.
#
# Usage:  Rscript data-raw/select_norm_anchors.R

load("data/pid_norms.rda")

norms <- as.data.frame(pid_norms)
tscored <- norms[!is.na(norms$tscore), ]
tscored$key <- paste(tscored$version, tscored$scale)
keys <- unique(tscored$key)

## The anchors test-norms.R already carries, by column. Three columns entered
## M034 with more than one and need nothing; the other 63 get one more.
existing <- list(
  "FULL negativeAffectivity" = c(35, 50),
  "FULL detachment"          = c(65, 72, 76),
  "FULL antagonism"          = 72,
  "FULL disinhibition"       = 90,
  "FULL psychoticism"        = 76,
  "SF negativeAffectivity"   = 50,
  "SF detachment"            = 60,
  "SF antagonism"            = 48,
  "SF disinhibition"         = 90,
  "SF psychoticism"          = 48,
  "BF total"                 = c(50, 95),
  "BF negativeAffectivity"   = 70,
  "BF detachment"            = 70,
  "BF antagonism"            = 50,
  "BF disinhibition"         = 70,
  "BF psychoticism"          = 70
)
## Every facet column is anchored at T = 65 (M033); the domains are listed above.
anchors_of <- function(key) if (is.null(existing[[key]])) 65 else existing[[key]]

needs_anchor <- keys[vapply(keys, function(k) length(anchors_of(k)) == 1L, logical(1))]

## (a) The T scores at which a column's percentile steps up from the row below.
eligible_of <- function(key) {
  s <- tscored[tscored$key == key, ]
  s <- s[order(s$tscore), ]
  steps <- s$tscore[-1][diff(s$percentile) != 0]
  setdiff(steps, anchors_of(key))
}
eligible <- lapply(needs_anchor, eligible_of)
names(eligible) <- needs_anchor

stopifnot(lengths(eligible) > 0L)

## Preference order: two bands, so the read is five row scans rather than 63
## lookups. Derived by greedy set cover over `eligible` and then frozen here, so
## this script reports a fixed choice rather than re-deriving one that could
## drift if the data changed.
preference <- c(44, 64, 63, 45, 46)

chosen <- vapply(needs_anchor, function(key) {
  hit <- preference[preference %in% eligible[[key]]]
  if (length(hit) == 0L) {
    stop("no preferred T is eligible for ", key, call. = FALSE)
  }
  hit[[1]]
}, numeric(1))

## (b) No two columns may agree on both values everywhere both are anchored.
## A T where one column has no row cannot be an agreement.
anchored <- lapply(keys, function(k) {
  sort(unique(c(anchors_of(k), if (k %in% needs_anchor) chosen[[k]])))
})
names(anchored) <- keys

cell <- function(key, t) {
  r <- tscored[tscored$key == key & tscored$tscore == t, ]
  if (nrow(r) == 0L) NULL else c(r$raw[[1]], r$percentile[[1]])
}

collisions <- character()
for (a in seq_along(keys)) {
  for (b in seq_len(a - 1L)) {
    ka <- keys[[a]]
    kb <- keys[[b]]
    alike <- TRUE
    for (t in union(anchored[[ka]], anchored[[kb]])) {
      va <- cell(ka, t)
      vb <- cell(kb, t)
      if (is.null(va) || is.null(vb) || !identical(va, vb)) {
        alike <- FALSE
        break
      }
    }
    if (alike) collisions <- c(collisions, paste(ka, "and", kb))
  }
}

if (length(collisions)) {
  stop(
    "columns indistinguishable under the chosen anchors: ",
    paste(collisions, collapse = "; "),
    call. = FALSE
  )
}

## ---- report --------------------------------------------------------------
## The read list, grouped by T so each group is one scan across the page.

cat("second anchors for", length(needs_anchor), "columns\n")
cat("columns already carrying two or more:",
    paste(setdiff(keys, needs_anchor), collapse = "; "), "\n\n")

for (t in sort(unique(chosen))) {
  cols <- sort(names(chosen)[chosen == t])
  cat("T = ", t, "  (", length(cols), " columns)\n", sep = "")
  for (k in cols) cat("  ", k, "\n", sep = "")
  cat("\n")
}

cat("pairwise distinctness: 0 indistinguishable pairs over ",
    length(keys) * (length(keys) - 1L) / 2L, " pairs\n", sep = "")
