## Seeded corruptions of `pid_norms`, shared by the two mutation harnesses
#
# A check that passes proves nothing about what it would catch. This file
# defines, one per list entry, each corruption the norms layers are meant to
# detect; the harnesses apply them one at a time and report what fired:
#
#   * data-raw/mutate_norms_check.R      -- runs tests/testthat/test-norms.R
#   * data-raw/mutate_norms_book_check.R -- runs the book comparison in
#     data-raw/verify_norms_against_book.R (needs the gitignored shelf epub)
#
# Sourced, never run on its own: it defines `norms_mutations` and the two
# selectors below and touches nothing on disk. Each entry carries a stable
# `id`, so a harness can name the cases it asserts rather than index into the
# list; `ac` records which acceptance criterion seeded it.

## Row selectors used by the mutations below.
row_of <- function(x, version, scale, tscore = NULL, raw = NULL) {
  hit <- x$version == version & x$scale == scale
  if (!is.null(tscore)) hit <- hit & !is.na(x$tscore) & x$tscore == tscore
  if (!is.null(raw)) hit <- hit & x$raw == raw
  i <- which(hit)
  stopifnot(length(i) == 1L)
  i
}

## Exchange two columns' values, matched on T score, leaving the scale labels
## where they are. A swap leaves every structural invariant intact -- both
## columns stay linear in T and both percentile columns stay monotone -- so only
## an anchor that reads differently on the two columns can witness it.
swap_columns <- function(x, version, a, b) {
  ia <- which(x$version == version & x$scale == a)
  ib <- which(x$version == version & x$scale == b)
  ia <- ia[order(x$tscore[ia])]
  ib <- ib[order(x$tscore[ib])]
  stopifnot(length(ia) == length(ib), identical(x$tscore[ia], x$tscore[ib]))
  raw <- x$raw
  percentile <- x$percentile
  x$raw[ia] <- raw[ib]
  x$raw[ib] <- raw[ia]
  x$percentile[ia] <- percentile[ib]
  x$percentile[ib] <- percentile[ia]
  x
}

norms_mutations <- list(
  list(
    id = "full-neg-t60-raw",
    ac = "AC4(a)",
    desc = "FULL negativeAffectivity raw at T = 60 raised by 0.02",
    f = function(x) {
      i <- row_of(x, "FULL", "negativeAffectivity", tscore = 60)
      x$raw[[i]] <- x$raw[[i]] + 0.02
      x
    }
  ),
  list(
    id = "sf-psy-floor",
    ac = "AC4(a)",
    desc = "SF psychoticism raw at T = 30 (on the zero floor) raised to 0.02",
    f = function(x) {
      i <- row_of(x, "SF", "psychoticism", tscore = 30)
      stopifnot(x$raw[[i]] == 0)
      x$raw[[i]] <- 0.02
      x
    }
  ),
  list(
    id = "bf-det-monotone",
    ac = "AC4(b)",
    desc = "BF detachment percentile at T = 70 dropped below its predecessor",
    f = function(x) {
      i <- row_of(x, "BF", "detachment", tscore = 70)
      prev <- row_of(x, "BF", "detachment", tscore = 69)
      x$percentile[[i]] <- x$percentile[[prev]] - 0.01
      x
    }
  ),
  list(
    id = "full-inc-out-of-range",
    ac = "AC4(c)",
    desc = "FULL INC percentile at a score of 5 set to 1.4",
    f = function(x) {
      i <- row_of(x, "FULL", "INC", raw = 5)
      x$percentile[[i]] <- 1.4
      x
    }
  ),
  list(
    id = "full-prd-nudge",
    ac = "AC4(d)",
    desc = "FULL PRD percentile at a score of 21 nudged 0.894 -> 0.895",
    f = function(x) {
      i <- row_of(x, "FULL", "PRD", raw = 21)
      stopifnot(x$percentile[[i]] == 0.894)
      x$percentile[[i]] <- 0.895
      x
    }
  ),
  list(
    id = "sf-ant-precorrection",
    ac = "AC4(d)",
    desc = "SF antagonism percentile at T = 48 reverted to the pre-correction 0.58",
    f = function(x) {
      i <- row_of(x, "SF", "antagonism", tscore = 48)
      stopifnot(x$percentile[[i]] == 0.52)
      x$percentile[[i]] <- 0.58
      x
    }
  ),
  ## M033's facet mutations. The first is the defect this dataset actually had
  ## once (a whole column off by one row), which no structural invariant can
  ## see: it leaves the raws on a line and the percentiles monotone.
  list(
    id = "full-hostility-raw-shift",
    ac = "M033 AC6",
    desc = "FULL hostility raw column displaced down one T row",
    f = function(x) {
      i <- which(x$version == "FULL" & x$scale == "hostility")
      i <- i[order(x$tscore[i])]
      x$raw[i] <- c(x$raw[[i[[1]]]], utils::head(x$raw[i], -1))
      x
    }
  ),
  list(
    id = "sf-perseveration-off-line",
    ac = "M033 AC5",
    desc = "SF perseveration raw at T = 55 pushed 0.02 off its column's line",
    f = function(x) {
      i <- row_of(x, "SF", "perseveration", tscore = 55)
      x$raw[[i]] <- x$raw[[i]] + 0.02
      x
    }
  ),
  ## A percentile column displaced on its own is the thinnest case in the
  ## dataset: it stays monotone, and `raw` -- which every other test reads -- is
  ## untouched, so only an anchor whose percentile steps at that T can see it.
  ## Both were NOT CAUGHT while each column had a single anchor at T = 65. M034
  ## placed every column's second anchor at exactly such a step, so these now
  ## fire -- and fire on the book-comparison test, not merely on an adequacy one.
  list(
    id = "sf-withdrawal-ptl-shift",
    ac = "M033 AC6",
    desc = "SF withdrawal percentile column displaced down one T row (raw untouched)",
    f = function(x) {
      i <- which(x$version == "SF" & x$scale == "withdrawal")
      i <- i[order(x$tscore[i])]
      x$percentile[i] <- c(x$percentile[[i[[1]]]], utils::head(x$percentile[i], -1))
      x
    }
  ),
  list(
    id = "full-anhedonia-ptl-shift",
    ac = "M033 AC6",
    desc = "FULL anhedonia percentile column displaced down one T row (raw untouched)",
    f = function(x) {
      i <- which(x$version == "FULL" & x$scale == "anhedonia")
      i <- i[order(x$tscore[i])]
      x$percentile[i] <- c(x$percentile[[i[[1]]]], utils::head(x$percentile[i], -1))
      x
    }
  ),
  list(
    id = "sf-anxiousness-ceiling-cut",
    ac = "M033 AC4",
    desc = "SF anxiousness ceiling run truncated -- its 12 rows at 4.00 cut to 1",
    f = function(x) {
      i <- which(x$version == "SF" & x$scale == "anxiousness" & x$raw == 4)
      stopifnot(length(i) == 12L)
      x[-utils::tail(i, -1), ]
    }
  ),
  ## M034's swap mutations. Both pairs read alike at T = 65 -- the one anchor
  ## every facet column carried before M034 -- so neither could witness a swap of
  ## the other. The second anchor is what separates them.
  list(
    id = "sf-impulsivity-intimacy-swap",
    ac = "M034 AC5",
    desc = "SF impulsivity and SF intimacyAvoidance columns swapped",
    f = function(x) swap_columns(x, "SF", "impulsivity", "intimacyAvoidance")
  ),
  list(
    id = "sf-manipulativeness-suspiciousness-swap",
    ac = "M034 AC5",
    desc = "SF manipulativeness and SF suspiciousness columns swapped",
    f = function(x) swap_columns(x, "SF", "manipulativeness", "suspiciousness")
  )
)

stopifnot(!anyDuplicated(vapply(norms_mutations, `[[`, character(1), "id")))

mutation_by_id <- function(id) {
  i <- match(id, vapply(norms_mutations, `[[`, character(1), "id"))
  stopifnot(!is.na(i))
  norms_mutations[[i]]
}

## The pristine dataset is copied aside, the body run against mutated copies,
## and the original put back on exit -- including on error -- with the restore
## checked by hash. Both harnesses share this, so neither can leave a corrupted
## data/pid_norms.rda behind.
with_pristine_norms <- function(rda, body) {
  stopifnot(file.exists(rda))
  pristine <- tempfile(fileext = ".rda")
  invisible(file.copy(rda, pristine, overwrite = TRUE))
  before <- tools::md5sum(rda)[[1]]
  on.exit({
    invisible(file.copy(pristine, rda, overwrite = TRUE))
    after <- tools::md5sum(rda)[[1]]
    cat("\nrestored ", rda, ": md5 ", after,
        if (identical(before, after)) " (unchanged)" else " (MISMATCH)", "\n",
        sep = "")
  }, add = TRUE)
  body(pristine)
}
