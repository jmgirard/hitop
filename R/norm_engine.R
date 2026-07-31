#' Internal normative-lookup engine shared by the norm_*() family
#'
#' Every conversion this package performs is a *single-row selection* over the
#' printed rows of a shipped normative table: pick the row whose printed raw is
#' nearest the observed value, break any remaining tie toward the middle of the
#' distribution, and return that one row's printed T score and printed
#' percentile. Nothing is interpolated and no constant fitted from a norms table
#' is used, so every returned number is a cell of the published source (D-022).
#'
#' The rule and its consequences were settled by an independent review of the
#' PID-5 tables (RR02, archived at `cairn/reviews/archive/`); see
#' [norm_pid5()]'s `@details` for the user-facing statement of it.
#'
#' @param x Numeric vector of observed scale scores.
#' @param rows A slice of a norms table (`pid_norms`) for one version and scale,
#'   carrying `tscore`, `raw`, and `percentile` columns.
#' @param version,scale Single strings selecting the slice.
#' @noRd
NULL

## Absolute tolerance for every lookup comparison -- exact match, nearest
## distance, and equidistance alike. Attainable SF domain scores are twelfths,
## which have no terminating binary representation, while printed raws are 2-dp
## decimals; the smallest nonzero gap between two competing distances is 1/300
## (~0.0033), five orders of magnitude above this tolerance, so it can neither
## miss a true equidistance nor manufacture one.
norm_tol <- 1e-8

## The printed rows for one version/scale, in printed order.
norm_rows <- function(version, scale) {
  pid_norms[pid_norms$version == version & pid_norms$scale == scale, , drop = FALSE]
}

## Which version/scale pairs the shipped table covers.
norm_covers <- function(version, scale) {
  any(pid_norms$version == version & pid_norms$scale == scale)
}

## Vectorized single-row selection. Returns one row index into `rows` per
## element of `x` (NA where `x` is NA).
##
## Ties are broken toward the middle of the distribution: by T nearest 50 where
## the table carries T scores, and -- since the four validity tables carry none
## -- by percentile nearest 0.50 where it does not. Every tie in the shipped
## PID-5 tables is a run of raw 0.00 at the floor, where the linear T the book
## tabulated predicts a negative raw and prints 0.00 instead; the toward-middle
## pick returns the one row of the run that renders an attainable score.
norm_select <- function(x, rows) {
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) {
    return(out)
  }

  ## Clamp to the printed raw range before measuring distances. Out-of-range
  ## values are capped to the nearest end anyway, and clamping first keeps the
  ## arithmetic well behaved: an infinite observation would otherwise sit at an
  ## infinite distance from *every* row, making all of them equally near, and a
  ## value past ~1e15 would collapse every distance to one float.
  xx <- pmin(pmax(x[ok], min(rows$raw)), max(rows$raw))

  ## Distance from each observed value to every printed raw (n x k).
  d <- abs(outer(xx, rows$raw, "-"))
  nearest <- apply(d, 1, min)
  candidate <- d <= nearest + norm_tol

  ## Tie-break key, constant across rows: distance from the middle of the
  ## distribution. max.col() on the negated key returns the first minimum, so a
  ## key tie (possible only on the percentile branch) takes the lower raw.
  key <- if (all(is.na(rows$tscore))) {
    abs(rows$percentile - 0.5)
  } else {
    abs(rows$tscore - 50)
  }
  key_mat <- matrix(key, nrow = nrow(d), ncol = length(key), byrow = TRUE)
  key_mat[!candidate] <- Inf

  out[ok] <- max.col(-key_mat, ties.method = "first")
  out
}

## Observed scores -> the selected rows' printed T scores and percentiles.
## Scales whose rows carry no T (the validity tables) yield an all-NA `t`.
norm_convert <- function(x, version, scale) {
  rows <- norm_rows(version, scale)
  idx <- norm_select(x, rows)
  list(t = rows$tscore[idx], ptl = rows$percentile[idx])
}

## A printed T score -> that row's printed raw. Exact lookup, never a
## computation: the tables print raws the linear form behind them would round
## differently in six cells, and the printed cell is the authority (D-022).
norm_t_to_raw <- function(t, version, scale) {
  rows <- norm_rows(version, scale)
  rows$raw[match(t, rows$tscore)]
}

## Which metric a normed scale is on, which is what decides how a four-option
## coding shifted off the official 0-3 range is reconciled before lookup
## (D-020, corrected by D-023). Three metrics, two of which move under a shift:
##
##   * "mean"      -- an item mean (the five domains, the brief form's total).
##   * "sum"       -- a plain sum over the scale's items (PRD).
##   * "invariant" -- unchanged by a shift. INC/INCS sum *within-pair absolute
##                    differences*, which a constant added to both members
##                    cancels out of; ORS counts items sitting at the range
##                    maximum (R/validity_pid5.R:153), and a shift moves the
##                    maximum with the items, leaving the same count.
##
## Each metric names its scales positively rather than falling through to
## "mean", so a scale the shipped tables cover but this partition does not name
## -- a PRDS or SDTD row arriving in a later `pid_norms` -- is a loud failure
## rather than a silent item-mean shift applied to a sum.
norm_mean_scales <- c(
  "negativeAffectivity",
  "detachment",
  "antagonism",
  "disinhibition",
  "psychoticism",
  "total"
)
norm_sum_scales <- "PRD"
norm_invariant_scales <- c("INC", "INCS", "ORS")

## `call` follows the convention the input validators in R/util.R use: the
## abort below is attributed to the exported function the user called, never to
## this internal helper, so the error reads `norm_pid5()`.
norm_metric <- function(scale, version, call = rlang::caller_env()) {
  out <- rep(NA_character_, length(scale))
  out[scale %in% norm_invariant_scales] <- "invariant"
  out[scale %in% norm_sum_scales] <- "sum"
  out[scale %in% norm_mean_scales] <- "mean"

  ## A scale nothing covers is never converted, so its metric is never used --
  ## the 25 facets reach here on every full-form call. Only an *unclassified
  ## covered* scale is a real gap in the partition.
  unknown <- is.na(out)
  if (any(unknown)) {
    covered <- vapply(
      scale[unknown],
      norm_covers,
      logical(1),
      version = version,
      USE.NAMES = FALSE
    )
    if (any(covered)) {
      bad <- scale[unknown][covered]
      cli::cli_abort(
        c(
          ## qty() sits immediately before the marker: cli takes the quantity
          ## from the last value interpolated ahead of it, and {version} would
          ## otherwise set it to 1 and force the singular.
          "The {version} normative tables carry {cli::qty(length(bad))}{?a scale/scales} with no metric formula in this package.",
          "x" = "Unclassified: {.val {bad}}.",
          "i" = "A shifted response coding is reconciled per metric, so an unclassified scale would silently take the item-mean adjustment. Add {cli::qty(length(bad))}{?it/them} to {.code norm_mean_scales}, {.code norm_sum_scales}, or {.code norm_invariant_scales} in {.file R/norm_engine.R}."
        ),
        call = call
      )
    }
    out[unknown] <- "mean"
  }
  out
}

## How much a score collected on a four-option coding starting at `low` exceeds
## the same responses coded 0-3: `low` per item for a mean, `low * nItems` for a
## sum, nothing for a coding-invariant scale. Item counts are read from
## `pid_items` at run time rather than hardcoded.
##
## The "sum" branch depends on a choice made elsewhere: validity_pid5() builds
## PRD with rowSums() and no `na.rm` (R/validity_pid5.R:172), so one unanswered
## item makes the whole sum NA and a *partial* PRD sum never reaches this
## function. `low * nItems` is therefore always subtracted from a complete
## 22-item sum. Were `na.rm = TRUE` ever added there, partial sums would start
## arriving here and this correction would over-subtract them in silence --
## it would have to become `low * (number of items actually answered)`.
norm_shift <- function(scale, metric, low) {
  out <- rep(0, length(scale))
  out[metric == "mean"] <- low
  is_sum <- metric == "sum"
  if (any(is_sum)) {
    n <- vapply(
      scale[is_sum],
      function(s) sum(!is.na(pid_items[[s]])),
      numeric(1)
    )
    out[is_sum] <- low * n
  }
  out
}

## Which observations fall outside a scale's printed raw range, flagged per end.
## Nearest-row selection already caps them -- the nearest row to an above-table
## value is the last printed row, and to a below-table value the floor run's
## selected row -- so this reports what happened rather than deciding it.
## Returns logical vectors rather than counts so a caller converting several
## scales can count each *observation* once, however many of its scores capped.
norm_capped <- function(x, rows) {
  ok <- !is.na(x)
  list(
    low = ok & x < min(rows$raw) - norm_tol,
    high = ok & x > max(rows$raw) + norm_tol
  )
}
