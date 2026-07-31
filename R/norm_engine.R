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

  ## Distance from each observed value to every printed raw (n x k).
  d <- abs(outer(x[ok], rows$raw, "-"))
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

## Observations falling outside a scale's printed raw range, counted per end.
## Nearest-row selection already caps them -- the nearest row to an above-table
## value is the last printed row, and to a below-table value the floor run's
## selected row -- so this reports what happened rather than deciding it.
norm_capped <- function(x, rows) {
  ok <- !is.na(x)
  c(
    low = sum(ok & x < min(rows$raw) - norm_tol),
    high = sum(ok & x > max(rows$raw) + norm_tol)
  )
}
