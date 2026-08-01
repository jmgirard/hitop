#' Estimate HiTOP-SR scale reliability
#'
#' Compute per-scale internal-consistency reliability — Cronbach's alpha and
#' McDonald's omega — for the HiTOP Self-Report (405 items). Reliability is
#' estimated on the reverse-keyed item responses for each of the scales that
#' [score_hitopsr()] outputs.
#'
#' @param data A data frame containing all HiTOP-SR items (numerically coded).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 405 HiTOP-SR items in order. Items must be supplied in
#'   instrument order; duplicated entries are an error.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-SR items, used for reverse-coding. (default = `c(1, 4)`)
#' @param alpha Optional logical; if `TRUE`, include a column of Cronbach's alpha
#'   per scale. (default = `TRUE`)
#' @param omega Optional logical; if `TRUE`, include a column of McDonald's omega
#'   (total) per scale, estimated via a one-factor CFA (requires the \pkg{lavaan}
#'   package). (default = `TRUE`)
#' @param subset An optional `hitop_subset` object, as returned by
#'   [hitop_subset()], describing a short form of the instrument. When supplied,
#'   `data` and `items` hold only that subset's item columns — in ascending
#'   instrument order, as the `generate_*_hitopsr()` forms lay them out — and one
#'   row is returned per subset scale. When `NULL`, all 405 items are expected
#'   and all 76 scales are estimated. (default = `NULL`)
#'
#' @details Alpha is computed by [calc_alpha()] (covariance-based, pairwise
#'   deletion) and omega by [calc_omega()] (one-factor lavaan CFA, FIML). A scale
#'   whose estimate cannot be computed (e.g. too few items or, for omega, a
#'   non-converging CFA or an uninstalled \pkg{lavaan}) is returned as `NA`
#'   rather than aborting the call.
#'
#' @return A \link[tibble]{tibble} with one row per scale and columns `scale`,
#'   `nItems`, and (when requested) `alpha` and `omega`.
#'
#' @examples
#' # Per-scale alpha for the HiTOP-SR
#' reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)
#'
#' # Per-scale alpha for data collected with a two-scale short form
#' s <- hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' reliability_hitopsr(sim_hitopsr[s$items], items = seq_len(s$nItems),
#'                     subset = s, omega = FALSE)
#'
#' @export
reliability_hitopsr <- function(
  data,
  items,
  srange = c(1, 4),
  alpha = TRUE,
  omega = TRUE,
  subset = NULL
) {
  ## Same three instrument-resolved inputs score_hitopsr() uses, remapped to
  ## subset-column positions when a `subset` is supplied.
  inputs <- hitopsr_engine_inputs(subset)

  reliability_engine(
    data = data,
    items = items,
    n_items = inputs$n_items,
    reverse_items = inputs$reverse_items,
    items_scales = inputs$items_scales,
    srange = srange,
    alpha = alpha,
    omega = omega
  )
}
