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
#' @export
reliability_hitopsr <- function(
  data,
  items,
  srange = c(1, 4),
  alpha = TRUE,
  omega = TRUE
) {
  reverse_items <-
    hitopsr_items[hitopsr_items$Reverse == TRUE, "HSR", drop = TRUE]

  reliability_engine(
    data = data,
    items = items,
    n_items = 405,
    reverse_items = reverse_items,
    items_scales = hitopsr_scales$itemNumbers,
    srange = srange,
    alpha = alpha,
    omega = omega
  )
}
