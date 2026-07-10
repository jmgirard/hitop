#' Estimate HiTOP-BR scale reliability
#'
#' Compute per-scale internal-consistency reliability — Cronbach's alpha and
#' McDonald's omega — for the HiTOP Brief-Report (45 items). Reliability is
#' estimated on the item responses for each of the scales that [score_hitopbr()]
#' outputs (the HiTOP-BR has no reverse-keyed items).
#'
#' @param data A data frame containing all HiTOP-BR items (numerically coded).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 45 HiTOP-BR items in order. Items must be supplied in
#'   instrument order; duplicated entries are an error.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-BR items, used for reverse-coding. (default = `c(1, 4)`)
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
#'   rather than aborting the call. The overlapping `externalizing` and `pFactor`
#'   scales are included alongside the six base spectra.
#'
#' @return A \link[tibble]{tibble} with one row per scale and columns `scale`,
#'   `nItems`, and (when requested) `alpha` and `omega`.
#'
#' @examples
#' # Per-scale alpha for the HiTOP-BR
#' reliability_hitopbr(sim_hitopbr, items = 1:45, omega = FALSE)
#'
#' @export
reliability_hitopbr <- function(
  data,
  items,
  srange = c(1, 4),
  alpha = TRUE,
  omega = TRUE
) {
  reverse_items <-
    hitopbr_items[hitopbr_items$Reverse == TRUE, "HBR", drop = TRUE]

  reliability_engine(
    data = data,
    items = items,
    n_items = 45,
    reverse_items = reverse_items,
    items_scales = hitopbr_scales$itemNumbers,
    srange = srange,
    alpha = alpha,
    omega = omega
  )
}
