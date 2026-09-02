#' Estimate HiTOP-SR scale reliability
#'
#' Compute per-scale internal-consistency reliability — Cronbach's alpha and
#' McDonald's omega — for the HiTOP Self-Report (405 items). Reliability is
#' estimated on the reverse-keyed item responses for each of the scales that
#' [score_hitopsr()] outputs.
#'
#' @param data A data frame containing the HiTOP-SR items (numerically coded):
#'   all 405 of them, or, when `module` is supplied, that module's items.
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the HiTOP-SR items held in `data` — all 405, or, when
#'   `module` is supplied, that module's items. Items must be supplied in
#'   instrument order; duplicated entries are an error.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-SR items, used for reverse-coding. (default = `c(1, 4)`)
#' @param alpha Optional logical; if `TRUE`, include a column of Cronbach's alpha
#'   per scale. (default = `TRUE`)
#' @param omega Optional logical; if `TRUE`, include a column of McDonald's omega
#'   (total) per scale, estimated via a one-factor CFA (requires the \pkg{lavaan}
#'   package). (default = `TRUE`)
#' @param module An optional `hitop_module` object, as returned by
#'   [hitop_module()], describing a module of the instrument. When supplied,
#'   `data` and `items` hold only that module's item columns — in ascending
#'   instrument order, as the `generate_*_hitopsr()` forms lay them out — and one
#'   row is returned per module scale. When `NULL`, all 405 items are expected
#'   and all 76 scales are estimated. (default = `NULL`)
#' @param subset Deprecated. The former name of `module`; supplying it warns.
#'   Supplying both `module` and `subset` is an error. (default = `NULL`)
#'
#' @details Alpha is computed by [calc_alpha()] (covariance-based, pairwise
#'   deletion) and omega by [calc_omega()] (one-factor lavaan CFA, FIML). A scale
#'   whose estimate cannot be computed (e.g. too few items or, for omega, a
#'   non-converging CFA or an uninstalled \pkg{lavaan}) is returned as `NA`
#'   rather than aborting the call.
#'
#' @return A \link[tibble]{tibble} with one row per scale and columns `Scale`
#'   (the scale's canonical display name, as the instrument's keying table spells
#'   it), `camelCase` (the stem that names the scale's column in the matching
#'   `score_*()` output, read from the same keying-table row), `nItems`
#'   (integer), and (when requested) `alpha` and `omega`.
#'
#' @examples
#' # Per-scale alpha for the HiTOP-SR
#' reliability_hitopsr(sim_hitopsr, items = 1:405, omega = FALSE)
#'
#' # Per-scale alpha for data collected with a two-scale module. Select the
#' # item columns by name: `m$items` holds original HiTOP-SR numbers, which are
#' # column positions only in a data frame that is exactly the 405 items in order.
#' m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' collected <- sim_hitopsr[sprintf("hsr_%03d", m$items)]
#' reliability_hitopsr(collected, items = names(collected), module = m, omega = FALSE)
#'
#' @export
reliability_hitopsr <- function(
  data,
  items,
  srange = c(1, 4),
  alpha = TRUE,
  omega = TRUE,
  module = NULL,
  subset = NULL
) {
  module <- resolve_module_arg(module, subset)
  ## Same three instrument-resolved inputs score_hitopsr() uses, remapped to
  ## module-column positions when a `module` is supplied.
  inputs <- hitopsr_engine_inputs(module)

  reliability_engine(
    data = data,
    items = items,
    n_items = inputs$n_items,
    reverse_items = inputs$reverse_items,
    items_scales = inputs$items_scales,
    scale_names = inputs$scale_names,
    scale_stems = inputs$scale_stems,
    srange = srange,
    alpha = alpha,
    omega = omega
  )
}
