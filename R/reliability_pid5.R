#' Estimate PID-5 scale reliability
#'
#' Compute per-scale internal-consistency reliability — Cronbach's alpha and
#' McDonald's omega — for the Personality Inventory for DSM-5: full version
#' (PID-5, 220 items), short form (PID-5-SF, 100 items), or brief form (PID-5-BF,
#' 25 items). Reliability is estimated on the reverse-keyed item responses, at
#' the facet level for FULL/SF and the domain level for BF (the same scales
#' [score_pid5()] outputs, before FULL/SF domain aggregation).
#'
#' @param data A data frame containing (at least) all the PID items (numerically
#'   scored and in order).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the PID items in order. Items must be supplied in
#'   instrument order; duplicated entries are an error.
#' @param version A string indicating the version of the PID to score: "FULL",
#'   "SF", or "BF". Will be automatically capitalized. (default = `"FULL"`)
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the items, used for reverse-coding. (default = `c(0, 3)`)
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
#' # Facet-level reliability for the full PID-5 (alpha only)
#' reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)
#'
#' @export
reliability_pid5 <- function(
  data,
  items,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  alpha = TRUE,
  omega = TRUE
) {
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))
  n_items <- switch(
    version,
    "FULL" = 220,
    "SF" = 100,
    "BF" = 25,
    cli::cli_abort("Invalid `version` argument")
  )

  reverse_items <- drop_na(
    pid_items[pid_items$Reverse == TRUE, version, drop = TRUE]
  )
  items_scales <- pid_scales[[version]]$itemNumbers

  reliability_engine(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    items_scales = items_scales,
    srange = srange,
    alpha = alpha,
    omega = omega
  )
}
