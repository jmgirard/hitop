#' Score the HiTOP-SR Instrument
#'
#' Create a data frame with scores on all the HiTOP-SR scales.
#'
#' @param data A data frame containing all HiTOP-SR items (numerically coded).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 405 HiTOP-SR items in order. Items must be supplied in
#'   instrument order; a misordered mapping silently scores the wrong items, so a
#'   warning is issued when the names share a common prefix and trailing number
#'   but those numbers are not ascending. Duplicated entries are an error.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-SR items, used for reverse-coding. (default = `c(1,
#'   4)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"hsr_"`)
#' @param na.rm An optional logical indicating whether missing values should be
#'   ignored when calculating scale scores. (default = `TRUE`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param alpha Optional logical; if `TRUE`, compute and print Cronbach’s alpha
#'   for each scale. (default = `FALSE`)
#' @param omega Optional logical; if `TRUE`, compute and print McDonald’s omega
#'   for each scale using Pearson correlations (i.e., non-ordinal). (default =
#'   `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`. (default = `TRUE`)
#'
#' @details If either `alpha` or `omega` are `TRUE`, the function prints a
#'   per-scale reliability summary. Only reliability columns that contain at
#'   least one non-`NA` value are shown (the `scale` column is always shown).
#'
#' @return A data frame containing all scale scores and standard errors (if
#'   requested) and all original `data` columns (if requested). Reliability
#'   estimates, when requested, are printed as a side effect.
#'
#' @examples
#' # Score all HiTOP-SR scales from the simulated data
#' score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE)
#'
#' @export
score_hitopsr <- function(
  data,
  items,
  srange = c(1, 4),
  prefix = "hsr_",
  na.rm = TRUE,
  calc_se = FALSE,
  alpha = FALSE,
  omega = FALSE,
  append = TRUE,
  tibble = TRUE
) {
  ## Resolve this instrument's data: which items reverse and the per-scale
  ## item-number lists. Shared arg validation and the pipeline run in the engine.
  reverse_items <-
    hitopsr_items[hitopsr_items$Reverse == TRUE, "HSR", drop = TRUE]

  score_engine(
    data = data,
    items = items,
    n_items = 405,
    reverse_items = reverse_items,
    items_scales = hitopsr_scales$itemNumbers,
    srange = srange,
    prefix = prefix,
    na.rm = na.rm,
    calc_se = calc_se,
    append = append,
    tibble = tibble,
    alpha = alpha,
    omega = omega
  )
}
