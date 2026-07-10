#' Score the HiTOP-BR Instrument
#'
#' Create a data frame with scores on all the HiTOP-BR scales.
#'
#' @param data A data frame containing all HiTOP-BR items (numerically coded).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 45 HiTOP-BR items in order. Items must be supplied in
#'   instrument order; a misordered mapping silently scores the wrong items, so a
#'   warning is issued when the names share a common prefix and trailing number
#'   but those numbers are not ascending. Duplicated entries are an error.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-BR items, used for reverse-coding. (default = `c(1,
#'   4)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"hbr_"`)
#' @param missing A string selecting how missing item responses are handled when
#'   computing scale scores. `"available"` (the default) averages whatever items
#'   are present (`rowMeans(na.rm = TRUE)`); `"complete"` returns `NA` for any
#'   scale with a missing item (`rowMeans(na.rm = FALSE)`). (default =
#'   `"available"`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#'
#' @details For per-scale reliability estimates (Cronbach's alpha, McDonald's
#'   omega), use [reliability_hitopbr()].
#'
#' @return A \link[tibble]{tibble} containing all scale scores and standard
#'   errors (if requested) and all original `data` columns (if requested).
#'
#' @examples
#' # Score all HiTOP-BR scales from the simulated data
#' score_hitopbr(sim_hitopbr, items = 1:45, append = FALSE)
#'
#' @export
score_hitopbr <- function(
  data,
  items,
  srange = c(1, 4),
  prefix = "hbr_",
  missing = c("available", "complete"),
  calc_se = FALSE,
  append = TRUE
) {
  missing <- match.arg(missing)
  ## Resolve this instrument's data: which items reverse and the per-scale
  ## item-number lists. Shared arg validation and the pipeline run in the engine.
  reverse_items <-
    hitopbr_items[hitopbr_items$Reverse == TRUE, "HBR", drop = TRUE]

  score_engine(
    data = data,
    items = items,
    n_items = 45,
    reverse_items = reverse_items,
    items_scales = hitopbr_scales$itemNumbers,
    srange = srange,
    prefix = prefix,
    missing = missing,
    calc_se = calc_se,
    append = append
  )
}
