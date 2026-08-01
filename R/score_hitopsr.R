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
#' @param missing A string selecting how missing item responses are handled when
#'   computing scale scores. `"available"` (the default) averages whatever items
#'   are present (`rowMeans(na.rm = TRUE)`); `"complete"` returns `NA` for any
#'   scale with a missing item (`rowMeans(na.rm = FALSE)`). (default =
#'   `"available"`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param subset An optional `hitop_subset` object, as returned by
#'   [hitop_subset()], describing a short form of the instrument. When supplied,
#'   `data` and `items` hold only that subset's item columns — in ascending
#'   instrument order, as the `generate_*_hitopsr()` forms lay them out — and
#'   only that subset's scales are scored. When `NULL`, all 405 items are
#'   expected and all 76 scales are scored. (default = `NULL`)
#'
#' @details For per-scale reliability estimates (Cronbach's alpha, McDonald's
#'   omega), use [reliability_hitopsr()].
#'
#' @return A \link[tibble]{tibble} containing all scale scores and standard
#'   errors (if requested) and all original `data` columns (if requested).
#'
#' @examples
#' # Score all HiTOP-SR scales from the simulated data
#' score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE)
#'
#' # Score data collected with a two-scale short form
#' s <- hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' score_hitopsr(sim_hitopsr[s$items], items = seq_len(s$nItems),
#'               subset = s, append = FALSE)
#'
#' @export
score_hitopsr <- function(
  data,
  items,
  srange = c(1, 4),
  prefix = "hsr_",
  missing = c("available", "complete"),
  calc_se = FALSE,
  append = TRUE,
  subset = NULL
) {
  missing <- match.arg(missing)
  ## Resolve this instrument's data: which items reverse and the per-scale
  ## item-number lists. With a `subset`, the same three inputs are remapped to
  ## positions within the subset's own columns; without one, item number and
  ## position coincide. Shared arg validation and the pipeline run in the engine.
  inputs <- hitopsr_engine_inputs(subset)

  score_engine(
    data = data,
    items = items,
    n_items = inputs$n_items,
    reverse_items = inputs$reverse_items,
    items_scales = inputs$items_scales,
    srange = srange,
    prefix = prefix,
    missing = missing,
    calc_se = calc_se,
    append = append
  )
}
