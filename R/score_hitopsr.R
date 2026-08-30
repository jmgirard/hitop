#' Score the HiTOP-SR Instrument
#'
#' Create a data frame with scores on all the HiTOP-SR scales.
#'
#' @param data A data frame containing the HiTOP-SR items (numerically coded):
#'   all 405 of them, or, when `module` is supplied, that module's items.
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the HiTOP-SR items held in `data` — all 405, or, when
#'   `module` is supplied, that module's items. Items must be supplied in
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
#' @param calc_se An optional logical indicating whether to calculate a
#'   standard error for each scale score: the SD of the items the respondent
#'   actually answered divided by the square root of how many of those items
#'   they answered. Each one summarizes how much a respondent's answers varied
#'   within a scale. It is not a standard error of measurement — no reliability
#'   estimate enters it — so it does not give a confidence interval for a
#'   respondent's true score; for measurement precision see
#'   [reliability_hitopsr()]. (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param module An optional `hitop_module` object, as returned by
#'   [hitop_module()], describing a module of the instrument. When supplied,
#'   `data` and `items` hold only that module's item columns — in ascending
#'   instrument order, as the `generate_*_hitopsr()` forms lay them out — and
#'   only that module's scales are scored. When `NULL`, all 405 items are
#'   expected and all 76 scales are scored. (default = `NULL`)
#' @param subset Deprecated. The former name of `module`; supplying it warns.
#'   Supplying both `module` and `subset` is an error. (default = `NULL`)
#'
#' @details For per-scale reliability estimates (Cronbach's alpha, McDonald's
#'   omega), use [reliability_hitopsr()].
#'
#'   **Errors.** With `append = TRUE`, a column of `data` whose name this call
#'   would also produce is an error rather than an overwrite or a duplicated
#'   column: the message names every colliding column. Re-run with
#'   `append = FALSE` to return only the new columns, or drop the colliding
#'   columns from `data` first. The condition is classed
#'   `hitop_append_collision`, so a caller can catch this refusal by name.
#'
#' @return A \link[tibble]{tibble} containing all scale scores and standard
#'   errors (if requested) and all original `data` columns (if requested).
#'
#' @examples
#' # Score all HiTOP-SR scales from the simulated data
#' score_hitopsr(sim_hitopsr, items = 1:405, append = FALSE)
#'
#' # Score data collected with a two-scale module. Select the item columns
#' # by name: `m$items` holds original HiTOP-SR numbers, which are column
#' # positions only in a data frame that is exactly the 405 items in order.
#' m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
#' collected <- sim_hitopsr[paste0("hsr_", m$items)]
#' score_hitopsr(collected, items = names(collected), module = m, append = FALSE)
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
  module = NULL,
  subset = NULL
) {
  missing <- match.arg(missing)
  module <- resolve_module_arg(module, subset)
  ## Resolve this instrument's data: which items reverse and the per-scale
  ## item-number lists. With a `module`, the same inputs are remapped to
  ## positions within the module's own columns; without one, item number and
  ## position coincide. Shared arg validation and the pipeline run in the engine.
  inputs <- hitopsr_engine_inputs(module)

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
    se_instead = "Use {.fn interval_hitopsr} for an interval around a true score.",
    append = append
  )
}
