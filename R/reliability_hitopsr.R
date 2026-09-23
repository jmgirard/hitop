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
#'   instrument order, or in the form's printed order under
#'   `layout = "printed"`; duplicated entries are an error. The
#'   ascending-name warning [score_hitopsr()] describes reads the names you
#'   supply, so under `layout = "printed"` it also fires for original-number
#'   names in printed order; it can be ignored there, or avoided by supplying
#'   positions. When `items` is omitted or `NULL`, the names on the
#'   `module`'s `columns` attribute are used, as in [score_hitopsr()]. A call
#'   with `items` omitted is an error if no `module` is supplied, if the
#'   module has no `columns` attribute, or under `layout = "printed"`. A
#'   supplied `items` is always used, whether or not the module has the
#'   attribute.
#'   Each column must be numeric or logical, or character holding only
#'   numbers (blank cells and `NA` values count as missing, but the text
#'   `"NA"` is refused). Any other column, such
#'   as the choice text of an online export or a factor, is an error of class
#'   `hitop_nonnumeric_items`.
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
#' @param layout The order the item columns are in. `"instrument"` (the
#'   default) is ascending HiTOP-SR order, as the `generate_*_hitopsr()` forms
#'   lay the items out. `"printed"` is the order a shuffled Word form printed
#'   them: column k holds the answer to the form's printed item k. It needs a
#'   `module` carrying an `item_order` attribute, the record a module
#'   descriptor written by [generate_docx_hitopsr()] with `randomize = TRUE`
#'   keeps and [read_module()] returns; the columns are put back into
#'   instrument order through that attribute before the estimates run. A call
#'   with `layout = "printed"` and no module, a module with no `item_order`,
#'   or an `item_order` that is not a permutation of the module's items is an
#'   error. (default = `"instrument"`)
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
#' # The same for data entered off a shuffled form: the columns are in the
#' # order the form printed its items, recorded on the module's `item_order`
#' # attribute (here set by hand; a descriptor written with `randomize = TRUE`
#' # carries it).
#' attr(m, "item_order") <- c(144L, 202L, 66L, 389L, 260L, 109L, 291L, 118L)
#' printed <- collected[match(attr(m, "item_order"), m$items)]
#' reliability_hitopsr(printed, items = seq_along(printed), module = m,
#'                     layout = "printed", omega = FALSE)
#'
#' @export
reliability_hitopsr <- function(
  data,
  items,
  srange = c(1, 4),
  alpha = TRUE,
  omega = TRUE,
  module = NULL,
  layout = c("instrument", "printed"),
  subset = NULL
) {
  # rlang's matcher, not match.arg(): its refusal names `layout` and both
  # permitted values and blames this call, where match.arg()'s blames itself.
  layout <- rlang::arg_match(layout)
  module <- resolve_module_arg(module, subset)
  ## Same three instrument-resolved inputs score_hitopsr() uses, remapped to
  ## module-column positions when a `module` is supplied.
  inputs <- hitopsr_engine_inputs(module)
  ## A missing or NULL `items` takes the module's `columns` attribute, or
  ## aborts saying to pass `items`.
  items <- module_column_items(missing(items), if (!missing(items)) items,
                               module, layout, data)
  ## Under layout = "printed", put the caller's printed-order `items` into
  ## instrument order through the module's item_order (refusing when there is
  ## none); the heuristic order warning has then already run on the caller's
  ## own vector, so the engine skips it. The caller's vector is kept to order
  ## any report of refused columns.
  caller_items <- items
  items <- layout_items(items, module, layout)

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
    omega = omega,
    check_order = identical(layout, "instrument"),
    caller_items = caller_items
  )
}
