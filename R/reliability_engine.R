#' Internal reliability engine shared by the reliability_*() family
#'
#' Each exported wrapper resolves only its instrument-specific data — which items
#' reverse-key and the per-scale item-number lists — and hands it here. The
#' engine validates, extracts, coerces, and reverse-keys the item columns (the
#' shared prep_items() used by score_engine()), then computes Cronbach's alpha
#' and/or McDonald's omega per scale and returns a per-scale tibble. Per-scale
#' estimation is wrapped in tryCatch so a degenerate scale (or an absent lavaan
#' for omega) yields NA rather than aborting the whole call.
#'
#' @param data,items,srange As in the wrappers.
#' @param n_items Expected length of `items` (the instrument's item count).
#' @param reverse_items Integer positions within `items` to reverse-key (may be
#'   empty).
#' @param items_scales Named list mapping each scale to the item positions that
#'   contribute to it.
#' @param alpha,omega Logical; whether to compute each coefficient. A coefficient
#'   is included as an output column only when its flag is TRUE.
#' @param call The calling environment, forwarded to the validators so aborts are
#'   attributed to the exported wrapper rather than to this internal engine.
#' @noRd
reliability_engine <- function(
  data,
  items,
  n_items,
  reverse_items,
  items_scales,
  srange,
  alpha = TRUE,
  omega = TRUE,
  call = rlang::caller_env()
) {
  stopifnot(rlang::is_bool(alpha))
  stopifnot(rlang::is_bool(omega))

  ## Validate, extract, coerce, and reverse-key the item columns
  data_items <- prep_items(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    srange = srange,
    call = call
  )

  out <- data.frame(
    scale = snakecase::to_title_case(names(items_scales)),
    nItems = lengths(items_scales),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  if (alpha) {
    out$alpha <- vapply(
      items_scales,
      function(idx) {
        df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
        tryCatch(calc_alpha(df_sub), error = function(e) NA_real_)
      },
      numeric(1)
    )
  }

  if (omega) {
    out$omega <- vapply(
      items_scales,
      function(idx) {
        df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
        tryCatch(calc_omega(df_sub), error = function(e) NA_real_)
      },
      numeric(1)
    )
  }

  tibble::as_tibble(out)
}
