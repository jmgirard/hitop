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
#' @param scale_names Character vector of the canonical display names, parallel
#'   to `items_scales`. Read from the instrument's keying table by the caller and
#'   passed through unchanged: the engine never derives a printed name from a
#'   camelCase stem, because the two spellings diverge on nine scales and the
#'   table's is the canonical one. A length that disagrees with `items_scales` is
#'   guarded explicitly below: `data.frame()` would recycle a divisor length
#'   rather than abort, silently labelling the rows with a repeating name.
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
  scale_names,
  srange,
  alpha = TRUE,
  omega = TRUE,
  call = rlang::caller_env()
) {
  validate_flag(alpha, arg = "alpha", call = call)
  validate_flag(omega, arg = "omega", call = call)

  ## Validate, extract, coerce, and reverse-key the item columns
  data_items <- prep_items(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    srange = srange,
    call = call
  )

  ## An explicit guard, not data.frame()'s: data.frame() aborts only when
  ## neither length divides the other, so a supplier handing over a shorter name
  ## vector whose length divides the scale count would recycle it and label the
  ## rows with a repeating name -- the wrong-name class this engine exists to
  ## avoid.
  cli_assert(
    length(scale_names) == length(items_scales),
    c(
      "{.arg scale_names} must have one name per scale.",
      "x" = "Got {length(scale_names)} name{?s} for {length(items_scales)} scale{?s}."
    ),
    call = call
  )

  out <- data.frame(
    Scale = scale_names,
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
