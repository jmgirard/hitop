#' Internal scoring engine shared by score_pid5()/score_hitopsr()/score_hitopbr()
#'
#' Each exported wrapper resolves only its instrument-specific data — which items
#' reverse-key, the per-scale item-number lists, and (for PID-5 FULL/SF) the
#' domain -> primary-facet map — and hands it here. The engine runs the whole
#' pipeline: validate -> extract -> coerce -> reverse-key (shared prep_items) ->
#' per-scale score (apa/available/complete) -> optional FULL/SF domain scores +
#' domain SE -> optional per-scale SE with NA-masking -> append -> tibble. The
#' `domain_map` feature is used only by PID-5; HiTOP-SR/BR pass NULL and get the
#' plain per-scale path with no domains. This is the scoring analog of the
#' generators' build_* internals. Reliability (alpha/omega) is no longer computed
#' here — it moved to the returning reliability_*() family (M15).
#'
#' @param data,items,srange,prefix,calc_se,append As in the wrappers.
#' @param n_items Expected length of `items` (the instrument's item count).
#' @param reverse_items Integer positions within `items` to reverse-key (may be
#'   empty).
#' @param items_scales Named list mapping each output scale to the item positions
#'   that contribute to it.
#' @param missing One of `"apa"`, `"available"`, or `"complete"` (resolved by the
#'   wrapper). `"apa"` applies the APA missing-data/proration rule (apa_mean);
#'   `"available"` averages the present items (rowMeans, na.rm = TRUE);
#'   `"complete"` returns NA for any scale with a missing item (rowMeans,
#'   na.rm = FALSE).
#' @param domain_map Named list of facet-stem vectors per domain (FULL/SF), or
#'   NULL to skip domain scoring.
#' @param mask_se_na Logical; if TRUE, a standard error is set to NA wherever its
#'   scale score is NA. score_pid5() has always done this (needed for APA scales
#'   dropped for >25% missing); score_hitopsr()/score_hitopbr() historically did
#'   not, so they pass FALSE to preserve their exact output. (Unifying the two is
#'   a deliberate scoring-output change left for a future milestone.)
#' @param call The calling environment, used so input-validation aborts are
#'   attributed to the exported wrapper (score_pid5() etc.) rather than to this
#'   internal engine. Defaults to the wrapper that called score_engine().
#' @noRd
score_engine <- function(
  data,
  items,
  n_items,
  reverse_items,
  items_scales,
  srange,
  prefix,
  missing,
  calc_se,
  append,
  domain_map = NULL,
  mask_se_na = FALSE,
  call = rlang::caller_env()
) {
  ## Scalar-argument assertions (item/data/srange checks run in prep_items, with
  ## call = the wrapper so errors blame it, not score_engine())
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_string(missing))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(append))

  ## Validate, extract, coerce, and reverse-key the item columns
  data_items <- prep_items(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    srange = srange,
    call = call
  )

  ## Calculate scores per scale (facets for FULL/SF, domains for BF, scales for
  ## HiTOP-SR/BR). `missing` selects the algorithm: apa_mean applies the
  ## 25%-missing cutoff and proration; "available" averages whatever items are
  ## present; "complete" returns NA if any item is missing.
  scale_fun <- switch(
    missing,
    apa = function(x) apa_mean(data_items[, x, drop = FALSE]),
    available = function(x) rowMeans(data_items[, x, drop = FALSE], na.rm = TRUE),
    complete = function(x) rowMeans(data_items[, x, drop = FALSE], na.rm = FALSE),
    cli::cli_abort("Invalid `missing` argument", call = call)
  )
  scale_scores <- bind_columns(lapply(items_scales, scale_fun))

  ## For FULL/SF (domain_map supplied), add the 5 personality-trait domain scores
  ## (APA key Step 3): each domain is the mean of its 3 PRIMARY facet average
  ## scores (map in domain_map). Under "apa"/"complete" a domain is NA if any one
  ## of its 3 primary facets is NA (na.rm = FALSE propagates the NA); under
  ## "available" it averages the facets that are present.
  if (!is.null(domain_map)) {
    domain_narm <- missing == "available"
    domain_scores <- bind_columns(
      lapply(
        domain_map,
        function(f) rowMeans(scale_scores[, f, drop = FALSE], na.rm = domain_narm)
      )
    )
    out <- cbind(scale_scores, domain_scores)
  } else {
    out <- scale_scores
  }

  ## Apply prefix to scale column names
  colnames(out) <- paste0(prefix, colnames(out))

  ## Add standard errors to output if requested
  if (calc_se) {
    sems_scales <-
      bind_columns(
        lapply(
          items_scales,
          function(x) apply(data_items[, x, drop = FALSE], MARGIN = 1, FUN = calc_sem)
        )
      )
    ## Domain SEs mirror facet SEs, computed over the 3 primary facet scores
    if (!is.null(domain_map)) {
      sems_domains <- bind_columns(
        lapply(
          domain_map,
          function(f) apply(scale_scores[, f, drop = FALSE], MARGIN = 1, FUN = calc_sem)
        )
      )
      sems_scales <- cbind(sems_scales, sems_domains)
    }
    ## A standard error is undefined where its scale score is NA (e.g. an APA
    ## scale dropped for >25% missing). `out` and `sems_scales` share column order.
    ## Gated: score_pid5() masks; HiTOP-SR/BR historically did not (see mask_se_na).
    if (mask_se_na) {
      sems_scales[is.na(out)] <- NA
    }
    colnames(sems_scales) <- paste0(prefix, colnames(sems_scales), "_se")
    out <- cbind(out, sems_scales)
  }

  ## Append output to input tibble if requested
  if (append == TRUE) {
    out <- cbind(data, out)
  }

  ## Always return a tibble
  tibble::as_tibble(out)
}
