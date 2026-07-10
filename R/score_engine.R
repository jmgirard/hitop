# Internal scoring engine shared by score_pid5()/score_hitopsr()/score_hitopbr().
#
# Each exported wrapper resolves only its instrument-specific data — which items
# reverse-key, the per-scale item-number lists, and (for PID-5 FULL/SF) the
# domain -> primary-facet map — and hands it here. The engine runs the whole
# pipeline: validate -> extract -> coerce -> reverse-key -> per-scale score
# (apa_mean vs rowMeans) -> optional FULL/SF domain scores + domain SE ->
# optional per-scale SE with NA-masking -> optional alpha/omega print -> append
# -> tibble. `apa_scoring` and `domain_map` are optional features used only by
# PID-5; HiTOP-SR/BR pass the defaults and get the plain rowMeans path with no
# domains. This is the scoring analog of the generators' build_* internals.
#
# @param data,items,srange,prefix,na.rm,calc_se,append,tibble As in the wrappers.
# @param n_items Expected length of `items` (the instrument's item count).
# @param reverse_items Integer positions within `items` to reverse-key (may be
#   empty).
# @param items_scales Named list mapping each output scale to the item positions
#   that contribute to it.
# @param apa_scoring Logical; if TRUE, use the APA missing-data/proration rule
#   (apa_mean) instead of rowMeans(na.rm).
# @param domain_map Named list of facet-stem vectors per domain (FULL/SF), or
#   NULL to skip domain scoring.
# @param alpha,omega Logical; if TRUE, compute and print per-scale reliability.
# @param mask_se_na Logical; if TRUE, a standard error is set to NA wherever its
#   scale score is NA. score_pid5() has always done this (needed for APA scales
#   dropped for >25% missing); score_hitopsr()/score_hitopbr() historically did
#   not, so they pass FALSE to preserve their exact output. (Unifying the two is
#   a deliberate scoring-output change left for a future milestone.)
# @noRd
score_engine <- function(
  data,
  items,
  n_items,
  reverse_items,
  items_scales,
  srange,
  prefix,
  na.rm,
  calc_se,
  append,
  tibble,
  apa_scoring = FALSE,
  domain_map = NULL,
  alpha = FALSE,
  omega = FALSE,
  mask_se_na = FALSE
) {
  ## Assertions
  validate_data(data)
  validate_items(items, n = n_items)
  validate_item_uniqueness(items)
  warn_item_order(items)
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(na.rm))
  stopifnot(rlang::is_bool(apa_scoring))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(alpha))
  stopifnot(rlang::is_bool(omega))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Under APA scoring, the published missing-data rule governs; na.rm is unused
  if (apa_scoring && !na.rm) {
    cli::cli_warn(c(
      "!" = "{.arg na.rm} is ignored when {.arg apa_scoring} is {.code TRUE}.",
      "i" = "The APA missing-data rule governs scoring. Pass {.code apa_scoring = FALSE} to use {.arg na.rm}."
    ))
  }

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)

  ## Reverse score the necessary items
  if (length(reverse_items) > 0) {
    data_items[reverse_items] <- lapply(
      reverse_items,
      function(i) {
        reverse(
          data_items[[i]],
          low = srange[[1]],
          high = srange[[2]]
        )
      }
    )
  }
  data_items <- bind_columns(data_items)

  ## Calculate scores per scale (facets for FULL/SF, domains for BF, scales for
  ## HiTOP-SR/BR). Under APA scoring, apa_mean applies the 25%-missing cutoff and
  ## proration; otherwise the traditional rowMeans(na.rm) averages whatever items
  ## are present.
  scale_fun <- if (apa_scoring) {
    function(x) apa_mean(data_items[, x, drop = FALSE])
  } else {
    function(x) rowMeans(data_items[, x, drop = FALSE], na.rm = na.rm)
  }
  scale_scores <- bind_columns(lapply(items_scales, scale_fun))

  ## For FULL/SF (domain_map supplied), add the 5 personality-trait domain scores
  ## (APA key Step 3): each domain is the mean of its 3 PRIMARY facet average
  ## scores (map in domain_map). Under APA scoring a domain is not computed if any
  ## one of its 3 primary facets is NA (na.rm = FALSE propagates the NA);
  ## otherwise it honors na.rm.
  if (!is.null(domain_map)) {
    domain_narm <- if (apa_scoring) FALSE else na.rm
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

  ## Preallocate reliability data frame
  reliability_df <- data.frame(
    scale = snakecase::to_title_case(names(items_scales)),
    alpha = NA_real_,
    omega = NA_real_
  )

  # Calculate alpha per scale if requested
  if (isTRUE(alpha)) {
    safe_alpha <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_alpha(df_sub),
        error = function(e) NA_real_
      )
    }
    reliability_df[, "alpha"] <- vapply(items_scales, safe_alpha, numeric(1))
  }

  # Calculate omega per scale if requested
  if (isTRUE(omega)) {
    safe_omega <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_omega(df_sub),
        error = function(e) NA_real_
      )
    }
    reliability_df[, "omega"] <- vapply(items_scales, safe_omega, numeric(1))
  }

  # If any reliabilities are requested, print those that were calculated
  if (isTRUE(alpha) || isTRUE(omega)) {
    keep <- c("scale", if (isTRUE(alpha)) "alpha", if (isTRUE(omega)) "omega")
    print(reliability_df[, keep, drop = FALSE], digits = 3)
  }

  ## Append output to input tibble if requested
  if (append == TRUE) {
    out <- cbind(data, out)
  }

  ## Coerce output to tibble if requested
  if (tibble == TRUE) {
    out <- tibble::as_tibble(out)
  }

  ## Return output
  out
}
