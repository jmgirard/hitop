#' Score the HiTOP-PRO Instrument
#'
#' Create a data frame with scores on all the HiTOP-PRO scales.
#'
#' @param data A data frame containing all HiTOP-PRO items (numerically coded).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 405 HiTOP-PRO items in order.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-PRO items, used for reverse-coding. (default = `c(1,
#'   4)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"pro_"`)
#' @param na.rm An optional logical indicating whether missing values should be
#'   ignored when calculating scale scores. (default = `TRUE`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param alpha Optional logical; if `TRUE`, compute and print Cronbach’s alpha
#'   for each scale. Uses pairwise item correlations when `alpha_pairwise = TRUE`.
#'   (default = `FALSE`)
#' @param omega Optional logical; if `TRUE`, compute and print McDonald’s omega
#'   for each scale using Pearson correlations (i.e., non-ordinal). (default = `FALSE`)
#' @param omega_ord Optional logical; if `TRUE`, compute and print McDonald’s omega
#'   for each scale using an ordinal (polychoric) approach. This is independent of
#'   `omega`; you may request one or both. (default = `FALSE`)
#' @param alpha_pairwise Optional logical; when `alpha = TRUE`, use pairwise
#'   complete observations to estimate the item correlation matrix for alpha.
#'   If `FALSE`, uses listwise deletion. (default = `TRUE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`. (default = `TRUE`)
#' @details
#' If any of `alpha`, `omega`, or `omega_ord` are `TRUE`, the function prints a
#' per-scale reliability summary. Only reliability columns that contain at least
#' one non-`NA` value are shown (the `scale` column is always shown).
#'
#' @return A data frame containing all scale scores and standard errors (if
#'   requested) and all original `data` columns (if requested).
#'   Reliability estimates, when requested, are printed as a side effect.
#'
#' @export
score_hitoppro <- function(
  data,
  items,
  srange = c(1, 4),
  prefix = "pro_",
  na.rm = TRUE,
  calc_se = FALSE,
  alpha = FALSE,
  omega = FALSE,
  omega_ord = FALSE,
  alpha_pairwise = TRUE,
  append = TRUE,
  tibble = TRUE
) {
  ## Assertions
  validate_data(data)
  validate_items(items, n = 405)
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(na.rm))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(alpha))
  stopifnot(rlang::is_bool(omega))
  stopifnot(rlang::is_bool(omega_ord))
  stopifnot(rlang::is_bool(alpha_pairwise))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)

  ## Reverse score the necessary items
  utils::data(hitoppro_items)
  items_rev <-
    hitoppro_items[hitoppro_items$Reverse == TRUE, "PRO", drop = TRUE]
  if (length(items_rev) > 0) {
    data_items[items_rev] <- lapply(
      items_rev,
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

  ## Find items per scale
  items_scales <- hitoppro_scales$itemNumbers

  ## Calculate mean scores per scale
  out <- bind_columns(
    lapply(
      items_scales,
      function(x) rowMeans(data_items[, x], na.rm = na.rm)
    )
  )

  ## Apply prefix to scale column names
  colnames(out) <- paste0(prefix, colnames(out))

  ## Add standard errors to output if requested
  if (calc_se) {
    sems_scales <-
      bind_columns(
        lapply(
          items_scales,
          function(x) apply(data_items[, x], MARGIN = 1, FUN = calc_sem)
        )
      )
    colnames(sems_scales) <- paste0(prefix, colnames(sems_scales), "_se")
    out <- cbind(out, sems_scales)
  }

  ## Preallocate reliability data frame
  reliability_df <- data.frame(
    scale = names(items_scales),
    alpha = NA_real_,
    omega = NA_real_,
    omega_ord = NA_real_
  )

  # Calculate alpha per scale if requested
  if (isTRUE(alpha)) {
    safe_alpha <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_alpha(df_sub, pairwise = alpha_pairwise),
        error = function(e) NA_real_
      )
    }
    reliability_df[, 2] <- vapply(items_scales, safe_alpha, numeric(1))
  }

  # Calculate omega per scale if requested
  if (isTRUE(omega)) {
    safe_omega <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_omega(df_sub, ordinal = FALSE),
        error = function(e) NA_real_
      )
    }
    reliability_df[, 3] <- vapply(items_scales, safe_omega, numeric(1))
  }

  # Calculate ordinal omega per scale if requested
  if (isTRUE(omega_ord)) {
    safe_omega <- function(idx) {
      df_sub <- as.data.frame(data_items[, idx, drop = FALSE])
      tryCatch(
        calc_omega(df_sub, ordinal = TRUE),
        error = function(e) NA_real_
      )
    }
    reliability_df[, 4] <- vapply(items_scales, safe_omega, numeric(1))
  }

  # If any reliabilities are requested, print those that were calculated
  if (isTRUE(alpha) || isTRUE(omega) || isTRUE(omega_ord)) {
    keep <- vapply(reliability_df, function(x) any(!is.na(x)), logical(1))
    if ("scale" %in% names(keep)) {
      keep["scale"] <- TRUE
    }
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
