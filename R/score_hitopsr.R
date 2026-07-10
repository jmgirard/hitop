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
  ## Assertions
  validate_data(data)
  validate_items(items, n = 405)
  validate_item_uniqueness(items)
  warn_item_order(items)
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(na.rm))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(alpha))
  stopifnot(rlang::is_bool(omega))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)

  ## Reverse score the necessary items
  items_rev <-
    hitopsr_items[hitopsr_items$Reverse == TRUE, "HSR", drop = TRUE]
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
  items_scales <- hitopsr_scales$itemNumbers

  ## Calculate mean scores per scale
  out <- bind_columns(
    lapply(
      items_scales,
      function(x) rowMeans(data_items[, x, drop = FALSE], na.rm = na.rm)
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
          function(x) {
            apply(
              data_items[, x, drop = FALSE],
              MARGIN = 1,
              FUN = calc_sem
            )
          }
        )
      )
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

#' Rename Columns to Standard HiTOP-SR Item Names
#'
#' Rename data frame columns to standard HiTOP-SR item names based on either
#' legacy ("Original") variable names or item text matching.
#'
#' @param data A data frame containing the HiTOP-SR items.
#' @param method A string specifying the matching method: `"original"` to match
#'   against the legacy item pool names in `hitopsr_items$Original`, or `"text"`
#'   to match against the literal item prompt text. (default = `"original"`)
#' @param item_cols An optional character vector of current column names to
#'   be renamed. Required if `method = "text"`.
#' @param item_text An optional character vector of item texts corresponding
#'   exactly to the columns specified in `item_cols`. Required if `method = "text"`.
#' @param prefix An optional string to add before each standardized item number.
#'   (default = `"HSR_"`)
#'
#' @return A data frame with renamed column names for the matched HiTOP-SR items.
#'
#' @examples
#' # Rename legacy item-pool columns to standard HiTOP-SR item names
#' legacy <- head(hitopsr_items$Original, 3)
#' df <- as.data.frame(matrix(0, nrow = 2, ncol = 3,
#'                            dimnames = list(NULL, legacy)))
#' names(rename_hitopsr_items(df, method = "original"))
#'
#' @export
rename_hitopsr_items <- function(
  data,
  method = c("original", "text"),
  item_cols = NULL,
  item_text = NULL,
  prefix = "HSR_"
) {
  method <- match.arg(method)

  ## Assertions
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_string(prefix))

  ## Track matched standard item numbers for the final summary warning
  matched_hsr <- numeric(0)

  if (method == "original") {
    data_cols <- colnames(data)
    locs <- match(data_cols, hitopsr_items$Original)

    matched_idx <- !is.na(locs)
    if (!any(matched_idx)) {
      cli::cli_warn(
        "No columns matched the legacy 'Original' names in {.var hitopsr_items}."
      )
      return(data)
    }

    matched_hsr <- hitopsr_items$HSR[locs[matched_idx]]
    colnames(data)[matched_idx] <- paste0(prefix, matched_hsr)
  } else if (method == "text") {
    if (is.null(item_cols) || is.null(item_text)) {
      cli::cli_abort(
        "Both {.arg item_cols} and {.arg item_text} must be provided when {.code method = 'text'}."
      )
    }
    if (length(item_cols) != length(item_text)) {
      cli::cli_abort(
        "{.arg item_cols} and {.arg item_text} must be of the same length."
      )
    }

    ## Verify columns exist in data
    data_locs <- match(item_cols, colnames(data))
    if (any(is.na(data_locs))) {
      cli::cli_abort(
        "Some names in {.arg item_cols} were not found in the data frame columns."
      )
    }

    ## Match text (using trimws for robustness against trailing whitespaces)
    locs <- match(trimws(item_text), trimws(hitopsr_items$Text))

    if (any(is.na(locs))) {
      missing_idx <- which(is.na(locs))
      unmatched_items <- item_text[missing_idx]

      # Name the vector elements "x" so cli formats them as red error bullets
      names(unmatched_items) <- rep("x", length(unmatched_items))

      # Use {qty()} to explicitly anchor the pluralization context
      cli::cli_warn(c(
        "{cli::qty(length(unmatched_items))}The following item text{?s} could not be matched and {?was/were} skipped:",
        unmatched_items
      ))

      data_locs <- data_locs[-missing_idx]
      locs <- locs[-missing_idx]
    }

    if (length(locs) > 0) {
      matched_hsr <- hitopsr_items$HSR[locs]
      colnames(data)[data_locs] <- paste0(prefix, matched_hsr)
    }
  }

  ## Check for completeness and issue a subset warning if appropriate
  n_matched <- length(unique(matched_hsr))
  if (n_matched > 0 && n_matched < 405) {
    cli::cli_warn(c(
      "Only {n_matched} out of 405 HiTOP-SR items were successfully matched and renamed.",
      "i" = "Note: If you plan to use {.fn score_hitopsr}, ensure uncollected items exist in the data frame as {.code NA} columns."
    ))
  }

  ## Return output
  data
}
