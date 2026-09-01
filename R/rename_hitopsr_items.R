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
#' @param prefix A string pasted literally before each standardized item
#'   number, which is zero-padded to three digits: `hsr_001` to `hsr_405` under
#'   the default, the pattern the shipped datasets and the package's Qualtrics
#'   and REDCap exports use. (default = `"hsr_"`)
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
  prefix = "hsr_"
) {
  method <- match.arg(method)

  ## Assertions
  validate_data(data)
  validate_string(prefix, arg = "prefix")

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
    colnames(data)[matched_idx] <- item_names(
      prefix, matched_hsr, max_n = max(hitopsr_items$HSR)
    )
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
      colnames(data)[data_locs] <- item_names(
        prefix, matched_hsr, max_n = max(hitopsr_items$HSR)
      )
    }
  }

  ## Check for completeness and warn if fewer than all items were matched
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
