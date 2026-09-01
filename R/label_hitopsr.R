#' Label HiTOP-SR Columns with Semantic Descriptions
#'
#' Add literal item text or clean scale names as attributes to data frame
#' columns, making them readable by data viewers and reporting packages.
#'
#' @param data A data frame containing HiTOP-SR items or scales.
#' @param target A string specifying what to label: `"items"` to label raw item
#'   columns with questionnaire text, or `"scales"` to label computed scale columns.
#' @param prefix A string specifying the prefix used on the column names. Item
#'   columns are expected as the prefix followed by the item number zero-padded
#'   to three digits (`hsr_001` to `hsr_405` under the default, the pattern the
#'   shipped datasets and the package's Qualtrics and REDCap exports use); scale
#'   columns as the prefix followed by the scale's `camelCase` name, which is
#'   what [score_hitopsr()] writes under its own default `prefix`.
#'   (default = `"hsr_"`)
#'
#' @return A data frame with labeled columns.
#'
#' @examples
#' # Attach item text as a `label` attribute to the raw item columns
#' labeled <- label_hitopsr(sim_hitopsr, target = "items")
#' attr(labeled$hsr_001, "label")
#'
#' @export
label_hitopsr <- function(
  data,
  target = c("items", "scales"),
  prefix = "hsr_"
) {
  target <- match.arg(target)
  validate_data(data)
  validate_string(prefix, arg = "prefix")

  data_cols <- colnames(data)

  if (target == "items") {
    # Reconstruct expected column names based on the prefix
    expected_names <- item_names(prefix, hitopsr_items$HSR)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected item names with prefix {.str {prefix}}."
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- hitopsr_items$Text[locs[i]]
    }
  } else if (target == "scales") {
    # Scale columns carry the camelCase name score_hitopsr() writes.
    expected_names <- paste0(prefix, hitopsr_scales$camelCase)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected scale names with prefix {.str {prefix}}."
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- hitopsr_scales$Scale[locs[i]]
    }
  }

  data
}
