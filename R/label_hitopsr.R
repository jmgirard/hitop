#' Label HiTOP-SR Columns with Semantic Descriptions
#'
#' Add literal item text or clean scale names as attributes to data frame
#' columns, making them readable by data viewers and reporting packages.
#'
#' @param data A data frame containing HiTOP-SR items or scales.
#' @param target A string specifying what to label: `"items"` to label raw item
#'   columns with questionnaire text, or `"scales"` to label computed scale columns.
#' @param prefix An optional string specifying the prefix used on the column names.
#'   (default = `"HSR_"`)
#'
#' @return A data frame with labeled columns.
#'
#' @examples
#' # Attach item text as a `label` attribute to the raw item columns
#' labeled <- label_hitopsr(sim_hitopsr, target = "items", prefix = "hsr_")
#' attr(labeled$hsr_1, "label")
#'
#' @export
label_hitopsr <- function(
  data,
  target = c("items", "scales"),
  prefix = "HSR_"
) {
  target <- match.arg(target)
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_string(prefix))

  data_cols <- colnames(data)

  if (target == "items") {
    # Reconstruct expected column names based on the prefix
    expected_names <- paste0(prefix, hitopsr_items$HSR)
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
    # Assuming hitopsr_scales uses the raw names or camelCase as keys
    # Adjust the matching column if your score_hitopsr function outputs snake_case
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
