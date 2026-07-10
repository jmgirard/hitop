#' Label HiTOP-BR Columns with Semantic Descriptions
#'
#' Add literal item text or clean scale names as attributes to data frame
#' columns for the HiTOP Brief Report (HiTOP-BR), making them readable by
#' data viewers and reporting packages.
#'
#' @param data A data frame containing HiTOP-BR items or scales.
#' @param target A string specifying what to label: `"items"` to label raw item
#'   columns with questionnaire text, or `"scales"` to label computed scale columns.
#' @param prefix An optional string specifying the prefix used on the column names.
#'   (default = `"HBR_"`)
#'
#' @return A data frame with labeled columns.
#'
#' @export
label_hitopbr <- function(
  data,
  target = c("items", "scales"),
  prefix = "HBR_"
) {
  target <- match.arg(target)
  stopifnot(is.data.frame(data))
  stopifnot(rlang::is_string(prefix))

  data_cols <- colnames(data)

  if (target == "items") {
    # Reconstruct expected column names based on the prefix
    expected_names <- paste0(prefix, hitopbr_items$HBR)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected item names with prefix {.str {prefix}}."
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- hitopbr_items$Text[locs[i]]
    }
  } else if (target == "scales") {
    expected_names <- paste0(prefix, hitopbr_scales$camelCase)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected scale names with prefix {.str {prefix}}."
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- hitopbr_scales$Scale[locs[i]]
    }
  }

  data
}
