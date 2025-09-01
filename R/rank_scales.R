#' Rank and list the top-\eqn{n} scales per row
#'
#' For each row of selected scale columns, return a comma-separated string of
#' the \code{top} highest (or lowest) scale names, optionally removing a common
#' \code{prefix} from the column names and optionally appending the result to
#' the original data as a tibble.
#'
#' @param data A data frame containing the scale columns.
#' @param scales A character or integer vector identifying the columns in
#'   \code{data} to rank (names or positions). These columns should be numeric.
#' @param prefix A length-1 string giving a leading pattern to strip from each
#'   selected column name before concatenation, or \code{NULL} to keep names as
#'   is. The match is interpreted as a regular expression anchored to the start
#'   of the name (i.e., \code{"^"} is prepended). Special regex characters in
#'   \code{prefix} will be treated as regex metacharacters.
#' @param top Integer (length 1). The number of columns to include per row
#'   after ranking. Must be between 1 and \code{length(scales)}.
#' @param dir Direction of ranking: \code{"high"} for largest values first or
#'   \code{"low"} for smallest values first.
#' @param append Logical. If \code{TRUE}, bind the result as a new column to
#'   \code{data}. If \code{FALSE}, return only the result vector.
#' @param tibble Logical. If \code{TRUE}, coerce the output to a tibble via
#'   \code{tibble::as_tibble()} (applies whether or not \code{append} is
#'   \code{TRUE}).
#'
#' @details
#' Ranking is performed row-wise using \code{order()} on the selected columns.
#' Ties are resolved by the original column order (the default behavior of
#' \code{order}), which is alphabetical if using the package's scoring
#' functions. Missing values are placed last by \code{order()} and will be
#' included only if there are fewer than \code{top} non-missing values in a row.
#'
#' If \code{prefix} is not \code{NULL}, the function removes the leading
#' pattern \code{paste0("^", prefix)} from each selected column name before
#' concatenation.
#'
#' When \code{append = TRUE}, the appended column is named \code{"out"}.
#' The order of existing columns in \code{data} is preserved.
#'
#' @return
#' If \code{append = FALSE} and \code{tibble = FALSE}: a character vector of
#' length \code{nrow(data)} where each element is a comma-separated list of the
#' selected scale names in ranked order.
#'
#' If \code{append = TRUE}: a data frame (or tibble if \code{tibble = TRUE})
#' equal to \code{data} with an added character column \code{out}.
#'
#' If \code{append = FALSE} and \code{tibble = TRUE}: a tibble with one column
#' \code{out}.
#'
#' @export
rank_scales <- function(
  data,
  scales,
  prefix = NULL,
  top = 5,
  dir = "high",
  append = TRUE,
  tibble = TRUE
) {
  ## Validate args
  validate_data(data)
  validate_scales(scales)
  stopifnot(is.null(prefix) || rlang::is_string(prefix))
  stopifnot(rlang::is_integerish(top, n = 1))
  stopifnot(top >= 1 && top <= length(scales))
  stopifnot(dir %in% c("high", "low"))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract scale columns
  data_scales <- data[scales]

  col_names <- colnames(data_scales)
  if (!is.null(prefix)) {
    col_names <- sub(paste0("^", prefix), "", col_names)
  }

  ## Find the top scales per subject
  out <- apply(data_scales, MARGIN = 1, function(row) {
    if (dir == "high") {
      idx <- order(row, decreasing = TRUE)[seq_len(top)]
    } else {
      idx <- order(row, decreasing = FALSE)[seq_len(top)]
    }
    paste(col_names[idx], collapse = ",")
  })

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
