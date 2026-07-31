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
#' @param prefix A length-1 string giving a leading string to strip from each
#'   selected column name before concatenation, or \code{NULL} to keep names as
#'   is. The match is literal, not a regular expression: a column name that does
#'   not begin with exactly \code{prefix} is left unchanged, and characters that
#'   are regex metacharacters (\code{.}, \code{(}, \code{+}) stand for
#'   themselves.
#' @param top Integer (length 1). The number of columns to include per row after
#'   ranking. Must be between 1 and \code{length(scales)}.
#' @param dir Direction of ranking: \code{"high"} for largest values first or
#'   \code{"low"} for smallest values first.
#' @param reverse A subset of \code{scales} (names or positions) identifying
#'   scales that are keyed in the opposite direction to the rest (higher =
#'   healthier, e.g. a well-being scale). Before ranking, each such column is
#'   reflected via \code{sum(srange) - value} so that all selected scales share a
#'   common "higher = more elevated" metric and \code{dir} applies uniformly. Set
#'   to \code{NULL} (the default) when every scale is keyed the same way.
#' @param srange A numeric vector of length 2 giving the minimum and maximum of
#'   the scale scores, used to reflect the \code{reverse} columns. Required when
#'   \code{reverse} is not \code{NULL}; ignored otherwise. (default = \code{NULL})
#' @param name A length-1 string giving the name of the output column holding the
#'   ranked-scale strings. (default = \code{"top_scales"})
#' @param append Logical. If \code{TRUE}, bind the result as a new column to
#'   \code{data}. If \code{FALSE}, return only the result column.
#'
#' @details Ranking is performed row-wise using \code{order()} on the selected
#'   columns. Ties are resolved by the original column order (the default
#'   behavior of \code{order}), which is alphabetical if using the package's
#'   scoring functions. Missing values are placed last by \code{order()} and
#'   will be included only if there are fewer than \code{top} non-missing values
#'   in a row.
#'
#' If \code{prefix} is not \code{NULL}, the function removes that exact leading
#' string from each selected column name before concatenation. Names that do not
#' begin with it are carried through whole.
#'
#' The output column is named by \code{name}. When \code{append = TRUE} it is
#' added after the existing columns of \code{data} (whose order is preserved);
#' when \code{append = FALSE} the result is a one-column tibble.
#'
#' @return A \link[tibble]{tibble}. If \code{append = TRUE}, it is \code{data}
#'   with an added character column named by \code{name}. If \code{append =
#'   FALSE}, it is a one-column tibble (named by \code{name}) whose values are
#'   the comma-separated ranked scale names, one per row of \code{data}.
#'
#' @examples
#' # List each respondent's 3 highest-scoring HiTOP-BR scales
#' scored <- score_hitopbr(sim_hitopbr, items = 1:45, append = FALSE)
#' rank_scales(scored, scales = names(scored), prefix = "hbr_", top = 3,
#'             append = FALSE)
#'
#' @export
rank_scales <- function(
  data,
  scales,
  prefix = NULL,
  top = 5,
  dir = "high",
  reverse = NULL,
  srange = NULL,
  name = "top_scales",
  append = TRUE
) {
  ## Validate args
  validate_data(data)
  validate_scales(scales)
  validate_string(prefix, arg = "prefix", allow_null = TRUE)
  ## `top` cannot ask for more scales than were supplied, so its upper bound is
  ## the length of `scales` rather than a constant.
  validate_count(top, arg = "top", max = length(scales))
  ## arg_match() lists the permitted values and suggests the near miss on a
  ## typo. Unlike match.arg() it requires an exact match, so no abbreviation
  ## becomes newly acceptable. It does insist on a character vector, where the
  ## membership test it replaces was satisfied by a factor
  ## (`factor("high") %in% c("high", "low")` is TRUE) -- a `dir` read from a
  ## factor column of a config table used to work, so it is coerced here rather
  ## than rejected. Accepted input is therefore unchanged; only the error is.
  if (is.factor(dir)) {
    dir <- as.character(dir)
  }
  dir <- rlang::arg_match(dir, values = c("high", "low"))
  validate_string(name, arg = "name")
  validate_flag(append, arg = "append")

  ## Extract scale columns
  data_scales <- data[scales]

  ## Reflect any reverse-directioned scales so all columns share a common
  ## "higher = more elevated" metric before ranking.
  if (!is.null(reverse)) {
    cli_assert(
      condition = all(reverse %in% scales),
      message = "The `reverse` scales must all be in `scales`."
    )
    cli_assert(
      condition = !is.null(srange),
      message = "`srange` must be supplied when `reverse` is set."
    )
    validate_range(srange)
    rev_idx <- which(scales %in% reverse)
    data_scales[rev_idx] <- lapply(
      data_scales[rev_idx],
      function(v) sum(srange) - v
    )
  }

  col_names <- colnames(data_scales)
  if (!is.null(prefix)) {
    col_names <- strip_prefix(col_names, prefix)
  }

  ## Find the top scales per subject
  ranked <- apply(data_scales, MARGIN = 1, function(row) {
    idx <- order(row, decreasing = (dir == "high"))[seq_len(top)]
    paste(col_names[idx], collapse = ",")
  })

  ## Assemble the output column under its requested name
  out_col <- stats::setNames(
    data.frame(ranked, stringsAsFactors = FALSE),
    name
  )

  ## Append to the input if requested; always return a tibble
  if (append == TRUE) {
    out_col <- cbind(data, out_col)
  }
  tibble::as_tibble(out_col)
}
