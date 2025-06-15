#' Score the B-HiTOP Instrument
#'
#' Create a data frame with scores on all the B-HiTOP scales.
#'
#' @param data A data frame containing all B-HiTOP items (numerically coded).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 45 B-HiTOP items in order.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the B-HiTOP items, used for reverse-coding. (default = `c(1, 4)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"bhitop_"`)
#' @param na.rm An optional logical indicating whether missing values should be
#'   ignored when calculating scale scores. (default = `TRUE`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`. (default = `TRUE`)
#' @return A data frame containing all scale scores and standard errors (if
#'   requested) and all original `data` columns (if requested)
#' @export
score_bhitop <- function(
    data,
    items,
    srange = c(1, 4),
    prefix = "bhitop_",
    na.rm = TRUE,
    calc_se = FALSE,
    append = TRUE,
    tibble = TRUE
) {

  ## Assertions
  validate_data(data)
  validate_items(items, n = 45)
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(na.rm))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)

  ## Reverse score the necessary items
  utils::data(bhitop_items)
  items_rev <-
    bhitop_items[bhitop_items$Reverse == TRUE, "BHITOP", drop = TRUE]
  if (length(items_rev) > 0) {
    data_items[items_rev] <- lapply(
      items_rev,
      function(i) reverse(
        data_items[[i]],
        low = srange[[1]],
        high = srange[[2]]
      )
    )
  }
  data_items <- bind_columns(data_items)

  ## Find items per scale
  items_scales <- bhitop_scales$itemNumbers

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

  ## Append output to input tibble if requested
  if (append == TRUE) out <- cbind(data, out)

  ## Coerce output to tibble if requested
  if (tibble == TRUE) out <- tibble::as_tibble(out)

  ## Return output
  out
}
