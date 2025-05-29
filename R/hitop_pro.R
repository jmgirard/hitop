#' Score the Full HiTOP-PRO Instrument
#'
#' Create a data frame with scores on all the HiTOP-PRO scales.
#'
#' @param data A data frame containing all HiTOP-PRO items (numerically coded).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 405 HiTOP-PRO items in order. If set to 
#'   `NULL` (the default), all non-`id` columns will be assumed to be the 
#'   `items` in order.
#' @param id An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to variables from `data` to keep in the output. If
#'   set to `NULL` (the default), no columns will be retained.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the HiTOP-PRO items, used for reverse-coding. 
#'   (default = `c(1, 4)`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`. (default = `FALSE`)
#' @return A data frame containing any `id` variables as well as all spectrum,
#'   subfactor, scale, and subscale scores.
#' @export
score_hitop_pro <- function(
  data,
  items = NULL,
  id = NULL,
  srange = c(1, 4),
  calc_se = FALSE,
  tibble = FALSE
) {

  ## Assertions
  validate_data(data)
  validate_items(items, n = 405)
  validate_id(id)
  validate_range(srange)
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(tibble))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(data)
    items <- items[!items %in% id]
  }
  data_items <- data[c(items, id)]

  ## Coerce values to numbers
  data_items[items] <- lapply(data_items[seq_along(items)], as.numeric)

  ## Reverse score the necessary items
  utils::data(hitop_pro_items)
  items_rev <- hitop_pro_items[hitop_pro_items$Reverse == TRUE, "PRO"]

  data_items[items_rev] <- lapply(
    items_rev,
    function(i) reverse(data_items[i], min = srange[[1]], max = srange[[2]])
  )

  ## Prepare output with id variables first
  out <- data[id]

  ## Find items per scale
  items_scales <- lapply(
    hitop_pro_scales$Scale,
    function(x) drop_na(hitop_pro_items[hitop_pro_items$Scale == x, "PRO"])
  )
  names(items_scales) <- hitop_pro_scales$camelCase

  ## Calculate mean scores per scale
  means_scales <-
    bind_columns(
      lapply(items_scales, function(x) rowMeans(data_items[x], na.rm = TRUE))
    )
  
  ## Append mean scores to output
  out <- cbind(out, means_scales)

  ## Add standard errors to output if requested
  if (calc_se) {
    sems_scales <-
      bind_columns(
        lapply(
          items_scales,
          function(x) apply(data_items[x], MARGIN = 1, FUN = calc_sem)
        )
      )
    colnames(sems_scales) <- paste0(colnames(sems_scales), "_se")
    out <- cbind(out, sems_scales)
  }

  ## Coerce output to tibble if requested
  if (tibble == TRUE) {
    rlang::check_installed("tibble")
    out <- tibble::as_tibble(out)
  }

  ## Return output
  out
}