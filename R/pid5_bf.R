#' Score the PID-5-BF Domain Scales
#'
#' Create a data frame with scores on the PID-5 Brief Form domain scales.
#'
#' @param data A data frame containing all PID-5-BF items (numerically scored).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 25 PID-5-BF items in order. If set to `NULL`
#'   (the default), all non-`id` columns will be assumed to be the `items` in
#'   order.
#' @param id An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to variables from `data` to keep in the output. If
#'   set to `NULL` (the default), no columns will be retained.
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`.
#' @return A data frame containing any `id` variables as well all domain scores,
#'   calculated using the DSM-5 algorithm and their SEs if requested.
#' @export
#' @references Anderson, J. L., Sellbom, M., & Salekin, R. T. (2016). Utility of
#'   the Personality Inventory for DSM-5-Brief Form (PID-5-BF) in the
#'   measurement of maladaptive personality and psychopathology. *Assessment,
#'   25*(5), 596–607. \url{https://doi.org/10.1177/1073191116676889}
score_pid5bf <- function(
  data,
  items,
  prefix = "pid_",
  na.rm = TRUE,
  calc_se = FALSE,
  append = TRUE,
  tibble = TRUE
) {

  ## Assertions
  validate_data(data)
  validate_items(items, n = 25)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(na.rm))
  stopifnot(rlang::is_bool(calc_se))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)
  data_items <- bind_columns(data_items)

  ## Find items per scale
  items_scales <- pid5bf_scales$itemNumbers

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
