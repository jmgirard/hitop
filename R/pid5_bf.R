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
score_pid5bf <- function(data,
                         items = NULL,
                         id = NULL,
                         calc_se = FALSE,
                         tibble = FALSE) {

  ## Assertions
  validate_data(data)
  validate_items(items, n = 25)
  validate_id(id)
  stopifnot(rlang::is_bool(tibble))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(data)
    items <- items[!items %in% id]
  }
  data_items <- data[, c(items, id)]

  ## Coerce values to numbers
  data_items[seq_along(items)] <- 
    lapply(data_items[seq_along(items)], as.numeric)

  ## Prepare output
  out <- data[id]
  utils::data(pid_items)

  ## Calculate domain scores
  items_scales <- list(
    d_negati = drop_na(pid_items[pid_items$Domain == "Negative affectivity", "PID5BF"]),
    d_detatc = drop_na(pid_items[pid_items$Domain == "Detachment", "PID5BF"]),
    d_antago = drop_na(pid_items[pid_items$Domain == "Antagonism", "PID5BF"]),
    d_disinh = drop_na(pid_items[pid_items$Domain == "Disinhibition", "PID5BF"]),
    d_psycho = drop_na(pid_items[pid_items$Domain == "Psychoticism", "PID5BF"])
  )

  means_scales <-
    bind_columns(
      lapply(
        items_scales,
        function(x) rowMeans(data_items[, x], na.rm = TRUE)
      )
    )

  out <- cbind(out, means_scales)

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

  if (tibble == TRUE) {
    rlang::check_installed("tibble")
    out <- tibble::as_tibble(out)
  }

  ## Return output
  out
}
