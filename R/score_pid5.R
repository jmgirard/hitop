#' Score the Personality Inventory for DSM-5
#'
#' Calculate scale scores on the Personality Inventory for DSM-5: full version
#' (PID-5, 220 items), faceted short form version (PID-5-FSF, 100 items), or
#' brief form version (PID-5-BF, 25 items) from item-level data.
#'
#' @param data A data frame containing all PID-5 items (numerically scored).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the PID items in order.
#' @param version A string indicating the version of the PID to score: "FULL",
#'   "FSF", or "BF". Will be automatically capitalized. (default = `"FULL"`)
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the items, used for reverse-coding. (default = `c(0, 3)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"pro_"`)
#' @param na.rm An optional logical indicating whether missing values should be
#'   ignored when calculating scale scores. (default = `TRUE`)
#' @param calc_se An optional logical indicating whether to calculate the
#'   standard error of each scale score. (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a \link[tibble]{tibble}. (default = `TRUE`)
#' @return A data frame containing all scale scores and standard errors (if
#'   requested) and all original `data` columns (if requested)
#' @export
#' @references Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2012). Initial construction of a maladaptive personality
#'   trait model and inventory for DSM-5. *Psychological Medicine, 42*,
#'   1879-1890. \doi{10.1017/s0033291711002674}
#' @references Anderson, J. L., Sellbom, M., & Salekin, R. T. (2016). Utility of
#'   the Personality Inventory for DSM-5-Brief Form (PID-5-BF) in the
#'   measurement of maladaptive personality and psychopathology. *Assessment,
#'   25*(5), 596–607. \doi{10.1177/1073191116676889}
#' @references Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W. L.,
#'   Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon, K.
#'   E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
#'   personality disorder trait model can be measured with a reduced set of
#'   items: An item response theory investigation of the personality inventory
#'   for DSM-5. *Psychological Assessment, 27*(4), 1195–1210.
#'   \doi{10.1037/pas0000120}
score_pid5 <- function(
    data,
    items,
    version = c("FULL", "FSF", "BF"),
    srange = c(0, 3),
    prefix = "pid_",
    na.rm = TRUE,
    calc_se = FALSE,
    append = TRUE,
    tibble = TRUE
) {

  ## Assertions
  validate_data(data)
  version <- toupper(version)
  version <- match.arg(version)
  n_items <- switch(
    version,
    "FULL" = 220,
    "FSF"  = 100,
    "BF"   = 25,
    stop("Invalid version string")
  )
  validate_items(items, n = n_items)
  # TODO: validate that the requested items are possible to subset to
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
  utils::data(pid_items)
  items_rev <- drop_na(
    pid_items[pid_items$Reverse == TRUE, version, drop = TRUE]
  )
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
  items_scales <- pid_scales[[version]]$itemNumbers

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
    colnames(sems_scales) <- paste0(colnames(sems_scales), "_se")
    out <- cbind(out, sems_scales)
  }

  ## Append output to input tibble if requested
  if (append == TRUE) out <- cbind(data, out)

  ## Coerce output to tibble if requested
  if (tibble == TRUE) out <- tibble::as_tibble(out)

  ## Return output
  out
}
