#' Score the Full PID-5 Domain and Facet Scales
#'
#' Create a data frame with scores on the full PID-5 domain and facet scales.
#'
#' @param data A data frame containing all PID-5 items (numerically scored).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 220 PID-5 items in order.
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
#'   converted to a `tibble::tibble()`. (default = `TRUE`)
#' @return A data frame containing all scale scores and standard errors (if
#'   requested) and all original `data` columns (if requested)
#' @export
#' @references - Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2012). Initial construction of a maladaptive personality
#'   trait model and inventory for DSM-5. *Psychological Medicine, 42*,
#'   1879-1890. \url{https://doi.org/10.1017/s0033291711002674}
score_pid5 <- function(
  data,
  items,
  srange = c(0, 3),
  prefix = "pid_",
  na.rm = TRUE,
  calc_se = FALSE,
  append = TRUE,
  tibble = TRUE
) {

  ## Assertions
  validate_data(data)
  validate_items(items, n = 220)
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
  items_rev <- unlist(pid_items[pid_items$Reverse == TRUE, "PID5"])
  data_items[items_rev] <- lapply(
    items_rev,
    function(i) reverse(data_items[[i]], low = srange[[1]], high = srange[[2]])
  )
  data_items <- bind_columns(data_items)

  ## Find items per scale
  items_scales <- pid5_scales$itemNumbers

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


#' Score the Full PID-5 Validity Scales
#'
#' Create a data frame with scores on the full PID-5 validity scales.
#'
#' @param .data A data frame containing all PID-5 items (numerically scored).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 220 PID-5 items in order. If set to `NULL`
#'   (the default), all non-`id` columns will be assumed to be the `items` in
#'   order.
#' @param id An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to variables from `.data` to keep in the output. If
#'   set to `NULL` (the default), no columns will be retained.
#' @param scales An optional character vector indicating which of the following
#'   validity scales to calculate: percent missing items (`"PNA"`), response
#'   inconsistency scale (`"INC"`), overreporting scale (`"ORS"`), positive
#'   impression management response distortion scale (`"PRD"`), and social
#'   desirability-total denial scale (`"SDTD"`). See details below for
#'   interpretation guidance.
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the PID-5 items, used for reverse-coding. (default = `c(0, 3)`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`.
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores.
#' @details A score of 17 or higher on the INC is indicative of inconsistent
#'   responding. A score of 3 or higher on the ORS is indicative of
#'   overreporting. A score of 10 or lower on the PRD is indicative of positive
#'   impression management (underreporting rather than genuine responding)
#'   whereas a score of 21 or higher on the PRD is indicative of genuine
#'   responding (rather than positive impression management). A score of 11 or
#'   lower on the SDTD is indicative of social desirability (rather than
#'   defensiveness), whereas a score of 19 or higher on the SDTD is indicative
#'   of defensiveness (rather than social desirability).
#'
#' @references - Keeley, J. W., Webb, C., Peterson, D., Roussin, L., & Flanagan,
#'   E. H. (2016). Development of a Response Inconsistency Scale for the
#'   Personality Inventory for DSM-5. *Journal of Personality Assessment,
#'   98*(4), 351-359. \url{https://doi.org/10.1080/00223891.2016.1158719}
#' @references - Sellbom, M., Dhillon, S., & Bagby, R. M. (2018). Development
#'   and validation of an overreporting scale for the Personality Inventory for
#'   DSM-5 (PID-5). *Psychological Assessment, 30*(5), 582-593.
#'   \url{https://doi.org/10.1037/pas0000507}
#' @references - Bagby, R. M., & Sellbom, M. (2018). The Validity and Clinical
#'   Utility of the Personality Inventory for DSM–5 Response Inconsistency
#'   Scale. *Journal of Personality Assessment, 100*(4), 398–405.
#'   \url{https://doi.org/10.1080/00223891.2017.1420659}
#' @references - Williams, M. M., Rogers, R., Sharf, A. J., & Ross, C. A.
#'   (2019). Faking Good: An Investigation of Social Desirability and
#'   Defensiveness in an Inpatient Sample With Personality Disorder Traits.
#'   *Journal of Personality Assessment, 101*(3), 253–263.
#'   \url{https://doi.org/10.1080/00223891.2018.1455691}
#' @export
validity_pid5 <- function(data,
                          items = NULL,
                          id = NULL,
                          scales = c("PNA", "INC", "ORS", "PRD", "SDTD"),
                          srange = c(0, 3),
                          tibble = FALSE) {

  # Assertions
  validate_data(data)
  validate_items(items, n = 220)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  validate_range(srange)
  stopifnot(rlang::is_logical(tibble, n = 1))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(data)
    items <- items[!items %in% id]
  }
  data_items <- data[, c(items, id)]

  ## Prepare output
  out <- data[, id, drop = FALSE]
  utils::data(pid_items)

  ## Percent Missing Items
  if ("PNA" %in% scales) {
    pna_df <- rowMeans(is.na(data_items))
    out <- cbind(out, v_pna = pna_df)
  }

  ## Response Inconsistency Scale
  if ("INC" %in% scales) {
    inc_items <- pid_items[!is.na(pid_items$INC), c("PID5", "INC")]
    inc_items <- inc_items[order(inc_items$INC), , drop = FALSE]
    inc_items$VAR <- rep(1:2, times = length(inc_items) / 2)

    inc_items <- stats::reshape(
      inc_items,
      v.names = "PID5",
      timevar = "VAR",
      idvar = "INC",
      direction = "wide"
    )

    inc_df <-
      rowSums(
        bind_columns(
          lapply(1:nrow(inc_items), \(x) adiff(data_items, inc_items, x))
        )
      )

    inc_warns <- sum(inc_df >= 17, na.rm = TRUE)
    inc_warns_p <- sprintf("%.1f%%", inc_warns / length(inc_df) * 100)
    inc_nas <- sum(is.na(inc_df))
    if (inc_warns > 0) {
      cli::cli_alert_warning('A total of {inc_warns} observations ({inc_warns_p}) met criteria for inconsistent responding ({inc_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, v_inc < 17)}')
    }

    out <- cbind(out, v_inc = inc_df)
  }

  # Over-Reporting Scale
  if ("ORS" %in% scales) {
    ors_items <- pid_items[!is.na(pid_items$ORS), "PID5"]
    ors_df <- rowSums(data_items[, ors_items] == srange[[2]])

    ors_warns <- sum(ors_df >= 3, na.rm = TRUE)
    ors_warns_p <- sprintf("%.1f%%", ors_warns / length(ors_df) * 100)
    ors_nas <- sum(is.na(ors_df))
    if (ors_warns > 0) {
      cli::cli_alert_warning('A total of {ors_warns} observations ({ors_warns_p}) met criteria for overreporting ({ors_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, v_ors < 3)}')
    }

    out <- cbind(out, v_ors = ors_df)
  }

  # Positive Impression Management (PIM) Response Distortion Scale
  if ("PRD" %in% scales) {
    prd_items <- pid_items[!is.na(pid_items$PRD), "PID5"]
    prd_df <- rowSums(data_items[, prd_items])

    prd_warns <- sum(prd_df < 11, na.rm = TRUE)
    prd_warns_p <- sprintf("%.1f%%", prd_warns / length(prd_df) * 100)
    prd_nas <- sum(is.na(prd_df))
    if (prd_warns > 0) {
      cli::cli_alert_warning('A total of {prd_warns} observations ({prd_warns_p}) met criteria for positive impression management ({prd_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, v_prd >= 11)}')
    }

    out <- cbind(out, v_prd = prd_df)
  }

  ## Social Desirability-Total Denial Scale
  if ("PRD" %in% scales) {
    sdtd_items <- pid_items[!is.na(pid_items$SDTD), "PID5"]
    sdtd_df <- rowSums(data_items[, sdtd_items])

    out <- cbind(out, v_sdtd = sdtd_df)
  }

  ## Append output to input tibble if requested
  if (append == TRUE) out <- cbind(data, out)

  ## Coerce output to tibble if requested
  if (tibble == TRUE) out <- tibble::as_tibble(out)

  ## Return output
  out
}
