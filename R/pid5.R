#' Score the Full PID-5 Domain and Facet Scales
#'
#' Create a data frame with scores on the full PID-5 domain and facet scales.
#'
#' @param .data A data frame containing all PID-5 items (numerically scored).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 220 PID-5 items in order. If set to `NULL`
#'   (the default), all non-`id` columns will be assumed to be the `items` in
#'   order.
#' @param id An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to variables from `.data` to keep in the output. If
#'   set to `NULL` (the default), no columns will be retained.
#' @param scales An optional character vector indicating whether to calculate
#'   domain scores, facets scores, or both. Matching allows users to specify
#'   partial arguments such as "d" or "f" (default is both).
#' @param range An optional numeric vector specifying the minimum and maximum
#'   values of the PID-5 items, used for reverse-coding. (default = `c(0, 3)`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`.
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores, calculated using the DSM-5 algorithm.
#' @export
#' @references - Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2012). Initial construction of a maladaptive personality
#'   trait model and inventory for DSM-5. *Psychological Medicine, 42*,
#'   1879-1890. \url{https://doi.org/10.1017/s0033291711002674}
score_pid5 <- function(.data,
                       items = NULL,
                       id = NULL,
                       scales = c("domains", "facets"),
                       range = c(0, 3),
                       tibble = FALSE) {

  ## Assertions
  validate_data(.data)
  validate_items(items, n = 220)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  validate_range(range)
  stopifnot(rlang::is_logical(tibble, n = 1))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(.data)
    items <- items[!items %in% id]
  }
  data_items <- .data[, c(items, id)]

  ## Reverse score the necessary items
  utils::data(pid_items)
  items_rev <- pid_items[pid_items$Reverse == TRUE, "PID5"]

  for (i in items_rev) {
    data_items[, i] <- reverse(
      data_items[, i],
      min = range[[1]],
      max = range[[2]]
    )
  }

  ## Prepare output
  out <- .data[, id, drop = FALSE]

  ## Calculate facet scores
  items_facets <- list(
    f_anhedo = pid_items[pid_items$Facet == "Anhedonia", "PID5"],
    f_anxiou = pid_items[pid_items$Facet == "Anxiousness", "PID5"],
    f_attent = pid_items[pid_items$Facet == "Attention Seeking", "PID5"],
    f_callou = pid_items[pid_items$Facet == "Callousness", "PID5"],
    f_deceit = pid_items[pid_items$Facet == "Deceitfulness", "PID5"],
    f_depres = pid_items[pid_items$Facet == "Depressivity", "PID5"],
    f_distra = pid_items[pid_items$Facet == "Distractibility", "PID5"],
    f_eccent = pid_items[pid_items$Facet == "Eccentricity", "PID5"],
    f_emotio = pid_items[pid_items$Facet == "Emotional Lability", "PID5"],
    f_grandi = pid_items[pid_items$Facet == "Grandiosity", "PID5"],
    f_hostil = pid_items[pid_items$Facet == "Hostility", "PID5"],
    f_impuls = pid_items[pid_items$Facet == "Impulsivity", "PID5"],
    f_intima = pid_items[pid_items$Facet == "Intimacy Avoidance", "PID5"],
    f_irresp = pid_items[pid_items$Facet == "Irresponsibility", "PID5"],
    f_manipu = pid_items[pid_items$Facet == "Manipulativeness", "PID5"],
    f_percep = pid_items[pid_items$Facet == "Perceptual Dysregulation", "PID5"],
    f_persev = pid_items[pid_items$Facet == "Perseveration", "PID5"],
    f_restri = pid_items[pid_items$Facet == "Restricted Affectivity", "PID5"],
    f_rigidp = pid_items[pid_items$Facet == "Rigid Perfectionism", "PID5"],
    f_riskta = pid_items[pid_items$Facet == "Risk Taking", "PID5"],
    f_separa = pid_items[pid_items$Facet == "Separation Insecurity", "PID5"],
    f_submis = pid_items[pid_items$Facet == "Submissiveness", "PID5"],
    f_suspis = pid_items[pid_items$Facet == "Suspiciousness", "PID5"],
    f_unusua = pid_items[pid_items$Facet == "Unusual Beliefs & Experiences", "PID5"],
    f_withdr = pid_items[pid_items$Facet == "Withdrawal", "PID5"]
  )

  means_facets <-
    bind_columns(
      lapply(
        items_facets,
        \(x) rowMeans(data_items[, x], na.rm = TRUE)
      )
    )

  ## Calculate domain scores
  means_domains <- data.frame(
    d_negati = rowMeans(
      means_facets[, c("f_emotio", "f_anxiou", "f_separa")],
      na.rm = TRUE
    ),
    d_detatc = rowMeans(
      means_facets[, c("f_withdr", "f_anhedo", "f_intima")],
      na.rm = TRUE
    ),
    d_antago = rowMeans(
      means_facets[, c("f_manipu", "f_deceit", "f_grandi")],
      na.rm = TRUE
    ),
    d_disinh = rowMeans(
      means_facets[, c("f_irresp", "f_impuls", "f_distra")],
      na.rm = TRUE
    ),
    d_psycho = rowMeans(
      means_facets[, c("f_unusua", "f_eccent", "f_percep")],
      na.rm = TRUE
    )
  )

  ## Update output
  if ("domains" %in% scales) {
    out <- cbind(out, means_domains)
  }

  if ("facets" %in% scales) {
    out <- cbind(out, means_facets)
  }

  if (tibble == TRUE) {
    rlang::check_installed("tibble")
    out <- tibble::as_tibble(out)
  }

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
#' @param scales An optional character vector indicating whether to calculate
#'   the percent missing items (`"PNA"`), response inconsistency scale (`"RIS"`)
#'   and/or the over-reporting scale (`"ORS"`). A score of 0.3 or higher on the
#'   RIS is indicative of inconsistent responding and a score of 0.2 or higher
#'   on the ORS is indicative of overreporting.
#' @param range An optional numeric vector specifying the minimum and maximum
#'   values of the PID-5 items, used for reverse-coding. (default = `c(0, 3)`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`.
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores, calculated as means after removing missing values.
#' @references - Keeley, J. W., Webb, C., Peterson, D., Roussin, L., & Flanagan,
#'   E. H. (2016). Development of a Response Inconsistency Scale for the
#'   Personality Inventory for DSM-5. *Journal of Personality Assessment,
#'   98*(4), 351-359. \url{https://doi.org/10.1080/00223891.2016.1158719}
#' @references - Sellbom, M., Dhillon, S., & Bagby, R. M. (2018). Development
#'   and validation of an overreporting scale for the Personality Inventory for
#'   DSM-5 (PID-5). *Psychological Assessment, 30*(5), 582-593.
#'   \url{https://doi.org/10.1037/pas0000507}
#' @export
validity_pid5 <- function(.data,
                          items = NULL,
                          id = NULL,
                          scales = c("PNA", "RIS", "ORS"),
                          range = c(0, 3),
                          tibble = FALSE) {

  # Assertions
  validate_data(.data)
  validate_items(items, n = 220)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  validate_range(range)
  stopifnot(rlang::is_logical(tibble, n = 1))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(.data)
    items <- items[!items %in% id]
  }
  data_items <- .data[, c(items, id)]

  ## Prepare output
  out <- .data[, id, drop = FALSE]
  utils::data(pid_items)

  ## Percent Missing Items
  if ("PNA" %in% scales) {
    pna_df <- rowMeans(is.na(data_items))
    out <- cbind(out, v_pna = pna_df)
  }

  ## Response Inconsistency Scale
  if ("RIS" %in% scales) {
    ris_items <- pid_items[!is.na(pid_items$RIS), c("PID5", "RIS")]
    ris_items <- ris_items[order(ris_items$RIS), , drop = FALSE]
    ris_items$VAR <- rep(1:2, times = length(ris_items) / 2)

    ris_items <- stats::reshape(
      ris_items,
      v.names = "PID5",
      timevar = "VAR",
      idvar = "RIS",
      direction = "wide"
    )

    ris_df <-
      rowMeans(
        bind_columns(
          lapply(1:nrow(ris_items), \(x) adiff(data_items, ris_items, x))
        )
      ) / diff(range)

    ris_df[is.nan(ris_df)] <- NA_real_

    out <- cbind(out, v_ris = ris_df)
  }

  # Over-Reporting Scale
  if ("ORS" %in% scales) {
    ors_items <- pid_items[!is.na(pid_items$ORS), "PID5"]
    ors_df <- rowMeans(data_items[, ors_items] == range[[2]])
    ors_df[is.nan(ors_df)] <- NA_real_
    out <- cbind(out, v_ors = ors_df)
  }

  if (tibble == TRUE) {
    rlang::check_installed("tibble")
    out <- tibble::as_tibble(out)
  }

  out
}
