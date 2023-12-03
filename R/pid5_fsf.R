#' Score the PID-5-FSF Domain and Facet Scales
#'
#' Create a data frame with scores on the PID-5 Faceted Short Form domain and
#' facet scales.
#'
#' @param .data A data frame containing all PID-5 items (numerically scored).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 100 PID-5 items in order. If set to `NULL`
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
#' @references - Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W.
#'   L., Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon,
#'   K. E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
#'   personality disorder trait model can be measured with a reduced set of
#'   items: An item response theory investigation of the personality inventory
#'   for DSM-5. *Psychological Assessment, 27*(4), 1195-1210.
#'   \url{https://doi.org/10.1037/pas0000120}
score_pid5fsf <- function(.data,
                       items = NULL,
                       id = NULL,
                       scales = c("domains", "facets"),
                       range = c(0, 3),
                       tibble = FALSE) {

  ## Assertions
  validate_data(.data)
  validate_items(items, n = 100)
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

  ## Calculate facet scores
  items_facets <- list(
    f_anhedo = drop_na(pid_items[pid_items$Facet == "Anhedonia", "PID5FSF"]),
    f_anxiou = drop_na(pid_items[pid_items$Facet == "Anxiousness", "PID5FSF"]),
    f_attent = drop_na(pid_items[pid_items$Facet == "Attention Seeking", "PID5FSF"]),
    f_callou = drop_na(pid_items[pid_items$Facet == "Callousness", "PID5FSF"]),
    f_deceit = drop_na(pid_items[pid_items$Facet == "Deceitfulness", "PID5FSF"]),
    f_depres = drop_na(pid_items[pid_items$Facet == "Depressivity", "PID5FSF"]),
    f_distra = drop_na(pid_items[pid_items$Facet == "Distractibility", "PID5FSF"]),
    f_eccent = drop_na(pid_items[pid_items$Facet == "Eccentricity", "PID5FSF"]),
    f_emotio = drop_na(pid_items[pid_items$Facet == "Emotional Lability", "PID5FSF"]),
    f_grandi = drop_na(pid_items[pid_items$Facet == "Grandiosity", "PID5FSF"]),
    f_hostil = drop_na(pid_items[pid_items$Facet == "Hostility", "PID5FSF"]),
    f_impuls = drop_na(pid_items[pid_items$Facet == "Impulsivity", "PID5FSF"]),
    f_intima = drop_na(pid_items[pid_items$Facet == "Intimacy Avoidance", "PID5FSF"]),
    f_irresp = drop_na(pid_items[pid_items$Facet == "Irresponsibility", "PID5FSF"]),
    f_manipu = drop_na(pid_items[pid_items$Facet == "Manipulativeness", "PID5FSF"]),
    f_percep = drop_na(pid_items[pid_items$Facet == "Perceptual Dysregulation", "PID5FSF"]),
    f_persev = drop_na(pid_items[pid_items$Facet == "Perseveration", "PID5FSF"]),
    f_restri = drop_na(pid_items[pid_items$Facet == "Restricted Affectivity", "PID5FSF"]),
    f_rigidp = drop_na(pid_items[pid_items$Facet == "Rigid Perfectionism", "PID5FSF"]),
    f_riskta = drop_na(pid_items[pid_items$Facet == "Risk Taking", "PID5FSF"]),
    f_separa = drop_na(pid_items[pid_items$Facet == "Separation Insecurity", "PID5FSF"]),
    f_submis = drop_na(pid_items[pid_items$Facet == "Submissiveness", "PID5FSF"]),
    f_suspis = drop_na(pid_items[pid_items$Facet == "Suspiciousness", "PID5FSF"]),
    f_unusua = drop_na(pid_items[pid_items$Facet == "Unusual Beliefs & Experiences", "PID5FSF"]),
    f_withdr = drop_na(pid_items[pid_items$Facet == "Withdrawal", "PID5FSF"])
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


#' Score the PID-5-FSF Validity Scales
#'
#' Create a data frame with scores on the PID-5 Faceted Short Form validity
#' scales.
#'
#' @param .data A data frame containing all PID-5 items (numerically scored).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 100 PID-5 items in order. If set to `NULL`
#'   (the default), all non-`id` columns will be assumed to be the `items` in
#'   order.
#' @param id An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to variables from `.data` to keep in the output. If
#'   set to `NULL` (the default), no columns will be retained.
#' @param scales An optional character vector indicating whether to calculate
#'   the percent of missing items (`"PNA"`), response inconsistency scale
#'   (`"RIS"`) and/or the over-reporting scale (`"ORS"`). Cut scores are
#'   unvalidated for the PID-5-FSF but the equivalents to the PID-5 versions
#'   would be RIS >= 0.3 and ORS >= 0.2.
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
validity_pid5fsf <- function(.data,
                          items = NULL,
                          id = NULL,
                          scales = c("PNA", "RIS", "ORS"),
                          range = c(0, 3),
                          tibble = FALSE) {

  # Assertions
  validate_data(.data)
  validate_items(items, n = 100)
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
    ris_items <- pid_items[!is.na(pid_items$RIS), c("PID5FSF", "RIS")]
    ris_items <- ris_items[order(ris_items$RIS), , drop = FALSE]
    ris_items$VAR <- rep(1:2, times = length(ris_items) / 2)

    ris_items <- stats::reshape(
      ris_items,
      v.names = "PID5FSF",
      timevar = "VAR",
      idvar = "RIS",
      direction = "wide"
    )
    ris_items <- stats::na.omit(ris_items)

    ris_df <-
      rowMeans(
        bind_columns(
          lapply(1:nrow(ris_items), \(x) adiff(data_items, ris_items, x))
        ),
        na.rm = TRUE
      ) / diff(range)

    ris_df[is.nan(ris_df)] <- NA_real_

    out <- cbind(out, v_ris = ris_df)
  }

  # Over-Reporting Scale
  if ("ORS" %in% scales) {
    ors_items <- drop_na(pid_items[!is.na(pid_items$ORS), "PID5FSF"])
    ors_df <- rowMeans(data_items[, ors_items] == range[[2]], na.rm = TRUE)
    ors_df[is.nan(ors_df)] <- NA_real_
    out <- cbind(out, v_ors = ors_df)
  }

  if (tibble == TRUE) {
    rlang::check_installed("tibble")
    out <- tibble::as_tibble(out)
  }

  out
}
