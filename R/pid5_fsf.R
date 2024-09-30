#' Score the PID-5-FSF Domain and Facet Scales
#'
#' Create a data frame with scores on the PID-5 Faceted Short Form domain and
#' facet scales.
#'
#' @param .data A data frame containing all PID-5-FSF items (numerically
#'   scored).
#' @param items An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to the 100 PID-5-FSF items in order. If set to
#'   `NULL` (the default), all non-`id` columns will be assumed to be the
#'   `items` in order.
#' @param id An optional vector of column names (as strings) or numbers (as
#'   integers) corresponding to variables from `.data` to keep in the output. If
#'   set to `NULL` (the default), no columns will be retained.
#' @param scales An optional character vector indicating whether to calculate
#'   domain scores, facets scores, or both. Matching allows users to specify
#'   partial arguments such as "d" or "f" (default is both).
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`.
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores, calculated using the DSM-5 algorithm.
#' @export
#' @references Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W.
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
                       tibble = FALSE) {

  ## Assertions
  validate_data(.data)
  validate_items(items, n = 100)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  stopifnot(rlang::is_logical(tibble, n = 1))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(.data)
    items <- items[!items %in% id]
  }
  data_items <- .data[, c(items, id)]

  ## Coerce values to numbers
  data_items[items] <- lapply(data_items[items], as.numeric)

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
#' @param scales An optional character vector indicating which of the following
#'   validity scales to calculate: percent of missing items (`"PNA"`), response
#'   inconsistency scale short form (`"INCS"`), over-reporting scale short form
#'   (`"ORSS"`), positive impression management response distortion scale short
#'   form (`"PRDS"`), and social desirability-total denial short form
#'   (`"SDTDS"`). See details below for interpretation guidance
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the PID-5 items, used for reverse-coding. (default = `c(0, 3)`)
#' @param tibble An optional logical indicating whether the output should be
#'   converted to a `tibble::tibble()`.
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores.
#' @details Scores of 8 or more on the INC-S are indicative of inconsistent
#'   responding. Cut-scores for the ORS-S, PRD-S, and SD-TD-S have not yet been
#'   validated.
#' @references - Lowmaster, S. E., Hartman, M. J., Zimmermann, J., Baldock, Z.
#'   C., & Kurtz, J. E. (2020). Further Validation of the Response Inconsistency
#'   Scale for the Personality Inventory for DSM-5. *Journal of Personality
#'   Assessment, 102*(6), 743–750.
#'   \cite{https://doi.org/10.1080/00223891.2019.1674320}
#' @export
validity_pid5fsf <- function(.data,
                          items = NULL,
                          id = NULL,
                          scales = c("PNA", "INCS", "ORSS", "PRDS", "SDTDS"),
                          srange = c(0, 3),
                          tibble = FALSE) {

  # Assertions
  validate_data(.data)
  validate_items(items, n = 100)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  stopifnot(rlang::is_logical(tibble, n = 1))

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(.data)
    items <- items[!items %in% id]
  }
  data_items <- .data[, c(items, id)]

  ## Coerce values to numbers
  data_items[items] <- lapply(data_items[items], as.numeric)

  ## Prepare output
  out <- .data[, id, drop = FALSE]
  utils::data(pid_items)

  ## Percent Missing Items
  if ("PNA" %in% scales) {
    pna_df <- rowMeans(is.na(data_items))
    out <- cbind(out, v_pna = pna_df)
  }

  ## Response Inconsistency Scale
  if ("INCS" %in% scales) {
    inc_items <- pid_items[!is.na(pid_items$INC), c("PID5FSF", "INCS")]
    inc_items <- inc_items[order(inc_items$INC), , drop = FALSE]
    inc_items$VAR <- rep(1:2, times = length(inc_items) / 2)

    inc_items <- stats::reshape(
      inc_items,
      v.names = "PID5FSF",
      timevar = "VAR",
      idvar = "INCS",
      direction = "wide"
    )
    inc_items <- stats::na.omit(inc_items)

    inc_df <-
      rowSums(
        bind_columns(
          lapply(1:nrow(inc_items), \(x) adiff(data_items, inc_items, x))
        )
      )

    inc_warns <- sum(inc_df >= 8, na.rm = TRUE)
    inc_warns_p <- sprintf("%.1f%%", inc_warns / length(inc_df) * 100)
    inc_nas <- sum(is.na(inc_df))
    if (inc_warns > 0) {
      cli::cli_alert_warning('A total of {inc_warns} observations ({inc_warns_p}) met criteria for inconsistent responding ({inc_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, v_incs < 8)}')
    }

    out <- cbind(out, v_incs = inc_df)
  }

  # Over-Reporting Scale
  if ("ORSS" %in% scales) {
    ors_items <- drop_na(pid_items[!is.na(pid_items$ORS), "PID5FSF"])
    ors_df <- rowSums(data_items[, ors_items] == srange[[2]])

    # ors_warns <- sum(ors_df >= 3, na.rm = TRUE)
    # if (ors_warns > 0) {
    #   cli::cli_alert_warning('A total of {ors_warns} observations exceeded criteria for overreporting.')
    #   cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, v_orss < 3)}')
    # }

    out <- cbind(out, v_orss = ors_df)
  }

  # Positive Impression Management (PIM) Response Distortion Scale
  if ("PRDS" %in% scales) {
    prd_items <- drop_na(pid_items[!is.na(pid_items$PRD), "PID5FSF"])
    prd_df <- rowSums(data_items[, prd_items])

    # prd_warns <- sum(prd_df < 11, na.rm = TRUE)
    # prd_warns_p <- sprintf("%.1f%%", prd_warns / length(prd_df) * 100)
    # prd_nas <- sum(is.na(prd_df))
    # if (prd_warns > 0) {
    #   cli::cli_alert_warning('A total of {prd_warns} observations ({prd_warns_p}) met criteria for positive impression management ({prd_nas} missing).')
    #   cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, v_prd >= 11)}')
    # }

    out <- cbind(out, v_prds = prd_df)
  }

  # Social Desirability-Total Denial Scale
  if ("SDTDS" %in% scales) {
    sdtd_items <- drop_na(pid_items[!is.na(pid_items$SDTD), "PID5FSF"])
    sdtd_df <- rowSums(data_items[, sdtd_items])

    out <- cbind(out, v_sdtds = sdtd_df)
  }

  if (tibble == TRUE) {
    rlang::check_installed("tibble")
    out <- tibble::as_tibble(out)
  }

  out
}
