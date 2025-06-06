#' Score the Personality Inventory for DSM-5 Validity Scales
#'
#' Calculate validity scale scores on the Personality Inventory for DSM-5: full
#' version (PID-5, 220 items), faceted short form version (PID-5-FSF, 100
#' items), or brief form version (PID-5-BF, 25 items) from item-level data and
#' return alerts when when observations meet criteria for invalidity.
#'
#' @inheritParams score_pid5
#' @return A \link[tibble]{tibble} containing all validity scores and all
#'   original `data` columns (if requested)
#' @details For the full PID-5, a score of 17 or higher on the INC is indicative
#'   of inconsistent responding. A score of 3 or higher on the ORS is indicative
#'   of overreporting. A score of 10 or lower on the PRD is indicative of
#'   positive impression management (underreporting rather than genuine
#'   responding) whereas a score of 21 or higher on the PRD is indicative of
#'   genuine responding (rather than positive impression management). A score of
#'   11 or lower on the SD-TD is indicative of social desirability (rather than
#'   defensiveness), whereas a score of 19 or higher on the SD-TD is indicative
#'   of defensiveness (rather than social desirability). For the PID-5-FSF,
#'   scores of 8 or more on the INC-S are indicative of inconsistent responding.
#'   Cut-scores for the ORS-S, PRD-S, and SD-TD-S have not yet been validated.
#' @references Keeley, J. W., Webb, C., Peterson, D., Roussin, L., & Flanagan,
#'   E. H. (2016). Development of a Response Inconsistency Scale for the
#'   Personality Inventory for DSM-5. *Journal of Personality Assessment,
#'   98*(4), 351-359. \doi{10.1080/00223891.2016.1158719}
#' @references Sellbom, M., Dhillon, S., & Bagby, R. M. (2018). Development and
#'   validation of an overreporting scale for the Personality Inventory for
#'   DSM-5 (PID-5). *Psychological Assessment, 30*(5), 582-593.
#'   \doi{10.1037/pas0000507}
#' @references Bagby, R. M., & Sellbom, M. (2018). The Validity and Clinical
#'   Utility of the Personality Inventory for DSM–5 Response Inconsistency
#'   Scale. *Journal of Personality Assessment, 100*(4), 398–405.
#'   \doi{10.1080/00223891.2017.1420659}
#' @references Williams, M. M., Rogers, R., Sharf, A. J., & Ross, C. A. (2019).
#'   Faking Good: An Investigation of Social Desirability and Defensiveness in
#'   an Inpatient Sample With Personality Disorder Traits.
#'   *Journal of Personality Assessment, 101*(3), 253–263.
#'   \doi{10.1080/00223891.2018.1455691}
#' @references Lowmaster, S. E., Hartman, M. J., Zimmermann, J., Baldock, Z. C.,
#'   & Kurtz, J. E. (2020). Further Validation of the Response Inconsistency
#'   Scale for the Personality Inventory for DSM-5. *Journal of Personality
#'   Assessment, 102*(6), 743–750. \doi{10.1080/00223891.2019.1674320}
#' @export
validity_pid5 <- function(
  data,
  items,
  version = c("FULL", "FSF", "BF"),
  srange = c(0, 3),
  prefix = "pid_",
  append = TRUE,
  tibble = TRUE
) {

  # Assertions
  validate_data(data)
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "FSF", "BF"))
  n_items <- switch(
    version,
    "FULL" = 220,
    "FSF"  = 100,
    "BF"   = 25,
    cli::cli_abort("Invalid `version` argument")
  )
  validate_items(items, n = n_items)
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(append))
  stopifnot(rlang::is_bool(tibble))

  ## Extract item columns
  data_items <- data[items]

  ## Coerce values to numbers
  data_items <- lapply(data_items, as.numeric)
  data_items <- bind_columns(data_items)

  ## For all versions
  ### Percent Missing Items
  pna_df <- rowMeans(is.na(data_items))
  out <- data.frame(pna_df)
  names(out) <- paste0(prefix, "PNA")

  ## For FULL and FSF versions
  if (version %in% c("FULL", "FSF")) {
    ### Response Inconsistency Scale (INC)
    inc_var <- ifelse(version == "FULL", "INC", "INCS")
    inc_items <- pid_items[!is.na(pid_items[[inc_var]]), c(version, inc_var)]
    inc_items <- inc_items[order(inc_items[[inc_var]]), , drop = FALSE]
    inc_items$VAR <- rep(1:2, times = nrow(inc_items) / 2)
    inc_items <- stats::reshape(
      as.data.frame(inc_items),
      v.names = version,
      timevar = "VAR",
      idvar = inc_var,
      direction = "wide"
    )
    inc_vec <-
      rowSums(
        bind_columns(
          lapply(
            1:nrow(inc_items),
            function(x) adiff(data_items, inc_items, x)
          )
        )
      )
    inc_col <- paste0(prefix, inc_var)
    inc_cut <- ifelse(version == "FULL", 17, 8)
    inc_warns <- sum(inc_vec >= inc_cut, na.rm = TRUE)
    inc_warns_p <- sprintf("%.1f%%", inc_warns / length(inc_vec) * 100)
    inc_nas <- sum(is.na(inc_vec))
    if (inc_warns > 0) {
      cli::cli_alert_warning('A total of {inc_warns} observations ({inc_warns_p}) met criteria for inconsistent responding on the {inc_var} ({inc_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, {inc_col} < {inc_cut})}')
    }
    out[[inc_col]] <- inc_vec

    ### Over-Reporting Scale (ORS)
    ors_var <- ifelse(version == "FULL", "ORS", "ORSS")
    ors_items <- pid_items[!is.na(pid_items[[ors_var]]), version, drop = TRUE]
    ors_vec <- rowSums(data_items[, ors_items] == srange[[2]])
    ors_col <- paste0(prefix, ors_var)
    ors_cut <- ifelse(version == "FULL", 3, NA_real_)
    ors_warns <- sum(ors_vec >= ors_cut, na.rm = TRUE)
    ors_warns_p <- sprintf("%.1f%%", ors_warns / length(ors_vec) * 100)
    ors_nas <- sum(is.na(ors_vec))
    if (ors_warns > 0) {
      cli::cli_alert_warning('A total of {ors_warns} observations ({ors_warns_p}) met criteria for overreporting on the {ors_var} ({ors_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, {ors_col} < {ors_cut})}')
    }
    out[[ors_col]] <- ors_vec

    ### Positive Impression Management Response Distortion Scale (PRD)
    prd_var <- ifelse(version == "FULL", "PRD", "PRDS")
    prd_items <- pid_items[!is.na(pid_items[[prd_var]]), version, drop = TRUE]
    prd_vec <- rowSums(data_items[, prd_items])
    prd_col <- paste0(prefix, prd_var)
    prd_cut <- ifelse(version == "FULL", 10, NA_real_)
    prd_warns <- sum(prd_vec <= prd_cut, na.rm = TRUE)
    prd_warns_p <- sprintf("%.1f%%", prd_warns / length(prd_vec) * 100)
    prd_nas <- sum(is.na(prd_vec))
    if (prd_warns > 0) {
      cli::cli_alert_warning('A total of {prd_warns} observations ({prd_warns_p}) met criteria for positive impression management ({prd_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, {prd_col} > {prd_cut})}')
    }
    out[[prd_col]] <- prd_vec

    ### Social Desirability Total Denial Scale (SD-TD)
    sdtd_var <- ifelse(version == "FULL", "SDTD", "SDTDS")
    sdtd_items <- pid_items[!is.na(pid_items[[sdtd_var]]), version, drop = TRUE]
    sdtd_vec <- rowSums(data_items[, sdtd_items])
    sdtd_col <- paste0(prefix, sdtd_var)
    sdtd_cut_low <- ifelse(version == "FULL", 11, NA_real_)
    sdtd_warns_low <- sum(sdtd_vec <= sdtd_cut_low, na.rm = TRUE)
    sdtd_warns_low_p <- sprintf("%.1f%%", sdtd_warns_low / length(prd_vec) * 100)
    sdtd_cut_high <- ifelse(version == "FULL", 19, NA_real_)
    sdtd_warns_high <- sum(sdtd_vec >= sdtd_cut_high, na.rm = TRUE)
    sdtd_warns_high_p <- sprintf("%.1f%%", sdtd_warns_high / length(prd_vec) * 100)
    sdtd_nas <- sum(is.na(sdtd_vec))
    if (sdtd_warns_low > 0) {
      cli::cli_alert_warning('A total of {sdtd_warns_low} observations ({sdtd_warns_low_p}) met criteria for social desirability ({sdtd_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, {sdtd_col} > {sdtd_cut_low})}')
    }
    if (sdtd_warns_high > 0) {
      cli::cli_alert_warning('A total of {sdtd_warns_high} observations ({sdtd_warns_high_p}) met criteria for defensiveness ({sdtd_nas} missing).')
      cli::cli_alert_info('Consider removing them with {.code dplyr::filter(df, {sdtd_col} < {sdtd_cut_high})}')
    }
    out[[sdtd_col]] <- sdtd_vec

    if (version == "FSF") {
      cli::cli_alert_warning('Cut scores for the ORS-S, PRD-S, and SDTD-S have not been developed.')
    }
  }

  ## Append output to input tibble if requested
  if (append == TRUE) out <- cbind(data, out)

  ## Coerce output to tibble if requested
  if (tibble == TRUE) out <- tibble::as_tibble(out)

  ## Return output
  out
}
