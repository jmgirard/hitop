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
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores, calculated using the DSM-5 algorithm.
#' @export
#' @references - Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2012). Initial construction of a maladaptive personality
#'   trait model and inventory for DSM-5. *Psychological Medicine, 42*, 1879-1890.
#'   \url{https://doi.org/10.1017/s0033291711002674}
score_pid5 <- function(.data,
                       items = NULL,
                       id = NULL,
                       scales = c("domains", "facets"),
                       range = c(0, 3)) {

  ## Assertions
  validate_data(.data)
  validate_items(items)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  validate_range(range)

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(.data)
    items <- items[!items %in% id]
  }
  data_items <- .data[, c(items, id)]

  ## Reverse score the necessary items
  items_rev <- c(7, 30, 35, 58, 87, 90, 96, 97, 98,
                 131, 142, 155, 164, 177, 210, 215)

  for (i in items_rev) {
    data_items[, i] <- reverse(
      data_items[, i],
      min = range[[1]],
      max = range[[2]]
    )
  }

  ## Prepare output
  out <- .data[, id]

  ## Calculate facet scores
  items_facets <- list(
    f_anhedo = c(1, 23, 26, 30, 124, 155, 157, 189),
    f_anxiou = c(79, 93, 95, 96, 109, 110, 130, 141, 174),
    f_attent = c(14, 43, 74, 111, 113, 173, 191, 211),
    f_callou = c(11, 13, 19, 54, 72, 73, 90, 153, 166, 183, 198, 200, 207, 208),
    f_deceit = c(41, 53, 56, 76, 126, 134, 142, 206, 214, 218),
    f_depres = c(27, 61, 66, 81, 86, 104, 119, 148, 151, 163, 168, 169, 178, 212),
    f_distra = c(6, 29, 47, 68, 88, 118, 132, 144, 199),
    f_eccent = c(5, 21, 24, 25, 33, 52, 55, 70, 71, 152, 172, 185, 205),
    f_emotio = c(18, 62, 102, 122, 138, 165, 181),
    f_grandi = c(40, 65, 114, 179, 187, 197),
    f_hostil = c(28, 32, 38, 85, 92, 116, 158, 170, 188, 216),
    f_impuls = c(4, 16, 17, 22, 58, 204),
    f_intima = c(89, 97, 108, 120, 145, 203),
    f_irresp = c(31, 129, 156, 160, 171, 201, 210),
    f_manipu = c(107, 125, 162, 180, 219),
    f_percep = c(36, 37, 42, 44, 59, 77, 83, 154, 192, 193, 213, 217),
    f_persev = c(46, 51, 60, 78, 80, 100, 121, 128, 137),
    f_restri = c(8, 45, 84, 91, 101, 167, 184),
    f_rigidp = c(34, 49, 105, 115, 123, 135, 140, 176, 196, 220),
    f_riskta = c(3, 7, 35, 39, 48, 67, 69, 87, 98, 112, 159, 164, 195, 215),
    f_separa = c(12, 50, 57, 64, 127, 149, 175),
    f_submis = c(9, 15, 63, 202),
    f_suspis = c(2, 103, 117, 131, 133, 177, 190),
    f_unusua = c(94, 99, 106, 139, 143, 150, 194, 209),
    f_withdr = c(10, 20, 75, 82, 136, 146, 147, 161, 182, 186)
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
#'   the response inconsistency scale (`"RIS"`) and/or the over-reporting scale
#'   (`"ORS"`). A score of 18 or higher on the RIS is indicative of inconsistent
#'   responding and a score of 3 or higher on the ORS is indicative of
#'   overreporting.
#' @param range An optional numeric vector specifying the minimum and maximum
#'   values of the PID-5 items, used for reverse-coding. (default = `c(0, 3)`)
#' @return A data frame containing any `id` variables as well any requested
#'   `scale` scores, calculated using the DSM-5 algorithm.
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
                          scales = c("RIS", "ORS"),
                          range = c(0, 3)) {

  # Assertions
  validate_data(.data)
  validate_items(items)
  validate_id(id)
  scales <- match.arg(scales, several.ok = TRUE)
  validate_range(range)

  ## Select items and id variables
  if (is.null(items)) {
    items <- colnames(.data)
    items <- items[!items %in% id]
  }
  data_items <- .data[, c(items, id)]

  ## Prepare output
  out <- .data[, id]

  ## Response Inconsistency Scale
  if ("RIS" %in% scales) {
    ris_df <-
      rowSums(
        data.frame(
          v1 =  adiff(data_items, 79, 174),
          v2 =  adiff(data_items, 109, 110),
          v3 =  adiff(data_items, 169, 148),
          v4 =  adiff(data_items, 102, 122),
          v5 =  adiff(data_items, 138, 181),
          v6 =  adiff(data_items, 38, 92),
          v7 =  adiff(data_items, 80, 128),
          v8 =  adiff(data_items, 105, 123),
          v9 =  adiff(data_items, 50, 127),
          v10 = adiff(data_items, 74, 173),
          v11 = adiff(data_items, 191, 211),
          v12 = adiff(data_items, 153, 166),
          v13 = adiff(data_items, 125, 180),
          v14 = adiff(data_items, 89, 145),
          v15 = adiff(data_items, 132, 144),
          v16 = adiff(data_items, 21, 55),
          v17 = adiff(data_items, 24, 25),
          v18 = adiff(data_items, 52, 152),
          v19 = adiff(data_items, 70, 71),
          v20 = adiff(data_items, 172, 185)
        )
      )
    out <- cbind(out, v_ris = ris_df)
  }

  # Over-reporting Scale
  if ("ORS" %in% scales) {
    ors_df <-
      rowSums(
        data.frame(
          v1 = highest(data_items, 2, range),
          v2 = highest(data_items, 8, range),
          v3 = highest(data_items, 39, range),
          v4 = highest(data_items, 40, range),
          v5 = highest(data_items, 44, range),
          v6 = highest(data_items, 150, range),
          v7 = highest(data_items, 166, range),
          v8 = highest(data_items, 170, range),
          v9 = highest(data_items, 171, range),
          v10 = highest(data_items, 178, range)
        )
      )
    out <- cbind(out, v_ors = ors_df)
  }

  out
}
