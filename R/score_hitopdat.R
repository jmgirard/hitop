#' Score the HiTOP-DAT Battery
#'
#' Create a data frame with a raw score on each HiTOP-DAT scale but General
#' Depression, and the share of each scale's items that were answered.
#'
#' @param data A data frame containing all 382 HiTOP-DAT items, coded with the
#'   answer values in [hitopdat_choices].
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the 382 HiTOP-DAT items in battery order, the order of
#'   `hitopdat_items$Item`. The REDCap and Qualtrics files that
#'   [generate_redcap_hitopdat()] and [generate_qualtrics_hitopdat()] write
#'   name these columns `hdat_001` to `hdat_382`. Duplicated entries are an
#'   error. As in [score_hitopbr()], a warning is issued when the names share
#'   a common prefix and trailing number but those numbers are not ascending,
#'   and a column that cannot be read as numbers is an error of class
#'   `hitop_nonnumeric_items`.
#' @param prefix An optional string to add before each output column name. If
#'   no prefix is desired, set to an empty string `""`. (default = `"dat_"`)
#' @param min_complete The smallest share of a scale's items that must be
#'   answered for the scale to score: one number greater than 0 and at most 1.
#'   The answered share is compared with it within rounding error, so
#'   `min_complete = 0.1 * 7`, which is slightly above 0.7 in floating
#'   point, still scores a scale with 7 of its 10 items answered. The default is the battery
#'   authors' choice. Set it to `1` to score only
#'   fully answered scales. (default = `0.8`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#'
#' @details The HiTOP-DAT (HiTOP Digital Assessment and Tracker) is a battery
#'   of seven measures. This function scores 56 of the 57 scales in
#'   [hitopdat_scales]: the WHODAS total, 18 IDAS-II scales, the AUDIT total,
#'   the DUDIT total, CAPE positive symptoms, 33 CAT-PD facets and the PHQ-15
#'   total. It returns raw scores, not T scores.
#'
#'   **Scoring rules.** Each measure is scored on the metric of the source
#'   named in its entry:
#'
#'   - WHODAS: the sum of its 12 items, 0 to 48 (Andrews et al., 2009, p. 2).
#'   - IDAS-II: the sum of each scale's items, the metric of the norms in
#'     Watson et al. (2012, Table 2).
#'   - AUDIT: the sum of its 10 items, 0 to 40 (Bergman & Källmén, 2002,
#'     p. 245).
#'   - DUDIT: the sum of its 11 items, 0 to 44 (Berman et al., 2005, p. 23).
#'   - PHQ-15: the sum of the battery's 14 items, 0 to 28. The battery leaves
#'     out the item on menstrual problems, as Hinz et al. (2017, p. 28) did.
#'   - CAT-PD facets: the mean of each facet's items, 1 to 5, the metric of the
#'     CAT-PD scoring workbook the authors use. Reverse keys follow the IPIP
#'     CAT-PD-SF key.
#'   - CAPE positive symptoms: the mean of its 20 items, 1 to 4, the metric of
#'     the authors' REDCap version of the battery.
#'
#'   A reversed item scores the lowest plus the highest value of its answer
#'   set, minus the answer.
#'
#'   **Missing answers.** An `NA` answer is missing. A scale scores when the
#'   share of its items answered is at least `min_complete`. A sum is then
#'   prorated to the scale's full item count: the answered items' sum times
#'   the item count, divided by the number answered. A mean is the answered
#'   items' mean. A scale below `min_complete` scores `NA`. Each scale's
#'   `_complete` column holds its answered share, from 0 to 1. The battery's
#'   authors chose this rule and its 80% default. For the PHQ-15 it departs
#'   from Hinz et al. (2017, p. 28), who filled one missing item with the
#'   rounded mean of the other items and did not score a form with more
#'   missing. This function prorates the PHQ-15 like the other sums, without
#'   rounding.
#'
#'   **General Depression.** The IDAS-II General Depression scale is not
#'   scored, at the battery authors' request. Its row stays in
#'   [hitopdat_scales] as a record of the IDAS-II key.
#'
#'   **Errors.** An answer that is not one of its item's values in
#'   [hitopdat_choices] is an error of class `hitop_invalid_answer`, so a
#'   caller can catch this refusal by name. The message names up to five
#'   refused columns, each with its battery item number and its first refused
#'   value. A value of 1 on an AUDIT or DUDIT item answered 0, 2 or 4 is one
#'   such answer. Set an unanswered item to `NA`.
#'
#'   With `append = TRUE`, a column of `data` whose name this call would also
#'   produce is an error rather than an overwrite or a duplicated column: the
#'   message names every colliding column. Re-run with `append = FALSE` to
#'   return only the new columns, or drop the colliding columns from `data`
#'   first. The condition is classed `hitop_append_collision`, so a caller can
#'   catch this refusal by name.
#'
#' @return A \link[tibble]{tibble} with one column per scored scale,
#'   `prefix` plus the scale's `camelCase` name from [hitopdat_scales] in that
#'   table's order, then one `_complete` column per scale in the same order,
#'   after all original `data` columns (if requested).
#'
#' @references
#' Andrews, G., Kemp, A., Sunderland, M., Von Korff, M., & Ustun, T. B.
#' (2009). Normative data for the 12 item WHO Disability Assessment Schedule
#' 2.0. *PLoS ONE, 4*(12), e8343. \doi{10.1371/journal.pone.0008343}
#'
#' Bergman, H., & Källmén, H. (2002). Alcohol use among Swedes and a
#' psychometric evaluation of the Alcohol Use Disorders Identification Test.
#' *Alcohol & Alcoholism, 37*(3), 245-251.
#'
#' Berman, A. H., Bergman, H., Palmstierna, T., & Schlyter, F. (2005).
#' Evaluation of the Drug Use Disorders Identification Test (DUDIT) in
#' criminal justice and detoxification settings and in a Swedish population
#' sample. *European Addiction Research, 11*(1), 22-31.
#' \doi{10.1159/000081413}
#'
#' Hinz, A., Ernst, J., Glaesmer, H., Brähler, E., Rauscher, F. G.,
#' Petrowski, K., & Kocalevent, R.-D. (2017). Frequency of somatic symptoms
#' in the general population: Normative values for the Patient Health
#' Questionnaire-15 (PHQ-15). *Journal of Psychosomatic Research, 96*, 27-31.
#' \doi{10.1016/j.jpsychores.2016.12.017}
#'
#' Watson, D., O'Hara, M. W., Naragon-Gainey, K., Koffel, E., Chmielewski, M.,
#' Kotov, R., Stasik, S. M., & Ruggero, C. J. (2012). Development and
#' validation of new anxiety and bipolar symptom scales for an expanded
#' version of the IDAS (the IDAS-II). *Assessment, 19*(4), 399-420.
#' \doi{10.1177/1073191112449857}
#'
#' @seealso [hitopdat_items], [hitopdat_choices], [hitopdat_scales]
#'
#' @examples
#' # Five simulated respondents, each answer drawn from its item's answer set
#' set.seed(1)
#' answers <- lapply(hitopdat_items$Choice_Set, function(set) {
#'   values <- hitopdat_choices$Value[hitopdat_choices$Choice_Set == set]
#'   values[sample.int(length(values), 5, replace = TRUE)]
#' })
#' names(answers) <- sprintf("hdat_%03d", hitopdat_items$Item)
#' responses <- as.data.frame(answers)
#' score_hitopdat(responses, items = names(responses), append = FALSE)
#'
#' # Score only the scales with every item answered
#' responses$hdat_001[1] <- NA
#' scores <- score_hitopdat(responses, items = names(responses),
#'                          min_complete = 1, append = FALSE)
#' scores[, c("dat_whodas", "dat_whodas_complete")]
#'
#' @export
score_hitopdat <- function(
  data,
  items,
  prefix = "dat_",
  min_complete = 0.8,
  append = TRUE
) {
  validate_string(prefix, arg = "prefix")
  validate_share(min_complete, arg = "min_complete")
  validate_flag(append, arg = "append")

  n_items <- nrow(hitopdat_items)
  validate_data(data)
  validate_items(items, n = n_items)
  validate_item_uniqueness(items)
  validate_items_present(data, items)
  warn_item_order(items)
  validate_item_columns(data, items)

  ## Each battery item's answer values, in battery order.
  sets <- lapply(
    hitopdat_items$Choice_Set[match(seq_len(n_items), hitopdat_items$Item)],
    function(set) hitopdat_choices$Value[hitopdat_choices$Choice_Set == set]
  )
  values <- lapply(data[items], item_numbers)
  validate_answer_sets(
    data, items, values, sets,
    hint = "Use the values {.code hitopdat_choices} lists for each item's answer set, and {.code NA} for an unanswered item."
  )

  ## General Depression is not scored: the battery's authors dropped it
  ## (cairn/SOURCES.md, "The authors' joint answers"). Its row stays in
  ## `hitopdat_scales` as the IDAS-II key's record.
  scales <- hitopdat_scales[hitopdat_scales$camelCase != "generalDepression", ]
  out_names <- c(
    paste0(prefix, scales$camelCase),
    paste0(prefix, scales$camelCase, "_complete")
  )
  if (append) {
    validate_no_output_collision(out_names, data)
  }

  ## The CAT-PD facets and CAPE positive symptoms score as item means; the
  ## other measures as sums (each measure's source; see ?score_hitopdat).
  mean_measures <- c("CAT-PD", "CAPE")
  answers <- matrix(
    unlist(values, use.names = FALSE),
    nrow = nrow(data),
    ncol = n_items
  )
  ## An answered share counts as meeting `min_complete` within rounding error,
  ## so `min_complete = 0.1 * 7` (slightly above 0.7) still scores 7 of 10
  ## items. Shares of one scale differ by at least 1/20, far above this
  ## tolerance.
  floor_share <- min_complete - sqrt(.Machine$double.eps)

  scored <- lapply(seq_len(nrow(scales)), function(s) {
    item_set <- scales$itemNumbers[[s]]
    x <- answers[, item_set, drop = FALSE]
    for (r in scales$reverseNumbers[[s]]) {
      j <- match(r, item_set)
      x[, j] <- min(sets[[r]]) + max(sets[[r]]) - x[, j]
    }
    n <- length(item_set)
    answered <- rowSums(!is.na(x))
    total <- rowSums(x, na.rm = TRUE)
    share <- answered / n
    score <- if (scales$Measure[[s]] %in% mean_measures) {
      total / answered
    } else {
      total * n / answered
    }
    score[share < floor_share | answered == 0] <- NA_real_
    list(score = score, share = share)
  })
  out <- cbind(
    do.call(cbind, lapply(scored, `[[`, "score")),
    do.call(cbind, lapply(scored, `[[`, "share"))
  )
  ## A zero-row `data` leaves cbind() with a 0 x 0 result, so the dimensions
  ## are set before the names.
  dim(out) <- c(nrow(data), length(out_names))
  colnames(out) <- out_names

  if (append) {
    out <- cbind(data, out)
  }
  tibble::as_tibble(out)
}
