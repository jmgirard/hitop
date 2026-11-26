#' Convert HiTOP-DAT Scale Scores to T Scores
#'
#' Converts the scale scores from [score_hitopdat()] to T scores against the
#' norms the battery's authors named, and flags high IDAS-II Suicidality and
#' CAT-PD Self-Harm T scores. The norms are in [hitopdat_norms].
#'
#' @param data A data frame holding the scale score columns that
#'   [score_hitopdat()] returns, named `prefix` plus each scale's `camelCase`
#'   name in [hitopdat_norms]. Other columns are ignored. Each score column
#'   must be numeric or logical; any other column, such as text, is an error
#'   of class `hitop_nonnumeric_items`.
#' @param prefix The prefix of the score column names, and of the columns this
#'   function adds. Use the `prefix` given to [score_hitopdat()]. Set it to an
#'   empty string `""` when the columns have no prefix. (default = `"dat_"`)
#' @param alert_t The T score at or above which an alert column is `TRUE`: one
#'   finite number. (default = `65`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#'
#' @details **T scores.** Each scale's T score is
#'   `50 + 10 * (score - M) / SD`, with the scale's `M` and `SD` from
#'   [hitopdat_norms], rounded to a whole number with halves rounded away
#'   from zero. Both of the authors' versions of the battery round T scores
#'   to whole numbers. A score of `NA` gives a T score of `NA`. The norms and
#'   their sources are:
#'
#'   - WHODAS: Andrews et al. (2009), Table 1, the total row and column.
#'   - The 18 IDAS-II scales: Watson et al. (2012), Table 2, the "Adults"
#'     column.
#'   - CAPE positive symptoms: Galletly et al. (2016), Table 1, the men's and
#'     women's means weighted by their n, with the SD printed for both,
#'     divided by the 20 items.
#'   - The 33 CAT-PD facets: the "Community" means and SDs of Len Simms's
#'     CAT-PD and IDAS scoring workbook. These norms are unpublished. The
#'     battery's authors use them, and this function follows the authors.
#'
#'   The AUDIT, DUDIT and PHQ-15 get no T score yet. Their norms are split by
#'   sex, and how to report them waits on the authors' answer. The IDAS-II
#'   General Depression scale is not scored (see [score_hitopdat()]).
#'
#'   **Alerts.** The battery alerts on two scales: IDAS-II Suicidality and
#'   CAT-PD Self-Harm. Each alert column is `TRUE` when its scale's rounded T
#'   score is at least `alert_t`, `FALSE` when it is lower, and `NA` when the
#'   T score is `NA`. The default of 65 is the alert point Katherine Jonas,
#'   the battery manual's first author, gave in her email to the package
#'   maintainer of 2026-10-15. She added that clinics may set another, for
#'   example to suit their state's laws.
#'
#'   **Errors.** A score column missing from `data` is an error that names
#'   it.
#'
#'   With `append = TRUE`, a column of `data` whose name this call would also
#'   produce is an error rather than an overwrite or a duplicated column: the
#'   message names every colliding column. Re-run with `append = FALSE` to
#'   return only the new columns, or drop the colliding columns from `data`
#'   first. The condition is classed `hitop_append_collision`, so a caller can
#'   catch this refusal by name.
#'
#' @return A \link[tibble]{tibble} with one `_t` column per scale in
#'   [hitopdat_norms] order (`prefix` plus the scale's `camelCase` name plus
#'   `"_t"`), then the logical columns `prefix` plus `"suicidality_alert"` and
#'   `prefix` plus `"selfHarm_alert"`, after all original `data` columns (if
#'   requested).
#'
#' @references
#' Andrews, G., Kemp, A., Sunderland, M., Von Korff, M., & Ustun, T. B.
#' (2009). Normative data for the 12 item WHO Disability Assessment Schedule
#' 2.0. *PLoS ONE, 4*(12), e8343. \doi{10.1371/journal.pone.0008343}
#'
#' Galletly, C., Clark, L., McFarlane, A., Searle, A., Sawyer, M., Sim, M.,
#' Baghurst, P., & van Hooff, M. (2016). Childhood lead exposure, childhood
#' trauma, substance use and subclinical psychotic experiences: A
#' longitudinal cohort study. *Psychiatry Research, 239*, 54-61.
#' \doi{10.1016/j.psychres.2016.02.066}
#'
#' Simms, L. J. (n.d.). *CAT-PD and IDAS scoring* \[Unpublished Excel
#' workbook\]. Sent to the package maintainer by K. G. Jonas, 2026-10-15.
#'
#' Watson, D., O'Hara, M. W., Naragon-Gainey, K., Koffel, E., Chmielewski, M.,
#' Kotov, R., Stasik, S. M., & Ruggero, C. J. (2012). Development and
#' validation of new anxiety and bipolar symptom scales for an expanded
#' version of the IDAS (the IDAS-II). *Assessment, 19*(4), 399-420.
#' \doi{10.1177/1073191112449857}
#'
#' @seealso [score_hitopdat()], [hitopdat_norms]
#'
#' @examples
#' # Five simulated respondents, each answer drawn from its item's answer set
#' set.seed(1)
#' answers <- lapply(hitopdat_items$Choice_Set, function(set) {
#'   values <- hitopdat_choices$Value[hitopdat_choices$Choice_Set == set]
#'   values[sample.int(length(values), 5, replace = TRUE)]
#' })
#' names(answers) <- sprintf("hdat_%03d", hitopdat_items$Item)
#' scores <- score_hitopdat(as.data.frame(answers), items = names(answers),
#'                          append = FALSE)
#' t_scores <- norm_hitopdat(scores, append = FALSE)
#' t_scores[, c("dat_suicidality_t", "dat_suicidality_alert",
#'              "dat_selfHarm_t", "dat_selfHarm_alert")]
#'
#' # A clinic that alerts at T 70
#' norm_hitopdat(scores, alert_t = 70, append = FALSE)$dat_selfHarm_alert
#'
#' @export
norm_hitopdat <- function(
  data,
  prefix = "dat_",
  alert_t = 65,
  append = TRUE
) {
  validate_data(data)
  validate_string(prefix, arg = "prefix")
  cli_assert(
    condition = is.numeric(alert_t) && length(alert_t) == 1L &&
      is.finite(alert_t),
    message = c(
      "The {.arg alert_t} argument must be a single finite number.",
      "x" = if (is.numeric(alert_t) && length(alert_t) == 1L) {
        "You supplied {alert_t}."
      } else {
        "You supplied {.cls {class(alert_t)}} of length {length(alert_t)}."
      }
    )
  )
  validate_flag(append, arg = "append")

  score_cols <- paste0(prefix, hitopdat_norms$camelCase)
  missing <- setdiff(score_cols, names(data))
  if (length(missing) > 0L) {
    n <- length(missing)
    cli::cli_abort(c(
      "{n} score column{?s} {?is/are} not found in {.arg data}.",
      "x" = "Not found: {.val {missing}}.",
      "i" = "Score the items with {.code score_hitopdat()} first, and set {.arg prefix} to the prefix it used."
    ))
  }
  scores <- data[score_cols]
  validate_numeric_columns(
    scores,
    headline = function(n) {
      cli::format_inline("{cli::qty(n)}The score column{?s} must be numeric.")
    },
    info = function(n) {
      cli::format_inline(
        "Use the scores {.code score_hitopdat()} returns, or convert {cli::qty(n)}{?it/them} to numbers first."
      )
    },
    class = "hitop_nonnumeric_items"
  )

  ## The authors alert on these two scales only (cairn/SOURCES.md, "The
  ## authors' REDCap version").
  alert_stems <- c("suicidality", "selfHarm")
  t_names <- paste0(score_cols, "_t")
  alert_names <- paste0(prefix, alert_stems, "_alert")
  if (append) {
    validate_no_output_collision(c(t_names, alert_names), data)
  }

  ## Whole-number T scores, halves away from zero, as both of the authors'
  ## versions round; the alerts compare the rounded T, as both versions do.
  t_scores <- lapply(seq_along(score_cols), function(s) {
    x <- as.numeric(scores[[s]])
    round_half_up(
      50 + 10 * (x - hitopdat_norms$M[[s]]) / hitopdat_norms$SD[[s]]
    )
  })
  names(t_scores) <- t_names
  alerts <- lapply(paste0(prefix, alert_stems, "_t"), function(nm) {
    t_scores[[nm]] >= alert_t
  })
  names(alerts) <- alert_names

  out <- tibble::as_tibble(c(t_scores, alerts))
  if (append) {
    out <- tibble::as_tibble(cbind(data, out))
  }
  out
}
