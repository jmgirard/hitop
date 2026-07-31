#' Convert PID-5 scale scores to normative T scores and percentiles
#'
#' Looks up scored PID-5 columns in the published normative tables shipped as
#' [pid_norms] and returns a T score and a percentile for each. Scores are
#' produced by [score_pid5()] and [validity_pid5()]; this function converts them
#' and never rescores.
#'
#' @param data A data frame containing scored PID-5 columns.
#' @param scores The score columns to convert, as column names or column
#'   positions (mirroring the `items` argument of [score_pid5()]).
#' @param version Which PID-5 version the scores came from: `"FULL"` (220
#'   items), `"SF"` (100 items), or `"BF"` (25 items). The normative tables
#'   differ by version.
#' @param srange The response range the items were coded on, as
#'   `c(low, high)`. Only the official `c(0, 3)` coding is supported here; see
#'   Details.
#' @param prefix The prefix [score_pid5()] applied to its output columns, used
#'   to match a score column back to its scale.
#' @param append Whether to return the input `data` with the conversion columns
#'   appended (`TRUE`, the default) or the conversion columns alone.
#'
#' @details For each named score column the tables are searched for the printed
#'   row whose raw score is **nearest** the observed value, and that one row's
#'   printed T score and printed percentile are returned. Nothing is
#'   interpolated: every returned number is a cell of Markon et al. (2024). The
#'   rules below were settled by an independent review of the tables, since the
#'   book prints the tables but no instruction for reading them.
#'
#'   * **Between printed rows.** The nearer row wins. Printed raws step by
#'     0.04-0.07 while attainable scores fall on much coarser grids (a 5-item
#'     brief-form domain mean can only be a multiple of 0.2), so most lookups
#'     land between rows.
#'   * **Ties.** Where two or more rows are equally near -- a raw printed in
#'     several rows, or a value exactly midway between two rows -- the row whose
#'     T score is nearest 50 is returned. The four validity scales carry no T
#'     score, so a tie there returns the row whose percentile is nearest 0.50.
#'   * **Scores of 0.** Each domain table prints raw 0.00 across a run of low T
#'     scores, because the linear T the book tabulated predicts a negative raw
#'     there and 0.00 is printed instead. The tie rule returns the run's highest
#'     T, the one row of the run that renders an attainable score. Its printed
#'     percentile is positive on some scales and 0.00 on others; that asymmetry
#'     is a property of the published tables, not of this function.
#'   * **Scores outside the table.** A score above the highest printed row
#'     returns that row's values and a score below the lowest returns the
#'     lowest's, rather than an extrapolation; a message reports how many
#'     observations were capped at each end. This is reachable in ordinary data:
#'     `PRD` is a 22-item sum reaching 66 while its table stops at 55.
#'   * **Unattainable printed rows.** Five domain tables print rows above the
#'     3.00 ceiling a 0-3 item mean can reach, so the top of those T ranges
#'     cannot be attained. A maximum score returns T = 84 (brief-form negative
#'     affectivity), 87 (brief-form detachment), 93 (brief-form disinhibition),
#'     87 (full-form negative affectivity), or 85 (short-form negative
#'     affectivity) -- each at percentile 1.00. Nothing is wrong with such data
#'     and no message fires.
#'   * **Comparison tolerance.** All comparisons use an absolute tolerance of
#'     1e-8, so that scores on grids with no exact binary representation (a
#'     short-form domain mean is a twelfth) match the printed 2-decimal raws as
#'     intended.
#'
#'   Columns the tables do not cover for the requested `version` -- the 25
#'   facets, for instance -- return `NA` in both conversion columns with a
#'   message naming them. An `NA` score returns `NA`.
#'
#'   **Response coding.** The normative tables are built on the official
#'   four-option 0-3 coding, so any other `srange` currently returns `NA` in
#'   every conversion column with a warning. Reconciling a shifted coding (1-4,
#'   say) to the official range is planned; until then, recode items to 0-3
#'   before scoring. Note that [validity_pid5()]'s published cut scores are not
#'   adapted to other codings either.
#'
#' @return A \link[tibble]{tibble} with a `_t` column for every converted scale
#'   whose normative rows carry a T score (the five domains, plus the brief
#'   form's total) and a `_ptl` column for every converted scale, alongside all
#'   original `data` columns if requested. The four validity scales
#'   (`INC`, `INCS`, `ORS`, `PRD`) are distributed as percentiles only and get
#'   no `_t` column.
#'
#' @references Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
#'   *Understanding the Personality Inventory for DSM-5 (PID-5).* American
#'   Psychiatric Association Publishing. The normative tables in [pid_norms],
#'   Appendix "Normative Score Distributions" (pp. 113-219), are the source for
#'   every value this function returns.
#'
#' @examples
#' # Score the brief form, then convert its domains and total
#' scored <- score_pid5(sim_pid5bf, items = 1:25, version = "BF")
#' norm_pid5(scored, scores = paste0("pid_", c("detachment", "total")),
#'           version = "BF", append = FALSE)
#'
#' @export
norm_pid5 <- function(
  data,
  scores,
  version = c("FULL", "SF", "BF"),
  srange = c(0, 3),
  prefix = "pid_",
  append = TRUE
) {
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))
  validate_data(data)
  validate_scales(scores)
  validate_items_present(data, scores)
  validate_range(srange)
  stopifnot(rlang::is_string(prefix))
  stopifnot(rlang::is_bool(append))

  ## Extract the score columns and recover each one's scale name: the output
  ## naming of score_pid5() is prefix + the camelCase scale, so stripping the
  ## prefix leaves the `pid_norms$scale` value to look up.
  score_cols <- data[scores]
  col_names <- names(score_cols)
  scale_names <- sub(paste0("^", prefix), "", col_names)

  ## Which requested scales the tables cover for this version. An uncovered
  ## scale still gets both columns, filled with NA (never silently absent).
  covered <- vapply(scale_names, norm_covers, logical(1), version = version)
  ## Only a *covered* scale whose printed rows carry no T score -- the four
  ## validity scales, distributed as percentiles only -- goes without a `_t`
  ## column. An uncovered scale gets both columns, filled with NA, so a missing
  ## conversion is visible in the output rather than absent from it.
  has_t <- vapply(
    scale_names,
    function(s) !norm_covers(version, s) || any(!is.na(norm_rows(version, s)$tscore)),
    logical(1)
  )

  ## The tables are built on the official 0-3 coding. Any other coding is
  ## refused outright rather than converted on a metric the tables do not share.
  official <- isTRUE(all.equal(as.numeric(srange), c(0, 3)))
  if (!official) {
    cli::cli_alert_warning(
      "Scores coded {.code c({srange[[1]]}, {srange[[2]]})} cannot be converted: the PID-5 normative tables are built on the official 0-3 response coding."
    )
    cli::cli_alert_info(
      "Recode the items to 0-3 and rescore before calling {.code norm_pid5()}."
    )
  }

  ## Convert each column, collecting the capping counts as we go.
  n <- nrow(data)
  out <- list()
  capped <- c(low = 0, high = 0)
  for (i in seq_along(col_names)) {
    s <- scale_names[[i]]
    x <- as.numeric(score_cols[[i]])
    if (official && covered[[i]]) {
      got <- norm_convert(x, version, s)
      capped <- capped + norm_capped(x, norm_rows(version, s))
    } else {
      got <- list(t = rep(NA_real_, n), ptl = rep(NA_real_, n))
    }
    if (has_t[[i]]) {
      out[[paste0(col_names[[i]], "_t")]] <- as.integer(got$t)
    }
    out[[paste0(col_names[[i]], "_ptl")]] <- as.numeric(got$ptl)
  }

  ## Report the scales the tables do not cover, once, naming them.
  if (official && any(!covered)) {
    uncovered <- col_names[!covered]
    cli::cli_alert_warning(
      "{length(uncovered)} score column{?s} {?is/are} not covered by the {version} normative tables: {.val {uncovered}}."
    )
    cli::cli_alert_info(
      "Their {.code _t} and {.code _ptl} columns are returned as {.code NA}. {.code pid_norms} carries the five domain scales, the brief-form total, and the validity scales only."
    )
  }

  ## Report the observations that fell outside a printed range, per end.
  if (any(capped > 0)) {
    cli::cli_alert_warning(
      "{capped[['low']]} observation{?s} below and {capped[['high']]} above the printed range were capped to the nearest printed row."
    )
    cli::cli_alert_info(
      "A capped score's T and percentile are the end row's printed values, not an extrapolation."
    )
  }

  out <- as.data.frame(out, check.names = FALSE)
  if (append == TRUE) {
    out <- cbind(data, out)
  }
  tibble::as_tibble(out)
}
