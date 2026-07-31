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
#'   `c(low, high)`. Any four-option coding is accepted and reconciled to the
#'   official `c(0, 3)` range before lookup; a coding with a different number of
#'   options is not convertible. See Details.
#' @param prefix The prefix [score_pid5()] applied to its output columns, used
#'   to match a score column back to its scale.
#' @param append Whether to return the input `data` with the conversion columns
#'   appended (`TRUE`, the default) or the conversion columns alone.
#'
#' @details For each named score column the tables are searched for the printed
#'   row whose raw score is **nearest** the observed value, and that one row's
#'   printed T score and printed percentile are returned. Nothing is
#'   interpolated: every returned number is a cell of Markon et al. (2024).
#'
#'   Markon et al. print the tables but give no instruction for reading them —
#'   no rounding, interpolation, or tie rule appears anywhere in the book — so
#'   the rules below are this package's, chosen and defended rather than quoted.
#'   They were settled by an independent review of the tables recorded in the
#'   package's development history as report RR02 (in the project repository,
#'   not the installed package).
#'
#'   * **Between printed rows.** The nearer row wins. Printed raws step by
#'     0.01-0.07 while attainable scores fall on much coarser grids (a 5-item
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
#'     returns that row's values, rather than an extrapolation. A score below
#'     the lowest returns whatever an observation *at* the lowest printed raw
#'     returns -- which, on the scales whose tables print a run of 0.00, is that
#'     run's highest-T row and not the table's first row, so the two agree
#'     instead of jumping. A message reports how many observations were capped at
#'     each end. This is reachable in ordinary data: `PRD` is a 22-item sum
#'     reaching 66 while its table stops at 55.
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
#'   four-option 0-3 coding. Data collected on a four-option coding that merely
#'   starts elsewhere -- 1-4, say -- carries the same information, so each score
#'   is reconciled to the official range before lookup and the conversion
#'   proceeds. A coding with a different *number* of options is a different
#'   metric: no mapping onto a four-option norm table is defined, so every
#'   conversion column is returned as `NA` with a warning, and the items must be
#'   recoded and rescored.
#'
#'   How much a shift moves a score depends on how the scale is computed, so the
#'   reconciliation is applied per scale rather than per item. No published
#'   source states these rules -- Markon et al. give the tables for the official
#'   coding only -- so, like the reading rules above, they are this package's,
#'   derived from each scale's own definition:
#'
#'   * **Item means** (the five domains, and the brief form's total) are
#'     reconciled by subtracting `srange[[1]]`. Shifting every item by a constant
#'     shifts their mean by the same constant.
#'   * **`PRD`** is a plain sum over its 22 items, so the same shift moves it by
#'     `srange[[1]]` times the number of items, which is what is subtracted. The
#'     item count is read from [pid_items] rather than assumed.
#'   * **`INC` and `INC-S`** are sums of *absolute differences within item
#'     pairs*. A constant added to both members of a pair cancels in the
#'     difference, so these are unchanged by a shift and nothing is subtracted.
#'   * **`ORS`** is a count of items answered at the top of the response range --
#'     [validity_pid5()] computes it by comparing each item to `srange[[2]]`
#'     rather than to a fixed value (`R/validity_pid5.R:153` in the package
#'     sources). A shift moves the top of the range along with the answers, so
#'     the same items are counted and the score is unchanged.
#'
#'   A shifted coding raises one warning per call naming which of the requested
#'   scales were adjusted and which were left alone; where every requested scale
#'   turns out to be coding-invariant, it says so rather than claiming an
#'   adjustment. The warning covers the scales the tables actually carry, so a
#'   request the tables cover nowhere raises the coverage message above instead
#'   and nothing about coding. The official coding is silent.
#'
#'   One consequence is worth stating plainly, because it can put two differently
#'   grounded numbers side by side in the same session: [validity_pid5()]'s
#'   published cut scores are **not** reconciled to a shifted coding. `PRD` and
#'   `SD-TD` are compared against fixed thresholds that assume 0-3 items, and
#'   `validity_pid5()` warns rather than adapting them. So a respondent scored on
#'   a 1-4 coding can receive a reconciled percentile from this function and, from
#'   `validity_pid5()`, a validity flag still read against the 0-3 thresholds.
#'   Adapting those cut scores is a separate, deliberately deferred question.
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
  ## prefix leaves the `pid_norms$scale` value to look up. The strip is a
  ## literal match, never a regex (D-026); a name the prefix does not start is
  ## left whole and falls through to the uncovered-scale report below.
  score_cols <- data[scores]
  col_names <- names(score_cols)
  scale_names <- strip_prefix(col_names, prefix)

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

  ## The tables are built on the official four-option 0-3 coding. A coding with
  ## a different *number* of options shares no metric with them and nothing is
  ## converted; a four-option coding merely shifted off 0-3 is reconciled per
  ## scale below (D-020, D-023).
  low <- srange[[1]]
  n_options <- srange[[2]] - srange[[1]] + 1
  usable <- isTRUE(all.equal(as.numeric(n_options), 4))
  shifted <- usable && !isTRUE(all.equal(as.numeric(low), 0))
  if (!usable) {
    cli::cli_warn(c(
      "!" = "{.arg srange} {.code c({srange[[1]]}, {srange[[2]]})} implies {n_options} response options, but the PID-5 normative tables are built on the official four-option 0-3 coding.",
      "i" = "No mapping is defined from a {n_options}-option metric onto a four-option norm table, so every {.code _t} and {.code _ptl} column is returned as {.code NA}.",
      "i" = "Recode the items to 0-3 and rescore before calling {.code norm_pid5()}."
    ))
  }

  ## Each scale's metric, and the amount a shifted coding adds to it.
  metric <- norm_metric(scale_names)
  shift <- norm_shift(scale_names, metric, low)

  ## Convert each column, collecting the capping counts as we go.
  n <- nrow(data)
  out <- list()
  ## Capping is tracked per observation, not per conversion: a respondent whose
  ## scores fall outside two scales' tables is one capped observation, not two.
  capped_low <- rep(FALSE, n)
  capped_high <- rep(FALSE, n)
  for (i in seq_along(col_names)) {
    s <- scale_names[[i]]
    x <- as.numeric(score_cols[[i]])
    if (usable && covered[[i]]) {
      ## Reconcile to the official range before lookup: a no-op on the official
      ## coding, and on any coding-invariant scale whatever the coding.
      x <- x - shift[[i]]
      got <- norm_convert(x, version, s)
      ends <- norm_capped(x, norm_rows(version, s))
      capped_low <- capped_low | ends$low
      capped_high <- capped_high | ends$high
    } else {
      got <- list(t = rep(NA_real_, n), ptl = rep(NA_real_, n))
    }
    if (has_t[[i]]) {
      out[[paste0(col_names[[i]], "_t")]] <- as.integer(got$t)
    }
    out[[paste0(col_names[[i]], "_ptl")]] <- as.numeric(got$ptl)
  }

  ## Report the reconciliation once per call, naming both groups, so a user on a
  ## shifted coding can see which of their scales moved and which did not. Only
  ## covered scales are listed: an uncovered one is returned as NA either way and
  ## is reported by the message below instead.
  if (shifted && any(covered)) {
    adjusted <- col_names[covered & metric != "invariant"]
    invariant <- col_names[covered & metric == "invariant"]
    ## Headline only claims a reconciliation when one happened: a request made
    ## entirely of coding-invariant scales needed none, and saying otherwise
    ## would describe work the function did not do.
    if (length(adjusted) > 0) {
      bullets <- c(
        "!" = "Scores coded {.code c({srange[[1]]}, {srange[[2]]})} were reconciled to the official 0-3 coding before lookup.",
        "*" = "Adjusted: {.val {adjusted}}."
      )
      if (length(invariant) > 0) {
        bullets <- c(
          bullets,
          "*" = "Left unchanged as coding-invariant: {.val {invariant}}."
        )
      }
    } else {
      bullets <- c(
        "!" = "Scores coded {.code c({srange[[1]]}, {srange[[2]]})} needed no reconciliation to the official 0-3 coding.",
        "*" = "Left unchanged as coding-invariant: {.val {invariant}}."
      )
    }
    bullets <- c(
      bullets,
      "i" = "{.code validity_pid5()}'s published cut scores are not reconciled, so a flag it raises on this data is still read against the 0-3 thresholds."
    )
    cli::cli_warn(bullets)
  }

  ## Report the scales the tables do not cover, once, naming them.
  if (usable && any(!covered)) {
    uncovered <- col_names[!covered]
    cli::cli_alert_warning(
      "{length(uncovered)} score column{?s} {?is/are} not covered by the {version} normative tables: {.val {uncovered}}."
    )
    cli::cli_alert_info(
      "Their {.code _t} and {.code _ptl} columns are returned as {.code NA}. {.code pid_norms} carries the five domain scales, the brief-form total, and the validity scales only."
    )
  }

  ## Report the observations that fell outside a printed range, per end.
  if (any(capped_low) || any(capped_high)) {
    n_low <- sum(capped_low)
    n_high <- sum(capped_high)
    cli::cli_alert_warning(
      "{n_low} observation{?s} below and {n_high} above the printed range were capped to the nearest printed row."
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
